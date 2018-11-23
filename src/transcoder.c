/* transcoder.c                                    -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: $
 */
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/transcoder.h"
#include "sagittarius/codec.h"
#include "sagittarius/port.h"
#include "sagittarius/symbol.h"
#include "sagittarius/builtin-symbols.h"
#include "sagittarius/core.h"
#include "sagittarius/number.h"
#include "sagittarius/vm.h"
#include "sagittarius/error.h"

#include "shortnames.incl"

/* Linux(maybe only ubuntu) has this stupid macro. so we need to undefine it. */
#ifdef putc
#undef putc
#endif
#ifdef getc
#undef getc
#endif

static SgClass *trans_cpl[] = {
  SG_CLASS_TRANSCODER,
  SG_CLASS_TOP,
  NULL
};

static void transcoder_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgTranscoder *t = SG_TRANSCODER(obj);
  SG_PORT_LOCK_WRITE(port);
  Sg_PutuzUnsafe(port, UC("#<transcoder "));
  Sg_PutsUnsafe(port, SG_CODEC_NAME(SG_TRANSCODER_CODEC(t)));
  Sg_PutcUnsafe(port, ' ');
  switch (t->eolStyle) {
  case LF:     Sg_PutuzUnsafe(port, UC("lf")); break;
  case CR:     Sg_PutuzUnsafe(port, UC("cr")); break;
  case NEL:    Sg_PutuzUnsafe(port, UC("nel")); break;
  case LS:     Sg_PutuzUnsafe(port, UC("ls")); break;
  case CRNEL:  Sg_PutuzUnsafe(port, UC("crnel")); break;
  case CRLF:   Sg_PutuzUnsafe(port, UC("crlf")); break;
  case E_NONE: Sg_PutuzUnsafe(port, UC("none")); break;
  }
  Sg_PutcUnsafe(port, ' ');
  
  switch (t->mode) {
  case SG_RAISE_ERROR:   Sg_PutuzUnsafe(port, UC("raise")); break;
  case SG_REPLACE_ERROR: Sg_PutuzUnsafe(port, UC("replace")); break;
  case SG_IGNORE_ERROR:  Sg_PutuzUnsafe(port, UC("ignore")); break;
  }
  Sg_PutcUnsafe(port, '>');
  SG_PORT_UNLOCK_WRITE(port);
}

SG_DEFINE_BUILTIN_CLASS(Sg_TranscoderClass,
			transcoder_print, NULL, NULL, NULL, trans_cpl);


static SgObject get_mode(int mode)
{
  switch (mode) {
  case SG_REPLACE_ERROR:
    return SG_SYMBOL_REPLACE;
  case SG_IGNORE_ERROR:
    return SG_SYMBOL_IGNORE;
  case SG_RAISE_ERROR:
    return SG_SYMBOL_RAISE;
  default:
    Sg_Panic("unknonw error handling type %d", mode);
  }
  return SG_UNDEF;		/* dummy */
}

#define SRC_PORT(src_port, port)				\
  do { src_port = SG_TRANSCODED_PORT_PORT(port); } while (0)

#define PORT_MARK(mark, src_port, port)					\
  do {									\
    SRC_PORT(src_port, port);						\
    if (SG_BINARY_PORTP(src_port)) {					\
      (mark) = SG_PORT(src_port)->position;				\
    } else {								\
      Sg_Panic("[internal error] transcoder got textual port");		\
      (mark) = -1;		/* dummy */				\
    }									\
  } while (0);

#define SAVE_CLOSED(port, save)				\
  do {							\
    (save) = SG_PORT(port)->closed;			\
    SG_PORT(port)->closed = SG_PORT_OPEN;		\
} while (0)
#define RESTORE_CLOSED(port, prev)			\
  do { SG_PORT(port)->closed = (prev); } while (0)

static SgChar get_char_internal(SgObject self, SgPort *port)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  int64_t mark;
  SgPort *src_port;
  /* we know here only binary or custom binary port can reach */
  PORT_MARK(mark, src_port, port);

  /* if the port has not read anything yet, then check bom */
  if (tran->codec->type == SG_BUILTIN_CODEC) {
    return SG_CODEC_BUILTIN(tran->codec)->getc(tran->codec, src_port,
					       tran->mode, (mark == 0));
  } else {
    SgObject c = SG_UNDEF;
    volatile int prev = TRUE;
    /* inside of custom codec, it needs to read bytes so port must be opened. */
    SAVE_CLOSED(src_port, prev);
    /* port must be re-closed no matter what happened */
    SG_UNWIND_PROTECT {
      c = Sg_Apply4(SG_CODEC_CUSTOM(tran->codec)->getc,
		    src_port, get_mode(tran->mode), 
		    SG_MAKE_BOOL(mark == 0),
		    SG_CODEC_CUSTOM(tran->codec)->data);
    } SG_WHEN_ERROR {
      RESTORE_CLOSED(src_port, prev);
      SG_NEXT_HANDLER;
    } SG_END_PROTECT;
    /* restore */
    RESTORE_CLOSED(src_port, prev);
    if (SG_CHARP(c)) {
      return SG_CHAR_VALUE(c);
    } else if (SG_EOFP(c)) {
      return EOF;
    } else {
      Sg_Error(UC("codec returned invalid object %S"), c);
    }
  }
  return -1;			/* dummy */
}

SgChar Sg_TranscoderGetc(SgObject self, SgPort *port)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  SgChar c = get_char_internal(self, port);
  if (tran->eolStyle == E_NONE) {
    if (c == LF) {
      SG_TRANSCODED_PORT_LINE_NO(port)++;
    }
    return c;
  }
  switch (c) {
  case LF:
  case NEL:
  case LS:
    SG_TRANSCODED_PORT_LINE_NO(port)++;
    return LF;
  case CR: {
    SgChar c2 = get_char_internal(self, port);
    SG_TRANSCODED_PORT_LINE_NO(port)++;
    switch (c2) {
    case LF:
    case NEL:
      return LF;
    default:
      /* call ports ungetc */
      Sg_UngetcUnsafe(port, c2);
      return LF;
    }
  }
  default:
    return c;
  }
}

/* 
   This must be
   +---+---+---+----+----+---+
   | a | b | c | \r | \n | d | ...
   +---+---+---+----+----+---+
   like this
   +---+---+---+----+---+
   | a | b | c | \n | d | ...
   +---+---+---+----+---+
   We want to share the memory.
 */
static int resolve_eol(SgPort *port, SgTranscoder *tran, SgChar *dst,
		       SgChar  *src, int64_t count)
{
  int64_t i;
  int diff = 0;
  SgChar *end = src + count;
  for (i = 0; i < count; i++) {
    /* check overflow*/
    if (src >= end) break;

    if (tran->eolStyle == E_NONE) {
      if (*src == LF) {
	SG_TRANSCODED_PORT_LINE_NO(port)++;
      }
      *dst++ = *src++;
      continue;
    }
    switch (*src) {
    case LF: case NEL: case LS:
      SG_TRANSCODED_PORT_LINE_NO(port)++;
      *dst++ = LF;
      src++;
      break;
    case CR: {
      SG_TRANSCODED_PORT_LINE_NO(port)++;
      switch (*++src) {
      case LF: case NEL:
	diff++; count++; src++;	/* 2 -> 1 */
	/* fall through */
      default:
	*dst++ = LF;
	break;
      }
      break;
    }
    default:
      *dst++ = *src++;
      break;
    }
  }
  return diff;
}

int64_t Sg_TranscoderRead(SgObject self, SgPort *port, 
			  SgChar *buf, int64_t size)
{
  volatile int64_t read = 0;
  int64_t mark;
  int diff;
  SgTranscoder *trans = SG_TRANSCODER(self);
  SgChar c;
  SgPort *src_port;
  /* we need to resolve a thing, begin flag */
  PORT_MARK(mark, src_port, port);
  if (mark == 0) {
    /* yes we need to make it not begein */
    c = Sg_TranscoderGetc(self, port);
    if (c == EOF) return 0;
    buf[read++] = c;
  }
  if (read == size) return read;

  /* now we can finally read from port */
 retry:
  if (trans->codec->type == SG_BUILTIN_CODEC) {
    int64_t r;
    r = SG_CODEC_BUILTIN(trans->codec)->readc(trans->codec, src_port,
					      buf+read, size-read,
					      trans->mode, FALSE);
    /* ok length 0 is EOF, to avoid infinite loop */
    if (r == 0) goto end;
    diff = resolve_eol(port, trans, buf+read, buf+read, r);
    read += r - diff;
  } else {
    SgObject r = SG_UNDEF;
    volatile int prev = TRUE;
    SAVE_CLOSED(src_port, prev);
    SG_UNWIND_PROTECT {
      r = Sg_Apply4(SG_CODEC_CUSTOM(trans->codec)->readc,
		    src_port, SG_MAKE_INT(size-read),
		    get_mode(trans->mode),
		    SG_CODEC_CUSTOM(trans->codec)->data);
    } SG_WHEN_ERROR {
      RESTORE_CLOSED(src_port, prev);
      SG_NEXT_HANDLER;
    } SG_END_PROTECT;
    RESTORE_CLOSED(src_port, prev);
    if (!SG_STRINGP(r)) {
      Sg_Error(UC("codec returned invalid object %S"), r);
      return -1;		/* dummy */
    } else {
      if (SG_STRING_SIZE(r) == 0) goto end; /* the same as builtin */
      diff = resolve_eol(port, trans, buf+read, SG_STRING_VALUE(r),
			 SG_STRING_SIZE(r));
      read += SG_STRING_SIZE(r) - diff;
    }
  }
  /* resolve_eol compress the size */
  if (read != size) goto retry;
 end:
  return read;
}

#define dispatch_putchar(codec, p, c, mode)				\
  do {									\
    if ((codec)->type == SG_BUILTIN_CODEC) {				\
      SG_CODEC_BUILTIN(codec)->putc(codec, (p), (c), mode);		\
    } else {								\
      volatile int __prev = TRUE;					\
      SAVE_CLOSED(p, __prev);						\
      SG_UNWIND_PROTECT {						\
	Sg_Apply4(SG_CODEC_CUSTOM(codec)->putc, (p), SG_MAKE_CHAR(c),	\
		  get_mode(mode),	SG_CODEC_CUSTOM(codec)->data);	\
      } SG_WHEN_ERROR {							\
	RESTORE_CLOSED(p, __prev);					\
	SG_NEXT_HANDLER;						\
      }	SG_END_PROTECT;							\
      RESTORE_CLOSED(p, __prev);					\
    }									\
  } while (0)

void Sg_TranscoderPutc(SgObject self, SgPort *tport, SgChar c)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  SgPort *port;
  SRC_PORT(port, tport);
  if (c == LF) {
    switch (tran->eolStyle) {
    case LF:
    case CR:
    case NEL:
    case LS:
      dispatch_putchar(tran->codec, port, tran->eolStyle, tran->mode);
      return;
    case E_NONE:
      dispatch_putchar(tran->codec, port, c, tran->mode);
      return;
    case CRLF:
      dispatch_putchar(tran->codec, port, CR, tran->mode);
      dispatch_putchar(tran->codec, port, LF, tran->mode);
      return;
    case CRNEL:
      dispatch_putchar(tran->codec, port, CR, tran->mode);
      dispatch_putchar(tran->codec, port, NEL, tran->mode);
      return;
    }
  } else {
    dispatch_putchar(tran->codec, port, c, tran->mode);
  }
}

#define dispatch_putstring(codec, p, s, c, mode, new_s)			\
  do {									\
    if ((codec)->type == SG_BUILTIN_CODEC) {				\
      return SG_CODEC_BUILTIN(codec)->writec(codec, (p), (s), (c), mode); \
    } else {								\
      SgObject i = SG_MAKE_INT(0);					\
      volatile int __prev = TRUE;					\
      if (!(new_s)) {							\
	(new_s) = Sg_String(s);						\
      }									\
      SAVE_CLOSED(p, __prev);						\
      SG_UNWIND_PROTECT {						\
	i = Sg_Apply4(SG_CODEC_CUSTOM(codec)->writec, (p), (new_s),	\
		      get_mode(mode), SG_CODEC_CUSTOM(codec)->data);	\
      } SG_WHEN_ERROR {							\
	RESTORE_CLOSED(p, __prev);					\
	SG_NEXT_HANDLER;						\
      } SG_END_PROTECT;							\
      RESTORE_CLOSED(p, __prev);					\
      return Sg_GetIntegerS64Clamp(i, SG_CLAMP_NONE, NULL);		\
    }									\
  } while (0)

int64_t Sg_TranscoderWrite(SgObject self, SgPort *tport,
			   SgChar *s, int64_t count)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  volatile SgObject new_s = NULL;
  SgPort *port;
  SRC_PORT(port, tport);
  /* a bit ugly and slow solution. create string buffer and construct
     proper linefeed there.
   */
  if (tran->eolStyle != E_NONE) {
    int64_t i;
    SgPort *out;
    SgStringPort tp;
    Sg_InitStringOutputPort(&tp, -1);
    out = SG_PORT(&tp);
    for (i = 0; i < count; i++) {
      SgChar c = s[i];
      if (c == LF) {
	switch (tran->eolStyle) {
	case LF: case NEL: case LS: case CR:
	  Sg_PutcUnsafe(out, tran->eolStyle);
	  break;
	case E_NONE:
	  Sg_PutcUnsafe(out, c);
	  break;
	case CRLF:
	  Sg_PutcUnsafe(out, CR);
	  Sg_PutcUnsafe(out, LF);
	  break;
	case CRNEL:
	  Sg_PutcUnsafe(out, CR);
	  Sg_PutcUnsafe(out, NEL);
	  break;
	}
      } else {
	Sg_PutcUnsafe(out, c);
      }
    }
    new_s = Sg_GetStringFromStringPort(&tp);
    SG_CLEAN_STRING_PORT(&tp);
    s = SG_STRING_VALUE(new_s);
    count = SG_STRING_SIZE(new_s);
  }
  dispatch_putstring(SG_TRANSCODER_CODEC(self), port, s, count,
		     SG_TRANSCODER_MODE(self), new_s);
}

SgObject Sg_MakeTranscoder(SgCodec *codec, SgEolStyle eolStyle,
			   SgErrorHandlingMode mode)
{
  SgTranscoder *z = SG_NEW(SgTranscoder);
  return Sg_InitTranscoder(z, codec, eolStyle, mode);
}

SgObject Sg_InitTranscoder(SgTranscoder *transcoder,
			   SgCodec *codec, SgEolStyle eolStyle,
			   SgErrorHandlingMode mode)
{
  SG_SET_CLASS(transcoder, SG_CLASS_TRANSCODER);
  transcoder->codec = codec;
  transcoder->eolStyle = eolStyle;
  transcoder->mode = mode;

  return SG_OBJ(transcoder);
}

/* compatible with ASCII */
SgObject Sg_MakeNativeTranscoder()
{
  static SgObject trans = NULL;
  if (trans == NULL) {
    trans = Sg_MakeTranscoder(Sg_MakeUtf8Codec(), Sg_NativeEol(), 
			      SG_REPLACE_ERROR);
  }
  return trans;
}


/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
