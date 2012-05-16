/* -*- C -*- */
/*
 * transcoder.c
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
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

#define DEFAULT_BUFFER_SIZE (128 * sizeof(SgChar))
#define INCREASE_SIZE       (32 * sizeof(SgChar))
#define SIZE2POS(sz)        ((int)(sz / sizeof(SgChar)))

/* Linux(maybe only ubuntu) has this stupid macro. so we need to undefine it. */
#ifdef putc
#undef putc
#endif
#ifdef getc
#undef getc
#endif

static SgClass *trans_cpl[] = {
  SG_CLASS_TRANSCODER,
  NULL
};

static void transcoder_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgTranscoder *t = SG_TRANSCODER(obj);
  SG_PORT_LOCK(port);
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
  SG_PORT_UNLOCK(port);
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


static void unget_char(SgObject self, SgChar c)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  if (EOF == c) return;
  if (tran->buffer == NULL) {
    /* manual alloc */
    tran->bufferSize = DEFAULT_BUFFER_SIZE;
    tran->buffer = SG_NEW_ATOMIC2(SgChar*, sizeof(SgChar) * tran->bufferSize);
    tran->bufferPosition = 0;
  }
  
  if (tran->bufferPosition == SIZE2POS(tran->bufferSize)) {
    int nextBufferSize = tran->bufferSize + INCREASE_SIZE;
    SgChar *tmp = SG_NEW_ATOMIC2(SgChar*, sizeof(SgChar) * nextBufferSize);
    if (tmp == NULL) {
      /** @todo error handling */
      exit(-1);
    }
    memcpy(tran->buffer, tmp, tran->bufferSize);
    tran->buffer = NULL; /* maybe gc frendliness? */
    tran->buffer = tmp;
    tran->bufferSize = nextBufferSize;
  }
  ASSERT(tran->buffer != NULL);
  ASSERT(tran->bufferSize != 0);
  ASSERT(SIZE2POS(tran->bufferSize) > tran->bufferPosition);
  tran->buffer[tran->bufferPosition++] = c;
  if (c == LF) {
    tran->lineNo--;
  }
}


static SgChar get_char_internal(SgObject self, SgPort *port)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  if (tran->isBegin) {
    tran->isBegin = FALSE;
    if (tran->codec->type == SG_BUILTIN_CODEC) {
      return SG_CODEC_BUILTIN(tran->codec)->getc(tran->codec, port,
						 tran->mode, TRUE);
    } else {
      SgObject c = Sg_Apply4(SG_CODEC_CUSTOM(tran->codec)->getc,
			     port, get_mode(tran->mode), SG_TRUE,
			     SG_CODEC_CUSTOM(tran->codec)->data);
      if (SG_CHARP(c)) {
	return SG_CHAR_VALUE(c);
      } else if (SG_EOFP(c)) {
	return EOF;
      } else {
	Sg_Error(UC("codec returned invalid object %S"), c);
      }
    }
  } else {
    SgChar c;
    if (tran->buffer == NULL || tran->bufferPosition == 0) {
      if (tran->codec->type == SG_BUILTIN_CODEC) {
	c = SG_CODEC_BUILTIN(tran->codec)->getc(tran->codec, port, 
						tran->mode, FALSE);
      } else {
	SgObject co = Sg_Apply4(SG_CODEC_CUSTOM(tran->codec)->getc,
				port, get_mode(tran->mode), SG_FALSE,
				SG_CODEC_CUSTOM(tran->codec)->data);
	if (SG_CHARP(co)) {
	  c =  SG_CHAR_VALUE(co);
	} else if (SG_EOFP(co)) {
	  return EOF;
	} else {
	  Sg_Error(UC("codec returned invalid object %S"), co);
	  return -1;		/* dummy */
	}
      }
    } else {
      c = tran->buffer[tran->bufferPosition - 1];
      tran->bufferPosition--;
    }
    return c;
  }
  return -1;			/* dummy */
}

static SgChar get_char(SgObject self, SgPort *port)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  SgChar c = get_char_internal(self, port);
  if (tran->eolStyle == E_NONE) {
    if (c == LF) {
      tran->lineNo++;
    }
    return c;
  }
  switch (c) {
  case LF:
  case NEL:
  case LS:
    tran->lineNo++;
    return LF;
  case CR: {
    SgChar c2 = get_char_internal(self, port);
    tran->lineNo++;
    switch (c2) {
    case LF:
    case NEL:
      return LF;
    default:
      unget_char(self, c2);
      return LF;
    }
  }
  default:
    return c;
  }
}

static int64_t get_string(SgObject self, SgPort *port,
			  SgChar *buf, int64_t size)
{
  int64_t i;
  for (i = 0; i < size; i++) {
    SgChar c = get_char(self, port);
    if (c == EOF) return i;
    buf[i] = c;
  }
  return i;
}

#define dispatch_putchar(codec, p, c, mode)				\
  do {									\
    if ((codec)->type == SG_BUILTIN_CODEC) {				\
      SG_CODEC_BUILTIN(codec)->putc(codec, (p), (c), mode);		\
    } else {								\
      Sg_Apply4(SG_CODEC_CUSTOM(codec)->putc, (p), SG_MAKE_CHAR(c),	\
		get_mode(mode),	SG_CODEC_CUSTOM(codec)->data);		\
    }									\
  } while (0)


static void put_char(SgObject self, SgPort *port, SgChar c)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  if (tran->bufferPosition != 0) {
    tran->bufferPosition--;
  }
  if (tran->eolStyle == E_NONE) {
    dispatch_putchar(tran->codec, port, c, tran->mode);
    return;
  } else if (c == LF) {
    switch (tran->eolStyle) {
    case LF:
    case CR:
    case NEL:
    case LS:
      dispatch_putchar(tran->codec, port, tran->eolStyle, tran->mode);
      break;
    case E_NONE:
      dispatch_putchar(tran->codec, port, c, tran->mode);
      break;
    case CRLF:
      dispatch_putchar(tran->codec, port, CR, tran->mode);
      dispatch_putchar(tran->codec, port, LF, tran->mode);
      break;
    case CRNEL:
      dispatch_putchar(tran->codec, port, CR, tran->mode);
      dispatch_putchar(tran->codec, port, NEL, tran->mode);
      break;
    }
  } else {
    dispatch_putchar(tran->codec, port, c, tran->mode);
  }
}

#define dispatch_putstring(codec, p, s, mode)				\
  do {									\
    if ((codec)->type == SG_BUILTIN_CODEC) {				\
      return SG_CODEC_BUILTIN(codec)->writec(codec, (p), (s), mode);	\
    } else {								\
      SgObject i = Sg_Apply4(SG_CODEC_CUSTOM(codec)->writec, (p), (s),	\
			     get_mode(mode), SG_CODEC_CUSTOM(codec)->data); \
      return Sg_GetIntegerS64Clamp(i, SG_CLAMP_NONE, NULL);		\
    }									\
  } while (0)



static int64_t put_string(SgObject self, SgPort *port, SgString *s)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  int i;
  if (tran->bufferPosition != 0) {
    tran->bufferPosition--;
  }
  /* a bit ugly and slow solution. create string buffer and construct
     proper linefeed there.
   */
  if (tran->eolStyle != E_NONE) {
    SgObject out = Sg_MakeStringOutputPort(-1);
    for (i = 0; i < SG_STRING_SIZE(s); i++) {
      SgChar c = SG_STRING_VALUE_AT(s, i);
      if (c == LF) {
	switch (tran->eolStyle) {
	case LF:
	case CR:
	case NEL:
	case LS:
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
    s = Sg_GetStringFromStringPort(out);
  }
  dispatch_putstring(SG_TRANSCODER_CODEC(self), port, s,
		     SG_TRANSCODER_MODE(self));
}

SgObject Sg_MakeTranscoder(SgCodec *codec, EolStyle eolStyle,
			   ErrorHandlingMode mode)
{
  SgTranscoder *z = SG_NEW(SgTranscoder);
  SG_SET_CLASS(z, SG_CLASS_TRANSCODER);
  z->codec = codec;
  z->eolStyle = eolStyle;
  z->mode = mode;
  z->lineNo = 1;
  z->buffer = NULL;
  z->bufferSize = 0;
  z->isBegin = TRUE;
  z->getChar = get_char;
  z->getString = get_string;
  z->unGetChar = unget_char;
  z->putChar = put_char;
  z->putString = put_string;
  return SG_OBJ(z);
}

/* compatible with ASCII */
SgObject Sg_MakeNativeTranscoder()
{
  return Sg_MakeTranscoder(Sg_MakeUtf8Codec(), Sg_NativeEol(), SG_RAISE_ERROR);
}


/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
