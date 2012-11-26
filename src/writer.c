/* -*- C -*- */
/*
 * writer.c
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
#include <stdarg.h>
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/writer.h"
#include "sagittarius/port.h"
#include "sagittarius/generic.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/core.h"
#include "sagittarius/clos.h"
#include "sagittarius/file.h"
#include "sagittarius/pair.h"
#include "sagittarius/keyword.h"
#include "sagittarius/string.h"
#include "sagittarius/number.h"
#include "sagittarius/error.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/identifier.h"
#include "sagittarius/library.h"
#include "sagittarius/vector.h"
#include "sagittarius/symbol.h"
#include "sagittarius/record.h"
#include "sagittarius/vm.h"
#include "sagittarius/unicode.h"
#include "sagittarius/builtin-symbols.h"

#define WRITE_LIMITED  0x10
#define WRITE_CIRCULAR 0x20

/* for convenient */
static void format_write(SgObject obj, SgPort *port, SgWriteContext *ctx,
			 int sharedp);
static void write_ss_rec(SgObject obj, SgPort *port, SgWriteContext *ctx);
static void write_ss(SgObject obj, SgPort *port, SgWriteContext *ctx);
static void write_object(SgObject obj, SgPort *port, SgWriteContext *ctx);
static SgObject write_object_fallback(SgObject *args, int nargs, SgGeneric *gf);

SG_DEFINE_GENERIC(Sg_GenericWriteObject, write_object_fallback, NULL);

/* 
   Main ans child thread must have different stack size
   (At least on Cygwin it does)
   Main  1M
   Child 64KB
 */
#define MAIN_THREAD_STACK_SIZE_LIMIT  0x100000
#define CHILD_THREAD_STACK_SIZE_LIMIT 0x10000


#define SET_STACK_SIZE(ctx)				\
  do {							\
    if (Sg_MainThreadP())				\
      (ctx)->stackSize = MAIN_THREAD_STACK_SIZE_LIMIT;	\
    else						\
      (ctx)->stackSize = CHILD_THREAD_STACK_SIZE_LIMIT;	\
  } while (0)
    

void Sg_Write(SgObject obj, SgObject p, int mode)
{
  SgWriteContext ctx;
  SgPort *port;

  if (!SG_OUTPORTP(p) && !SG_INOUTPORTP(p)) {
    Sg_Error(UC("output port required, but got %S"), p);
  }
  if (!SG_TEXTUAL_PORTP(p)) {
    /* for now I asuume it's a binary port. */
    SgTranscoder *trans = Sg_UTF16ConsolePortP(p)
      ? SG_TRANSCODER(Sg_MakeNativeConsoleTranscoder())
      : SG_TRANSCODER(Sg_MakeNativeTranscoder());
    port = SG_PORT(Sg_MakeTranscodedOutputPort(SG_PORT(p), trans));
  } else {
    /* for now I assume it's a textual port */
    port = SG_PORT(p);
  }
  ctx.mode = mode;
  ctx.table= NULL;
  ctx.flags = 0;
  ctx.sharedId = 0;
  SET_STACK_SIZE(&ctx);

  SG_PORT_LOCK(port);
  if (SG_WRITE_MODE(&ctx) == SG_WRITE_SHARED) {
    write_ss(obj, port, &ctx);
  } else {
    write_ss_rec(obj, port, &ctx);
  }
  SG_PORT_UNLOCK(port);
}

int Sg_WriteCircular(SgObject obj, SgObject port, int mode, int width)
{
  SgWriteContext ctx;
  SgString *str;
  SgObject out;
  int nc, sharedp = FALSE;

  if (!SG_OUTPORTP(port) && !SG_INOUTPORTP(port)) {
    Sg_Error(UC("output port required, but got %S"), port);
  }
  ctx.mode = mode;
  ctx.flags = WRITE_CIRCULAR;
  if (width > 0) {
    ctx.flags |= WRITE_LIMITED;
    ctx.limit = width;
  }
  ctx.ncirc = 0;
  ctx.table = Sg_MakeHashTableSimple(SG_HASH_EQ, 8);
  ctx.sharedId = 0;
  SET_STACK_SIZE(&ctx);

  if (width <= 0) {
    SG_PORT_LOCK(SG_PORT(port));
    format_write(obj, SG_PORT(port), &ctx, TRUE);
    SG_PORT_UNLOCK(SG_PORT(port));
    return 0;
  }

  out = Sg_MakeStringOutputPort(0);
  sharedp = SG_WRITE_MODE(&ctx) == SG_WRITE_SHARED;
  format_write(obj, SG_PORT(out), &ctx, sharedp);
  str = SG_STRING(Sg_GetStringFromStringPort(out));
  nc = str->size;
  if (nc > width) {
    SgObject sub = Sg_Substring(str, 0, width);
    Sg_Puts(port, sub);
    return -1;
  } else {
    Sg_Puts(port, str);
    return nc;
  }
}

int Sg_WriteLimited(SgObject obj, SgObject port, int mode, int width)
{
  SgWriteContext ctx;
  SgString *str;
  SgObject out;
  int nc, sharedp = FALSE;

  if (!SG_OUTPORTP(port) && !SG_INOUTPORTP(port)) {
    Sg_Error(UC("output port required, but got %S"), port);
  }
  out = Sg_MakeStringOutputPort(0);
  ctx.mode = mode;
  ctx.flags = WRITE_LIMITED;
  ctx.limit = width;
  ctx.sharedId = 0;
  ctx.table = NULL;
  SET_STACK_SIZE(&ctx);

  sharedp = SG_WRITE_MODE(&ctx) == SG_WRITE_SHARED;
  format_write(obj, SG_PORT(out), &ctx, sharedp);
  str = SG_STRING(Sg_GetStringFromStringPort(out));
  nc = str->size;
  if (nc > width) {
    SgObject sub = Sg_Substring(str, 0, width);
    Sg_Puts(port, sub);
    return -1;
  } else {
    Sg_Puts(port, str);
    return nc;
  }
}

#define NEXT_ARG(arg, args)						\
  do {									\
    if (!SG_PAIRP(args)) {						\
      Sg_Error(UC("too few arguments for format string: %S"), fmt);	\
    }									\
    arg = SG_CAR(args);							\
    args = SG_CDR(args);						\
    argcount++;								\
  } while(0)

#define MAX_PARAMS 5

static void format_pad(SgPort *out, SgString *str,
		       int mincol, int colinc, SgChar padchar,
		       int rightalign)
{
  int padcount = mincol - SG_STRING_SIZE(str);
  int i;

  if (padcount > 0) {
    if (colinc > 1) {
      padcount = ((padcount + colinc - 1) / colinc) * colinc;
    }
    if (rightalign) {
      for (i = 0; i < padcount; i++) Sg_PutcUnsafe(out, padchar);
    }
    Sg_PutsUnsafe(out, str);
    if (!rightalign) {
      for (i = 0; i < padcount; i++) Sg_PutcUnsafe(out, padchar);
    }
  } else {
    Sg_PutsUnsafe(out, str);
  }
}

/* ~s and ~a writer */
static void format_sexp(SgPort *out, SgObject arg,
			SgObject *params, int nparams,
			int rightalign, int dots, int mode)
{
  int mincol = 0, colinc = 1, minpad = 0, maxcol = -1, nwritten = 0, i;
  SgChar padchar = ' ';
  SgObject tmpout;
  SgString *tmpstr;
  
  if (nparams > 0 && SG_INTP(params[0])) mincol = SG_INT_VALUE(params[0]);
  if (nparams > 1 && SG_INTP(params[1])) colinc = SG_INT_VALUE(params[1]);
  if (nparams > 2 && SG_INTP(params[2])) minpad = SG_INT_VALUE(params[2]);
  if (nparams > 3 && SG_CHARP(params[3])) padchar = SG_CHAR_VALUE(params[3]);
  if (nparams > 4 && SG_INTP(params[4])) maxcol = SG_INT_VALUE(params[4]);

  tmpout = Sg_MakeStringOutputPort((maxcol > 0) ? maxcol
				   : (minpad > 0) ? minpad
				   : 0);
  if (minpad > 0 && rightalign) {
    for (i = 0; i < minpad; i++) Sg_PutcUnsafe(tmpout, padchar);
  }
  if (maxcol > 0) {
    nwritten = Sg_WriteLimited(arg, tmpout, mode, maxcol);
  } else {
    Sg_Write(arg, tmpout, mode);
  }
  if (minpad > 0 && !rightalign) {
    for (i = 0; i < minpad; i++) Sg_PutcUnsafe(tmpout, padchar);
  }
  tmpstr = SG_STRING(Sg_GetStringFromStringPort(tmpout));

  if (maxcol > 0 && nwritten < 0) {
    const SgChar *s = SG_STRING_VALUE(tmpstr);
    int size = SG_STRING_SIZE(tmpstr);
    if (dots && maxcol > 4) {
      for (i = 0; i < size - 4; i++) {
	Sg_PutcUnsafe(out, *s++);
      }
      Sg_PutuzUnsafe(out, UC(" ..."));
    } else {
      for (i = 0; i < size; i++) {
	Sg_PutcUnsafe(out, *s++);
      }
    }
  } else {
    format_pad(out, tmpstr, mincol, colinc, padchar, rightalign);
  }
}

/* ~d, ~b, ~o and ~x */
static void format_integer(SgPort *out, SgObject arg, SgObject *params,
			   int nparams, int radix, int delimited,
			   int alwayssign, int use_upper)
{
  int mincol = 0, commainterval = 3;
  SgChar padchar = ' ', commachar = ',';
  SgObject str;
  if (!Sg_IntegerP(arg)) {
    SgWriteContext ictx;
    ictx.mode = SG_WRITE_DISPLAY;
    ictx.flags = 0;
    ictx.table = NULL;
    ictx.sharedId = 0;
    format_write(arg, out, &ictx, FALSE);
    return;
  }
  if (SG_FLONUMP(arg)) arg = Sg_Exact(arg);
  if (nparams > 0 && SG_INTP(params[0])) mincol = SG_INT_VALUE(params[0]);
  if (nparams > 1 && SG_CHARP(params[1])) padchar = SG_CHAR_VALUE(params[1]);
  if (nparams > 2 && SG_CHARP(params[2])) commachar = SG_CHAR_VALUE(params[2]);
  if (nparams > 3 && SG_INTP(params[3])) commainterval = SG_INT_VALUE(params[3]);
  str = Sg_NumberToString(arg, radix, use_upper);
  if (alwayssign && SG_STRING_VALUE_AT(str, 0) != '-') {
    str = Sg_StringAppend2(Sg_MakeString(UC("+"), SG_LITERAL_STRING),
			   str);
  }
  if (delimited && commainterval) {
    int i;
    const SgChar *ptr = SG_STRING_VALUE(str);
    unsigned int num_digits = SG_STRING_SIZE(str), colcnt;
    SgObject strout = Sg_MakeStringOutputPort(num_digits + (num_digits % commainterval));

    if (*ptr == '-' || *ptr == '+') {
      Sg_PutcUnsafe(strout, *ptr);
      ptr++;
      num_digits--;
    }
    colcnt = num_digits % commainterval;
    if (colcnt != 0) {
      for (i = 0; (unsigned int)i < colcnt; i++) {
	Sg_Putc(strout, *(ptr + i));
      }
    }
    while (colcnt < num_digits) {
      if (colcnt != 0) Sg_PutcUnsafe(strout, commachar);
      for (i = 0; i < commainterval; i++) {
	Sg_Putc(strout, *(ptr + colcnt + i));
      }
      colcnt += commainterval;
    }
    str = Sg_GetStringFromStringPort(strout);
  }
  format_pad(out, SG_STRING(str), mincol, 1, padchar, TRUE);
}

static void format_proc(SgPort *port, SgString *fmt, SgObject args, int sharedp)
{
  SgChar ch = 0;
  SgObject arg;
  SgPort *fmtstr = SG_PORT(Sg_MakeStringInputPort(fmt, FALSE));
  int backtracked = FALSE;
  int arglen, argcount;
  SgWriteContext sctx, actx;	/* context for ~s and ~a */

  arglen = Sg_Length(args);
  argcount = 0;

  sctx.mode = SG_WRITE_WRITE;
  sctx.table = NULL;
  sctx.flags = 0;
  sctx.sharedId = 0;
  SET_STACK_SIZE(&sctx);

  actx.mode = SG_WRITE_DISPLAY;
  actx.table = NULL;
  actx.flags = 0;
  actx.sharedId = 0;
  SET_STACK_SIZE(&actx);

  for (;;) {
    int atflag, colonflag;
    SgObject params[MAX_PARAMS];
    int numParams;
    ch = Sg_GetcUnsafe(fmtstr);
    if (ch == EOF) {
      if (!backtracked && !SG_NULLP(args)) {
	Sg_Error(UC("too many arguments for format string: %S"), fmt);
      }
      return;
    }
    if (ch != '~') {
      Sg_PutcUnsafe(port, ch);
      continue;
    }
    numParams = 0;
    atflag = colonflag = FALSE;
    for (;;) {
      ch = Sg_GetcUnsafe(fmtstr);
      switch (ch) {
      case EOF:
	Sg_Error(UC("imcomplete format string: %S"), fmt);
	break;
      case '%':
	/* TODO get eol from port */
	Sg_PutcUnsafe(port, '\n');
	break;
      case 's': case 'S':
	NEXT_ARG(arg, args);
	if (numParams == 0) {
	  format_write(arg, port, &sctx, sharedp);
	} else {
	  format_sexp(port, arg, params, numParams, atflag, colonflag,
		      sharedp ? SG_WRITE_SHARED : SG_WRITE_WRITE);
	}
	break;
      case 'a': case 'A':
	NEXT_ARG(arg, args);
	if (numParams == 0) {
	  format_write(arg, port, &actx, sharedp);
	} else {
	  format_sexp(port, arg, params, numParams, atflag, colonflag,
		      SG_WRITE_DISPLAY);
	}
	break;
      case 'd': case 'D':
	NEXT_ARG(arg, args);
	if (numParams == 0 && !atflag && !colonflag) {
	  format_write(arg, port, &actx, FALSE);
	} else {
	  format_integer(port, arg, params, numParams, 10,
			 colonflag, atflag, FALSE);
	}
	break;
      case 'b': case 'B':
	NEXT_ARG(arg, args);
	if (numParams == 0 && !atflag && !colonflag) {
	  if (Sg_IntegerP(arg)) {
	    format_write(Sg_NumberToString(arg, 2, FALSE), port, &actx, FALSE);
	  } else {
	    format_write(arg, port, &actx, FALSE);
	  }
	} else {
	  format_integer(port, arg, params, numParams, 2,
			 colonflag, atflag, FALSE);
	}
	break;
      case 'o': case 'O':
	NEXT_ARG(arg, args);
	if (numParams == 0 && !atflag && !colonflag) {
	  if (Sg_IntegerP(arg)) {
	    format_write(Sg_NumberToString(arg, 8, FALSE), port, &actx, FALSE);
	  } else {
	    format_write(arg, port, &actx, FALSE);
	  }
	} else {
	  format_integer(port, arg, params, numParams, 8,
			 colonflag, atflag, FALSE);
	}
	break;
      case 'x': case 'X':
	NEXT_ARG(arg, args);
	if (numParams == 0 && !atflag && !colonflag) {
	  if (Sg_IntegerP(arg)) {
	    format_write(Sg_NumberToString(arg, 16, ch == 'X'), port, &actx, FALSE);
	  } else {
	    format_write(arg, port, &actx, FALSE);
	  }
	} else {
	  format_integer(port, arg, params, numParams, 16,
			 colonflag, atflag, ch == 'X');
	}
	break;
      case '@':
	if (atflag) {
	  Sg_Error(UC("too many @-flag for formatting directive: %S"), fmt);
	}
	atflag = TRUE;
	continue;
      case ':':
	if (colonflag) {
	  Sg_Error(UC("too many :-flag for formatting directive: %S"), fmt);
	}
	colonflag = TRUE;
	continue;
      case '\'':
	if (atflag || colonflag) goto badfmt;
	if (numParams >= MAX_PARAMS) goto badfmt;
	ch = Sg_GetcUnsafe(fmtstr);
	if (ch == EOF) goto badfmt;
	params[numParams++] = SG_MAKE_CHAR(ch);
	ch = Sg_GetcUnsafe(fmtstr);
	if (ch != ',') Sg_UngetcUnsafe(fmtstr, ch);
	continue;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
      case '-': case '+':
	if (atflag || colonflag || numParams >= MAX_PARAMS) {
	  goto badfmt;
	} else {
	  int sign = (ch == '-') ? -1 : 1;
	  unsigned long value = isdigit(ch) ? (ch - '0') : 0;
	  for (;;) {
	    ch = Sg_GetcUnsafe(fmtstr);
	    /* TODO check valid character */
	    if (!isdigit(ch)) {
	      if (ch != ',') Sg_UngetcUnsafe(fmtstr, ch);
	      params[numParams++] = Sg_MakeInteger(sign * value);
	      break;
	    }
	    /* TODO check over flow */
	    value = value * 10 + (ch - '0');
	  }
	}
	continue;
      case ',':
	if (atflag || colonflag || numParams >= MAX_PARAMS) {
	  goto badfmt;
	} else {
	  params[numParams++] = SG_FALSE;
	  continue;
	}
      default:
	Sg_PutcUnsafe(port, ch);
	break;
      }
      break;
    }
  }
 badfmt:
  Sg_Error(UC("illegal format string: %S"), fmt);
  return;
}

void Sg_Format(SgPort *port, SgString *fmt, SgObject args, int ss)
{
  SgPort *out;

  if (!SG_OUTPORTP(port) && !SG_INOUTPORTP(port)) {
    Sg_Error(UC("output port required, but got %S"), port);
  }
  if (!SG_TEXTUAL_PORTP(port)) {
    /* for now I asuume it's a binary port. */
    SgTranscoder *trans = Sg_UTF16ConsolePortP(port)
      ? SG_TRANSCODER(Sg_MakeNativeConsoleTranscoder())
      : SG_TRANSCODER(Sg_MakeNativeTranscoder());
    out = SG_PORT(Sg_MakeTranscodedOutputPort(port, trans));
  } else {
    /* for now I assume it's a textual port */
    out = SG_PORT(port);
  }
  SG_PORT_LOCK(out);
  format_proc(out, fmt, args, ss);
  SG_PORT_UNLOCK(out);
}

void Sg_Printf(SgPort *port, const SgChar *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  Sg_Vprintf(port, fmt, ap, FALSE);
  va_end(ap);
}

void Sg_PrintfShared(SgPort *port, const SgChar *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  Sg_Vprintf(port, fmt, ap, TRUE);
  va_end(ap);
}

static char special[] = {
 /* NUL .... */
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 /* .... */
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 /*    !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  */
    3, 0, 3, 3, 0, 0, 0, 3, 3, 3, 0, 1, 3, 1, 1, 0,
 /* 0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?  */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ,3, 0, 0, 0, 0,
 /* @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  */
    1, 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
 /* P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  */
    16,16,16,16,16,16,16,16,16,16,16,3, 11,3, 0, 0,
 /* `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  */
    3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
 /* p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~  ^? */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 11,3, 0, 7
};

static int symbol_need_bar(const SgChar *s, int n)
{
  switch (s[0]) {
  case '@': return TRUE;
  case '+':
    if (s[1] == 0) return FALSE;
    return TRUE;
  case '-':
    if (s[1] == 0) return FALSE;
    if (s[1] != '>') return TRUE;
    break;
  case '.':
    if (s[1] != '.') return TRUE;
    if (s[2] != '.') return TRUE;
    if (s[3] == 0) return FALSE;
    return TRUE;
  }

  if (isdigit(s[0])) {
    return TRUE;
  } else {
    SgChar c;
    while ((c = *s++) != 0 && n--) {
      if (c < 32) continue;
      if (c == 127) continue;
      if (c & 0x80) continue;
	  if (c >= 0xFF) return TRUE;
      if (isalnum(c)) continue;
      if (strchr("!$%&/:*<=>?^_~+-.@", (char)c)) continue;
      return TRUE;
    }
    return FALSE;
  }
}

#define SPBUFSIZ  50
#define CASE_ITAG(obj, str)				\
  case SG_ITAG(obj): Sg_PutuzUnsafe(port, str); break;

/* character name table (first 33 chars of ASCII)*/
static const char *char_names[] = {
    "nul",    "x01",   "x02",    "x03",   "x04",   "x05",   "x06",   "x07",
    "x08",    "tab",   "newline","x0b",   "x0c",   "return","x0e",   "x0f",
    "x10",    "x11",   "x12",    "x13",   "x14",   "x15",   "x16",   "x17",
    "x18",    "x19",   "x1a",    "esc",   "x1c",   "x1d",   "x1e",   "x1f",
    "space"
};

static void write_object(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Apply2(SG_OBJ(&Sg_GenericWriteObject), obj, port);
}

static SgObject write_object_fallback(SgObject *args, int argc, SgGeneric *gf)
{
  SgClass *klass;
  if (argc != 2 || (argc == 2 && !SG_OUTPORTP(args[1]))) {
    Sg_Error(UC("no applicable method for write-object with %S"),
	     Sg_ArrayToList(args, argc));
  }
  klass = Sg_ClassOf(args[0]);
  Sg_Printf(SG_PORT(args[1]), UC("#<%A %p>"),
	    klass->name, args[0]);
  return SG_TRUE;
}

static void write_general(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgClass *c = Sg_ClassOf(obj);
  if (c->printer) c->printer(obj, port, ctx);
  else          write_object(obj, port, ctx);
}

static void write_noptr(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  if (SG_IMMEDIATEP(obj)) {
    switch (SG_ITAG(obj)) {
      CASE_ITAG(SG_FALSE,   UC("#f"));
      CASE_ITAG(SG_TRUE,    UC("#t"));
      CASE_ITAG(SG_NIL,     UC("()"));
      CASE_ITAG(SG_EOF,     UC("#<eof>"));
      CASE_ITAG(SG_UNDEF,   UC("#<unspecified>"));
      CASE_ITAG(SG_UNBOUND, UC("#<unbound variable>"));
    default:
      Sg_Panic("write: unknown itag object: %08x", SG_WORD(obj));
    }
  } else if (SG_INTP(obj)) {
    char buf[SPBUFSIZ];
    snprintf(buf, sizeof(buf), "%ld", SG_INT_VALUE(obj));
    Sg_PutzUnsafe(port, buf);
  } else if (SG_CHARP(obj)) {
    SgChar ch = SG_CHAR_VALUE(obj);
    if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
      Sg_PutcUnsafe(port, ch);
    } else {
      Sg_PutuzUnsafe(port, UC("#\\"));
      if (ch <= 0x20)      Sg_PutzUnsafe(port, char_names[ch]);
      else if (ch == 0x7f) Sg_PutuzUnsafe(port, UC("delete"));
      else                 Sg_PutcUnsafe(port, ch);
    }

  } 
#ifdef USE_IMMEDIATE_FLONUM
  else if (SG_IFLONUMP(obj)) {
    write_general(obj, port, ctx);
  }
#endif	/* USE_IMMEDIATE_FLONUM */
  else {
    Sg_Panic("write: got a bogus object: %08x", SG_WORD(obj));
  }
  return;
}

/* check stack.
   write context holds stack info.

   FIXME: this assumes stack grows downward;
*/
#define CHECK_BOUNDARY(s, p, c)						\
  do {									\
    if ((char *)&(s) < (char *)&((c)->mode) - (c)->stackSize) {		\
      Sg_IOWriteError((SG_WRITE_MODE(c) == SG_WRITE_DISPLAY)		\
		      ? SG_INTERN("display")				\
		      : SG_INTERN("write"),				\
		      SG_MAKE_STRING("stack overflow"), (p));		\
      return;								\
    }									\
  } while(0)

void write_ss_rec(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgObject e;
  SgHashTable *ht = ctx->table;
  
  CHECK_BOUNDARY(e, port, ctx);
  
  if (ctx->flags & WRITE_LIMITED) {
    /*
      if the flag has WRITE_LIMITED, then output port must be
      string output port
    */
    char_buffer *start = SG_TEXTUAL_PORT(port)->src.ostr.start;
    char_buffer *current = SG_TEXTUAL_PORT(port)->src.ostr.current;
    size_t count = 0;
    for (; start != current; start = start->next) count++;
    if (count >= ctx->limit) return;
  }

  if (!obj) {
    Sg_PutuzUnsafe(port, UC("#<null>"));
    return;
  }

  if (!SG_PTRP(obj)) {
    write_noptr(obj, port, ctx);
    return;
  }

  if (SG_NUMBERP(obj)) {
    write_general(obj, port, ctx);
    return;
  }

  if (ht) {
    char numbuf[SPBUFSIZ];
    e = Sg_HashTableRef(ht, obj, SG_FALSE);
    if (!SG_FALSEP(e)) {
      if (SG_INTP(e)) {
	/* This object is already printed. */
	snprintf(numbuf, sizeof(numbuf), "#%ld#", SG_INT_VALUE(e));
	Sg_PutzUnsafe(port, numbuf);
	return;
      } else {
	/* This object will be seen again. Put a reference tag. */
	Sg_HashTableSet(ht, obj, SG_MAKE_INT(ctx->sharedId), 0);
	snprintf(numbuf, sizeof(numbuf), "#%d=", ctx->sharedId);
	Sg_PutzUnsafe(port, numbuf);
	ctx->sharedId++;
      }
    }
  }

  if (SG_PAIRP(obj)) {
    /* special case for quote etc */
    if (SG_PAIRP(SG_CDR(obj)) && SG_NULLP(SG_CDDR(obj))) {
      int special = TRUE;
      if (SG_CAR(obj) == SG_SYMBOL_QUOTE) {
	Sg_PutcUnsafe(port, '\'');
      } else if (SG_CAR(obj) == SG_SYMBOL_QUASIQUOTE) {
	Sg_PutcUnsafe(port, '`');
      } else if (SG_CAR(obj) == SG_SYMBOL_UNQUOTE) {
	Sg_PutcUnsafe(port, ',');
      } else if (SG_CAR(obj) == SG_SYMBOL_UNQUOTE_SPLICING) {
	Sg_PutuzUnsafe(port, UC(",@"));
      } else if (SG_CAR(obj) == SG_SYMBOL_SYNTAX) {
	Sg_PutuzUnsafe(port, UC("#'"));
      } else if (SG_CAR(obj) == SG_SYMBOL_QUASISYNTAX) {
	Sg_PutuzUnsafe(port, UC("#`"));
      } else if (SG_CAR(obj) == SG_SYMBOL_UNSYNTAX) {
	Sg_PutuzUnsafe(port, UC("#,"));
      } else if (SG_CAR(obj) == SG_SYMBOL_UNSYNTAX_SPLICING) {
	Sg_PutuzUnsafe(port, UC("#,@"));
      } else {
	special = FALSE;
      }
      if (special) {
	write_ss_rec(SG_CADR(obj), port, ctx);
	return;
      }
    }
    /* normal case */
    Sg_PutcUnsafe(port, '(');
    for (;;) {
      write_ss_rec(SG_CAR(obj), port, ctx);
      obj = SG_CDR(obj);
      if (SG_NULLP(obj)) {
	Sg_PutcUnsafe(port, ')');
	return;
      }
      if (!SG_PAIRP(obj)) {
	Sg_PutuzUnsafe(port, UC(" . "));
	write_ss_rec(obj, port, ctx);
	Sg_PutcUnsafe(port, ')');
	return;
      }
      if (ht) {
	e = Sg_HashTableRef(ht, obj, SG_FALSE);
	if (!SG_FALSEP(e)) {
	  Sg_PutuzUnsafe(port, UC(" . "));
	  write_ss_rec(obj, port, ctx);
	  Sg_PutcUnsafe(port, ')');
	  return;
	}
      }
      Sg_PutcUnsafe(port, ' ');
    }
  } else if (SG_VECTORP(obj)) {
    int len, i;
    SgObject *elts;
    Sg_PutuzUnsafe(port, UC("#("));
    len = SG_VECTOR(obj)->size;
    elts = SG_VECTOR(obj)->elements;
    for (i = 0; i < len; i++) {
      if (i != 0) Sg_PutcUnsafe(port, ' ');
      write_ss_rec(elts[i], port, ctx);
    }
    Sg_PutcUnsafe(port, ')');
  } else {
    write_general(obj, port, ctx);
  }
  return;
}

/* FIXME, merge it */
static void write_walk_circular(SgObject obj, SgWriteContext *ctx,
				int cycleonlyp)
{
  SgHashTable *ht;
  SgObject elt;

  CHECK_BOUNDARY(elt, SG_FALSE, ctx);
#define REGISTER(obj)						\
  do {								\
    SgObject e = Sg_HashTableRef(ht, (obj), SG_UNBOUND);	\
    if (SG_INTP(e)) {						\
      int v = SG_INT_VALUE(e);					\
      Sg_HashTableSet(ht, (obj), SG_MAKE_INT(v + 1), 0);	\
      if (v > 0) return;					\
    } else {							\
      Sg_HashTableSet(ht, (obj), SG_MAKE_INT(0), 0);		\
    }								\
  } while(0)

#define UNREGISTER(obj)						\
  do {								\
    if (cycleonlyp) {						\
      SgObject e = Sg_HashTableRef(ht, (obj), SG_MAKE_INT(0));	\
      int v = SG_INT_VALUE(e);					\
      if (v <= 1) {						\
	Sg_HashTableDelete(ht, (obj));				\
      }								\
    }								\
  } while(0)

  ht = ctx->table;
  if (SG_PAIRP(obj) || SG_VECTORP(obj)) {
    REGISTER(obj);
    if (SG_PAIRP(obj)) {
      elt = SG_CAR(obj);
      if (SG_PTRP(elt)) write_walk_circular(elt, ctx, cycleonlyp);
      write_walk_circular(SG_CDR(obj), ctx, cycleonlyp);
    } else if (SG_VECTORP(obj) && SG_VECTOR_SIZE(obj) > 0) {
      int i, len = SG_VECTOR_SIZE(obj);
      for (i = 0; i < len; i++) {
	elt = SG_VECTOR_ELEMENT(obj, i);
	if (SG_PTRP(elt)) write_walk_circular(elt, ctx, cycleonlyp);
      }
    }
    UNREGISTER(obj);
  }
#undef REGISTER
#undef UNREGISTER
}

static void write_walk(SgObject obj, SgWriteContext *ctx)
{
  SgHashTable *ht;
  SgObject elt;

  CHECK_BOUNDARY(elt, SG_FALSE, ctx);

#define REGISTER(obj)						\
  do {								\
    SgObject e = Sg_HashTableRef(ht, (obj), SG_UNBOUND);	\
    if (!SG_UNBOUNDP(e)) {					\
      Sg_HashTableSet(ht, (obj), SG_TRUE, 0);			\
      return;							\
    }								\
    Sg_HashTableSet(ht, (obj), SG_FALSE, 0);			\
  } while(0)

  ht = ctx->table;
  for (;;) {
    if (!SG_PTRP(obj) || SG_KEYWORDP(obj) || SG_NUMBERP(obj)
	|| (SG_SYMBOLP(obj) && SG_INTERNED_SYMBOL(obj))) {
      return;
    }
    if (SG_PAIRP(obj)) {
      REGISTER(obj);
      elt = SG_CAR(obj);
      if (SG_PTRP(elt)) write_walk(SG_CAR(obj), ctx);
      obj = SG_CDR(obj);
      continue;
    }
    if (SG_STRINGP(obj) && SG_STRING(obj)->size != 0) {
      REGISTER(obj);
      return;
    }
    if (SG_VECTORP(obj) && SG_VECTOR_SIZE(obj) > 0) {
      int i, len = SG_VECTOR_SIZE(obj);
      REGISTER(obj);
      for (i = 0; i < len; i++) {
	elt = SG_VECTOR_ELEMENT(obj, i);
	if (SG_PTRP(elt)) write_walk(elt, ctx);
      }
      return;
    }
    if (SG_RECORD_TYPEP(obj)) {
      REGISTER(SG_RECORD_TYPE_RTD(obj));
      REGISTER(SG_RECORD_TYPE_RCD(obj));
      return;
    }
    if (SG_SYMBOLP(obj)) {
      ASSERT(!SG_INTERNED_SYMBOL(obj));
      REGISTER(obj);
      return;
    }
    return;
  }
}

void write_ss(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  ctx->table = Sg_MakeHashTableSimple(SG_HASH_EQ, 0);
  if (ctx->flags & WRITE_CIRCULAR) {
    SgObject seen = Sg_MakeHashTableSimple(SG_HASH_EQ, 64);
    SgHashTable *save = ctx->table;
    SgHashIter iter;
    SgHashEntry *e;
    ctx->table = SG_HASHTABLE(seen);
    write_walk_circular(obj, ctx, TRUE);
    /* extract */
    ctx->table = save;
    Sg_HashIterInit(SG_HASHTABLE_CORE(seen), &iter);
    while ((e = Sg_HashIterNext(&iter)) != NULL) {
      SgObject v = SG_HASH_ENTRY_VALUE(e);
      if (SG_INT_VALUE(v) > 1) {
	Sg_HashTableSet(ctx->table, SG_HASH_ENTRY_KEY(e), SG_TRUE, 0);
      }
    }
  } else {
    write_walk(obj, ctx);
  }

  write_ss_rec(obj, port, ctx);
}

void format_write(SgObject obj, SgPort *port, SgWriteContext *ctx, int sharedp)
{
  if (sharedp) {
    write_ss(obj, port, ctx);
  } else {
    write_ss_rec(obj, port, ctx);
  }
}

static void vprintf_proc(SgPort *port, const SgChar *fmt, 
			 SgObject args, int sharedp)
{
  const SgChar *fmtp = fmt;
  SgObject value;
  SgChar c;
  char buf[SPBUFSIZ], tmp[SPBUFSIZ];
  int longp = 0, len, mode;

  while ((c = *fmtp++) != 0) {
    int width, prec, dot_appeared, pound_appeared, index;
    int minus_appeared;

    if (c != '%') {
      Sg_PutcUnsafe(port, c);
      continue;
    }
#define get_value()				\
    do {					\
      ASSERT(SG_PAIRP(args));			\
      value = SG_CAR(args);			\
      args = SG_CDR(args);			\
    } while (0)
#define put_tmp_to_buf(c, getter)		\
    tmp[index++] = (char)(c);			\
    tmp[index++] = 0;				\
    snprintf(buf, sizeof(buf), tmp, (getter))

    width = 0, prec = 0, dot_appeared = 0, pound_appeared = 0;
    index = 0; minus_appeared = 0;
    tmp[index++] = c;		/* store % to tmp */
    while ((c = *fmtp++) != 0) {
      switch (c) {
      case 'l':
	longp++;
	tmp[index++] = (char)c;
	continue;
      case 'd': case 'i': case 'c':
	{
	  get_value();
	  ASSERT(Sg_ExactP(value));
	  put_tmp_to_buf(c, Sg_GetInteger(value));
	  Sg_PutzUnsafe(port, buf);
	  break;
	}
      case 'U':
	{
	  SgChar ucs4;
	  SgWriteContext wctx;
	  wctx.mode = SG_WRITE_WRITE;
	  wctx.table = NULL;
	  wctx.flags = 0;
	  wctx.sharedId = 0;
	  SET_STACK_SIZE(&wctx);

	  get_value();
	  ASSERT(SG_CHARP(value));
	  ucs4 = SG_CHAR_VALUE(value);
	  if (ucs4 < 128) {
	    /* put char in '~' or \tab or U+10 */
	    switch (ucs4) {
	    case   0: Sg_PutuzUnsafe(port, UC("nul(U+0000)"));         break;
	    case   7: Sg_PutuzUnsafe(port, UC("alarm(U+0007)"));       break;
	    case   8: Sg_PutuzUnsafe(port, UC("backspace(U+0008)"));   break;
	    case   9: Sg_PutuzUnsafe(port, UC("tab(U+0009)"));         break;
	    case  10: Sg_PutuzUnsafe(port, UC("linefeed(U+000A)"));    break;
	    case  11: Sg_PutuzUnsafe(port, UC("vtab(U+000B)"));        break;
	    case  12: Sg_PutuzUnsafe(port, UC("page(U+000C)"));        break;
	    case  13: Sg_PutuzUnsafe(port, UC("return(U+000D)"));      break;
	    case  27: Sg_PutuzUnsafe(port, UC("esc(U+001B)"));         break;
	    case  32: Sg_PutuzUnsafe(port, UC("space(U+0020)"));       break;
	    case 127: Sg_PutuzUnsafe(port, UC("delete(U+007F)"));      break;
	    default:
	      if (ucs4 < 32) {
		snprintf(buf, sizeof(buf), "U+%04X", ucs4);
		Sg_PutzUnsafe(port, buf);
	      } else {
		Sg_PutcUnsafe(port, '\'');
		format_write(value, port, &wctx, sharedp);
		Sg_PutcUnsafe(port, '\'');
	      }
	      break;
	    }
	  } else {
	    Sg_PutcUnsafe(port, '\'');
	    format_write(value, port, &wctx, sharedp);
	    Sg_PutcUnsafe(port, '\'');
	    snprintf(buf, sizeof(buf), "(U+%04X)", ucs4);
	    Sg_PutzUnsafe(port, buf);
	  }
	  break;
	}
      case 'o': case 'u': case 'x': case 'X':
	{
	  get_value();
	  ASSERT(Sg_ExactP(value));
	  put_tmp_to_buf(c, Sg_GetUInteger(value));
	  Sg_PutzUnsafe(port, buf);
	  break;
	}
      case 'e': case 'E': case 'f': case 'g': case 'G':
	{
	  get_value();
	  ASSERT(SG_FLONUMP(value));
	  put_tmp_to_buf(c, Sg_GetDouble(value));
	  Sg_PutzUnsafe(port, buf);
	  break;
	}
      case 's':
	{
	  get_value();
	  if (width < 0) {
	    for (len = SG_STRING(value)->size; len < -width; len++) {
	      Sg_PutcUnsafe(port, ' ');
	    } 
	  }
	  Sg_PutsUnsafe(port, SG_STRING(value));
	  if (width > 0) {
	    for (len = SG_STRING(value)->size; len < width; len++) {
	      Sg_PutcUnsafe(port, ' ');
	    }
	  }
	  break;
	}
      case '%':
	Sg_PutcUnsafe(port, '%');
	break;
      case 'p':
	{
	  get_value();
	  ASSERT(Sg_ExactP(value));
	  put_tmp_to_buf(c, (void*)(intptr_t)Sg_GetUInteger(value));
	  Sg_PutzUnsafe(port, buf);
	  break;
	}
      case 'S': case 'A': case 'L':
	{
	  SgWriteContext wctx;
	  get_value();
	  mode = (c == 'A') ? SG_WRITE_DISPLAY 
	    : (c == 'L') ? SG_WRITE_LIBPATH
	    : SG_WRITE_WRITE;
	  wctx.mode = mode;
	  wctx.table = NULL;
	  wctx.flags = 0;
	  wctx.sharedId = 0;
	  SET_STACK_SIZE(&wctx);
	  if (pound_appeared) {
	    int  n = Sg_WriteCircular(value, SG_OBJ(port), mode, width);
	    if (n < 0 && prec > 0) {
	      Sg_PutuzUnsafe(port, UC(" ..."));
	    }
	    if (n > 0) {
	      for (; n < prec; n++) Sg_PutcUnsafe(port, ' ');
	    }
	  } else if (width == 0) {
	    format_write(value, port, &wctx, sharedp);
	  } else if (dot_appeared) {
	    int  n = Sg_WriteLimited(value, SG_OBJ(port), mode, width);
	    if (n < 0 && prec > 0) {
	      Sg_PutuzUnsafe(port, UC(" ..."));
	    }
	    if (n > 0) {
	      for (; n < prec; n++) Sg_PutcUnsafe(port, ' ');
	    }
	  } else {
	    format_write(value, port, &wctx, sharedp);
	  }
	  break;
	}
      case 'C':
	{
	  get_value();
	  ASSERT(Sg_ExactP(value));
	  Sg_PutcUnsafe(port, Sg_GetInteger(value));
	}
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
	{
	  if (dot_appeared) {
	    prec = prec * 10 + (c - '0');
	  } else {
	    width = width * 10 + (c - '0');
	    if (minus_appeared) {
	      /* not so smart... */
	      width = 0 - abs(width);
	    }
	  }
	  goto fallback;
	}
      case '-':
	minus_appeared++;
	goto fallback;
      case '.':
	dot_appeared++;
	goto fallback;
      case '#':
	pound_appeared++;
	goto fallback;
      case '*':
	{
	  get_value();
	  if (dot_appeared) {
	    prec = Sg_GetInteger(value);
	  } else {
	    width = Sg_GetInteger(value);
	  }
	  goto fallback;
	}
      fallback:
      default:
	tmp[index++] = c;
	continue;
      }
      break;
    }
    if (c == 0) {
      Sg_Error(UC("incomplete %%-directive in format string: %s"), fmt);
    }
  }  
}

/* 
   I think it's better to convert binary port to textual port implicitly,
   especially for this method. I need to think about 'format' and 'display'.
*/
void Sg_Vprintf(SgPort *port, const SgChar *fmt, va_list sp, int sharedp)
{
  SgObject h = SG_NIL, t = SG_NIL;
  SgPort *out;
  const SgChar *fmtp = fmt;
  int c;
  if (!SG_OUTPORTP(port) && !SG_INOUTPORTP(port)) {
    Sg_Error(UC("output port required, but got %S"), port);
  }
  if (!SG_TEXTUAL_PORTP(port)) {
    /* for now I asuume it's a binary port. */
    SgTranscoder *trans = Sg_UTF16ConsolePortP(port)
      ? SG_TRANSCODER(Sg_MakeNativeConsoleTranscoder())
      : SG_TRANSCODER(Sg_MakeNativeTranscoder());
    out = Sg_MakeTranscodedOutputPort(port, trans);
  } else {
    /* for now I assume it's a textual port */
    out = port;
  }

  while ((c = *fmtp++) != 0) {
    if (c != '%') continue;
    while ((c = *fmtp++) != 0) {
      switch (c) {
      case 'd': case 'i': case 'c': case 'C': case '*':
	{
	  int value = va_arg(sp, int);
	  SG_APPEND1(h, t, Sg_MakeInteger(value));
	  break;
	}
      case 'U':
	{
	  SgChar value = va_arg(sp, SgChar);
	  SG_APPEND1(h, t, SG_MAKE_CHAR(value));
	  break;
	}
      case 'o': case 'u': case 'x': case 'X':
	{
	  unsigned long value = va_arg(sp, unsigned long);
	  SG_APPEND1(h, t, Sg_MakeIntegerU(value));
	  break;
	}
      case 'e': case 'E': case 'f': case 'g': case 'G':
	{
	  double value = va_arg(sp, double);
	  SG_APPEND1(h, t, Sg_MakeFlonum(value));
	  break;
	}
      case 's':
	{
	  SgChar *value = va_arg(sp, SgChar*);
	  /* for safety */
	  if (value != NULL) {
	    SG_APPEND1(h, t, Sg_MakeString(value, SG_HEAP_STRING));
	  } else {
	    SG_APPEND1(h, t, SG_MAKE_STRING("(null)"));
	  }
	  break;
	}
      case '%':
	break;
      case 'p':
	{
	  void *value = va_arg(sp, void *);
	  SG_APPEND1(h, t, Sg_MakeIntegerU((unsigned long)value));
	  break;
	}
      case 'S': case 'A': case 'L':
	{
	  SgObject value = va_arg(sp, SgObject);
	  SG_APPEND1(h, t, value);
	  break;
	}
      default:
	continue;
      }
      break;
    }
  }
  SG_PORT_LOCK(out);
  vprintf_proc(out, fmt, h, sharedp);
  SG_PORT_UNLOCK(out);
}

SgObject Sg_Sprintf(const SgChar *fmt, ...)
{
  SgObject r;
  va_list ap;
  va_start(ap, fmt);
  r = Sg_Vsprintf(fmt, ap, FALSE);
  va_end(ap);
  return r;
}

SgObject Sg_SprintfShared(const SgChar *fmt, ...)
{
  SgObject r;
  va_list ap;
  va_start(ap, fmt);
  r = Sg_Vsprintf(fmt, ap, TRUE);
  va_end(ap);
  return r;
}

SgObject Sg_Vsprintf(const SgChar *fmt, va_list args, int sharedp)
{
  /* use default size */
  SgObject port = Sg_MakeStringOutputPort(0);
  Sg_Vprintf(port, fmt, args, sharedp);
  return Sg_GetStringFromStringPort(port);
}

void Sg_WriteSymbolName(SgString *snam, SgPort *port,
			SgWriteContext *ctx, int flags)
{
  const SgChar *p = snam->value, *q;
  int size = snam->size;
  int escape = FALSE;
  int r6rsMode = SG_VM_IS_SET_FLAG(Sg_VM(), SG_R6RS_MODE);
  int mode = SG_WRITE_MODE(ctx);

  SG_PORT_LOCK(port);
  if (size == 0) {
    if (!(flags & SG_SYMBOL_WRITER_NOESCAPE_EMPTY)) {
      Sg_PutuzUnsafe(port, UC("||"));
    }
    return;
  }
  if (size == 1 && (*p == '+' || *p == '-')) {
    Sg_PutcUnsafe(port, *p);
    return;
  }
  /* R6RS does not have '|' */
  if (mode != SG_WRITE_LIBPATH &&
      (!(flags & SG_SYMBOL_WRITER_NOESCAPE_INITIAL))) {
    escape = symbol_need_bar(p, size);
  }
  if (escape && !r6rsMode) {
    Sg_PutcUnsafe(port, '|');
    for (q = p; q < p + size; q++) {
      SgChar ch = *q;
      if (ch < 128) {
	if (special[ch] & 8) {
	  Sg_PutcUnsafe(port, '\\');
	  Sg_PutcUnsafe(port, ch);
	} else if (special[ch] & 4) {
	  Sg_Printf(port, UC("\\x%02x"), ch);
	} else {
	  Sg_PutcUnsafe(port, ch);
	}
      } else {
	Sg_PutcUnsafe(port, ch);
      }
    }
    Sg_PutcUnsafe(port, '|');
    return;
  } else {
    if (r6rsMode && (mode != SG_WRITE_LIBPATH)) {
      for (q = p; q < p + size; q++) {
	SgChar ch = *q;
	if ((q == p && Sg_Ucs4ConstituentP(ch)) ||
	    (q != p && Sg_Ucs4SubsequentP(ch))) {
	  Sg_PutcUnsafe(port, ch);
	} else {
	  char buf[16];
	  /* ... */
	  if (q == p) {
	    if (size == 3) {
	      if (q[0] == '.' && q[1] == '.' && q[2] == '.') {
		Sg_PutuzUnsafe(port, UC("..."));
		return;
	      }
	    }
	    if (size > 2) {
	      if (q[0] == '-' && q[1] == '>') {
		Sg_PutuzUnsafe(port, UC("->"));
		q += 2;
		continue;
	      }
	    }
	  }
	  snprintf(buf, sizeof(buf), "\\x%X;", ch);
	  Sg_PutzUnsafe(port, buf);
	}
      }
    } else {
      Sg_PutsUnsafe(port, snam);
    }
  }
  SG_PORT_UNLOCK(port);
}

void Sg__InitWrite()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  Sg_InitBuiltinGeneric(&Sg_GenericWriteObject, UC("write-object"), lib);
			
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
