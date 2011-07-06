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
#include "sagittarius/code.h"
#include "sagittarius/codec.h"
#include "sagittarius/core.h"
#include "sagittarius/closure.h"
#include "sagittarius/subr.h"
#include "sagittarius/file.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/pair.h"
#include "sagittarius/keyword.h"
#include "sagittarius/macro.h"
#include "sagittarius/string.h"
#include "sagittarius/number.h"
#include "sagittarius/error.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/identifier.h"
#include "sagittarius/library.h"
#include "sagittarius/vector.h"
#include "sagittarius/values.h"
#include "sagittarius/symbol.h"
#include "sagittarius/generic.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/vm.h"
#include "sagittarius/record.h"
#include "sagittarius/port.h"
#include "sagittarius/gloc.h"
#include "sagittarius/unicode.h"
#include "sagittarius/builtin-symbols.h"

#define WRITE_LIMITED  0x10
#define WRITE_CIRCULAR 0x20

/* for convenient */
static void format_write(SgObject obj, SgPort *port, SgWriteContext *ctx, int sharedp);
static void write_ss_rec(SgObject obj, SgPort *port, SgWriteContext *ctx);
static void write_ss(SgObject obj, SgPort *port, SgWriteContext *ctx);

void Sg_Write(SgObject obj, SgObject p, int mode)
{
  SgWriteContext ctx;
  SgPort *port;

  if (!SG_OUTPORTP(p) && !SG_INOUTPORTP(p)) {
    Sg_Error(UC("output port required, but got %S"), p);
  }
  if (!SG_TEXTUAL_PORTP(p)) {
    /* for now I asuume it's a binary port. */
    SgTranscoder *trans = Sg_MakeNativeTranscoder();
    port = SG_PORT(Sg_MakeTranscodedOutputPort(p, trans));
  } else {
    /* for now I assume it's a textual port */
    port = SG_PORT(p);
  }
  ctx.mode = mode;
  ctx.table= NULL;
  ctx.flags = 0;
  ctx.sharedId = 0;

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
  out = Sg_MakeStringOutputPort(0);
  ctx.mode = mode;
  ctx.flags = WRITE_CIRCULAR;
  if (width > 0) {
    ctx.flags |= WRITE_LIMITED;
    ctx.limit = width;
  }
  ctx.ncirc = 0;
  ctx.table = Sg_MakeHashTableSimple(SG_HASH_EQ, 8);
  ctx.sharedId = 0;

  if (width <= 0) {
    SG_PORT_LOCK(SG_PORT(port));
    format_write(obj, SG_PORT(port), &ctx, TRUE);
    SG_PORT_UNLOCK(SG_PORT(port));
    return 0;
  }

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
static void format_integer(SgPort *out, SgObject arg, SgObject *params, int nparams,
			   int radix, int delimited, int alwayssign, int use_upper)
{
  int mincol = 0, commainterval = 3;
  SgChar padchar = ' ', commachar = ',';
  SgObject str;
  if (!Sg_IntegerP(arg)) {
    SgWriteContext ictx;
    ictx.mode = SG_WRITE_DISPLAY;
    ictx.flags = 0;
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
      for (i = 0; i < colcnt; i++) {
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
  SgObject arg, oargs = args;
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
  actx.mode = SG_WRITE_DISPLAY;
  actx.table = NULL;
  actx.flags = 0;
  actx.sharedId = 0;

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
    SgTranscoder *trans = Sg_MakeNativeTranscoder();
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

static void write_symbol_name(SgString *snam, SgPort *port, SgWriteContext *ctx, int flags)
{
  const SgChar *p = snam->value, *q;
  int size = snam->size, i;
  int escape = FALSE;
  int spmask = 0x12;
  int r6rsMode = SG_VM_IS_SET_FLAG(Sg_VM(), SG_R6RS_MODE);
  int mode = SG_WRITE_MODE(ctx);

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
  if (*p < 128
      && (special[*p] & 1)
      && (!(flags & SG_SYMBOL_WRITER_NOESCAPE_INITIAL))) {
    /* R6RS does not have '|' */
    escape = TRUE && (mode != SG_WRITE_LIBPATH);
  } else {
    for (i = 0, q = p; i < size; i++, q++) {
      if (*q < 128
	  && (special[*q] & spmask)) {
	escape = TRUE && (mode != SG_WRITE_LIBPATH);
	break;
      }
    }
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
}

static void write_symbol(SgSymbol *obj, SgPort *port, SgWriteContext *ctx)
{
  ASSERT(SG_STRINGP(obj->name));
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_PutsUnsafe(port, obj->name);
  } else {
    if (SG_UNINTERNED_SYMBOL(obj)) Sg_PutuzUnsafe(port, UC("#:"));
    write_symbol_name(obj->name, port, ctx, 0);
  }
}

static void write_string(SgString *obj, SgPort *port, SgWriteContext *ctx)
{
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_PutsUnsafe(port, obj);
  } else {
    SgChar *s = obj->value;
    int i, size = obj->size;
    Sg_PutcUnsafe(port, '"');
    for (i = 0; i < size; i++) {
      SgChar ch = s[i];
      switch (ch) {
      case '\\':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, '\\');
	break;
      case '\n':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, 'n');
	break;
      case '\a':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, 'a');
	break;
      case '\b':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, 'b');
	break;
      case '\t':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, 't');
	break;
      case '\v':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, 'v');
	break;
      case '\r':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, 'r');
	break;
      case '\f':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, 'f');
	break;
      default:
	{
	  const int ASCII_SPC = 0x20;
	  const int ASCII_DEL = 0x7f;
	  if ((ch != 0xa && ch != 0xd && ch < ASCII_SPC) ||
	      ch == ASCII_DEL ||
	      ch == 0x80 ||
	      ch == 0xff ||
	      ch == 0xD7FF ||
	      ch == 0xE000 ||
	      ch == 0x10FFFF) { // todo
	    char buf[32];
	    snprintf(buf, sizeof(buf), "\\x%X;", ch);
	    Sg_PutzUnsafe(port, buf);
	  } else {
	    Sg_PutcUnsafe(port, ch);
	  }
	}
      }
    }
    Sg_PutcUnsafe(port, '"');
  } 
}

static void write_identifier(SgIdentifier *id, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<identifier "));
  write_symbol(id->name, port, ctx);
  Sg_PutcUnsafe(port, '#');
  write_symbol(id->library->name, port, ctx);
#if 1
  if (SG_WRITE_MODE(ctx) == SG_WRITE_WRITE) {
    char buf[50];
    Sg_PutcUnsafe(port, ' ');
    snprintf(buf, sizeof(buf), "(%p)", id->envs);
    Sg_PutzUnsafe(port, buf);
    snprintf(buf, sizeof(buf), "(%p)", id);
    Sg_PutcUnsafe(port, ' ');
    Sg_PutzUnsafe(port, buf);
  }
#endif
  Sg_PutcUnsafe(port, '>');
}

static void write_library(SgLibrary *lib, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<library "));
  write_symbol(lib->name, port, ctx);
  Sg_PutcUnsafe(port, '>');
}

static void write_closure(SgClosure *cl, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<closure "));
  write_ss_rec(SG_PROCEDURE_NAME(cl), port, ctx);
  Sg_PutcUnsafe(port, '>');
}

static void write_subr(SgSubr *s, SgPort *port, SgWriteContext *ctx)
{
  int mode = ctx->mode; /* save */
  Sg_PutuzUnsafe(port, UC("#<subr "));
  ctx->mode = SG_WRITE_DISPLAY;
  write_ss_rec(SG_PROCEDURE_NAME(s), port, ctx);
  ctx->mode = mode;
  Sg_PutcUnsafe(port, '>');
}

static void write_syntax(SgSyntax *s, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<syntax "));
  if (SG_SYMBOLP(s->name)) {
    write_symbol(s->name, port, ctx);
  } else if (SG_IDENTIFIERP(s->name)) {
    write_symbol(SG_IDENTIFIER(s->name)->name, port, ctx);
  } else {
    write_ss_rec(s->name, port, ctx);
  }
  Sg_PutcUnsafe(port, '>');
}

static void write_macro(SgMacro *m, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<macro "));
  write_ss_rec(m->name, port, ctx);
  Sg_PutcUnsafe(port, '>');
}

static void write_keyword(SgKeyword *k, SgPort *port, SgWriteContext *ctx)
{
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    write_string(k->name, port, ctx);
  } else {
    Sg_PutcUnsafe(port, ':');
    write_symbol_name(k->name, port, ctx,
		      (SG_SYMBOL_WRITER_NOESCAPE_INITIAL
		       |SG_SYMBOL_WRITER_NOESCAPE_EMPTY));
  }
}

static void write_box(SgBox *b, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<box "));
  Sg_Printf(port, UC("0x%x"), b);
  Sg_PutcUnsafe(port, '>');
}

static void write_code_builder(SgCodeBuilder *cb, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<code-builder "));
  write_ss_rec(cb->name, port, ctx);
  Sg_Printf(port, UC(" (%d %d %d)>"), cb->argc, cb->optional, cb->freec);
}

static void write_hashtable(SgHashTable *ht, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<hashtable "));
  if (SG_IMMUTABLE_HASHTABLE_P(ht)) {
    Sg_PutuzUnsafe(port, UC("immutable "));
  }
  switch (ht->type) {
  case SG_HASH_EQ:
    Sg_PutuzUnsafe(port, UC("eq?"));
    break;
  case SG_HASH_EQV:
    Sg_PutuzUnsafe(port, UC("eqv?"));
    break;
  case SG_HASH_EQUAL:
    Sg_PutuzUnsafe(port, UC("equal?"));
    break;
  case SG_HASH_STRING:
    Sg_PutuzUnsafe(port, UC("string=?"));
    break;
  case SG_HASH_GENERAL:
    write_ss_rec(SG_HASHTABLE_CORE(ht)->generalHasher, port, ctx);
    Sg_PutcUnsafe(port, ' ');
    write_ss_rec(SG_HASHTABLE_CORE(ht)->generalCompare, port, ctx);
    break;
  }
  Sg_PutcUnsafe(port, '>');
}

static void write_values(SgValues *v, SgPort *port, SgWriteContext *ctx)
{
  int i;
  Sg_PutuzUnsafe(port, UC("#<values"));
  for (i = 0; i < v->size; i++) {
    Sg_PutcUnsafe(port, ' ');
    write_ss_rec(v->elements[i], port, ctx);
  }
  Sg_PutcUnsafe(port, '>');
}

static void write_generic(SgGeneric *g, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<generic "));
  write_symbol(g->name, port, ctx);
  Sg_PutcUnsafe(port, '>');
}

static void write_instance(SgInstance *i, SgPort *port, SgWriteContext *ctx)
{
  SgGeneric *generic = i->generic;
  if (!SG_NULLP(generic->printer) && SG_PROCEDUREP(generic->printer)) {
    Sg_Apply2(generic->printer, i, port);
    return;
  } else {
    /* search parent printer */
    SgObject parents = generic->parents;
    SgObject parent;
  retry:
    SG_FOR_EACH(parent, parents) {
      SgObject gen = SG_CAR(parent);
      /* doesn't have to be, but just in case */
      if (SG_GENERICP(gen)) {
	generic = SG_GENERIC(gen);
	if (!generic->virtualP) {
	  /* non virtual class has to be only one */
	  if(!SG_NULLP(generic->printer) && SG_PROCEDUREP(generic->printer)) {
	    Sg_Apply2(generic->printer, i, port);
	    return;
	  } else {
	    parents = generic->parents;
	    goto retry;
	  }
	}
      }
    }
  }
  Sg_PutuzUnsafe(port, UC("#<instance "));
  write_symbol(i->generic->name, port, ctx);
  Sg_PutcUnsafe(port, '>');
}

static void write_bytevector(SgByteVector *b, SgPort *port, SgWriteContext *ctx)
{
  int i, size = b->size;
  uint8_t *u8 = b->elements;
  char buf[32];
  Sg_PutuzUnsafe(port, UC("#vu8("));
  if (size != 0) {
    for (i = 0; i < size - 1; i++) {
      snprintf(buf, array_sizeof(buf), "%u", u8[i]);
      Sg_PutzUnsafe(port, buf);
      Sg_PutcUnsafe(port, ' ');
    }
    snprintf(buf, array_sizeof(buf), "%u", u8[i]);
    Sg_PutzUnsafe(port, buf);
  }
  Sg_PutcUnsafe(port, ')');
}

static void write_port(SgPort *p, SgPort *port, SgWriteContext *ctx)
{
  SgObject file = SG_FALSE;
  SgObject transcoder = SG_FALSE;
  Sg_PutuzUnsafe(port, UC("#<"));
  if (SG_BINARY_PORTP(p)) {
    switch (SG_BINARY_PORT(p)->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("file"));
      break;
    case SG_BYTE_ARRAY_BINARY_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("bytearray"));
      break;
    case SG_CUSTOM_BINARY_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("custom"));
      break;
    default:
      /* never happen */
      Sg_PutuzUnsafe(port, UC("unknown"));
    }
    Sg_PutuzUnsafe(port, UC("-binary"));
  } else if (SG_TEXTUAL_PORTP(p)) {
    switch (SG_TEXTUAL_PORT(p)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("transcoded"));
      break;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("string"));
      break;
    case SG_CUSTOM_TEXTUAL_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("custom"));
      break;
    default:
      /* never happen */
      Sg_PutuzUnsafe(port, UC("unknown"));
    }
    Sg_PutuzUnsafe(port, UC("-textual"));
  } else if (SG_CUSTOM_PORTP(p)) {
    Sg_PutuzUnsafe(port, UC("custom"));
    switch (SG_CUSTOM_PORT(p)->type) {
    case SG_BINARY_CUSTOM_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("-binary"));
      break;
    case SG_TEXTUAL_CUSTOM_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("-textual"));
      break;
    default:
      /* never happen */
      Sg_PutuzUnsafe(port, UC("-unknown"));
    }
  } else {
    /* never happen */
    Sg_PutuzUnsafe(port, UC("-unknown"));
  }
  if (SG_INPORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-input-port"));
  } else if (SG_OUTPORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-output-port"));
  } else if (SG_INOUTPORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-input/output-port"));
  }
  if (SG_CUSTOM_PORTP(p)) {
    Sg_PutcUnsafe(port, ' ');
    Sg_PutsUnsafe(port, SG_CUSTOM_PORT(p)->id);
  }

  file = Sg_FileName(p);
  if (!SG_FALSEP(file)) {
    Sg_PutcUnsafe(port, ' ');
    Sg_PutuzUnsafe(port, SG_FILE(file)->name);
  }
  transcoder = Sg_PortTranscoder(p);
  if (!SG_FALSEP(transcoder)) {
    Sg_PutcUnsafe(port, ' ');
    write_ss_rec(SG_CODEC_NAME(SG_TRANSCODER_CODEC(transcoder)), port, ctx);
  }
  if (Sg_PortClosedP(p)) {
    Sg_PutcUnsafe(port, ' ');
    Sg_PutuzUnsafe(port, UC("closed"));
  }
  Sg_PutcUnsafe(port, '>');
}

static void write_record_type(SgRecordType *rt, SgPort *port, SgWriteContext *ctx)
{
  SgWriteContext rtctx;
  Sg_PutuzUnsafe(port, UC("#<record-type "));
  write_symbol(rt->name, port, ctx);
  Sg_PutcUnsafe(port, ' ');
  write_ss(rt->rtd, port, &rtctx);
  Sg_PutcUnsafe(port, ' ');
  write_ss_rec(rt->rcd, port, &rtctx);
  Sg_PutcUnsafe(port, '>');
}

static void write_gloc(SgGloc *g, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<gloc "));
  write_symbol(g->name, port, ctx);
  Sg_PutcUnsafe(port, ' ');
  write_library(g->library, port, ctx);
  Sg_PutcUnsafe(port, '>');
}

static void write_transcoder(SgTranscoder *t, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<transcoder "));
  Sg_PutsUnsafe(port, SG_CODEC_NAME(SG_TRANSCODER_CODEC(t)));
  Sg_PutcUnsafe(port, ' ');
  switch (t->eolStyle) {
  case LF:     write_symbol(SG_INTERN("lf"), port, ctx); break;
  case CR:     write_symbol(SG_INTERN("cr"), port, ctx); break;
  case NEL:    write_symbol(SG_INTERN("nel"), port, ctx); break;
  case LS:     write_symbol(SG_INTERN("ls"), port, ctx); break;
  case CRNEL:  write_symbol(SG_INTERN("crnel"), port, ctx); break;
  case CRLF:   write_symbol(SG_INTERN("crlf"), port, ctx); break;
  case E_NONE: write_symbol(SG_INTERN("none"), port, ctx); break;
  }
  Sg_PutcUnsafe(port, ' ');
  
  switch (t->mode) {
  case SG_RAISE_ERROR:   write_symbol(SG_INTERN("raise"), port, ctx); break;
  case SG_REPLACE_ERROR: write_symbol(SG_INTERN("replace"), port, ctx); break;
  case SG_IGNORE_ERROR:  write_symbol(SG_INTERN("ignore"), port, ctx); break;
  }
  Sg_PutcUnsafe(port, '>');
}

static void write_codec(SgCodec *c, SgPort *port, SgWriteContext *ctx)
{
  int save = SG_WRITE_MODE(ctx);
  ctx->mode = SG_WRITE_DISPLAY;
  Sg_PutuzUnsafe(port, UC("#<codec "));
  write_ss_rec(SG_CODEC_NAME(c), port, ctx);
  Sg_PutcUnsafe(port, '>');
  ctx->mode = save;
}

static void write_meta_obj(SgObject *m, SgPort *port, SgWriteContext *ctx)
{
  if (SG_GET_META_OBJ(m)->printer) {
    SG_GET_META_OBJ(m)->printer(port, m, ctx);
  } else {
    Sg_PutuzUnsafe(port, UC("#<ext-obj>"));
  }
}

static void write_thread(SgVM *vm, SgPort *port, SgWriteContext *ctx)
{
  char buf[50];
  Sg_PutuzUnsafe(port, UC("#<thread "));
  write_ss_rec(vm->name, port, ctx);
  switch (vm->threadState) {
  case SG_VM_NEW:
    Sg_PutzUnsafe(port, " new");
    break;
  case SG_VM_RUNNABLE:
    Sg_PutzUnsafe(port, " runnable");
    break;
  case SG_VM_STOPPED:
    Sg_PutzUnsafe(port, " stopped");
    break;
  case SG_VM_TERMINATED:
    Sg_PutzUnsafe(port, " terminated");
    break;
  default:
    Sg_PutzUnsafe(port, " (unknonw state)");
    break;
  }
  snprintf(buf, sizeof(buf), " %p>", vm);
  Sg_PutzUnsafe(port, buf);
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

void write_ss_rec(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgObject e;
  SgHashTable *ht = ctx->table;

  if (ctx->flags & WRITE_LIMITED) {
    if (SG_TEXTUAL_PORT(port)->src.buffer.index >= ctx->limit) return;
  }

  if (!SG_PTRP(obj)) {
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
    } else {
      Sg_Panic("write: got a bogus object: %08x", SG_WORD(obj));
    }
    return;
  }

  if (SG_NUMBERP(obj)) {
    SgObject numStr = Sg_NumberToString(obj, 10, FALSE);
    short mode = SG_WRITE_MODE(ctx); /* save */
    ctx->mode = SG_WRITE_DISPLAY;    /* number must be display mode */
    write_string(numStr, port, ctx);
    ctx->mode = mode;
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
  } else if (SG_STRINGP(obj)) {
    write_string(SG_STRING(obj), port, ctx);
  } else if (SG_SYMBOLP(obj)) {
    write_symbol(SG_SYMBOL(obj), port, ctx);
  } else if (SG_IDENTIFIERP(obj)) {
    write_identifier(SG_IDENTIFIER(obj), port, ctx);
  } else if (SG_LIBRARYP(obj)) {
    write_library(SG_LIBRARY(obj), port, ctx);
  } else if (SG_CLOSUREP(obj)) {
    write_closure(SG_CLOSURE(obj), port, ctx);
  } else if (SG_SUBRP(obj)) {
    write_subr(SG_SUBR(obj), port, ctx);
  } else if (SG_SYNTAXP(obj)) {
    write_syntax(SG_SYNTAX(obj), port, ctx);
  } else if (SG_MACROP(obj)) {
    write_macro(SG_MACRO(obj), port, ctx);
  } else if (SG_KEYWORDP(obj)) {
    write_keyword(SG_KEYWORD(obj), port, ctx);
  } else if (SG_BOXP(obj)) {
    write_box(SG_BOX(obj), port, ctx);
  } else if (SG_CODE_BUILDERP(obj)) {
    write_code_builder(SG_CODE_BUILDER(obj), port, ctx);
  } else if (SG_HASHTABLE_P(obj)) {
    write_hashtable(SG_HASHTABLE(obj), port, ctx);
  } else if (SG_VALUESP(obj)) {
    write_values(SG_VALUES(obj), port, ctx);
  } else if (SG_GENERICP(obj)) {
    write_generic(SG_GENERIC(obj), port, ctx);
  } else if (SG_INSTANCEP(obj)) {
    write_instance(SG_INSTANCE(obj), port, ctx);
  } else if (SG_BVECTORP(obj)) {
    write_bytevector(SG_BVECTOR(obj), port, ctx);
  } else if (SG_PORTP(obj)) {
    write_port(SG_PORT(obj), port, ctx);
  } else if (SG_RECORD_TYPEP(obj)) {
    write_record_type(SG_RECORD_TYPE(obj), port, ctx);
  } else if (SG_GLOCP(obj)) {
    write_gloc(SG_GLOC(obj), port, ctx);
  } else if (SG_TRANSCODERP(obj)) {
    write_transcoder(SG_TRANSCODER(obj), port, ctx);
  } else if (SG_CODECP(obj)) {
    write_codec(SG_CODEC(obj), port, ctx);
  } else if (SG_META_OBJ_P(obj)) {
    write_meta_obj(obj, port, ctx);
  } else if (SG_VMP(obj)) {
    write_thread(SG_VM(obj), port, ctx);
  } else {
    Sg_PutuzUnsafe(port, UC("#<unknown datum>"));
  }
  return;
}

static void write_walk(SgObject obj, SgWriteContext *ctx)
{
  SgHashTable *ht;
  SgObject elt;

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
  write_walk(obj, ctx);

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

static void vprintf_proc(SgPort *port, const SgChar *fmt, SgObject args, int sharedp)
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
    SgTranscoder *trans = Sg_MakeNativeTranscoder();
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
	  if (value != NULL) SG_APPEND1(h, t, Sg_MakeString(value, SG_LITERAL_STRING));
	  else SG_APPEND1(h, t, Sg_MakeString(UC("(null)"), SG_LITERAL_STRING));
	  break;
	}
      case '%':
	break;
      case 'p':
	{
	  void *value = va_arg(sp, void *);
	  SG_APPEND1(h, t, Sg_MakeIntegerU((uintptr_t)value));
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

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
