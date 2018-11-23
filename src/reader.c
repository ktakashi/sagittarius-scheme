/* reader.c                                        -*- mode:c; coding:utf-8; -*-
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
#include <ctype.h>
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/reader.h"
#include "sagittarius/core.h"
#include "sagittarius/port.h"
#include "sagittarius/pair.h"
#include "sagittarius/symbol.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/compare.h"
#include "sagittarius/keyword.h"
#include "sagittarius/builtin-symbols.h"
#include "sagittarius/error.h"
#include "sagittarius/gloc.h"
#include "sagittarius/writer.h"
#include "sagittarius/unicode.h"
#include "sagittarius/values.h"
#include "sagittarius/vector.h"
#include "sagittarius/number.h"
#include "sagittarius/vm.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/unicode.h"
#include "sagittarius/weak.h"
#include "sagittarius/writer.h"
#include "sagittarius/library.h"

#include "shortnames.incl"

static uint8_t CHAR_MAP[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
 /*     !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /  */
    0,  3,  4,  4,  3,  3,  3,  0,  4,  4,  3,  1,  0,  1,  1,  3,
 /* 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?  */
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  3,  4,  3,  3,  3,  3,
 /* @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O  */
    1,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
 /* P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _  */
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  0,  4,  3,  3,
 /* `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o  */
    0,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
 /* p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   ^? */
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  0,  0,  0,  3,  0,
};
#define CHAR_MAP_SYMBOL    0x01
#define CHAR_MAP_INITIAL   0x02
#define CHAR_MAP_DELIMITER 0x04

#define SYMBOL_CHARP(x)    ((CHAR_MAP[x] & CHAR_MAP_SYMBOL) != 0)
#define INITIAL_CHARP(x)   ((CHAR_MAP[x] & CHAR_MAP_INITIAL) != 0)
#define DELIMITER_CHARP(x) ((CHAR_MAP[x] & CHAR_MAP_DELIMITER) != 0)

static SgObject pair_infos[] = {
  SG_UNDEF,
  SG_UNDEF
};

#define SYM_CONST       pair_infos[0]
#define SYM_SOURCE_INFO pair_infos[1]

static int convert_hex_char_to_int(SgChar c)
{
  if ((c >= '0') & (c <= '9')) return c - '0';
  else if ((c >= 'a') & (c <= 'f')) return c - 'a' + 10;
  else if ((c >= 'A') & (c <= 'F')) return c - 'A' + 10;
  return -1;
}

static void sref_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgSharedRef *ref = SG_SHAREDREF(obj);
  Sg_Putuz(port, UC("#<shared-ref "));
  if (SG_NUMBERP(ref->index)) {
    Sg_Puts(port, Sg_NumberToString(ref->index, 10, FALSE));
  } else {
    Sg_Putuz(port, UC("???"));
  }
  Sg_Putc(port, '>');
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SharedRefClass, sref_print);

/* TODO, we probably want to have slot access */
static void rctx_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_PutuzUnsafe(port, UC("#<read-context>"));
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ReadContextClass, rctx_print);

static SgReadContext * make_read_context()
{
  SgReadContext *ctx = SG_NEW(SgReadContext);
  SG_SET_CLASS(ctx, SG_CLASS_READ_CONTEXT);
  return ctx;
}
static SgReadContext *DEFAULT_CONTEXT = NULL;

SgObject Sg_MakeDefaultReadContext()
{
  return SG_OBJ(make_read_context());
}

SgObject Sg_MakeReadContextForLoad()
{
  SgReadContext *ctx = make_read_context();
  ctx->flags = SG_READ_SOURCE_INFO | SG_CHANGE_VM_MODE;
  ctx->graph = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_EQ, 1));
  return SG_OBJ(ctx);
}

static SgSharedRef* make_shared_ref(int mark)
{
  SgSharedRef *z = SG_NEW(SgSharedRef);
  SG_SET_CLASS(z, SG_CLASS_SHARED_REF);
  z->index = SG_MAKE_INT(mark);
  return z;
}

/* ctx utility */
static void parsing_range(SgReadContext *ctx, int from, int to)
{
  ctx->parsingLineFrom = from;
  ctx->parsingLineTo = to;
}

static void parsing_line(SgReadContext *ctx, int line)
{
  parsing_range(ctx, line, line);
}


typedef struct
{
  int value;
  int present;
} dispmacro_param;

typedef SgObject (*macro_function)(SgPort *, SgChar, SgReadContext *);
typedef SgObject (*dispmacro_function)(SgPort *, SgChar,
				       dispmacro_param *, SgReadContext *);

typedef enum {
  CT_ILLEGAL,
  CT_CONSTITUENT,
  CT_SINGLE_ESCAPE,
  CT_WHITE_SPACE,
  CT_TERM_MACRO,
  CT_NON_TERM_MACRO
} char_type;

typedef struct
{
  SgObject sfunc;
  dispmacro_function cfunc;
} disptab_t;

/* we only support ASCII */
#define MAX_READTABLE_CHAR 128

typedef struct
{
  char_type type;
  SgObject  sfunc;
  macro_function cfunc;
  disptab_t *disp;
} readtab_t;

struct readtable_rec_t
{
  int      insensitiveP;
  SgObject (*symbol_reader)(SgPort *, SgChar, SgReadContext *,
			    SgObject *errorp);
  readtab_t readtable[MAX_READTABLE_CHAR];
};

/* template readtables */
static readtable_t r6rs_read_table;
static readtable_t r7rs_read_table;
/* including r7rs #u8() */
static readtable_t compat_read_table;

static readtable_t* default_readtable(int copyP);

#define DEFAULT_TABLEP(port)				\
  (!SG_PORT_READTABLE(port) ||				\
   SG_PORT_READTABLE(port) == default_readtable(FALSE))

#define ENSURE_COPIED_TABLE(port)				\
  do {								\
    if (DEFAULT_TABLEP(port)) {					\
      SG_PORT_READTABLE(port) = default_readtable(TRUE);	\
    }								\
  } while(0)

static int delimited(SgPort *p, SgChar c)
{
  readtable_t *table = SG_PORT_READTABLE(p);
  if (c > 127 && Sg_Ucs4WhiteSpaceP(c)) return TRUE;
  if (c > 127) return FALSE;
  /* return DELIMITER_CHARP(c); */
  if (!table) {
    /* use default table */
    table = default_readtable(FALSE);
  }
  switch (table->readtable[c].type) {
  case CT_NON_TERM_MACRO:
  case CT_CONSTITUENT:
    /* is escape a delmiter? */
  case CT_SINGLE_ESCAPE:
    return FALSE;
  default: return TRUE;
  }
}

static SgObject read_expr4(SgPort *port, int flags, SgChar delim, 
			   SgReadContext *ctx);
#define ACCEPT_EOF 1
#define ACCEPT_DOT 2

#define read_expr(p, c) read_expr4((p), 0, EOF, (c))
/* one char reader */
static SgObject read_open_paren(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_close_paren(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_double_quote(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_quote(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_quasiquote(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_unquote(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_colon(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_vertical_bar(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_open_bracket(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_close_bracket(SgPort *port, SgChar c, SgReadContext *ctx);
static SgObject read_semicolon(SgPort *port, SgChar c, SgReadContext *ctx);
/* two chars */
/* list of two char syntax
   #-',`!|uvtTfFobBOdDxXiIeE(\;=#
 */
static SgObject read_hash_quote(SgPort *port, SgChar c,
				dispmacro_param *param, SgReadContext *ctx);
static SgObject read_hash_quasiquote(SgPort *port, SgChar c,
				     dispmacro_param *param, 
				     SgReadContext *ctx);
static SgObject read_hash_unquote(SgPort *port, SgChar c,
				  dispmacro_param *param, SgReadContext *ctx);
static SgObject read_hash_bang(SgPort *port, SgChar c, dispmacro_param *param,
			       SgReadContext *ctx);
static SgObject read_hash_bar(SgPort *port, SgChar c, dispmacro_param *param,
			      SgReadContext *ctx);
static SgObject read_hash_v(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_u(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_t(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_f(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_b(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_o(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_d(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_x(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_i(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_e(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx);
static SgObject read_hash_open_paren(SgPort *port, SgChar c,
				     dispmacro_param *param,
				     SgReadContext *ctx);
static SgObject read_hash_semicolon(SgPort *port, SgChar c,
				    dispmacro_param *param, SgReadContext *ctx);
static SgObject read_hash_escape(SgPort *port, SgChar c, dispmacro_param *param,
				 SgReadContext *ctx);
static SgObject read_hash_equal(SgPort *port, SgChar c, dispmacro_param *param,
				SgReadContext *ctx);
static SgObject read_hash_hash(SgPort *port, SgChar c, dispmacro_param *param,
			       SgReadContext *ctx);
static SgObject read_hash_less(SgPort *port, SgChar c, dispmacro_param *param,
			       SgReadContext *ctx);
static SgObject read_hash_colon(SgPort *port, SgChar c, dispmacro_param *param,
				SgReadContext *ctx);

/* mode  */
static SgObject read_r6rs_symbol(SgPort *port, SgChar c, SgReadContext *ctx,
				 SgObject *errorp);
static SgObject read_compatible_symbol(SgPort *port, SgChar c,
				       SgReadContext *ctx, SgObject *errorp);
/* utility */
static SgObject read_symbol_or_number(SgPort *port, SgChar c,
				      readtable_t *table, SgReadContext *ctx);
static SgObject macro_reader(SgPort *port, SgChar c, readtab_t *tab,
			     SgReadContext *ctx);
static SgObject dispmacro_reader(SgPort *port, SgChar c, SgReadContext *ctx);

static void add_read_table(readtable_t *src, readtable_t *dst);
/* TODO use this in lexical_error */
static SgObject lexical_error_msg(SgPort * port, SgReadContext *ctx,
				  const SgChar *fmt, ...)
{
  va_list ap;
  SgObject msg, line, file;

  va_start(ap, fmt);
  msg = Sg_Vsprintf(fmt, ap, TRUE);
  va_end(ap);

  file = Sg_FileName(port);
  if (ctx->parsingLineFrom == ctx->parsingLineTo) {
    line = Sg_Sprintf(UC("file %S, line %d"),
		      file,
		      ctx->parsingLineFrom);
  } else {
    line = Sg_Sprintf(UC("file %S, line %d-%d"),
		      file,
		      ctx->parsingLineFrom, ctx->parsingLineTo);
  }
  return Sg_Sprintf(UC("%A (%A)"), msg, line);
}

static void lexical_error(SgPort * port, SgReadContext *ctx,
			  const SgChar *fmt, ...)
{
  va_list ap;
  SgObject msg, line, file;

  va_start(ap, fmt);
  msg = Sg_Vsprintf(fmt, ap, TRUE);
  va_end(ap);

  file = Sg_FileName(port);
  if (ctx->parsingLineFrom == ctx->parsingLineTo) {
    line = Sg_Sprintf(UC("file %S, line %d"),
		      file,
		      ctx->parsingLineFrom);
  } else {
    line = Sg_Sprintf(UC("file %S, line %d-%d"),
		      file,
		      ctx->parsingLineFrom, ctx->parsingLineTo);
  }
  Sg_ReadError(UC("%A (%A)"), msg, line);
}

static int read_thing(SgPort *port, SgReadContext *ctx, SgChar *buf,
		      size_t size, SgChar initial)
{
  size_t i = 0;
  if (initial != -1) {
    buf[i++] = initial;
  }
  while (i < size) {
    SgChar c = Sg_PeekcUnsafe(port);
    if (c == EOF || delimited(port, c)) {
      buf[i] = 0;
      return i;
    }
    Sg_GetcUnsafe(port);
    buf[i++] = c;
  }
  lexical_error(port, ctx,
		UC("token buffer overflow while reading identifier, %s ..."),
		buf);
  return -1;			/* dummy */
}

static SgChar read_hex_scalar_value(SgPort *port, SgReadContext *ctx)
{
  int n;
  SgChar ucs4 = 0, c = Sg_GetcUnsafe(port);
  if (c == EOF) {
    lexical_error(port, ctx,
		  UC("unexpected end-of-file while reading hex scalar value"));
  }
  if (delimited(port, c)) {
    lexical_error(port, ctx,
		  UC("expected hex digit, but got %c, while reading hex scalar value"), c);
  }
  Sg_UngetcUnsafe(port, c);

  while (TRUE) {
    c = Sg_GetcUnsafe(port);
    if (c == EOF || delimited(port, c)) {
      Sg_UngetcUnsafe(port, c);
      return Sg_EnsureUcs4(ucs4);
    }
    n = convert_hex_char_to_int(c);
    if (n < 0) {
      lexical_error(port, ctx,
		    UC("expected hex digit, but got %c, while reading hex scalar value"), c);
    }
    ucs4 = (ucs4 << 4) + n;
    if (ucs4 > 0x10ffff) {
      lexical_error(port, ctx, UC("hex scalar value out of range"));
    }
  }
}

static SgChar read_escape(SgPort *port, SgReadContext *ctx)
{
  SgChar c = Sg_GetcUnsafe(port);
  switch (c) {
  case 'x':
    c = Sg_GetcUnsafe(port);
    if (c == EOF) {
      lexical_error(port, ctx,
		    UC("unexpected end-of-file while reading escape sequence"));
    }
    Sg_UngetcUnsafe(port, c);
    c = read_hex_scalar_value(port, ctx);
    if (Sg_GetcUnsafe(port) != ';') {
      lexical_error(port, ctx,
		    UC("inline hex escape missing terminating semi-colon"));
    }
    return c;
  case 'a':  return 0x0007;
  case 'b':  return 0x0008;
  case 't':  return 0x0009;
  case 'n':  return 0x000A;
  case 'v':  return 0x000B;
  case 'f':  return 0x000C;
  case 'r':  return 0x000D;
  case '"':  return 0x0022;
  case '\\': return 0x005C;
    /* R7RS */
  case '|':  return 0x007C;
  case EOF: 
    lexical_error(port, ctx, 
		  UC("unexpected end-of-file while reading escape sequence"));
  default:
    if (ctx->escapedp) return c;
    lexical_error(port, ctx, 
		  UC("invalid escape sequence, \\%c"), c);
  }

  return -1;			/* dummy */
}

typedef int (*read_helper)(SgPort *, SgReadContext *, SgChar *, 
			   int, SgChar, SgObject, readtable_t *table,
			   SgObject *errorp);
/* convenient macro, used both symbol and string reader*/
#define append_char(buf, p, c, i)		\
  do {						\
    if (p) {					\
      Sg_PutcUnsafe(p, (c));			\
    } else {					\
      (buf)[(i)++] = (c);			\
    }						\
  } while (0)

#define READ_SYMBOL_MAX_SIZE 256

static SgObject read_symbol_generic(SgPort *port, SgChar initial,
				    read_helper helper,
				    SgReadContext *ctx, SgObject *errorp)
{
  SgChar buf[READ_SYMBOL_MAX_SIZE], c;
  int i = 0;
  readtable_t *table = Sg_PortReadTable(port);
  SgObject out = NULL;

  if (initial > 0) {
    if (initial > 127) {
      Sg_EnsureUcs4(initial);
      if (i == 0) {
	if (Sg_Ucs4ConstituentP(initial)) {
	  append_char(buf, out, initial, i);
	  goto next;
	}
      } else {
	if (Sg_Ucs4SubsequentP(initial)) {
	  append_char(buf, out, initial, i);
	  goto next;
	}
      }
      lexical_error(port, ctx,
		    UC("invalid character %U during reading identifier"),
		    initial);
    } else {
      i = helper(port, ctx, buf, i, initial, out, table, errorp);
    }
  }
 next:
  while (out != NULL || i < array_sizeof(buf)) {
    c = Sg_PeekcUnsafe(port);
    if (c == EOF || delimited(port, c)) {
      if (out) {
	return Sg_GetStringFromStringPort(out);
      } else {
	SgObject s;
	buf[i] = 0;
	s = Sg_MakeString(buf, SG_LITERAL_STRING, i);
	if (table->insensitiveP) {
	  s = Sg_StringFoldCase(SG_STRING(s));
	}
	return s;
      }
    }
    Sg_GetcUnsafe(port);
    if (c == '\\') {
      /* TODO how should I treat '\\' during reading symbol */
      c = Sg_GetcUnsafe(port);
      if (c == 'x') {
	Sg_UngetcUnsafe(port, c);
	append_char(buf, out, read_escape(port, ctx), i);
	continue;
      }
      lexical_error(port, ctx, 
		    UC("invalid character '\\' during reading identifier"));
    }
    if (c > 127) {
      Sg_EnsureUcs4(c);
      if (i == 0) {
	if (Sg_Ucs4ConstituentP(c)) {
	  append_char(buf, out, c, i);
	  continue;
	}
      } else {
	if (Sg_Ucs4SubsequentP(c)) {
	  append_char(buf, out, c, i);
	  continue;
	}
      }
      lexical_error(port, ctx,
		    UC("invalid character %U during reading identifier"), c);
    }
    i = helper(port, ctx, buf, i, c, out, table, errorp);
  }
  out = Sg_ConvertToStringOutputPort(buf, i);
  goto next;
  /* lexical_error(port, ctx, */
  /* 		UC("token buffer overflow during reading identifier")); */
  return SG_UNDEF;
#undef check_range
}

static int read_r6rs_symbol_helper(SgPort *port, SgReadContext *ctx, 
				   SgChar *buf, int i, SgChar c,
				   SgObject out,
				   readtable_t *table, SgObject *errorp)
{
  if (c > 127) {
    if (buf != NULL) append_char(buf, out, c, i);
    return i;
  }
  if (i == 0) {
    if (INITIAL_CHARP(c)) {
      if (buf != NULL) append_char(buf, out, c, i);
      return i;
    }
  } else {
    if (SYMBOL_CHARP(c)) {
      if (buf != NULL) append_char(buf, out, c, i);
      return i;
    }
  }
  if (errorp) {
    if (buf != NULL) append_char(buf, out, c, i);
    *errorp = 
      lexical_error_msg(port, ctx,
			UC("invalid character %U while reading identifier"), c);
    return i;
  } else {
    lexical_error(port, ctx,
		  UC("invalid character %U while reading identifier"), c);
  }
  return -1;			/* dummy */
}

SgObject read_r6rs_symbol(SgPort *port, SgChar initial, SgReadContext *ctx,
			  SgObject *errorp)
{
  return read_symbol_generic(port, initial, read_r6rs_symbol_helper, ctx,
			     errorp);
}

static int read_compat_symbol_helper(SgPort *port, SgReadContext *ctx, 
				     SgChar *buf, int i, SgChar c,
				     SgObject out,
				     readtable_t *table, SgObject *errorp)
{
  if (!delimited(port, c)) {
    append_char(buf, out, c, i);
    return i;
  }
  if (errorp) {
    append_char(buf, out, c, i);
    *errorp = 
      lexical_error_msg(port, ctx,
			UC("invalid character %U while reading identifier"), c);
    return i;
  } else {
    lexical_error(port, ctx,
		  UC("invalid character %U while reading identifier"), c);
  }
  return -1;			/* dummy */
}

SgObject read_compatible_symbol(SgPort *port, SgChar initial, 
				SgReadContext *ctx, SgObject *errorp)
{
  return read_symbol_generic(port, initial, read_compat_symbol_helper, ctx,
			     errorp);
}

static SgObject read_escaped_symbol(SgPort *port, SgReadContext *ctx)
{
  SgObject str = read_compatible_symbol(port, -1, ctx, NULL);
  return Sg_Intern(str);
}

SgObject read_symbol_or_number(SgPort *port, SgChar c,
			       readtable_t *table, SgReadContext *ctx)
{
  SgObject str, num, tmp;
  SgObject error = SG_FALSE;
  str = table->symbol_reader(port, c, ctx, &error);
  if (table->symbol_reader == read_r6rs_symbol) {
    tmp = str;
  } else {
    /* R7RS requires capital NaN or InF */
    tmp = Sg_StringDownCase(str);
  }
  num  = Sg_StringToNumber(tmp, 10, TRUE);
  if (!SG_FALSEP(num)) return num;

  /* check special case first */
  if (SG_STRING_SIZE(str) == 1 && SG_STRING_VALUE_AT(str, 0) == '.')
    return SG_SYMBOL_DOT;
  /* well for now we do not check */
  if (table->symbol_reader == read_r6rs_symbol &&
      SG_STRING_VALUE_AT(str, 0) < 128) {
    int i;
    if (SG_STRING_SIZE(str) == 1 &&
	(SG_STRING_VALUE_AT(str, 0)=='+' || SG_STRING_VALUE_AT(str, 0)=='-')) {
      return Sg_Intern(str);
    }
    if (ustrcmp(SG_STRING_VALUE(str), "...") == 0) return SG_SYMBOL_ELLIPSIS;
    if (SG_STRING_SIZE(str) >= 2 &&
	SG_STRING_VALUE_AT(str, 0) == '-'
	&& SG_STRING_VALUE_AT(str, 1) == '>') {
      for (i = 2; i < SG_STRING_SIZE(str); i++) {
	SgChar c = SG_STRING_VALUE_AT(str, i);
	if (c > 127) continue;
	if (SYMBOL_CHARP(c)) continue;
	lexical_error(port, ctx, UC("invalid lexical syntax %A"), str);
      }
      return Sg_Intern(str);
    }
  }
  /* if there is an lexical error after all checks, then raise it */
  if (!SG_FALSEP(error)) Sg_ReadError(UC("%A"), error);
  return Sg_Intern(str);

}

SgObject macro_reader(SgPort *port, SgChar c, readtab_t *tab,
			     SgReadContext *ctx)
{
  if (tab[c].cfunc) return (*tab[c].cfunc)(port, c, ctx);
  if (SG_EQ(tab[c].sfunc, SG_UNBOUND))
    lexical_error(port, ctx, UC("'%c' is not a macro char"), c);
  /* FIXME: we should eliminate this type of dispatch */
  if (SG_PROCEDURE_REQUIRED(tab[c].sfunc) > 2) {
    return Sg_Apply3(tab[c].sfunc, port, SG_MAKE_CHAR(c), ctx);
  } else {
    return Sg_Apply2(tab[c].sfunc, port, SG_MAKE_CHAR(c));
  }
}

static SgObject read_list_int(SgPort *port, SgChar closer, SgReadContext *ctx,
			      int start_line)
{
  SgObject start = SG_NIL, last = SG_NIL, item;
  item = read_expr4(port, ACCEPT_EOF, closer, ctx);
  if (SG_EQ(item, SG_EOF)) goto eoferr;
  /* return '() */
  if (!ctx->escapedp && SG_EQ(item, SG_SYMBOL_RPAREN)) return start;

  SG_APPEND1(start, last, item);

  for (;;) {
    ctx->escapedp = FALSE;
    item = read_expr4(port, ACCEPT_EOF | ACCEPT_DOT, closer, ctx);
    if (SG_EQ(item, SG_EOF)) goto eoferr;
    if (!ctx->escapedp && SG_EQ(item, SG_SYMBOL_RPAREN)) return start;
    if (!ctx->escapedp && SG_EQ(item, SG_SYMBOL_DOT)) {
      SG_SET_CDR(last, read_expr(port, ctx));
      item = read_expr4(port, ACCEPT_EOF, closer, ctx);
      if (!SG_EQ(item, SG_SYMBOL_RPAREN)) {
	parsing_range(ctx, start_line, Sg_LineNo(port));
	lexical_error(port, ctx, UC("bad dot syntax"));
      }
      if (SG_EQ(item, SG_EOF)) goto eoferr;
      return start;
    }
    SG_APPEND1(start, last, item);
  }
 eoferr:
  parsing_range(ctx, start_line, Sg_LineNo(port));
  lexical_error(port, ctx, UC("unexpected end-of-file while reading a list"));
  return SG_UNDEF;		/* dummy */
}

static SgObject read_list(SgPort *port, SgChar closer, SgReadContext *ctx)
{
  int line = Sg_LineNo(port);
  SgObject r = read_list_int(port, closer, ctx, line);
  if (SG_PAIRP(r) && line >= 0) {
    SgVM *vm = Sg_VM();
    if (!SG_VM_IS_SET_FLAG(vm, SG_NO_DEBUG_INFO)) {
      SgObject info = Sg_FileName(port);
      if (!SG_FALSEP(info) && ctx->flags & SG_READ_SOURCE_INFO) {
	r = Sg_SetPairAnnotation(r, SYM_SOURCE_INFO, 
				 Sg_Cons(info, SG_MAKE_INT(line)));
      }
    }
  }
  return r;
}

SgObject read_open_paren(SgPort *port, SgChar c, SgReadContext *ctx)
{
  return read_list(port, ')', ctx);
}

SgObject read_close_paren(SgPort *port, SgChar c, SgReadContext *ctx)
{
  lexical_error(port, ctx, UC("unexpected close paren ')'"));
  return SG_UNDEF;		/* dummy */
}

SgObject read_open_bracket(SgPort *port, SgChar c, SgReadContext *ctx)
{
  return read_list(port, ']', ctx);
}

SgObject read_close_bracket(SgPort *port, SgChar c, SgReadContext *ctx)
{
  lexical_error(port, ctx, UC("unexpected close bracket ']'"));
  return SG_UNDEF;		/* dummy */
}

SgObject read_double_quote(SgPort *port, SgChar c, SgReadContext *ctx)
{
  SgChar buf[READ_STRING_MAX_SIZE];
  int i = 0;
  SgObject out = NULL;

#define handle_linefeed(c, hndl)				\
  switch (c) {							\
  case CR:							\
    (c) = Sg_GetcUnsafe(port);					\
    if ((c) != LF && (c) != NEL) Sg_UngetcUnsafe(port, c);	\
  case LF: case NEL: case LS:					\
    hndl;							\
  }

 next:
  while (out != NULL || i < array_sizeof(buf)) {
    SgChar c = Sg_GetcUnsafe(port);
    if (c == EOF)
      lexical_error(port, ctx,
		    UC("unexpected end-of-file while reading string"));

    handle_linefeed(c, { append_char(buf, out, LF, i); continue; });

    if (c == '"') {
      if (out) {
	return Sg_GetStringFromStringPort(out);
      } else {
	buf[i] = 0;
	return Sg_MakeString(buf, SG_LITERAL_STRING, i);
      }
    }
    if (c == '\\') {
      c = Sg_GetcUnsafe(port);
      if (Sg_Ucs4IntralineWhiteSpaceP(c)) {
	do {
	  c = Sg_GetcUnsafe(port);
	  if (c == EOF) {
	    lexical_error(port, ctx,
			  UC("unexpected end-of-file while"
			     " reading intraline whitespace"));
	  }
	} while (Sg_Ucs4IntralineWhiteSpaceP(c));
	/* internal line feed is LF*/
	handle_linefeed(c, break; default:
			lexical_error(port, ctx,
				      UC("unexpected character %U while"
					 " reading intraline whitespace"), c)
			);
	do { c = Sg_GetcUnsafe(port); } while (Sg_Ucs4IntralineWhiteSpaceP(c));
	Sg_UngetcUnsafe(port, c);
	continue;
      }
      handle_linefeed(c,
		      {
			do { c = Sg_GetcUnsafe(port); }
			while (Sg_Ucs4IntralineWhiteSpaceP(c));
			Sg_UngetcUnsafe(port, c);
			continue;
		      }
		      );
      Sg_UngetcUnsafe(port, c);
      c = read_escape(port, ctx);
      append_char(buf, out, c, i);
      continue;
    } else {
      append_char(buf, out, c, i);
    }
  }
  out = Sg_ConvertToStringOutputPort(buf, i);
  goto next;
  /* lexical_error(port, ctx, UC("token buffer overflow while reading string")); */
  return SG_UNDEF; /* dummy */
}

SgObject read_quote(SgPort *port, SgChar c, SgReadContext *ctx)
{
  SgObject o = read_expr(port, ctx);
  if (SG_EQ(o, SG_EOF)) {
    lexical_error(port, ctx,
		  UC("unexpected end-of-file following quotation-mark(')"));
  }
  if (SG_PAIRP(o)) {
    o = Sg_AddConstantLiteral(o);
  }
  return SG_LIST2(SG_SYMBOL_QUOTE, o);
}

SgObject read_quasiquote(SgPort *port, SgChar c, SgReadContext *ctx)
{
  SgObject o = read_expr(port, ctx);
  if (SG_EQ(o, SG_EOF)) {
    lexical_error(port, ctx,
		  UC("unexpected end-of-file following grave-accent(`)"));
  }
  return SG_LIST2(SG_SYMBOL_QUASIQUOTE, o);
}

SgObject read_unquote(SgPort *port, SgChar c, SgReadContext *ctx)
{
  c = Sg_GetcUnsafe(port);
  if (c == EOF) {
    lexical_error(port, ctx, UC("unexpected end-of-file following comma(,)"));
  }
  if (c == '@')
    return SG_LIST2(SG_SYMBOL_UNQUOTE_SPLICING, read_expr(port, ctx));
  Sg_UngetcUnsafe(port, c);
  return SG_LIST2(SG_SYMBOL_UNQUOTE, read_expr(port, ctx));
}

static SgObject read_quoted_symbol(SgPort *port, SgReadContext *ctx,
				   int interned)
{
  SgChar buf[SYMBOL_MAX_SIZE];
  int i = 0;
  /* TODO flag check */
  ctx->escapedp = TRUE;
  while (i < array_sizeof(buf)) {
    SgChar c = Sg_GetcUnsafe(port);
    if (c == EOF) {
      lexical_error(port, ctx,
		    UC("unexpected end-of-file while reading quoted symbol"));
    }
    if (c == '|') {
      SgChar *real = SG_NEW_ATOMIC2(SgChar *, (i + 1) * sizeof(SgChar));
      buf[i] = 0;
      memcpy(real, buf, (i + 1) * sizeof(SgChar));
      return Sg_MakeSymbol(Sg_MakeString(real, SG_LITERAL_STRING, i),
			   interned);
    }
    if (c == '\\') {
      c = read_escape(port, ctx);
    }
    buf[i++] = c;
  }
  lexical_error(port, ctx,
		UC("token buffer overflow while reading quoted symbol"));
  return SG_UNDEF; /* dummy */
}

SgObject read_colon(SgPort *port, SgChar c, SgReadContext *ctx)
{
  int c2 = Sg_GetcUnsafe(port);
  SgObject name;
  SgChar buf[SYMBOL_MAX_SIZE];	/* too small? */

  if (c2 == '|') {
    name = read_quoted_symbol(port, ctx, FALSE);
    return Sg_MakeKeyword(SG_SYMBOL(name)->name);
  } else {
    int size;
    Sg_UngetcUnsafe(port, c2);
    size = read_thing(port, ctx, buf, array_sizeof(buf), -1);
    buf[size] = 0;
    return Sg_MakeKeyword(Sg_MakeString(buf, SG_LITERAL_STRING, size));
  }
}

SgObject read_vertical_bar(SgPort *port, SgChar c, SgReadContext *ctx)
{
  return read_quoted_symbol(port, ctx, TRUE);
}

SgObject read_semicolon(SgPort *port, SgChar c, SgReadContext *ctx)
{
  while ((c = Sg_GetcUnsafe(port)) != EOF) {
    if (c == LF) return NULL;
  }
  return SG_EOF;  
}

SgObject read_hash_quote(SgPort *port, SgChar c,
			 dispmacro_param *param, SgReadContext *ctx)
{
  return SG_LIST2(SG_SYMBOL_SYNTAX, read_expr(port, ctx));
}

SgObject read_hash_quasiquote(SgPort *port, SgChar c,
			      dispmacro_param *param, SgReadContext *ctx)
{
  return SG_LIST2(SG_SYMBOL_QUASISYNTAX, read_expr(port, ctx));
}

SgObject read_hash_unquote(SgPort *port, SgChar c,
			   dispmacro_param *param, SgReadContext *ctx)
{
  c = Sg_GetcUnsafe(port);
  if (c == EOF) {
    lexical_error(port, ctx,
		  UC("unexpected end-of-file following sharp-comma(,)"));
  }
  if (c == '@') {
    return SG_LIST2(SG_SYMBOL_UNSYNTAX_SPLICING, read_expr(port, ctx));
  }
  Sg_UngetcUnsafe(port, c);
  return SG_LIST2(SG_SYMBOL_UNSYNTAX, read_expr(port, ctx));  
}

static SgObject construct_lib_name(SgObject s)
{
  SgObject h = SG_NIL, t = SG_NIL;
  int i, prev;
  for (i = 0, prev = 0; i < SG_STRING_SIZE(s); i++) {
    if (SG_STRING_VALUE_AT(s, i) == '/') {
      SG_APPEND1(h, t, Sg_Intern(Sg_Substring(s, prev, i)));
      prev = i + 1;
    }
  }
  if (i != prev) {
    SG_APPEND1(h, t, Sg_Intern(Sg_Substring(s, prev, i)));
  }
  return h;
}

/* FIXME: we may want to add more directive on runtime (like Gauche) */
SgObject Sg_ApplyDirective(SgPort *port, SgObject desc, SgReadContext *ctx)
{
  SgString *tag = SG_SYMBOL(desc)->name;
  
  if (!ctx) {
    ctx = DEFAULT_CONTEXT;
  }
  /* a bit of optimisation... */
  switch (SG_STRING_VALUE_AT(tag, 0)) {
  case 'c':
    if (ustrcmp(tag->value, "compatible") == 0) {
      SgVM *vm = Sg_VM();
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(vm, SG_COMPATIBLE_MODE);
	SG_VM_UNSET_FLAG(vm, SG_R6RS_MODE);
	SG_VM_UNSET_FLAG(vm, SG_R7RS_MODE);
	SG_VM_SET_FLAG(vm, SG_ALLOW_OVERWRITE);
	SG_VM_UNSET_FLAG(vm, SG_ERROR_UNBOUND);
      }
      Sg_SetPortReadTable(port, Sg_CopyReadTable(&compat_read_table));
      return desc;
    }
    if (ustrcmp(tag->value, "core") == 0) {
      SgVM *vm = Sg_VM();
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_UNSET_FLAG(vm, SG_COMPATIBLE_MODE);
	SG_VM_UNSET_FLAG(vm, SG_R6RS_MODE);
	SG_VM_UNSET_FLAG(vm, SG_R7RS_MODE);
	SG_VM_UNSET_FLAG(vm, SG_ALLOW_OVERWRITE);
	SG_VM_UNSET_FLAG(vm, SG_ERROR_UNBOUND);
      }
      Sg_SetPortReadTable(port, Sg_CopyReadTable(&compat_read_table));
      return desc;
    }
    if (ustrcmp(tag->value, "cache") == 0) {
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_UNSET_FLAG(Sg_VM(), SG_DISABLE_CACHE);
      }
      return desc;
    }
    break;
  case 'd':
    if (ustrcmp(tag->value, "deprecated") == 0) {
      Sg_Warn(UC("deprecated file is being loaded %S"), Sg_FileName(port));
      return desc;
    }
    break;
  case 'f':
    if (ustrcmp(tag->value, "fold-case") == 0) {
      ENSURE_COPIED_TABLE(port);
      SG_PORT_READTABLE(port)->insensitiveP = TRUE;
      /* we need to preserve for include-ci with #!fold-case */
      ctx->flags |= SG_READ_NO_CASE;
      ctx->flags &= ~SG_READ_CASE;
      return desc;
    }
    break;
  case 'n':
    if (ustrcmp(tag->value, "no-overwrite") == 0) {
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_UNSET_FLAG(Sg_VM(), SG_ALLOW_OVERWRITE);
      }
      return desc;
    }
    if (ustrcmp(tag->value, "no-fold-case") == 0) {
      ENSURE_COPIED_TABLE(port);
      SG_PORT_READTABLE(port)->insensitiveP = FALSE;
      ctx->flags &= ~SG_READ_NO_CASE;
      ctx->flags |= SG_READ_CASE;
      return desc;
    }
    if (ustrcmp(tag->value, "nocache") == 0) {
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(Sg_VM(), SG_DISABLE_CACHE);
      }
      return desc;
    }
    if (ustrcmp(tag->value, "noinlineasm") == 0) {
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(Sg_VM(), SG_NO_INLINE_ASM);
      }
      return desc;
    }
    if (ustrcmp(tag->value, "noinlinelocal") == 0) {
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(Sg_VM(), SG_NO_INLINE_LOCAL);
      }
      return desc;
    }
    if (ustrcmp(tag->value, "nolambdalifting") == 0) {
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(Sg_VM(), SG_NO_LAMBDA_LIFT);
      }
      return desc;
    }
    if (ustrcmp(tag->value, "nooptimization") == 0) {
      SgVM *vm = Sg_VM();
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_ASM);
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_LOCAL);
	SG_VM_SET_FLAG(vm, SG_NO_LAMBDA_LIFT);
      }
      return desc;
    }
    if (ustrcmp(tag->value, "nobacktrace") == 0) {
      SgVM *vm = Sg_VM();
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(vm, SG_NO_DEBUG_INFO);
      }
      return desc;
    }
    if (ustrncmp(tag->value, "nounbound", 7) == 0) {
      SgVM *vm = Sg_VM();
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(vm, SG_ERROR_UNBOUND);
      }
      return desc;
    }
    break;
  case 'o':
    if (ustrcmp(tag->value, "overwrite") == 0) {
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(Sg_VM(), SG_ALLOW_OVERWRITE);
      }
      return desc;
    }
    break;
  case 'r':
    if (ustrcmp(tag->value, "r6rs") == 0) {
      SgVM *vm = Sg_VM();
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(vm, SG_R6RS_MODE);
	SG_VM_UNSET_FLAG(vm, SG_ALLOW_OVERWRITE);
	SG_VM_UNSET_FLAG(vm, SG_R7RS_MODE);
	SG_VM_UNSET_FLAG(vm, SG_COMPATIBLE_MODE);
	SG_VM_SET_FLAG(vm, SG_ERROR_UNBOUND);
      }
      Sg_SetPortReadTable(port, Sg_CopyReadTable(&r6rs_read_table));
      return desc;
    }
    if (ustrcmp(tag->value, "r7rs") == 0) {
      SgVM *vm = Sg_VM();
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_SET_FLAG(vm, SG_R7RS_MODE);
	SG_VM_UNSET_FLAG(vm, SG_ALLOW_OVERWRITE);
	SG_VM_UNSET_FLAG(vm, SG_COMPATIBLE_MODE);
	SG_VM_UNSET_FLAG(vm, SG_R6RS_MODE);
	/* TODO should we? */
	SG_VM_UNSET_FLAG(vm, SG_ERROR_UNBOUND);
      }
      Sg_SetPortReadTable(port, Sg_CopyReadTable(&r7rs_read_table));
      return desc;
    }
    if (ustrncmp(tag->value, "reader=", 7) == 0) {
      SgObject name = construct_lib_name(Sg_Substring(tag, 7, -1));
      SgObject lib = Sg_FindLibrary(name, FALSE);
      /* should we raise error or not? */
      if (SG_FALSEP(lib)) {
	lexical_error(port, ctx, UC("no library named %S"), name);
	return desc;
      }
      if (!SG_FALSEP(SG_LIBRARY_READER(lib))) {
	SG_PORT_READER(port) = SG_LIBRARY_READER(lib);
      }
      /* to let replaced reader read next expression, otherwise current
	 reader keep reading the next one.
      */
      return SG_UNDEF;
    }
    /* for portability with other implementation */
    if (ustrncmp(tag->value, "read-macro=", 11) == 0) {
      SgObject name = construct_lib_name(Sg_Substring(tag, 11, -1));
      SgObject lib = Sg_FindLibrary(name, FALSE);
      /* should we raise error or not? */
      if (SG_FALSEP(lib)) {
	lexical_error(port, ctx, UC("no library named %S"), name);
	return desc;
      }
      if (SG_LIBRARY_READTABLE(lib)) {
	ENSURE_COPIED_TABLE(port);
	add_read_table(SG_LIBRARY_READTABLE(lib), Sg_PortReadTable(port));
      }
      return desc;
    }
    break;
  case 'u':	
    if (ustrncmp(tag->value, "unbound", 7) == 0) {
      SgVM *vm = Sg_VM();
      if (ctx->flags & SG_CHANGE_VM_MODE) {
	SG_VM_UNSET_FLAG(vm, SG_ERROR_UNBOUND);
      }
      return desc;
    }
    break;
  default: break;
  }
  return desc;			/* for convenience */
}

/*
  TODO: some SRFIs (as far as I know only 105) requires speciall sh-bang
  it's better to have hook or something.
 */
SgObject read_hash_bang(SgPort *port, SgChar c, dispmacro_param *param,
			SgReadContext *ctx)
{
  SgChar c2 = Sg_GetcUnsafe(port);
  /* if it starts with '#!/' or '#! ' we assume it's a script */
  if (c2 == '/' || c2 == ' ') {
    for (;;) {
      c2 = Sg_GetcUnsafe(port);
      /* internal eol-style is LF */
      if (c2 == LF) return NULL;
      if (c2 == EOF) return SG_EOF;
    }
    /* NOTREACHED */
  } else {
    readtable_t *table = Sg_PortReadTable(port);
    SgObject desc = read_symbol_or_number(port, c2, table, ctx);
    if (SG_SYMBOLP(desc)) {
      Sg_ApplyDirective(port, desc, ctx);
    }
    return NULL;
  }
}
SgObject read_hash_bar(SgPort *port, SgChar c, dispmacro_param *param,
		       SgReadContext *ctx)
{
  SgChar c1, c2;
  int nest = 0;
  
 seek_c1:
  c1 = Sg_GetcUnsafe(port);

 seek_c2:
  c2 = Sg_GetcUnsafe(port);
  if (c2 == EOF) {
    lexical_error(port, ctx, 
		  UC("unexpected end-of-file while reading comments"));
  }
  if (c1 == '|' && c2 == '#') {
    if (nest == 0) return NULL;
    nest -= 1;
    goto seek_c1;
  }
  if (c1 == '#' && c2 == '|') {
    nest += 1;
    goto seek_c1;
  }
  c1 = c2;
  if (c1 == '|' || c1 == '#') goto seek_c2;
  goto seek_c1;
}

static SgObject read_bytevector(SgPort *port, SgChar *buf, SgReadContext *ctx)
{
#define CAST_FIXNUM_TO_U8(DATUM, REF)					\
  ((SG_INT_VALUE(datum) >= 0 && SG_INT_VALUE(datum) <= UINT8_MAX)	\
   ? (*ref = (SG_INT_VALUE(datum) & 0xFF), TRUE) : FALSE)
#define READ_BVECTOR(s_type, c_type, s_type_test, c_type_test)		\
  do {									\
    if (ustrcmp(buf, s_type) == 0) {					\
      int m = n * sizeof(c_type);					\
      int i;								\
      SgByteVector *bvector = Sg_MakeByteVector(m, 0);			\
      for (i = 0; i < m; i += sizeof(c_type)) {				\
	SgObject datum = SG_CAR(lst);					\
	if ( s_type_test (datum)) {					\
	  c_type * ref = (c_type *)&bvector->elements[i];		\
	  if ( c_type_test (datum, ref)) {				\
	    lst = SG_CDR(lst);						\
	    continue;							\
	  }								\
	}								\
	lexical_error(port, ctx, UC("expected " s_type ", but got %S"),	\
		      SG_CAR(lst));					\
      }									\
      bvector = Sg_AddConstantLiteral(bvector);				\
      return bvector;							\
    }									\
  } while (0)

  int line_begin = Sg_LineNo(port);
  int n;
  SgObject lst = read_list(port, ')', ctx);
  parsing_range(ctx, line_begin, Sg_LineNo(port));
  n = Sg_Length(lst);
  READ_BVECTOR("u8", uint8_t, SG_INTP, CAST_FIXNUM_TO_U8);
  lexical_error(port, ctx, UC("invalid lexical syntax #v%s ..."), buf);
#undef CAST_FIXNUM_TO_U8
#undef READ_BVECTOR
  return SG_UNDEF;		/* dummy */
}

SgObject read_hash_v(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  SgChar buf[16] = {0};
  int i;
  for (i = 0; i < 2; i++) {
    buf[i] = Sg_GetcUnsafe(port);
    if (buf[i] == EOF || delimited(port, buf[i])) break;
  }
  if (i != 2) {
    lexical_error(port, ctx, UC("invalid lexical syntax #v%s%A ..."), buf,
		  SG_MAKE_CHAR(c));
  }
  c = Sg_GetcUnsafe(port);
  if (c == '(') {
    return read_bytevector(port, buf, ctx);
  }
  lexical_error(port, ctx, UC("invalid lexical syntax #v%s%A ..."), buf,
		SG_MAKE_CHAR(c));
  return SG_UNDEF;		/* dummy */
}

SgObject read_hash_u(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  SgChar buf[16] = {0};
  buf[0] = 'u';
  buf[1] = Sg_GetcUnsafe(port);
  if (buf[1] != '8') {
    lexical_error(port, ctx, UC("invalid lexical syntax #%s ..."), buf);
  }
  c = Sg_GetcUnsafe(port);
  if (c == '(') {
    return read_bytevector(port, buf, ctx);
  }
  lexical_error(port, ctx, UC("invalid lexical syntax #%s%A ..."), buf,
		SG_MAKE_CHAR(c));
  return SG_UNDEF;		/* dummy */
}

SgObject read_hash_t(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  SgChar c2 = Sg_GetcUnsafe(port);
  readtable_t *table = Sg_PortReadTable(port);
  if (c2 == EOF || delimited(port, c2)) {
    Sg_UngetcUnsafe(port, c2);
    return SG_TRUE;
  }
  /* R7RS allow #true so we need to check */
  if ((c == 't' && c2 == 'r') ||
      (table->insensitiveP && (c2 == 'r' || c2 == 'R'))) {
    SgObject rest = read_compatible_symbol(port, c2, ctx, NULL);
    if (ustrcmp(SG_STRING_VALUE(rest), "rue") == 0) {
      return SG_TRUE;
    }    
  }
  lexical_error(port, ctx, UC("invalid lexical syntax #%S%S"),
		SG_MAKE_CHAR(c), SG_MAKE_CHAR(c2));
  return SG_UNDEF;		/* dummy */
}

SgObject read_hash_f(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  SgChar c2 = Sg_GetcUnsafe(port);
  readtable_t *table = Sg_PortReadTable(port);
  if (c2 == EOF || delimited(port, c2)) {
    Sg_UngetcUnsafe(port, c2);
    return SG_FALSE;
  }
  /* R7RS allow #false so we need to check */
  if ((c == 'f' && c2 == 'a') ||
      (table->insensitiveP && (c2 == 'a' || c2 == 'A'))) {
    SgObject rest = read_compatible_symbol(port, c2, ctx, NULL);
    if (ustrcmp(SG_STRING_VALUE(rest), "alse") == 0) {
      return SG_FALSE;
    }    
  }
  lexical_error(port, ctx, UC("invalid lexical syntax #%S%S"),
		SG_MAKE_CHAR(c), SG_MAKE_CHAR(c2));
  return SG_UNDEF;		/* dummy */
}

static SgObject read_prefixed_number(SgChar initial, SgPort *port,
				     SgReadContext *ctx)
{
  SgChar buf[4096];
  SgString *str;
  SgObject num;
  SgChar c;
  int offset = 0;
  buf[offset++] = '#';
  /* check if it's #%c#%c type or not */
  c = Sg_PeekcUnsafe(port);
  if (c == '#') {
    buf[offset++] = initial;
    buf[offset++] = c;
    Sg_GetcUnsafe(port);	/* discard */
    initial = Sg_GetcUnsafe(port);
  }
  read_thing(port, ctx, buf + offset, array_sizeof(buf) - offset, initial);
  str = Sg_HeapString(buf);
  num = Sg_StringToNumber(str, 10, TRUE);
  if (SG_FALSEP(num)) {
    lexical_error(port, ctx,
		  UC("invalid lexical syntax %s while reading number"), buf);
  }
  return num;
}

SgObject read_hash_b(SgPort *port, SgChar c, dispmacro_param *param,
			    SgReadContext *ctx)
{
  return read_prefixed_number(c, port, ctx);
}

SgObject read_hash_o(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  return read_prefixed_number(c, port, ctx);
}
SgObject read_hash_d(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  return read_prefixed_number(c, port, ctx);
}
SgObject read_hash_x(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  return read_prefixed_number(c, port, ctx);
}
SgObject read_hash_i(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  return read_prefixed_number(c, port, ctx);
}
SgObject read_hash_e(SgPort *port, SgChar c, dispmacro_param *param,
		     SgReadContext *ctx)
{
  return read_prefixed_number(c, port, ctx);
}
SgObject read_hash_open_paren(SgPort *port, SgChar c, dispmacro_param *param,
			      SgReadContext *ctx)
{
  SgObject v = Sg_ListToVector(read_list(port, ')', ctx), 0, -1);
  v = Sg_AddConstantLiteral(v);
  return v;
}

SgObject read_hash_semicolon(SgPort *port, SgChar c,
			     dispmacro_param *param, SgReadContext *ctx)
{
  read_expr(port, ctx);
  return NULL;
}

static const struct {
  const char* name;
  int code;
} s_char_name[] = {
  { "nul",        0x0000 },
  { "null",       0x0000 },
  { "alarm",      0x0007 },
  { "backspace",  0x0008 },
  { "tab",        0x0009 },
  { "linefeed",   0x000A },
  { "newline",    0x000A },
  { "vtab",       0x000B },
  { "page",       0x000C },
  { "return",     0x000D },
  { "esc",        0x001B },
  { "escape",     0x001B },
  { "space",      0x0020 },
  { "delete",     0x007F }
};

SgObject read_hash_escape(SgPort *port, SgChar c, dispmacro_param *param,
			  SgReadContext *ctx)
{
  c = Sg_GetcUnsafe(port);
  if (c == 'x') {
    c = Sg_PeekcUnsafe(port);
    if (c == EOF || delimited(port, c)) return SG_MAKE_CHAR('x');
    return SG_MAKE_CHAR(read_hex_scalar_value(port, ctx));
  } else {
    SgChar buf[16];
    int i;
    if (c == '(') {
      c = Sg_PeekcUnsafe(port);
      if (c == EOF || delimited(port, c)) return SG_MAKE_CHAR('(');
      read_thing(port, ctx, buf, array_sizeof(buf), -1);
      lexical_error(port, ctx, UC("invalid lexical syntax #\\(%s"), buf);
    }
    Sg_UngetcUnsafe(port, c);
    read_thing(port, ctx, buf, array_sizeof(buf), -1);
    if (buf[0] == 0) {
      c = Sg_GetcUnsafe(port);
      if (c == EOF) {
	lexical_error(port, ctx,
		      UC("unexpected end-of-file while reading character"));
      }
      return SG_MAKE_CHAR(c);
    }
    if (buf[1] == 0) return SG_MAKE_CHAR(buf[0]);
    for (i = 0; i < array_sizeof(s_char_name); i++) {
      if (ustrcmp(buf, s_char_name[i].name) == 0)
	return SG_MAKE_CHAR(s_char_name[i].code);
    }
    /* I assume this is not happen */
    lexical_error(port, ctx, UC("invalid lexical syntax #\\%s"), buf);
  }
  return SG_UNDEF; /* dummy */
}

SgObject read_hash_equal(SgPort *port, SgChar c, dispmacro_param *param,
			 SgReadContext *ctx)
{
  if (!ctx->graph) {
    lexical_error(port, ctx, UC("invalid lexical syntax #="));
  }
  if (param->present) {
    SgObject obj = read_expr(port, ctx);
    intptr_t mark = param->value;
    if (SG_EOFP(obj)) {
      lexical_error(port, ctx,
		    UC("unexpected end-of-file while reading tag #%ld="), mark);
    }
    if (SG_UNDEFP(Sg_HashTableRef(ctx->graph, SG_MAKE_INT(mark), SG_UNDEF))) {
      Sg_HashTableSet(ctx->graph, SG_MAKE_INT(mark), obj, 0);
      return obj;
    }
    lexical_error(port, ctx, UC("duplicate tag #%ld="), mark);
  } else {
    lexical_error(port, ctx,
		  UC("invalid lexical syntax %S"), SG_MAKE_CHAR(c));
  }
  return SG_UNDEF;		/* dummy */
}

SgObject read_hash_hash(SgPort *port, SgChar c, dispmacro_param *param,
			SgReadContext *ctx)
{
  if (param->present) {
    intptr_t mark = param->value;
    SgSharedRef *ref = make_shared_ref(mark);
    ctx->graphRef = TRUE;
    return SG_OBJ(ref);
  } else {
    lexical_error(port, ctx,
		  UC("invalid lexical syntax %S"), SG_MAKE_CHAR(c));
  }
  return SG_UNDEF;		/* dummy */
}

SgObject read_hash_less(SgPort *port, SgChar c, dispmacro_param *param,
			SgReadContext *ctx)
{
  /* #<(library) ...> imports readtables */
  SgObject libs = read_list(port, '>', ctx);
  SgObject cp;
  SG_FOR_EACH(cp, libs) {
    SgObject name = SG_CAR(cp);
    if (SG_PAIRP(name)) {
      SgObject lib = Sg_FindLibrary(name, FALSE);
      if (SG_FALSEP(lib)) {
	lexical_error(port, ctx, UC("no library named %S"), name);
	return NULL;
      }
      if (SG_LIBRARY_READTABLE(lib)) {
	ENSURE_COPIED_TABLE(port);
	add_read_table(SG_LIBRARY_READTABLE(lib), Sg_PortReadTable(port));
      }      
    } else {
      lexical_error(port, ctx,
		    UC("library name required but got %S"), name);
      return NULL;
    }
  }
  return NULL;
}

SgObject read_hash_colon(SgPort *port, SgChar c, dispmacro_param *param,
			 SgReadContext *ctx)
{
  /* TODO how to handle |? */
  SgString *s = read_compatible_symbol(port, -1, ctx, NULL);
  return Sg_MakeSymbol(s, FALSE);
}

SgObject dispmacro_reader(SgPort *port, SgChar c, SgReadContext *ctx)
{
  readtable_t *table;
  disptab_t *disptab;
  if (c >= MAX_READTABLE_CHAR) {
    lexical_error(port, ctx, UC("macro char %S is out of range"),
		  SG_MAKE_CHAR(c));
  }
  table = Sg_PortReadTable(port);
  disptab = table->readtable[c].disp;
  if (!disptab) {
    lexical_error(port, ctx,
		  UC("%S is not a dispatch macro character"), SG_MAKE_CHAR(c));
  } else {
    dispmacro_param param;
    SgChar c2 = Sg_GetcUnsafe(port);
    if (c2 >= '0' && c2 <= '9') {
      param.present = TRUE;
      param.value = c2 - '0';
      while (1) {
	c2 = Sg_GetcUnsafe(port);
	if (c2 < '0' || c2 > '9') break;
	param.value = param.value * 10 + c2 - '0';
	if (param.value < 0 || (long)param.value > SG_INT_MAX) {
	  lexical_error(port, ctx,
			UC("invalid object tag, value out of range"));
	}
      }
    } else {
      param.present = FALSE;
    }
    if (c2 == EOF) {
      lexical_error(port, ctx, UC("imcoplete dispatch macro"));
    }
    if (c2 >= MAX_READTABLE_CHAR) {
      lexical_error(port, ctx, UC("macro sub char %S is out of range"),
		    SG_MAKE_CHAR(c2));
    }
    if (disptab[c2].cfunc) {
      return (*disptab[c2].cfunc)(port, c2, &param, ctx);
    }
    if (SG_EQ(disptab[c2].sfunc, SG_UNBOUND)) {
      lexical_error(port, ctx, UC("%S is not a dispatch macro sub character"),
		    SG_MAKE_CHAR(c2));
    }
    /* FIXME: we should eliminate this type of dispatch. */
    if (SG_PROCEDURE_REQUIRED(disptab[c2].sfunc) > 3) {
      return Sg_Apply4(disptab[c2].sfunc, port, SG_MAKE_CHAR(c2),
		       param.present ? SG_MAKE_INT(param.value) : SG_FALSE,
		       ctx);
    } else {
      return Sg_Apply3(disptab[c2].sfunc, port, SG_MAKE_CHAR(c2),
		       param.present ? SG_MAKE_INT(param.value) : SG_FALSE);
    }
  }
  return SG_UNDEF;		/* dummy */
}

SgObject read_expr4(SgPort *port, int flags, SgChar delim, SgReadContext *ctx)
{
  SgChar c;
  readtable_t *table;
  SgObject item;
  SgVM *vm = Sg_VM();
  while (1) {
  top:
    /* when previous execution was (values), then valuesCount = 0 and
       this skips all read expression. to prevent that we need to
       set it 1 here.
       this call must return a value anyway :)
    */
    vm->valuesCount = 1;
    c = Sg_GetcUnsafe(port);
    /* we ignore unicode space for now. */
    if (c == EOF) {
      if (flags & ACCEPT_EOF)
	return SG_EOF;
      lexical_error(port, ctx, UC("unexpected end-of-file"));
    }
    if (c > 127 && Sg_Ucs4WhiteSpaceP(c)) goto top;
    /* for reading list it does not matter either ')' or ']'
       if we got delmiter we can simply return RPAREN*/
    if (c == delim) return SG_SYMBOL_RPAREN;

    parsing_line(ctx, Sg_LineNo(port));
    /* need to be re-get for #!read-macro */
    table = Sg_PortReadTable(port);
    if (c < 128 && isdigit(c)) {
      return read_symbol_or_number(port, c, table, ctx);
    }

    if (c < MAX_READTABLE_CHAR) {
      /* lookup readtable */
      switch (table->readtable[c].type) {
      case CT_WHITE_SPACE: goto top;
      case CT_SINGLE_ESCAPE:
	/* TODO how to treat? */
	Sg_UngetcUnsafe(port, c);
	return read_escaped_symbol(port, ctx);
      case CT_TERM_MACRO:
      case CT_NON_TERM_MACRO: {
	SgObject o = macro_reader(port, c, table->readtable, ctx);
	/* if the (values) is the result of reader then we ignore the result */
	if (o && vm->valuesCount) return o;
	break;
      }
      case CT_ILLEGAL:
	lexical_error(port, ctx,
		      UC("invalid character %U during reading identifier"), c);
	break;
      default:
	goto read_sym_or_num;
      }
    } else {
      goto read_sym_or_num;
    }
  }
 read_sym_or_num:
  item = read_symbol_or_number(port, c, table, ctx);
  if (!ctx->escapedp && SG_EQ(item, SG_SYMBOL_DOT)) {
    if (flags & ACCEPT_DOT) return item;
    lexical_error(port, ctx, UC("misplaced dot('.')"));    
  }
  return item;
}
static SgObject lookup_graph(SgPort *port, SgReadContext *ctx,
			     SgSharedRef *ref)
{
  SgObject obj = Sg_HashTableRef(ctx->graph, ref->index, SG_UNDEF);
  if (SG_SHAREDREF_P(obj)) return lookup_graph(port, ctx, SG_SHAREDREF(obj));
  if (obj != SG_UNDEF) return obj;
  lexical_error(port, ctx, UC("attempt to reference undefined tag #%A#"),
		ref->index);
  return SG_UNDEF; /* dummy */
}

static void link_graph(SgPort *port, SgReadContext *ctx, SgObject obj)
{
  if (SG_PAIRP(obj)) {
    if (SG_SHAREDREF_P(SG_CAR(obj))) {
      SG_SET_CAR(obj, lookup_graph(port, ctx, SG_SHAREDREF(SG_CAR(obj))));
    } else {
      link_graph(port, ctx, SG_CAR(obj));
    }
    if (SG_SHAREDREF_P(SG_CDR(obj))) {
      SG_SET_CDR(obj, lookup_graph(port, ctx, SG_SHAREDREF(SG_CDR(obj))));
    } else {
      link_graph(port, ctx, SG_CDR(obj));
    }
    return;
  }
  if (SG_VECTORP(obj)) {
    int n = SG_VECTOR_SIZE(obj), i;
    for (i = 0; i < n; i++) {
      if (SG_SHAREDREF_P(SG_VECTOR_ELEMENT(obj, i))) {
	SG_VECTOR_ELEMENT(obj, i)
	  = lookup_graph(port, ctx, SG_SHAREDREF(SG_VECTOR_ELEMENT(obj, i)));
      } else {
	link_graph(port, ctx, SG_VECTOR_ELEMENT(obj, i));
      }
    }
    return;
  }
}

SgObject Sg_ReadWithContext(SgObject port, SgReadContext *ctx)
{
  SgObject obj;
  /* extends_loading_table(port); */
  if (ctx->graph) {
    /* clear it */
    Sg_HashCoreClear(SG_HASHTABLE_CORE(ctx->graph), 0);
  }
  /* we only set if the flag is explicitly set.
     this makes #!fold-case one time only. */
  if (ctx->flags & SG_READ_NO_CASE ||
      ctx->flags & SG_READ_CASE) {
    ENSURE_COPIED_TABLE(port);
    /* one or the other */
    SG_PORT_READTABLE(port)->insensitiveP = (ctx->flags & SG_READ_NO_CASE);
  }

  ctx->firstLine = Sg_LineNo(port);
  obj = read_expr4(port, ACCEPT_EOF, EOF, ctx);
  if (!ctx->escapedp && SG_EQ(obj, SG_SYMBOL_DOT)) {
    lexical_error(port, ctx, UC("misplaced dot('.')"));
  }
  if (ctx->graph && ctx->graphRef) link_graph(port, ctx, obj);
  parsing_range(ctx, ctx->firstLine, Sg_LineNo(port));
  return obj;
}

SgObject Sg_Read(SgObject port, int readSharedObject)
{
  SgReadContext ctx = SG_STATIC_READ_CONTEXT;
  ASSERT(SG_PORTP(port));
  /* ASSERT(SG_TEXTUAL_PORTP(port)); */
  /* make read context for shared object */
  if (readSharedObject) {
    SgHashTable graph;
    Sg_InitHashTableSimple(&graph, SG_HASH_EQ, 1);
    ctx.graph = &graph;
  }  
  ctx.graphRef = FALSE;
  ctx.flags = SG_READ_SOURCE_INFO;
  return Sg_ReadWithContext(SG_PORT(port), &ctx);
}

SgObject Sg_ReadDelimitedList(SgObject port, SgChar delim, int sharedP)
{
  SgObject obj;
  SgReadContext ctx = SG_STATIC_READ_CONTEXT;
  ASSERT(SG_PORTP(port));

  /* extends_loading_table(port); */
  /* make read context for shared object */
  if (sharedP) {
    SgHashTable graph;
    Sg_InitHashTableSimple(&graph, SG_HASH_EQ, 1);
    ctx.graph = &graph;
  }
  ctx.graphRef = FALSE;
  ctx.firstLine = Sg_LineNo(port);
  obj = read_list(port, delim, &ctx);
  if (ctx.graph && ctx.graphRef) link_graph(port, &ctx, obj);

  return obj;
}

static disptab_t* alloc_disptab();

static readtable_t* make_readtable(int init)
{
  readtable_t *tab = SG_NEW(readtable_t);
  tab->insensitiveP = FALSE;
  if (init) {
    readtab_t *r = tab->readtable;
    int i;
    for (i = 0; i <= ' '; i++) {
      r[i].type = CT_ILLEGAL;
      r[i].cfunc = NULL;
      r[i].sfunc = SG_UNBOUND;
      r[i].disp = NULL;
    }
    for (; i < MAX_READTABLE_CHAR; i++) {
      r[i].type = CT_CONSTITUENT;
      r[i].cfunc = NULL;
      r[i].sfunc = SG_UNBOUND;
      r[i].disp = NULL;
    }
  }
  return tab;
}

void add_read_table(readtable_t *src, readtable_t *dst)
{
  int i;
  readtab_t *sr = src->readtable;
  readtab_t *dr = dst->readtable;
  for (i = 0; i < MAX_READTABLE_CHAR; i++) {
    if (sr[i].disp) {
      int j;
      disptab_t *d = dr[i].disp;
      /* TODO should not be overwritten? */
      dr[i] = sr[i];
      if (!d) dr[i].disp = alloc_disptab();
      else dr[i].disp = d;

      for (j = 0; j < MAX_READTABLE_CHAR; j++) {
	if (!SG_UNBOUNDP(sr[i].disp[j].sfunc) || sr[i].disp[j].cfunc) {
	  dr[i].disp[j] = sr[i].disp[j];
	}
      }
    } else if (sr[i].cfunc) {
      dr[i] = sr[i];
    } else if (!SG_UNBOUNDP(sr[i].sfunc)) {
      dr[i] = sr[i];
    }
  }
}

static readtable_t* default_readtable(int copyP)
{
  readtable_t* table;
  if (SG_VM_IS_SET_FLAG(Sg_VM(), SG_R6RS_MODE)) {
    table = &r6rs_read_table;
  } else if (SG_VM_IS_SET_FLAG(Sg_VM(), SG_R7RS_MODE)) {
    table = &r7rs_read_table;
  } else {
    table = &compat_read_table;
  }
  if (copyP) {
    return Sg_CopyReadTable(table);
  } else {
    return table;
  }
}

readtable_t* Sg_DefaultReadTable()
{
  return default_readtable(TRUE);
}

void Sg_SetPortReadTable(SgPort *port, readtable_t *table)
{
  SG_PORT_READTABLE(port) = table;
}

readtable_t* Sg_PortReadTable(SgPort *port)
{
  readtable_t* table = SG_PORT_READTABLE(port);
  if (table) return table;
  return default_readtable(FALSE);
}

readtable_t* Sg_EnsureCopiedReadTable(SgPort *port)
{
  ENSURE_COPIED_TABLE(port);
  return SG_PORT_READTABLE(port);
}

readtable_t* Sg_CopyReadTable(readtable_t *src)
{
  readtable_t *newr = make_readtable(FALSE);
  readtab_t *newt = newr->readtable, *oldt = src->readtable;
  int i;
  *newr = *src;
  for (i = 0; i < MAX_READTABLE_CHAR; i++) {
    if (oldt[i].disp) {
      newt[i].disp = alloc_disptab();
      memcpy(newt[i].disp, oldt[i].disp,
	     sizeof(*(newt[i].disp)) * MAX_READTABLE_CHAR);
    }
  }
  return newr;
}
int Sg_PortCaseInsensitiveP(SgPort *port)
{
  if (SG_PORT_READTABLE(port)) {
    return SG_PORT_READTABLE(port)->insensitiveP;
  }
  return FALSE;
}

static SgInternalMutex obtable_mutax;
static SgHashTable *obtable = NULL;

int Sg_ConstantLiteralP(SgObject o)
{
  SgObject e;
  if (SG_PAIRP(o)) {
    /* simple check */
    return !SG_FALSEP(Sg_GetPairAnnotation(o, SYM_CONST));
  } else if (SG_VECTORP(o)) {
    /* again simple check */
    return SG_LITERAL_VECTORP(o);
  }
  e = Sg_HashTableRef(obtable, o, SG_UNBOUND);
  if (SG_UNBOUNDP(e)) return FALSE;
  /* constant literal must satisfy eq? */
  return e == o;
}

SgObject Sg_AddConstantLiteral(SgObject o)
{
  SgObject e;
  Sg_LockMutex(&obtable_mutax);
  e = Sg_HashTableRef(obtable, o, SG_UNBOUND);
  if (SG_UNBOUNDP(e)) {
    Sg_HashTableSet(obtable, o, o, SG_HASH_NO_OVERWRITE);
    /* TODO after CLOS, we should not use header bits */
    if (SG_VECTORP(o)) {
      SG_VECTOR_SET_LITERAL(o);
    }
    if (SG_BVECTORP(o)) {
      SG_BVECTOR_SET_LITERAL(o);
    }
    if (SG_PAIRP(o)) {
      /* do the cdr parts. */
      Sg_SetPairAnnotation(o, SYM_CONST, SG_TRUE);
      if (SG_PAIRP(SG_CAR(o))) {
	SG_SET_CAR(o, Sg_AddConstantLiteral(SG_CAR(o)));
      }
      if (SG_PAIRP(SG_CDR(o))) {
	SG_SET_CDR(o, Sg_AddConstantLiteral(SG_CDR(o)));
      }
    }
  } else {
    o = e;
  }
  Sg_UnlockMutex(&obtable_mutax);
  return o;
}

int Sg_DelimitedCharP(SgChar c, SgPort *p)
{
  return delimited(p, c);
}

#define SCHEME_OBJ(NAME) SG_CPP_CAT(NAME, _stub)
#define STUB_NAME(NAME) SG_CPP_CAT(NAME, stub)

/* initialize */
#define DEFINE_MACRO_STUB(FN, NAME)					\
  static SgObject STUB_NAME(FN) (SgObject *args, int argc, void *data_)	\
  {									\
    SgReadContext ctx = SG_STATIC_READ_CONTEXT;				\
    SgPort *p;								\
    SgChar c;								\
    SgObject r;								\
    if (argc != 2) {							\
      Sg_WrongNumberOfArgumentsAtLeastViolation(SG_INTERN(NAME),	\
						2, argc, SG_NIL);	\
    }									\
    if (!SG_PORTP(args[0])) {						\
      Sg_WrongTypeOfArgumentViolation(SG_INTERN(NAME),			\
				      SG_MAKE_STRING("port"),		\
				      args[0], SG_NIL);			\
    }									\
    if (!SG_CHARP(args[1])) {						\
      Sg_WrongTypeOfArgumentViolation(SG_INTERN(NAME),			\
				      SG_MAKE_STRING("char"),		\
				      args[1], SG_NIL);			\
    }									\
    p = SG_PORT(args[0]);						\
    c = SG_CHAR_VALUE(args[1]);						\
    r = (FN)(p, c, &ctx);						\
    if (r) return r;							\
    else return SG_UNDEF;						\
  }									\
  SG_DEFINE_SUBR(SCHEME_OBJ(FN), 2, 0, STUB_NAME(FN), SG_FALSE, NULL)

#define DEFINE_DISPMACRO_STUB(FN, NAME)				\
  static SgObject STUB_NAME(FN)					\
  (SgObject *args, int argc, void *data_)			\
  {								\
    SgReadContext ctx = SG_STATIC_READ_CONTEXT;			\
    SgObject param_scm, r;					\
    SgPort *p;							\
    SgChar c;							\
    dispmacro_param param;					\
    if (argc != 3) {						\
      Sg_WrongNumberOfArgumentsAtLeastViolation(SG_INTERN(NAME),\
						3, argc, SG_NIL);\
    }								\
    if (!SG_PORTP(args[0])) {					\
      Sg_WrongTypeOfArgumentViolation(SG_INTERN(NAME),		\
				      SG_MAKE_STRING("port"),	\
				      args[0], SG_NIL);		\
    }								\
    if (!SG_CHARP(args[1])) {					\
      Sg_WrongTypeOfArgumentViolation(SG_INTERN(NAME),		\
				      SG_MAKE_STRING("char"),	\
				      args[1], SG_NIL);		\
    }								\
    p = SG_PORT(args[0]);					\
    c = SG_CHAR_VALUE(args[1]);					\
    param_scm = args[2];					\
    if (SG_FALSEP(param_scm)) {					\
      param.present = FALSE;					\
      param.value = 0;						\
    } else if (SG_INTP(param_scm)) {				\
      param.present = TRUE;					\
      param.value = SG_INT_VALUE(param_scm);			\
    } else {							\
      Sg_WrongTypeOfArgumentViolation(SG_INTERN(NAME),		\
				      SG_MAKE_STRING("fixnum"),	\
				      param_scm, SG_NIL);	\
      return SG_UNDEF;						\
    }								\
    r = (FN)(p, c, &param, &ctx);				\
    if (r) return r;						\
    else return SG_UNDEF;					\
  }								\
  SG_DEFINE_SUBR(SCHEME_OBJ(FN), 3, 0, STUB_NAME(FN), SG_FALSE, NULL)

DEFINE_MACRO_STUB(read_vertical_bar,   "|-reader");
DEFINE_MACRO_STUB(read_double_quote,   "\"-reader");
DEFINE_MACRO_STUB(read_quote,          "'-reader");
DEFINE_MACRO_STUB(read_open_paren,     "(-reader");
DEFINE_MACRO_STUB(read_close_paren,    ")-reader");
DEFINE_MACRO_STUB(read_open_bracket,   "[-reader");
DEFINE_MACRO_STUB(read_close_bracket,  "]-reader");
DEFINE_MACRO_STUB(read_semicolon,      ";-reader");
DEFINE_MACRO_STUB(read_quasiquote,     "`-reader");
DEFINE_MACRO_STUB(read_unquote,        ",-reader"); 
DEFINE_MACRO_STUB(read_colon,          ":-reader");
DEFINE_MACRO_STUB(dispmacro_reader,    "dispatch-macro-reader");

DEFINE_DISPMACRO_STUB(read_hash_quote,      "#'-reader");
DEFINE_DISPMACRO_STUB(read_hash_quasiquote, "#`-reader");
DEFINE_DISPMACRO_STUB(read_hash_unquote,    "#,-reader");
DEFINE_DISPMACRO_STUB(read_hash_bang,       "#!-reader");
DEFINE_DISPMACRO_STUB(read_hash_v, 	    "#v-reader");
DEFINE_DISPMACRO_STUB(read_hash_u, 	    "#u-reader");
DEFINE_DISPMACRO_STUB(read_hash_t, 	    "#t-reader");
DEFINE_DISPMACRO_STUB(read_hash_f, 	    "#f-reader");
DEFINE_DISPMACRO_STUB(read_hash_b, 	    "#b-reader");
DEFINE_DISPMACRO_STUB(read_hash_o, 	    "#o-reader");
DEFINE_DISPMACRO_STUB(read_hash_d, 	    "#d-reader");
DEFINE_DISPMACRO_STUB(read_hash_x, 	    "#x-reader");
DEFINE_DISPMACRO_STUB(read_hash_i, 	    "#i-reader");
DEFINE_DISPMACRO_STUB(read_hash_e, 	    "#e-reader");
DEFINE_DISPMACRO_STUB(read_hash_open_paren, "#(-reader");
DEFINE_DISPMACRO_STUB(read_hash_semicolon,  "#;-reader");
DEFINE_DISPMACRO_STUB(read_hash_bar,        "#|-reader");
DEFINE_DISPMACRO_STUB(read_hash_escape,     "#\\-reader");
DEFINE_DISPMACRO_STUB(read_hash_equal,      "#=-reader");
DEFINE_DISPMACRO_STUB(read_hash_hash,       "##-reader");
DEFINE_DISPMACRO_STUB(read_hash_less,       "#<-reader");
DEFINE_DISPMACRO_STUB(read_hash_colon,      "#:-reader");


SgObject Sg_GetMacroCharacter(SgChar c, readtable_t *table)
{
  ASSERT(table);
  if (c < MAX_READTABLE_CHAR) {
    readtab_t *r = &table->readtable[c];
    SgObject term;
    if (r->type == CT_NON_TERM_MACRO) term = SG_TRUE;
    else if (r->type == CT_TERM_MACRO) term = SG_FALSE;
    else return Sg_Values2(SG_FALSE, SG_FALSE);
    return Sg_Values2(SG_UNBOUNDP(r->sfunc) ? SG_UNBOUND : r->sfunc, term);
  }
  return Sg_Values2(SG_FALSE, SG_FALSE);
}

#define macro_function_item(name) { (name), SG_OBJ(&(SCHEME_OBJ(name))) }
static macro_function get_macro_function(SgObject fn)
{
  static const struct {
    macro_function f;
    SgObject s;
  } x[] = {
    macro_function_item(read_vertical_bar),
    macro_function_item(read_double_quote),
    macro_function_item(read_quote),
    macro_function_item(read_open_paren),
    macro_function_item(read_close_paren),
    macro_function_item(read_open_bracket),
    macro_function_item(read_close_bracket),
    macro_function_item(read_semicolon),
    macro_function_item(read_quasiquote),
    macro_function_item(read_unquote),
    macro_function_item(read_colon),
    macro_function_item(dispmacro_reader)
  };
  int i;
  for (i = 0; i < array_sizeof(x); i++) {
    if (SG_EQ(fn, x[i].s)) return x[i].f;
  }
  return NULL;
}

void Sg_SetMacroCharacter(SgChar c, SgObject proc, int nontermP, 
			  readtable_t *table)
{
  ASSERT(table);
  if (!isdigit(c) && c < MAX_READTABLE_CHAR) {
    readtab_t *r = &table->readtable[c];
    r->type = nontermP ? CT_NON_TERM_MACRO : CT_TERM_MACRO;
    r->sfunc = proc;
    r->cfunc = get_macro_function(proc);
    if (r->disp) r->disp = 0;
  } else {
    Sg_ImplementationRestrictionViolation
      (SG_INTERN("set-macro-character"),
       SG_MAKE_STRING("non ascii char is not supported"),
       SG_MAKE_CHAR(c));
  }
}

SgObject Sg_GetDispatchMacroCharacter(SgChar c, SgChar subc, readtable_t *table)
{
  ASSERT(table);
  if (c < MAX_READTABLE_CHAR) {
    readtab_t *r = &table->readtable[c];
    if (!r->disp) {
      Sg_AssertionViolation
	(SG_INTERN("get-dispatch-macro-character"),
	 SG_MAKE_STRING("given character is not dispatch macro character"),
	 SG_MAKE_CHAR(c));      
    }
    return SG_UNBOUNDP(r->disp[subc].sfunc) ? SG_UNBOUND : r->disp[subc].sfunc;
  }
  return SG_FALSE;
}

static dispmacro_function get_dispatch_macro_function(SgObject fn)
{
  static const struct {
    dispmacro_function f;
    SgObject s;
  } x[] = {
    macro_function_item(read_hash_quote),
    macro_function_item(read_hash_quasiquote),
    macro_function_item(read_hash_unquote),
    macro_function_item(read_hash_bang),
    macro_function_item(read_hash_v),
    macro_function_item(read_hash_u),
    macro_function_item(read_hash_t),
    macro_function_item(read_hash_f),
    macro_function_item(read_hash_b),
    macro_function_item(read_hash_o),
    macro_function_item(read_hash_d),
    macro_function_item(read_hash_x),
    macro_function_item(read_hash_i),
    macro_function_item(read_hash_e),
    macro_function_item(read_hash_open_paren),
    macro_function_item(read_hash_semicolon),
    macro_function_item(read_hash_bar),
    macro_function_item(read_hash_escape),
    macro_function_item(read_hash_equal),
    macro_function_item(read_hash_hash),
    macro_function_item(read_hash_less),
    macro_function_item(read_hash_colon)
  };
  int i;
  for (i = 0; i < array_sizeof(x); i++) {
    if (SG_EQ(fn, x[i].s)) return x[i].f;
  }
  return NULL;
}

int Sg_MakeDispatchMacroCharacter(SgChar c, int nontermP, readtable_t *table)
{
  ASSERT(table);
  if (!isdigit(c) && c < MAX_READTABLE_CHAR) {
    readtab_t *r = &table->readtable[c];
    if (!r->disp) r->disp = alloc_disptab();
    r->type = nontermP ? CT_NON_TERM_MACRO : CT_TERM_MACRO;
    r->sfunc = SG_OBJ(&SCHEME_OBJ(dispmacro_reader));
    r->cfunc = dispmacro_reader;
  } else {
    Sg_ImplementationRestrictionViolation
      (SG_INTERN("make-dispatch-macro-character"),
       SG_MAKE_STRING("non ascii char is not supported"),
       SG_MAKE_CHAR(c));
  }
  return TRUE;
}

void Sg_SetDispatchMacroCharacter(SgChar c, SgChar subc, SgObject proc,
				  readtable_t *table)
{
  ASSERT(table);
  if (!isdigit(c) && !isdigit(subc) &&
      c < MAX_READTABLE_CHAR && subc < MAX_READTABLE_CHAR) {
    readtab_t *r = &table->readtable[c];
    if (!r->disp) {
      Sg_AssertionViolation
	(SG_INTERN("set-dispatch-macro-character"),
	 SG_MAKE_STRING("given character is not dispatch macro character"),
	 SG_MAKE_CHAR(c));
    }
    r->disp[subc].sfunc = proc;
    r->disp[subc].cfunc = get_dispatch_macro_function(proc);
  } else {
    Sg_ImplementationRestrictionViolation
      (SG_INTERN("set-dispatch-macro-character"),
       SG_MAKE_STRING("non ascii char is not supported"),
       SG_LIST2(SG_MAKE_CHAR(c), SG_MAKE_CHAR(subc)));
  }
}

void Sg_EnsureLibraryReadTable(SgLibrary *library)
{
  if (!SG_LIBRARY_READTABLE(library)) {
    SG_LIBRARY_READTABLE(library) = make_readtable(TRUE);
  }
}

#define SET_READ_MACRO(R, CH, FN, TYPE)			\
  ((R)[(CH)].type = (TYPE),				\
   (R)[(CH)].cfunc = (FN),				\
   (R)[(CH)].sfunc = SG_OBJ(&SG_CPP_CAT(FN, _stub)))
#define SET_TERM_MACRO(R, CH, FN) SET_READ_MACRO(R, CH, FN, CT_TERM_MACRO)
#define SET_NONTERM_MACRO(R, CH, FN)		\
  SET_READ_MACRO(R, CH, FN, CT_NON_TERM_MACRO)
#define SET_DISP_MACRO(R, CH, FN)			\
  ((R)[(CH)].cfunc = (FN),				\
   (R)[(CH)].sfunc = SG_OBJ(&SG_CPP_CAT(FN, _stub)))

disptab_t* alloc_disptab()
{
  disptab_t * d = SG_NEW_ARRAY(disptab_t, MAX_READTABLE_CHAR);
  int i;
  for (i = 0; i < MAX_READTABLE_CHAR; i++) {
    d[i].cfunc = 0;
    d[i].sfunc = SG_UNBOUND;
  }
  return d;
}

#define INIT_COMPAT 0
#define INIT_R6RS   1
#define INIT_R7RS   2

static void init_readtable(readtable_t *table, int type)
{
  int i;
  readtab_t *r = table->readtable;
  disptab_t *d = alloc_disptab();
  for (i = 0; i <= ' '; i++) {
    r[i].type = CT_ILLEGAL;
    r[i].cfunc = NULL;
    r[i].sfunc = SG_UNBOUND;
    r[i].disp = NULL;
  }
  for (; i < MAX_READTABLE_CHAR; i++) {
    r[i].type = CT_CONSTITUENT;
    r[i].cfunc = NULL;
    r[i].sfunc = SG_UNBOUND;
    r[i].disp = NULL;
  }
  r['\t'].type = CT_WHITE_SPACE;
  r['\n'].type = CT_WHITE_SPACE;
  r['\v'].type = CT_WHITE_SPACE;
  r['\f'].type = CT_WHITE_SPACE;
  r['\r'].type = CT_WHITE_SPACE;
  r[' '].type = CT_WHITE_SPACE;
  r['\\'].type = CT_SINGLE_ESCAPE;

  SET_TERM_MACRO(r, '"', read_double_quote);
  SET_TERM_MACRO(r, '(', read_open_paren);
  SET_TERM_MACRO(r, ')', read_close_paren);
  SET_TERM_MACRO(r, '[', read_open_bracket);
  SET_TERM_MACRO(r, ']', read_close_bracket);
  SET_TERM_MACRO(r, ';', read_semicolon);
  

  r['#'].disp = d;

  SET_DISP_MACRO(d, '\'', read_hash_quote);
  SET_DISP_MACRO(d, '`', read_hash_quasiquote);
  SET_DISP_MACRO(d, ',', read_hash_unquote);
  SET_DISP_MACRO(d, '!', read_hash_bang);
  /* strict mode things */
  if (type == INIT_R6RS) {
    SET_DISP_MACRO(d, 'v', read_hash_v);
    SET_TERM_MACRO(r, '#', dispmacro_reader);
    /* tread these as nonterm and let validator raise an error
       for "foo'bar" case.
     */
    SET_NONTERM_MACRO(r, '\'', read_quote);
    SET_NONTERM_MACRO(r, '`', read_quasiquote);
    SET_NONTERM_MACRO(r, ',', read_unquote); 

    table->symbol_reader = read_r6rs_symbol;
  } else {
    SET_NONTERM_MACRO(r, '#', dispmacro_reader);
    SET_DISP_MACRO(d, 'u', read_hash_u);
    SET_NONTERM_MACRO(r, '|', read_vertical_bar);
    SET_TERM_MACRO(r, '\'', read_quote);
    SET_TERM_MACRO(r, '`', read_quasiquote);
    SET_TERM_MACRO(r, ',', read_unquote); 

    table->symbol_reader = read_compatible_symbol;
  }

  /* only compat mode */
  if (type == INIT_COMPAT) {
    /* #:a is only for compat mode */
    SET_DISP_MACRO(d, ':', read_hash_colon);
    SET_NONTERM_MACRO(r, ':', read_colon);
    /* well it's set only R6RS above but compat mode should read this as well */
    SET_DISP_MACRO(d, 'v', read_hash_v);
  }
  SET_DISP_MACRO(d, 't', read_hash_t);
  SET_DISP_MACRO(d, 'T', read_hash_t);
  SET_DISP_MACRO(d, 'f', read_hash_f);
  SET_DISP_MACRO(d, 'F', read_hash_f);
  SET_DISP_MACRO(d, 'b', read_hash_b);
  SET_DISP_MACRO(d, 'B', read_hash_b);
  SET_DISP_MACRO(d, 'o', read_hash_o);
  SET_DISP_MACRO(d, 'O', read_hash_o);
  SET_DISP_MACRO(d, 'd', read_hash_d);
  SET_DISP_MACRO(d, 'D', read_hash_d);
  SET_DISP_MACRO(d, 'x', read_hash_x);
  SET_DISP_MACRO(d, 'X', read_hash_x);
  SET_DISP_MACRO(d, 'i', read_hash_i);
  SET_DISP_MACRO(d, 'I', read_hash_i);
  SET_DISP_MACRO(d, 'e', read_hash_e);
  SET_DISP_MACRO(d, 'E', read_hash_e);
  SET_DISP_MACRO(d, '(', read_hash_open_paren);
  SET_DISP_MACRO(d, ';', read_hash_semicolon);
  SET_DISP_MACRO(d, '|', read_hash_bar);
  SET_DISP_MACRO(d, '\\', read_hash_escape);
  SET_DISP_MACRO(d, '=', read_hash_equal);
  SET_DISP_MACRO(d, '#', read_hash_hash);
  SET_DISP_MACRO(d, '<', read_hash_less);
}

void Sg__InitReader()
{
  init_readtable(&r6rs_read_table, INIT_R6RS);
  init_readtable(&r7rs_read_table, INIT_R7RS);
  init_readtable(&compat_read_table, INIT_COMPAT);

  Sg_InitMutex(&obtable_mutax, TRUE);
  obtable = Sg_MakeHashTableSimple(SG_HASH_EQUAL, 4096);

#define SET_READER_NAME(fn, name)			\
  (SG_PROCEDURE_NAME(&(SCHEME_OBJ(fn))) = SG_MAKE_STRING(name))
  SET_READER_NAME(read_vertical_bar,   "|-reader");
  SET_READER_NAME(read_double_quote,   "\"-reader");
  SET_READER_NAME(read_quote,          "'-reader");
  SET_READER_NAME(read_open_paren,     "(-reader");
  SET_READER_NAME(read_close_paren,    ")-reader");
  SET_READER_NAME(read_open_bracket,   "[-reader");
  SET_READER_NAME(read_close_bracket,  "]-reader");
  SET_READER_NAME(read_semicolon,      ";-reader");
  SET_READER_NAME(read_quasiquote,     "`-reader");
  SET_READER_NAME(read_unquote,        ",-reader"); 
  SET_READER_NAME(read_colon,          ":-reader");
  SET_READER_NAME(dispmacro_reader,    "dispatch-macro-reader");

  SET_READER_NAME(read_hash_quote,      "#'-reader");
  SET_READER_NAME(read_hash_quasiquote, "#`-reader");
  SET_READER_NAME(read_hash_unquote,    "#,-reader");
  SET_READER_NAME(read_hash_bang,       "#!-reader");
  SET_READER_NAME(read_hash_v, 	    "#v-reader");
  SET_READER_NAME(read_hash_u, 	    "#u-reader");
  SET_READER_NAME(read_hash_t, 	    "#t-reader");
  SET_READER_NAME(read_hash_f, 	    "#f-reader");
  SET_READER_NAME(read_hash_b, 	    "#b-reader");
  SET_READER_NAME(read_hash_o, 	    "#o-reader");
  SET_READER_NAME(read_hash_d, 	    "#d-reader");
  SET_READER_NAME(read_hash_x, 	    "#x-reader");
  SET_READER_NAME(read_hash_i, 	    "#i-reader");
  SET_READER_NAME(read_hash_e, 	    "#e-reader");
  SET_READER_NAME(read_hash_open_paren, "#(-reader");
  SET_READER_NAME(read_hash_semicolon,  "#;-reader");
  SET_READER_NAME(read_hash_bar,        "#|-reader");
  SET_READER_NAME(read_hash_escape,     "#\\-reader");
  SET_READER_NAME(read_hash_equal,      "#=-reader");
  SET_READER_NAME(read_hash_hash,       "##-reader");
  SET_READER_NAME(read_hash_less,       "#<-reader");
  SET_READER_NAME(read_hash_colon,      "#:-reader");

}

void Sg__InitReaderClass()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
#define CINIT(cl, nam)					\
  Sg_InitStaticClassWithMeta(cl, UC(nam), lib, NULL, SG_FALSE, NULL, 0)

  /* shared-ref */
  CINIT(SG_CLASS_SHARED_REF,   "<shared-ref>");
  /* for now no slot def */
  CINIT(SG_CLASS_READ_CONTEXT, "<read-context>");

  DEFAULT_CONTEXT = Sg_MakeDefaultReadContext();
  SYM_CONST = SG_INTERN("const");
  SYM_SOURCE_INFO = SG_INTERN("source-info");
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
