// -*- C -*-
/*
 * reader.c
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
#include <ctype.h>
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/reader.h"
#include "sagittarius/port.h"
#include "sagittarius/pair.h"
#include "sagittarius/symbol.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/compare.h"
#include "sagittarius/keyword.h"
#include "sagittarius/builtin-symbols.h"
#include "sagittarius/error.h"
#include "sagittarius/writer.h"
#include "sagittarius/unicode.h"
#include "sagittarius/vector.h"
#include "sagittarius/number.h"
#include "sagittarius/vm.h"
#include "sagittarius/generic.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/unicode.h"

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

static int convert_hex_char_to_int(SgChar c)
{
  if ((c >= '0') & (c <= '9')) return c - '0';
  else if ((c >= 'a') & (c <= 'f')) return c - 'a' + 10;
  else if ((c >= 'A') & (c <= 'F')) return c - 'A' + 10;
  return -1;
}

static int delimited(SgChar c)
{
  if (Sg_Ucs4WhiteSpace(c)) return TRUE;
  if (c > 127) return FALSE;
  return DELIMITER_CHARP(c);
}

static int compare_cstr_ucs4str(const char* cstr, const SgChar* ustr)
{
  int len = strlen(cstr), i, ret;
  for (i = 0; i < len; i++) {
    ret &= (cstr[i] == ustr[i]);
  }
  return ret;
}

static SgSharedRef* make_shared_ref(int mark)
{
  SgSharedRef *z = SG_NEW(SgSharedRef);
  SG_SET_HEADER(z, TC_SHAREDREF);
  z->index = SG_MAKE_INT(mark);
  return z;
}

/* reader context */
typedef struct SgReaderContextRec
{
  SgHashTable *graph; /* for shared object*/
  int          graphRef;
  int          firstLine;
  int          parsingLineFrom;
  int          parsingLineTo;
} SgReaderContext;

static void lexical_error(SgPort * port, SgReaderContext *ctx, const SgChar *fmt, ...)
{
  va_list ap;
  SgObject msg, line, file;

  va_start(ap, fmt);
  msg = Sg_Vsprintf(fmt, ap, TRUE);
  va_end(ap);

  file = Sg_FileName(port);
  if (ctx->parsingLineFrom == ctx->parsingLineTo) {
    line = Sg_Sprintf(UC("read error: file %S, line %d"),
		      file,
		      ctx->parsingLineFrom);
  } else {
    line = Sg_Sprintf(UC("read error: file %S, line %d-%d"),
		      file,
		      ctx->parsingLineFrom, ctx->parsingLineTo);
  }
  Sg_ReadError(UC("%A\n"
		  "message: %A"), line, msg);
}


/* forward declaration */
static void parsing_range(SgReaderContext *ctx, int from, int to);
static void parsing_line(SgReaderContext *ctx, int line);
static SgObject read_expr(SgPort *port, SgReaderContext *ctx);
static SgObject read_token(SgPort *port, SgReaderContext *ctx);
static SgObject read_number(SgPort *port, SgReaderContext *ctx);
static SgObject read_string(SgPort *port, SgReaderContext *ctx);
static SgObject read_keyword(SgPort *port, SgReaderContext *ctx);
static SgObject read_bytevector(SgPort *port, SgReaderContext *ctx);
static SgObject read_char(SgPort *port, SgReaderContext *ctx);
static SgObject read_list(SgPort *port, SgReaderContext *ctx, int bracketedp, int vectorp);
static void read_thing(SgPort *port, SgReaderContext *ctx, SgChar *buf, size_t size, SgChar initial);
static SgObject read_prefixed_number(SgPort *port, SgReaderContext *ctx);
static void link_graph(SgPort *port, SgReaderContext *ctx, SgObject obj);

static inline SgObject skip_line(SgPort *port, SgReaderContext *ctx)
{
  SgChar c;
  while ((c = Sg_GetcUnsafe(port)) != EOF) {
    if (c == LF) return read_token(port, ctx);
  }
  return SG_EOF;
}

static SgObject skip_srfi30(SgPort *port, SgReaderContext *ctx)
{
  SgChar c1, c2;
  int nest = 0;
  
 seek_c1:
  c1 = Sg_GetcUnsafe(port);

 seek_c2:
  c2 = Sg_GetcUnsafe(port);
  if (c2 == EOF) {
    lexical_error(port, ctx, UC("unexpected end-of-file while reading comments"));
  }
  if (c1 == '|' && c2 == '#') {
    if (nest == 0) return read_token(port, ctx);
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

static SgChar read_hex_scalar_value(SgPort *port, SgReaderContext *ctx)
{
  SgChar ucs4 = 0, c = Sg_GetcUnsafe(port);
  if (c == EOF) lexical_error(port, ctx, UC("unexpected end-of-file while reading hex scalar value"));
  if (delimited(c)) lexical_error(port, ctx, UC("expected hex digit, but got %U, while reading hex scalar value"), c);
  Sg_UngetcUnsafe(port, c);
  int n;
  while (TRUE) {
    c = Sg_GetcUnsafe(port);
    if (c == EOF || delimited(c)) {
      Sg_UngetcUnsafe(port, c);
      return Sg_EnsureUcs4(ucs4);
    }
    n = convert_hex_char_to_int(c);
    if (n < 0) lexical_error(port, ctx, UC("expected hex digit, but got %U, while reading hex scalar value"), c);
    ucs4 = (ucs4 << 4) + n;
    if (ucs4 > 0x10ffff) lexical_error(port, ctx, UC("hex scalar value out of range"));
  }
}

static SgChar read_escape_sequence(SgPort *port, SgReaderContext *ctx)
{
  SgChar c = Sg_GetcUnsafe(port);
  switch (c) {
  case 'x':
    c = Sg_GetcUnsafe(port);
    if (c == EOF) lexical_error(port, ctx, UC("unexpected end-of-file while reading escape sequence"));
    Sg_UngetcUnsafe(port, c);
    c = read_hex_scalar_value(port, ctx);
    if (Sg_GetcUnsafe(port) != ';') lexical_error(port, ctx, UC("inline hex escape missing terminating semi-colon"));
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
  case EOF: lexical_error(port, ctx, UC("unexpected end-of-file while reading escape sequence"));
  }
  lexical_error(port, ctx, UC("invalid escape sequence, \\%S"), SG_MAKE_CHAR(c));
  return -1; /* dummy */
}

static SgObject read_symbol(SgPort *port, SgReaderContext *ctx)
{
  SgChar buf[SYMBOL_MAX_SIZE];
  int i = 0;
  SgChar c;
  
  while (i < array_sizeof(buf)) {
    c = Sg_PeekcUnsafe(port);
    if (c == EOF || delimited(c)) {
      /* TODO ugh... maybe i need to make hash-set like Ypsilon just be able to compare
	 with hash value.
       */
      SgChar *real = SG_NEW_ATOMIC2(SgChar *, (i + 1) * sizeof(SgChar));
      buf[i] = 0;
      memcpy(real, buf, (i + 1) * sizeof(SgChar));
      return Sg_Intern(Sg_MakeString(real, SG_LITERAL_STRING));
    }
    Sg_GetcUnsafe(port);
    if (c == '\\') {
      c = Sg_GetcUnsafe(port);
      if (c == 'x') {
	Sg_UngetcUnsafe(port, c);
	c = read_escape_sequence(port, ctx);
	//i += Sg_ConvertUcs4ToUtf8(Sg_EnsureUcs4(c), (uint8_t*)(buf + i), SG_ERROR_HANDLING_MODE(port));
	buf[i++] = c;
	continue;
      } else {
	lexical_error(port, ctx, UC("invalid character '\\' while reading identifier"));
      }
    }
    if (c > 127) {
      Sg_EnsureUcs4(c);
      if (i == 0) {
	if (Sg_Ucs4Constituent(c)) {
	  buf[i++] = c;
	  continue;
	}
      } else {
	if (Sg_Ucs4Subsequent(c)) {
	  buf[i++] = c;
	  continue;
	}
      }
      lexical_error(port, ctx, UC("invalid character %c while reading identifier"), c);
    }
    /* TODO get vm flag or something */
    if (TRUE) {
      if (SYMBOL_CHARP(c)) {
	buf[i++] = c;
	continue;
      }
    } else {
      if (i == 0) {
	if (INITIAL_CHARP(c)) {
	  buf[i++] = c;
	  continue;
	}
      } else {
	if (SYMBOL_CHARP(c)) {
	  buf[i++] = c;
	  continue;
	}
      }
    }
    lexical_error(port, ctx, UC("invalid character %c while reading identifier"), c);
  }
  lexical_error(port, ctx, UC("token buffer overflow while reading identifier"));
  return SG_UNDEF;
}

static SgObject read_quoted_symbol(SgPort *port, SgReaderContext *ctx, int interned)
{
  SgChar buf[SYMBOL_MAX_SIZE];
  int i = 0;
  /* TODO flag check */

  while (i < array_sizeof(buf)) {
    SgChar c = Sg_GetcUnsafe(port);
    if (c == EOF) {
      lexical_error(port, ctx, UC("unexpected end-of-file while reading quoted symbol"));
    }
    if (c == '|') {
      SgChar *real = SG_NEW_ATOMIC2(SgChar *, (i + 1) * sizeof(SgChar));
      buf[i] = 0;
      memcpy(real, buf, (i + 1) * sizeof(SgChar));
      return Sg_MakeSymbol(Sg_MakeString(real, SG_LITERAL_STRING), interned);
    }
    if (c == '\\') c = Sg_GetcUnsafe(port);
    buf[i++] = c;
  }
  lexical_error(port, ctx, UC("token buffer overflow while reading quoted symbol"));
  return SG_UNDEF; /* dummy */
}

SgObject read_number(SgPort *port, SgReaderContext *ctx)
{
  SgChar buf[4096];
  SgString *str;
  SgObject num;
  read_thing(port, ctx, buf, array_sizeof(buf), -1);
  str = Sg_MakeString(buf, SG_HEAP_STRING);
  num = Sg_StringToNumber(str, 10, TRUE);
  if (!SG_FALSEP(num)) return num;
  else return Sg_Intern(str);
}

SgObject read_prefixed_number(SgPort *port, SgReaderContext *ctx)
{
  SgChar buf[4096];
  SgString *str;
  SgObject num;
  read_thing(port, ctx, buf, array_sizeof(buf), '#');
  str = Sg_MakeString(buf, SG_HEAP_STRING);
  num = Sg_StringToNumber(str, 10, TRUE);
  if (SG_FALSEP(num)) lexical_error(port, ctx, UC("invalid lexical syntax %S while reading number"), buf);
  return num;
}

void  read_thing(SgPort *port, SgReaderContext *ctx, SgChar *buf, size_t size, SgChar initial)
{
  size_t i = 0;
  if (initial != -1) {
    buf[i++] = initial;
  }
  while (i < size) {
    SgChar c = Sg_PeekcUnsafe(port);
    if (c == EOF || delimited(c)) {
      buf[i] = 0;
      return;
    }
    Sg_GetcUnsafe(port);
    buf[i++] = c;
  }
  lexical_error(port, ctx, UC("token buffer overflow while reading identifier, %s ..."), buf);
}

static const struct {
  const char* name;
  int code;
} s_char_name[] = {
  { "nul",        0x0000 },
  { "alarm",      0x0007 },
  { "backspace",  0x0008 },
  { "tab",        0x0009 },
  { "linefeed",   0x000A },
  { "newline",    0x000A },
  { "vtab",       0x000B },
  { "page",       0x000C },
  { "return",     0x000D },
  { "esc",        0x001B },
  { "space",      0x0020 },
  { "delete",     0x007F }
};

SgObject read_char(SgPort *port, SgReaderContext *ctx)
{
  SgChar c = Sg_GetcUnsafe(port);
  if (c == 'x') {
    c = Sg_PeekcUnsafe(port);
    if (c == EOF || delimited(c)) return SG_MAKE_CHAR('x');
    return SG_MAKE_CHAR(read_hex_scalar_value(port, ctx));
  } else {
    SgChar buf[32];
    int i, n;
    SgChar ucs4;
    if (c == '(') {
      c = Sg_PeekcUnsafe(port);
      if (c == EOF || delimited(c)) return SG_MAKE_CHAR('(');
      read_thing(port, ctx, buf, array_sizeof(buf), -1);
      lexical_error(port, ctx, UC("invalid lexical syntax #\\(%s"), buf);
    }
    Sg_UngetcUnsafe(port, c);
    read_thing(port, ctx, buf, array_sizeof(buf), -1);
    if (buf[0] == 0) {
      c = Sg_GetcUnsafe(port);
      if (c == EOF) lexical_error(port, ctx, UC("unexpected end-of-file while reading character"));
      return SG_MAKE_CHAR(c);
    }
    if (buf[1] == 0) return SG_MAKE_CHAR(buf[0]);
    for (i = 0; i < array_sizeof(s_char_name); i++) {
      if (compare_cstr_ucs4str(s_char_name[i].name, buf)) return SG_MAKE_CHAR(s_char_name[i].code);
    }
    n = Sg_ConvertUtf8ToUcs4((uint8_t*)buf, &ucs4, SG_ERROR_HANDLING_MODE(port));
    if (n > 0 && buf[n] == 0) return SG_MAKE_CHAR(ucs4);
    lexical_error(port, ctx, UC("invalid lexical syntax #\\%s"), buf);
  }
  return SG_UNDEF; /* dummy */
}

SgObject read_bytevector(SgPort *port, SgReaderContext *ctx)
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
	  lexical_error(port, ctx, UC("expected " s_type ", but got %S"), SG_CAR(lst)); \
	}								\
      }									\
      return bvector;							\
    }									\
  } while (0)

  SgChar buf[16];
  read_thing(port, ctx, buf, array_sizeof(buf), -1);
  SgChar c = Sg_GetcUnsafe(port);
  if (c == '(') {
    int line_begin = Sg_LineNo(port);
    int n;
    SgObject lst = read_list(port, ctx, FALSE, FALSE);
    parsing_range(ctx, line_begin, Sg_LineNo(port));
    n = Sg_Length(lst);
    READ_BVECTOR("u8", uint8_t, SG_INTP, CAST_FIXNUM_TO_U8);
  }
  lexical_error(port, ctx, UC("invalid lexical syntax #v%s%A ..."), buf, SG_MAKE_CHAR(c));
#undef CAST_FIXNUM_TO_U8
#undef READ_BVECTOR
  return SG_UNDEF;		/* dummy */
}

SgObject read_list(SgPort *port, SgReaderContext *ctx, int bracketedp, int vectorp)
{
  SgObject h = SG_NIL, t = SG_NIL, token;
  int line_begin = Sg_LineNo(port);
  int first_token = TRUE;
  while ((token = read_token(port, ctx)) != SG_EOF) {
    if (SG_EQ(token, SG_SYMBOL_RPAREN)) {
      if (bracketedp) {
	parsing_range(ctx, line_begin, Sg_LineNo(port));
	lexical_error(port, ctx, UC("bracketed list terminated by parenthesis"));
      }
      if (!SG_NULLP(h)) {
	SG_SOURCE_INFO(h) = Sg_Cons(Sg_FileName(port), SG_MAKE_INT(line_begin));
      }
      return h;
    }
    if (SG_EQ(token, SG_SYMBOL_RBRACK)) {
      if (!bracketedp) {
	parsing_range(ctx, line_begin, Sg_LineNo(port));
	lexical_error(port, ctx, UC("bracketed list terminated by bracket"));
      }
      if (!SG_NULLP(h)) {
	SG_SOURCE_INFO(h) = Sg_Cons(Sg_FileName(port), SG_MAKE_INT(line_begin));
      }
      return h;
    }
    if (SG_EQ(token, SG_SYMBOL_LPAREN)) {
      SG_APPEND1(h, t, read_list(port, ctx, FALSE, FALSE));
      continue;
    }
    if (SG_EQ(token, SG_SYMBOL_LBRACK)) {
      SG_APPEND1(h, t, read_list(port, ctx, TRUE, FALSE));
      continue;
    }
    if (SG_EQ(token, SG_SYMBOL_DOT)) {
      if (vectorp) {
	lexical_error(port, ctx, UC("misplaced dot('.') while reading vector"));
      } else if (SG_NULLP(h)) {
	parsing_range(ctx, line_begin, Sg_LineNo(port));
	lexical_error(port, ctx, UC("misplaced dot('.') while reading list"));
      } else {
	SgObject rest = read_expr(port, ctx);
	if (SG_EQ(rest, SG_SYMBOL_DOT)) lexical_error(port, ctx, UC("misplaced dot('.') while reading list"));
	token = read_token(port, ctx);
	if (SG_EQ(token, SG_SYMBOL_RPAREN)) {
	  if (bracketedp) {
	    parsing_range(ctx, line_begin, Sg_LineNo(port));
	    lexical_error(port, ctx, UC("bracketed list terminated by parenthesis"));
	  }
	  SG_SET_CDR(t, rest);
	  if (!SG_NULLP(h)) {
	    SG_SOURCE_INFO(h) = Sg_Cons(Sg_FileName(port), SG_MAKE_INT(line_begin));
	  }	  
	  return h;
	}
	if (SG_EQ(token, SG_SYMBOL_RBRACK)) {
	  if (!bracketedp) {
	    parsing_range(ctx, line_begin, Sg_LineNo(port));
	    lexical_error(port, ctx, UC("bracketed list terminated by bracket"));
	  }
	  SG_SET_CDR(t, rest);
	  if (!SG_NULLP(h)) {
	    SG_SOURCE_INFO(h) = Sg_Cons(Sg_FileName(port), SG_MAKE_INT(line_begin));
	  }
	  return h;
	}
	parsing_range(ctx, line_begin, Sg_LineNo(port));
	if (SG_EQ(token, SG_EOF)) lexical_error(port, ctx, UC("unexpected end-of-file while reading list"));
	lexical_error(port, ctx, UC("more then one item following dot('.') while reading list"));
      }
    }
    /* default read time constructor */
    if (!vectorp && first_token && SG_SYMBOLP(token)) {
      SgVM *vm = Sg_VM();
      SgObject generic = Sg_Memq(token, vm->defaultConstructors);
      if (!SG_FALSEP(generic)) {
	generic = Sg_RetrieveGeneric(SG_CAR(generic));
	SgObject args = read_list(port, ctx, bracketedp, vectorp);
	SgObject ret = Sg_Apply(SG_GENERIC_READER(generic), args);
	return ret;
      }
    }
    SG_APPEND1(h, t, token);
    first_token = FALSE;
  }
  parsing_range(ctx, line_begin, Sg_LineNo(port));
  lexical_error(port, ctx, UC("unexpected end-of-file while reading list"));
  return SG_UNDEF; /* dummy */
}


SgObject read_string(SgPort *port, SgReaderContext *ctx)
{
  SgChar buf[READ_STRING_MAX_SIZE];
  int i = 0;
  while (i < array_sizeof(buf)) {
    SgChar c = Sg_GetcUnsafe(port);
    if (c == EOF) lexical_error(port, ctx, UC("unexpected end-of-file while reading string"));
    if (c == LF) {
      buf[i++] = LF;
      continue;
    }
    if (c == '"') {
      SgChar *real = SG_NEW_ATOMIC2(SgChar *, (i + 1) * sizeof(SgChar));
      buf[i] = 0;
      memcpy(real, buf, (i + 1) * sizeof(SgChar));
      return Sg_MakeString(buf, SG_LITERAL_STRING);
    }
    if (c == '\\') {
      c = Sg_GetcUnsafe(port);
      if (Sg_Ucs4IntralineWhiteSpace(c)) {
	do {
	  c = Sg_GetcUnsafe(port);
	  if (c == EOF) lexical_error(port, ctx, UC("unexpected end-of-file while reading intraline whitespace"));
	} while (Sg_Ucs4IntralineWhiteSpace(c));
	/* internal line feed is LF*/
	if (c != LF) {
	  lexical_error(port, ctx, UC("unexpected character %U while reading intraline whitespace"), c);
	}
	do { c = Sg_GetcUnsafe(port); } while (Sg_Ucs4IntralineWhiteSpace(c));
	Sg_UngetcUnsafe(port, c);
	continue;
      }
      if (c == LF) {
	do { c = Sg_GetcUnsafe(port); } while (Sg_Ucs4IntralineWhiteSpace(c));
	Sg_UngetcUnsafe(port, c);
	continue;
      }
      c = read_escape_sequence(port, ctx);
      buf[i++] = c;
      continue;
    }
    buf[i++] = c;
  }
  lexical_error(port, ctx, UC("token buffer overflow while reading string"));
  return SG_UNDEF; /* dummy */
}


SgObject read_keyword(SgPort *port, SgReaderContext *ctx)
{
  int c2 = Sg_GetcUnsafe(port);
  SgObject name;
  SgChar buf[SYMBOL_MAX_SIZE];	/* too small? */

  if (c2 == '|') {
    name = read_quoted_symbol(port, ctx, FALSE);
    return Sg_MakeKeyword(SG_SYMBOL(name)->name);
  } else {
    Sg_UngetcUnsafe(port, c2);
    read_thing(port, ctx, buf, array_sizeof(buf), -1);
    return Sg_MakeKeyword(Sg_MakeString(buf, SG_LITERAL_STRING));
  }
}


SgObject read_token(SgPort *port, SgReaderContext *ctx)
{
  SgChar c;

 top:
  c = Sg_GetcUnsafe(port);
  if (c == EOF) return SG_EOF;
  if (Sg_Ucs4WhiteSpace(c)) goto top;
  parsing_line(ctx, Sg_LineNo(port));
  if (c < 128 && isdigit(c)) {
    Sg_UngetcUnsafe(port, c);
    return read_number(port, ctx);
  }
  switch (c) {
  case ';': return skip_line(port, ctx);
  case '"': return read_string(port, ctx);
  case '|': return read_quoted_symbol(port, ctx, TRUE);
  case '(': return SG_SYMBOL_LPAREN;
  case ')': return SG_SYMBOL_RPAREN;
  case '[': return SG_SYMBOL_LBRACK;
  case ']': return SG_SYMBOL_RBRACK;
  case '\'': {
    SgObject obj = read_expr(port, ctx);
    if (SG_EQ(obj, SG_EOF)) lexical_error(port, ctx, UC("unexpected end-of-file following quotation-mark(')"));
    return SG_LIST2(SG_SYMBOL_QUOTE, obj);
  }
  case '`': {
    SgObject obj = read_expr(port, ctx);
    if (SG_EQ(obj, SG_EOF)) lexical_error(port, ctx, UC("unexpected end-of-file following grave-accent(`)"));
    return SG_LIST2(SG_SYMBOL_QUASIQUOTE, obj);
  }
  case '+':
  case '.':
  case '-': {
    Sg_UngetcUnsafe(port, c);
    return read_number(port, ctx);
  }
  case '#':
    c = Sg_GetcUnsafe(port);
    switch (c) {
    case EOF: lexical_error(port, ctx, UC("unexpected end-of-file following sharp-sign(`)"));
    case '!': {
      SgObject desc = read_symbol(port, ctx);
      if (SG_SYMBOLP(desc)) {
	SgString *tag = SG_SYMBOL(desc)->name;
	/* TODO add flags */
	if (compare_cstr_ucs4str("r6rs", tag->value) == 0) {
	}
      }
      goto top;
    }
    case 'v': return read_bytevector(port, ctx);
    case 'f':
    case 'F': {
      SgChar c2 = Sg_GetcUnsafe(port);
      if (c2 == EOF || delimited(c2)) {
	Sg_UngetcUnsafe(port, c2);
	return SG_FALSE;
      }
      lexical_error(port, ctx, UC("invalid lexical syntax %S%S"), SG_MAKE_CHAR(c), SG_MAKE_CHAR(c2));
    }
    case 't':
    case 'T': {
      SgChar c2 = Sg_GetcUnsafe(port);
      if (c2 == EOF || delimited(c2)) {
	Sg_UngetcUnsafe(port, c2);
	return SG_TRUE;
      }
      lexical_error(port, ctx, UC("invalid lexical syntax #%S%S"), SG_MAKE_CHAR(c), SG_MAKE_CHAR(c2));
    }
    case '(': return Sg_ListToVector(read_list(port, ctx, FALSE, TRUE), 0, -1);
    case '|': return skip_srfi30(port, ctx);
    case '\\': return read_char(port, ctx);
    case ';': read_expr(port, ctx); goto top;
    case 'b': case 'B':
    case 'o': case 'O':
    case 'd': case 'D':
    case 'x': case 'X':
    case 'i': case 'I':
    case 'e': case 'E':
      Sg_UngetcUnsafe(port, c);
      return read_prefixed_number(port, ctx);
    case '\'':
      return SG_LIST2(SG_SYMBOL_SYNTAX, read_expr(port, ctx));
    case '`':
      return SG_LIST2(SG_SYMBOL_QUASISYNTAX, read_expr(port, ctx));
    case ',':
      c = Sg_GetcUnsafe(port);
      if (c == EOF) lexical_error(port, ctx, UC("unexpected end-of-file following sharp comma(#,)"));
      if (c == '@') return SG_LIST2(SG_SYMBOL_UNSYNTAX_SPLICING, read_expr(port, ctx));
      Sg_UngetcUnsafe(port, c);
      return SG_LIST2(SG_SYMBOL_UNSYNTAX, read_expr(port, ctx));
    default:
      if (ctx->graph == NULL) break;
      if (c >= '0' && c <= '9') {
	intptr_t mark = c - '0';
	SgChar c2;
	while (TRUE) {
	  c2 = Sg_GetcUnsafe(port);
	  if (c2 >= '0' && c2 <= '9') {
	    mark = mark * 10 + c2;
	    if (mark < 0 || mark > SG_INT_MAX) lexical_error(port, ctx, UC("invalid object tag, value out of range"));
	    continue;
	  }
	  if (c2 == EOF) lexical_error(port, ctx, UC("unexpected end-of-file while reading tag #%ld="), mark);
	  if (c2 == '=') {
	    SgObject obj = read_expr(port, ctx);
	    if (SG_EQ(obj, SG_EOF)) lexical_error(port, ctx, UC("unexpected end-of-file while reading tag #%ld="), mark);
	    if (SG_EQ(Sg_HashTableRef(ctx->graph, SG_MAKE_INT(mark), SG_UNDEF), SG_UNDEF)) {
	      Sg_HashTableSet(ctx->graph, SG_MAKE_INT(mark), obj, 0);
	      return obj;
	    }
	    lexical_error(port, ctx, UC("duplicate tag #%ld="), mark);
	  }
	  if (c2 == '#') {
	    SgSharedRef *ref = make_shared_ref(mark);
	    ctx->graphRef = TRUE;
	    return SG_OBJ(ref);
	  }
	  break;
	}
      }
      break;
    }
    lexical_error(port, ctx, UC("invalid lexical syntax %S"), SG_MAKE_CHAR(c));
  case ',':
    c = Sg_GetcUnsafe(port);
    if (c == EOF) lexical_error(port, ctx, UC("unexpected end-of-file following comma(,)"));
    if (c == '@') return SG_LIST2(SG_SYMBOL_UNQUOTE_SPLICING, read_expr(port, ctx));
    Sg_UngetcUnsafe(port, c);
    return SG_LIST2(SG_SYMBOL_UNQUOTE, read_expr(port, ctx));
  case ':':
    return read_keyword(port, ctx);
  default:
    Sg_UngetcUnsafe(port, c);
    return read_symbol(port, ctx);
  }
}

SgObject read_expr(SgPort *port, SgReaderContext *ctx)
{
  SgObject token = read_token(port, ctx);
  if (token == SG_SYMBOL_RPAREN) lexical_error(port, ctx, UC("unexpected closing parenthesis"));
  if (token == SG_SYMBOL_RBRACK) lexical_error(port, ctx, UC("unexpected closing bracket"));
  if (token == SG_SYMBOL_LPAREN) return read_list(port, ctx, FALSE, FALSE);
  if (token == SG_SYMBOL_LBRACK) return read_list(port, ctx, TRUE, FALSE);
  return token;
}

void parsing_range(SgReaderContext *ctx, int from, int to)
{
  ctx->parsingLineFrom = from;
  ctx->parsingLineTo = to;
}

void parsing_line(SgReaderContext *ctx, int line)
{
  parsing_range(ctx, line, line);
}

static SgObject lookup_graph(SgPort *port, SgReaderContext *ctx, SgSharedRef *ref)
{
  SgObject obj = Sg_HashTableRef(ctx->graph, ref->index, SG_UNDEF);
  if (SG_SHAREDREF_P(obj)) return lookup_graph(port, ctx, SG_SHAREDREF(obj));
  if (obj != SG_UNDEF) return obj;
  lexical_error(port, ctx, UC("attempt to reference undefined tag #%d#"), ref->index);
  return SG_UNDEF; /* dummy */
}

void link_graph(SgPort *port, SgReaderContext *ctx, SgObject obj)
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
	SG_VECTOR_ELEMENT(obj, i) = lookup_graph(port, ctx, SG_SHAREDREF(SG_VECTOR_ELEMENT(obj, i)));
      } else {
	link_graph(port, ctx, SG_VECTOR_ELEMENT(obj, i));
      }
    }
    return;
  }
}

static SgReaderContext* make_reader_context(int readSharedObject)
{
  SgReaderContext *ctx = SG_NEW(SgReaderContext);
  if (readSharedObject) {
    ctx->graph = Sg_MakeHashTableSimple(SG_HASH_EQ, 1);
  }
  ctx->graphRef = FALSE;
  return ctx;
}

static SgObject read_with_context(SgPort *port, SgReaderContext *ctx)
{
  ctx->firstLine = Sg_LineNo(port);
  SgObject obj = read_expr(port, ctx);
  if (SG_EQ(obj, SG_SYMBOL_DOT)) lexical_error(port, ctx, UC("misplaced dot('.')"));
  if (ctx->graph && ctx->graphRef) link_graph(port, ctx, obj);
  parsing_range(ctx, ctx->firstLine, Sg_LineNo(port));
  return obj;
}

SgObject Sg_Read(SgObject port, int readSharedObject)
{
  ASSERT(SG_PORTP(port));
  ASSERT(SG_TEXTUAL_PORTP(port))
  return read_with_context(SG_PORT(port), make_reader_context(readSharedObject));
}

void Sg__InitReader()
{
  SgVM *vm = Sg_VM();
  /* it must be null. so we don't have to check */
  vm->defaultConstructors =
    Sg_Append2(vm->defaultConstructors, SG_LIST1(SG_SYMBOL_FILE_OPTIONS));
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
