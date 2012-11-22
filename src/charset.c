/*                                                        -*- coding: utf-8; -*-
 * charset.c
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
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/charset.h"
#include "sagittarius/error.h"
#include "sagittarius/library.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/symbol.h"
#include "sagittarius/treemap.h"
#include "sagittarius/writer.h"
#include "sagittarius/cache.h"

static int compare(SgTreeMap *tm, intptr_t a, intptr_t b)
{
  if (a > b) return 1;
  if (a < b) return -1;
  return 0;
}

static void charset_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgCharSet *cs = SG_CHAR_SET(obj);
  SgObject ranges = Sg_CharSetRanges(cs), cp;

  Sg_Putuz(port, UC("#<char-set"));
  SG_FOR_EACH(cp, ranges) {
    SgObject cell = SG_CAR(cp);
    SgChar start = SG_INT_VALUE(SG_CAR(cell)), end = SG_INT_VALUE(SG_CDR(cell));
    Sg_Putc(port, ' ');
    if (start > SG_CHAR_SET_SMALL_CHARS) {
      Sg_Printf(port, UC("#x%x"), start);
    } else {
      Sg_Write(SG_MAKE_CHAR(start), port, SG_WRITE_WRITE);
    }
    Sg_Putc(port, '-');
    if (end > SG_CHAR_SET_SMALL_CHARS) {
      Sg_Printf(port, UC("#x%x"), end);
    } else {
      Sg_Write(SG_MAKE_CHAR(end), port, SG_WRITE_WRITE);
    }
  }
  Sg_Putc(port, '>');
}

static int charset_compare(SgObject x, SgObject y, int equalp)
{
  SgCharSet *xx = SG_CHAR_SET(x);
  SgCharSet *yy = SG_CHAR_SET(y);

  if (equalp) {
    return Sg_CharSetEq(xx, yy)? 0 : 1;
  } else {
    if (Sg_CharSetEq(xx, yy)) return 0;
    if (Sg_CharSetLe(xx, yy)) return -1;
    if (Sg_CharSetLe(yy, xx)) return 1;
    Sg_Error(UC("cannot compare char-set: %S vs %S"), x, y);
    return 0;			/* dummy */
  }
}

static SgObject charset_cache_reader(SgPort *port, SgReadCacheCtx *ctx)
{
  SgObject ranges = Sg_ReadCacheObject(port, ctx);
  SgCharSet *cs = SG_CHAR_SET(Sg_MakeEmptyCharSet());
  SG_FOR_EACH(ranges, ranges) {
    SgObject r = SG_CAR(ranges);
    Sg_CharSetAddRange(cs, SG_INT_VALUE(SG_CAR(r)), SG_INT_VALUE(SG_CDR(r)));
  }
  return SG_OBJ(cs);
}

static SgObject charset_cache_scanner(SgObject obj, SgObject cbs,
				      SgWriteCacheCtx *ctx)
{
  /* char-set always creates its range with fresh list so we can't scan */
  return cbs;
}

static void charset_cache_writer(SgObject obj, SgPort *port,
				 SgWriteCacheCtx *ctx)
{
  SgCharSet *cs = SG_CHAR_SET(obj);
  SgObject range = Sg_CharSetRanges(cs);
  Sg_WriteObjectCache(range, port, ctx);
}

#define DEFINE_CLASS_WITH_CACHE SG_DEFINE_BUILTIN_CLASS_WITH_CACHE

DEFINE_CLASS_WITH_CACHE(Sg_CharSetClass,
			charset_cache_reader, charset_cache_scanner, 
			charset_cache_writer,
			charset_print, charset_compare, NULL, NULL,
			SG_CLASS_DEFAULT_CPL);

static SgCharSet* make_charset()
{
  SgCharSet *cs = SG_NEW(SgCharSet);
  SG_SET_CLASS(cs, SG_CLASS_CHAR_SET);
  /* bits set */
  memset(cs->small, 0, SG_CHAR_SET_SMALL_CHARS);
  cs->large = Sg_MakeRBTreeMap(compare);
  return cs;
}

SgObject Sg_MakeEmptyCharSet()
{
  return SG_OBJ(make_charset());
}

SgObject Sg_CharSetCopy(SgCharSet *src)
{
  SgCharSet *dest = make_charset();
  memcpy(dest->small, src->small, SG_CHAR_SET_SMALL_CHARS);
  if (src->large) {
    dest->large = Sg_TreeMapCopy(src->large);
  }
  return SG_OBJ(dest);
}

int Sg_CharSetEq(SgCharSet *x, SgCharSet *y)
{
  if (memcmp(x->small, y->small, SG_CHAR_SET_SMALL_CHARS) != 0)
    return FALSE;
  if (x->large != NULL) {
    if (y->large != NULL) {
      return Sg_TreeMapEq(x->large, y->large);
    }
  }
  return FALSE;
}

int Sg_CharSetLe(SgCharSet *x, SgCharSet *y)
{
  SgTreeIter xi;
  SgTreeEntry *xe, *ye, *yl;
  if (memcmp(x->small, y->small, SG_CHAR_SET_SMALL_CHARS) > 0)
    return FALSE;

  Sg_TreeIterInit(&xi, x->large, NULL);
  for (xe = Sg_TreeIterNext(&xi); xe; xe = Sg_TreeIterNext(&xi)) {
    ye = Sg_TreeMapCoreRef(y->large, SG_OBJ(xe->key));
    if (ye) {
      if (ye->value < xe->value) return FALSE;
    } else {
      yl = Sg_TreeMapLowerEntry(y->large, SG_OBJ(xe->key));
      if (yl) {
	if (yl->value < xe->value) return FALSE;
      } else {
	return FALSE;
      }
    }
  }
  return TRUE;
}

#define bit_fill(bit, from, to, value)			\
  do {							\
    int i;						\
    for (i = (from); i < (to); i++) (bit)[i] = (value);	\
  } while (0)
    

SgObject Sg_CharSetAddRange(SgCharSet *cs, SgChar from, SgChar to)
{
  SgTreeEntry *e, *lo, *hi;

  if (to < from) return SG_OBJ(cs);
  if (from < SG_CHAR_SET_SMALL_CHARS) {
    if (to < SG_CHAR_SET_SMALL_CHARS) {
      bit_fill(cs->small, (int)from, (int)to+1, TRUE);
      return SG_OBJ(cs);
    }
    bit_fill(cs->small, (int)from, SG_CHAR_SET_SMALL_CHARS, TRUE);
    from = SG_CHAR_SET_SMALL_CHARS;
  }
  /* Let e have the lower bound. */
  e = Sg_TreeMapCoreRef(cs->large, SG_OBJ(from));
  lo = Sg_TreeMapLowerEntry(cs->large, SG_OBJ(from));
  if (!e) {
    if (!lo || lo->value < from-1) {
      e = Sg_TreeMapCoreSet(cs->large, SG_OBJ(from), SG_OBJ(0), 0);
    } else {
      e = lo;
    }
  }
  /* Set up the upper bound */
  if (e->value >= to) return SG_OBJ(cs);

  hi = e;
  while ((hi = Sg_TreeMapHigherEntry(cs->large, SG_OBJ(hi->key))) != NULL) {
    if (hi->key > to+1) {
      e->value = to;
      return SG_OBJ(cs);
    }
    Sg_TreeMapDelete(cs->large, SG_OBJ(hi->key));
    if (hi->value > to) {
      e->value = hi->value;
      return SG_OBJ(cs);
    }
  }
  e->value = to;
  return SG_OBJ(cs);
}

#define bit_operate2(target, op, bits1, bits2, start, end)	\
  do {								\
    int i;							\
    for (i = (start); i < (end); i++) {				\
      (target)[i] = (bits1)[i] op (bits2)[i];			\
    }								\
  } while (0)

SgObject Sg_CharSetAdd(SgCharSet *dest, SgCharSet *src)
{
  SgTreeIter iter;
  SgTreeEntry *e;
  if (dest == src) return SG_OBJ(dest);

  bit_operate2(dest->small, |, dest->small, src->small,
	      0, SG_CHAR_SET_SMALL_CHARS);
  Sg_TreeIterInit(&iter, src->large, NULL);
  while ((e = Sg_TreeIterNext(&iter)) != NULL) {
    Sg_CharSetAddRange(dest, SG_CHAR(e->key), SG_CHAR(e->value));
  }
  return SG_OBJ(dest);
}

#define bit_operate1(target, op, bits, start, end)		\
  do {								\
    int i;							\
    for (i = (start); i < (end); i++) {				\
      (target)[i] = ((bits)[i]) ? FALSE : TRUE;			\
    }								\
  } while (0)


SgObject Sg_CharSetComplement(SgCharSet *cs)
{
  int last;
  SgTreeEntry *e;
  bit_operate1(cs->small, ~, cs->small, 0, SG_CHAR_SET_SMALL_CHARS);
  last = SG_CHAR_SET_SMALL_CHARS - 1;
  while ((e = Sg_TreeMapHigherEntry(cs->large, SG_OBJ(last))) != NULL) {
    Sg_TreeMapDelete(cs->large, SG_OBJ(e->key));
    if (last < e->key - 1) {
      Sg_TreeMapCoreSet(cs->large, SG_OBJ(last+1), SG_OBJ(e->key-1), 0);
    }
    last = (int)e->value;
  }
  if (last < SG_CHAR_MAX) {
    Sg_TreeMapCoreSet(cs->large, SG_OBJ(last+1), SG_OBJ(SG_CHAR_MAX), 0);
  }
  return SG_OBJ(cs);
}

SgObject Sg_StringToCharSet(SgString *input, int error_p)
{
  int i, len = SG_STRING_SIZE(input);
  SgCharSet *cs = make_charset();
  int complement = FALSE, inrange = TRUE;
  SgChar lastChar = -1;
  
  for (i = 0; i < len; i++) {
    SgChar ch = SG_STRING_VALUE_AT(input, i);
    if (i == 0 && ch == '^') {
      complement = TRUE;
      continue;
    }
    switch (ch) {
    case '-':
      if (inrange) goto ordchar;
      inrange = TRUE;
      continue;
    case '\\':
      /* TODO */
      break;
    ordchar:
    default:
      if (inrange) {
	if (lastChar < 0) {
	  Sg_CharSetAddRange(cs, '-', '-');
	  Sg_CharSetAddRange(cs, ch, ch);
	  lastChar = ch;
	} else {
	  Sg_CharSetAddRange(cs, lastChar, ch);
	  lastChar = -1;
	}
      } else {
	Sg_CharSetAddRange(cs, ch, ch);
	lastChar = ch;
      }
      continue;
    }
  }
  if (complement) {
    Sg_CharSetComplement(cs);
  }
  return SG_OBJ(cs);
}

int Sg_CharSetContains(SgCharSet *cs, SgChar c)
{
  if (c < 0) return FALSE;
  if (c < SG_CHAR_SET_SMALL_CHARS) return cs->small[c];
  else {
    SgTreeEntry *e, *l;

    e = Sg_TreeMapCoreRef(cs->large, SG_OBJ(c));
    if (e) return TRUE;

    l = Sg_TreeMapLowerEntry(cs->large, SG_OBJ(c));
    if (l && l->value >= c) return TRUE;

    return FALSE;
  }
}
/* returns a list of ranges contained in the charset */
SgObject Sg_CharSetRanges(SgCharSet *cs)
{
  SgObject h = SG_NIL, t = SG_NIL, cell;
  int i, begin = 0, prev = FALSE;
  SgTreeIter iter;
  SgTreeEntry *e;

  for (i = 0; i < SG_CHAR_SET_SMALL_CHARS; i++) {
    int bit = cs->small[i];
    if (!prev && bit) begin = i;
    if (prev && !bit) {
      cell = Sg_Cons(SG_MAKE_INT(begin), SG_MAKE_INT(i-1));
      SG_APPEND1(h, t, cell);
    }
    prev = bit;
  }
  if (prev) {
    cell = Sg_Cons(SG_MAKE_INT(begin), SG_MAKE_INT(i-1));
    SG_APPEND1(h, t, cell);
  }
  Sg_TreeIterInit(&iter, cs->large, NULL);
  while ((e = Sg_TreeIterNext(&iter)) != NULL) {
    cell = Sg_Cons(SG_MAKE_INT(e->key), SG_MAKE_INT(e->value));
    SG_APPEND1(h, t, cell);
  }
  return h;
}

static SgCharSet *predef_charsets[SG_CHAR_SET_NUM_PREDEFINED_SETS] = {NULL};

#define CS(n) predef_charsets[n]
static void install_charsets()
{
  int i, code;
  /* we do not lock, because it'll installed when Sg_Init() runs */
#define MASK_SET(cs, code) ((cs)->small[(code)] = 1)
#define issymbl(c)							\
  ((c) == '$' || (c) == '+' || (c) == '<' || (c) == '=' ||		\
   (c) == '>' || (c) == '^' || (c) == '`' || (c) == '|' || (c) == '~')

  for (i = 0; i < SG_CHAR_SET_NUM_PREDEFINED_SETS; i++) {
    CS(i) = SG_CHAR_SET(make_charset());
  }
  for (code = 0; code < SG_CHAR_SET_SMALL_CHARS; code++) {
    if (isalnum(code)) MASK_SET(CS(SG_CHAR_SET_ALNUM), code);
    if (isalpha(code)) MASK_SET(CS(SG_CHAR_SET_ALPHA), code);
    if (iscntrl(code)) MASK_SET(CS(SG_CHAR_SET_CNTRL), code);
    if (isdigit(code)) MASK_SET(CS(SG_CHAR_SET_DIGIT), code);
    if (isgraph(code)) MASK_SET(CS(SG_CHAR_SET_GRAPH), code);
    if (islower(code)) MASK_SET(CS(SG_CHAR_SET_LOWER), code);
    if (isprint(code)) MASK_SET(CS(SG_CHAR_SET_PRINT), code);
    if (issymbl(code)) MASK_SET(CS(SG_CHAR_SET_SYMBL), code);
    if (ispunct(code)) MASK_SET(CS(SG_CHAR_SET_PUNCT), code);
    if (isspace(code)) MASK_SET(CS(SG_CHAR_SET_SPACE), code);
    if (isupper(code)) MASK_SET(CS(SG_CHAR_SET_UPPER), code);
    if (isxdigit(code)) MASK_SET(CS(SG_CHAR_SET_XDIGIT), code);
    if (isascii(code)) MASK_SET(CS(SG_CHAR_SET_ASCII), code);
    if (isalnum(code) || code == '_')
      MASK_SET(CS(SG_CHAR_SET_WORD), code);
    if (code == ' ' || code == '\t')
      MASK_SET(CS(SG_CHAR_SET_BLANK), code);
    
  }
}

SgObject Sg_GetStandardCharSet(int id)
{
  if (id < 0 || id >= SG_CHAR_SET_NUM_PREDEFINED_SETS) {
    Sg_Error(UC("bad id for predefined charset index: %d"), id);
  }
  if (predef_charsets[id] == NULL) {
    /* never happen but just incase */
    install_charsets();
  }
  return SG_OBJ(predef_charsets[id]);
}

void Sg__InitCharSet()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius)"), FALSE);
  SgCharSet *empty = Sg_MakeEmptyCharSet(); /* for complement */
  install_charsets();

  /* charset can not be serialized in cache, so it should not be constant */
#define insert_binding(name, value)			\
  Sg_MakeBinding(lib, SG_INTERN(#name), (value), FALSE);
  /* srfi-14 standard charsets */
  insert_binding(char-set:lower-case  , CS(SG_CHAR_SET_LOWER));
  insert_binding(char-set:upper-case  , CS(SG_CHAR_SET_UPPER));
  insert_binding(char-set:title-case  , Sg_MakeEmptyCharSet());
  insert_binding(char-set:letter      , CS(SG_CHAR_SET_ALPHA));
  insert_binding(char-set:digit       , CS(SG_CHAR_SET_DIGIT));
  insert_binding(char-set:letter+digit, CS(SG_CHAR_SET_ALNUM));
  insert_binding(char-set:graphic     , CS(SG_CHAR_SET_GRAPH));
  insert_binding(char-set:printing    , CS(SG_CHAR_SET_PRINT));
  insert_binding(char-set:whitespace  , CS(SG_CHAR_SET_SPACE));
  insert_binding(char-set:iso-control , CS(SG_CHAR_SET_CNTRL));
  insert_binding(char-set:punctuation , CS(SG_CHAR_SET_PUNCT));
  insert_binding(char-set:symbol      , CS(SG_CHAR_SET_SYMBL));
  insert_binding(char-set:hex-digit   , CS(SG_CHAR_SET_XDIGIT));
  insert_binding(char-set:blank       , CS(SG_CHAR_SET_BLANK));
  insert_binding(char-set:ascii       , CS(SG_CHAR_SET_ASCII));
  insert_binding(char-set:empty       , Sg_MakeEmptyCharSet());
  insert_binding(char-set:full        , Sg_CharSetComplement(empty));

  /* for regexp */
  insert_binding(:ascii:  , CS(SG_CHAR_SET_ASCII));
  insert_binding(:alnum:  , CS(SG_CHAR_SET_ALNUM));
  insert_binding(:alpha:  , CS(SG_CHAR_SET_ALPHA));
  insert_binding(:blank:  , CS(SG_CHAR_SET_BLANK));
  insert_binding(:cntrl:  , CS(SG_CHAR_SET_CNTRL));
  insert_binding(:digit:  , CS(SG_CHAR_SET_DIGIT));
  insert_binding(:graph:  , CS(SG_CHAR_SET_GRAPH));
  insert_binding(:lower:  , CS(SG_CHAR_SET_LOWER));
  insert_binding(:print:  , CS(SG_CHAR_SET_PRINT));
  insert_binding(:punct:  , CS(SG_CHAR_SET_PUNCT));
  insert_binding(:space:  , CS(SG_CHAR_SET_SPACE));
  insert_binding(:upper:  , CS(SG_CHAR_SET_UPPER));
  insert_binding(:xdigit:  , CS(SG_CHAR_SET_XDIGIT));

  insert_binding(*char-code-max*      , SG_MAKE_INT(SG_CHAR_MAX));
}
