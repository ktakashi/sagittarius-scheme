/* -*- C -*- */
/*
 * string.c
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
#include <sagittarius/config.h>
#include <string.h>

#ifndef __GNUC__
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
#pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#else
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# endif
# ifdef HAVE_MALLOC_H
/* MinGW helds alloca() in "malloc.h" instead of "alloca.h" */
#  include <malloc.h>
# endif
#endif

#define LIBSAGITTARIUS_BODY
#include "sagittarius/string.h"
#include "sagittarius/collection.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/unicode.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/error.h"
#include "sagittarius/number.h"
#include "sagittarius/thread.h"
#include "sagittarius/values.h"
#include "sagittarius/writer.h"

static void string_print(SgObject o, SgPort *port, SgWriteContext *ctx)
{
  SgString *obj = SG_STRING(o);
  SG_PORT_LOCK(port);
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
      case '\"':
	Sg_PutcUnsafe(port, '\\'); Sg_PutcUnsafe(port, '\"');
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
  SG_PORT_UNLOCK(port);
}

SG_DEFINE_BUILTIN_CLASS(Sg_StringClass, string_print, NULL, NULL, NULL,
			SG_CLASS_SEQUENCE_CPL);

#define STRING_ALLOC_SIZE(size)			\
  (sizeof(SgString)+sizeof(SgChar)*size)

static SgString* make_string(int size)
{
  SgString *z = SG_NEW_ATOMIC2(SgString *, STRING_ALLOC_SIZE(size));
  SG_SET_CLASS(z, SG_CLASS_STRING);
  z->size = size;
  z->literalp = FALSE;
  return z;
}

#define COPY_STRING(ret, src, size, offset)				\
  do {									\
    int i;								\
    for (i = 0; i < (size); i++) {					\
      ((ret)->value)[i + (offset)] = (src[i]);				\
    }									\
  } while (0)

static SgInternalMutex smutex;
static SgHashTable *stable;

#ifdef _MSC_VER
/* _alloca is in <malloc.h> */
#include <malloc.h>
#define alloca _alloca
#endif

#ifdef HAVE_ALLOCA
#define ALLOC_TEMP_STRING(var, size)					\
    (var) = SG_STRING(alloca(STRING_ALLOC_SIZE(size)));			\
    SG_SET_CLASS(var, SG_CLASS_STRING);					\
    SG_STRING_SIZE(var) = size;
#else
#define ALLOC_TEMP_STRING(var, size) (var) = make_string(size);
#endif



SgObject Sg_MakeStringEx(const SgChar *value, SgStringType flag, int length)
{
  SgObject r;
  SgString *z;

  if (flag == SG_LITERAL_STRING) {
    SgString *tmp;
    Sg_LockMutex(&smutex);
    ALLOC_TEMP_STRING(tmp, length);
    COPY_STRING(tmp, value, length, 0);
    r = Sg_HashTableRef(stable, SG_OBJ(tmp), SG_FALSE);
    Sg_UnlockMutex(&smutex);
    if (!SG_FALSEP(r)) {
      ASSERT(SG_STRINGP(r));
      return r;
    }
  }

  z = make_string(length);
  COPY_STRING(z, value, z->size, 0);
  z->value[z->size] = 0;

  /* store it if it's literal */
  if (flag == SG_LITERAL_STRING) {
    Sg_LockMutex(&smutex);
    z->literalp = TRUE;
    r = Sg_HashTableSet(stable, SG_OBJ(z), SG_OBJ(z),
			SG_HASH_NO_OVERWRITE);
    Sg_UnlockMutex(&smutex);
  } else {
    r = SG_OBJ(z);
  }
  return r;
}

SgObject Sg_MakeString(const SgChar *value, SgStringType flag)
{
  int len = (int)ustrlen(value);
  return Sg_MakeStringEx(value, flag, len);
}

/* This method assumes given value as ASCII for now */
SgObject Sg_MakeStringC(const char *value)
{
#if 1
  SgString *z;
  z = make_string((int)strlen(value));
  COPY_STRING(z, value, z->size, 0);
  z->value[z->size] = 0;
  return SG_OBJ(z);
#else
  int size = strlen(value), i;
  SgChar *z, *t;
  z = t = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * (size + 1));
  
  for (i = 0; i < size; i++) {
    *t++ = *value++;
  }
  *t = 0;
  return Sg_MakeString(z, SG_LITERAL_STRING);
#endif
}

SgObject Sg_ReserveString(int size, SgChar fill)
{
  SgString *z = make_string(size);
  int i;
  for (i = 0; i < size; i++) {
    z->value[i] = fill;
  }
  z->value[size] = 0;
  return SG_OBJ(z);
}

SgObject Sg_MakeEmptyString()
{
  SgString *z = make_string(0);
  return SG_OBJ(z);
}

static int string_equal(SgChar *s1, int size1, SgChar *s2, int size2)
{
  if (size1 != size2) return FALSE;
  else {
    int i;
    for (i = 0; i < size1; i++) {
      if (s1[i] != s2[i]) return FALSE;
    }
  }
  return TRUE;
}

int Sg_StringEqual(SgString *s1, SgString *s2)
{
  return string_equal(s1->value, s1->size, s2->value, s2->size);
}

static inline int string_compare_rec(SgString *s1, SgString *s2, int len)
{
  int i;
  for (i = 0; i < len; i++) {
    if (SG_STRING_VALUE_AT(s1, i) > SG_STRING_VALUE_AT(s2, i)) {
      return 1;
    } else if (SG_STRING_VALUE_AT(s1, i) < SG_STRING_VALUE_AT(s2, i)) {
      return -1;
    }
  }
  return 0;
}

int Sg_StringCompare(SgString *s1, SgString *s2)
{
  int s1_len = SG_STRING_SIZE(s1);
  int s2_len = SG_STRING_SIZE(s2);
  int len = (s1_len > s2_len) ? s2_len : s1_len;
  int result = string_compare_rec(s1, s2, len);
  if (result == 0) {
    if (s1_len == s2_len) return 0;
    else if (s1_len > s2_len) return 1;
    else return -1;
  } else {
    return result;
  }
}

SgObject Sg_StringAppend2(SgString *a, SgString *b)
{
  SgString *z = make_string(a->size + b->size);
  COPY_STRING(z, a->value, a->size, 0);
  COPY_STRING(z, b->value, b->size, a->size);
  z->value[a->size + b->size] = '\0';
  return SG_OBJ(z);
}

SgObject Sg_StringAppendC(SgString *a, const SgChar *s, int sizey)
{
  int sizex = a->size;
  SgString *p = make_string(sizex + sizey);
  /* manual copy */
  COPY_STRING(p, a->value, sizex, 0);
  COPY_STRING(p, s, sizey, sizex);
  p->value[sizex + sizey] = '\0';

  return SG_OBJ(p);
}

SgObject Sg_StringAppend(SgObject args)
{
  int len = 0, off = 0;
  SgObject cp;
  SgString *r;
  /* calculate length */
  SG_FOR_EACH(cp, args) {
    if (!SG_STRINGP(SG_CAR(cp))) {
      Sg_Error(UC("string required, but got %S"), SG_CAR(cp));
    }
    len += SG_STRING(SG_CAR(cp))->size;
  }
  if (!SG_NULLP(cp)) {
    Sg_Error(UC("improper list is not allowed"), args);
  }
  r = make_string(len);
  /* append */
  SG_FOR_EACH(cp, args) {
    COPY_STRING(r, SG_STRING(SG_CAR(cp))->value,
		SG_STRING(SG_CAR(cp))->size, off);
    off += SG_STRING(SG_CAR(cp))->size;
  }
  r->value[len] = 0;
  return SG_OBJ(r);
}

SgObject Sg_StringToList(SgString *s, int start, int end)
{
  int size = SG_STRING_SIZE(s), i;
  const SgChar *buf = SG_STRING_VALUE(s);
  SgObject h = SG_NIL, t = SG_NIL;
  SG_CHECK_START_END(start, end, size);
  for (i = start; i < end; i++) {
    SG_APPEND1(h, t, SG_MAKE_CHAR(buf[i]));
  }
  return h;
}

SgObject Sg_ListToString(SgObject chars)
{
  SgObject cp;
  int len = 0;
  SgChar ch;
  SgChar *buf, *bufp;

  SG_FOR_EACH(cp, chars) {
    if (!SG_CHARP(SG_CAR(cp))) {
      Sg_Error(UC("character required, but got %S"), SG_CAR(cp));
    }
    len++;
  }
  bufp = buf = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * (len + 1));
  SG_FOR_EACH(cp, chars) {
    ch = SG_CHAR_VALUE(SG_CAR(cp));
    *bufp++ = ch;
  }
  *bufp = 0;
  return Sg_MakeStringEx(buf, SG_HEAP_STRING, len);
}

SgObject Sg_CopyString(SgString *a)
{
  /* TODO consider if src string was literal */
  SgString *s = make_string(a->size);
  COPY_STRING(s, a->value, a->size, 0);
  s->value[s->size] = '\0';
  return SG_OBJ(s);
}

SgChar Sg_StringRef(SgString *s, int k)
{
  if (k > SG_STRING_SIZE(s) || k < 0) {
    Sg_Error(UC("string-ref: index out of bounds. %S %d"), s, k);
  }
  return SG_STRING_VALUE_AT(s, k);
}

static inline int boyer_moore(const SgChar *ss1, int siz1,
                              const SgChar *ss2, int siz2)
{
  uint32_t shift[256];
  int i, j, k;
  for (i = 0; i < 256; i++) { shift[i] = siz2; }
  for (j = 0; j < siz2-1; j++) {
    shift[(uint32_t)ss2[j]] = siz2-j-1;
  }
  for (i = siz2 - 1; i < siz1; i += shift[(uint32_t)ss1[i]]) {
    for (j = siz2 - 1, k = i; j >= 0 && ss1[k] == ss2[j]; j--, k--)
      ;
    if (j == -1) return k+1;
  }
  return -1;
}

static SgObject string_scan(SgString *s, const SgChar *ss2,
			    int size2, int retmode)
{
  int i;
  const SgChar *ss1 = SG_STRING_VALUE(s);
  int size1 = SG_STRING_SIZE(s);
  const SgObject nullstr = Sg_MakeString(UC(""), SG_LITERAL_STRING);

  if (retmode < 0 || retmode > SG_STRING_SCAN_BOTH) {
    Sg_Error(UC("return mode out of range' %d"), retmode);
  }
  if (size2 == 0) {
    /* shortcut */
    switch (retmode) {
    case SG_STRING_SCAN_INDEX: return SG_MAKE_INT(0);
    case SG_STRING_SCAN_BEFORE: return nullstr;
    case SG_STRING_SCAN_AFTER:  return Sg_CopyString(s);
    case SG_STRING_SCAN_BEFORE2:;
    case SG_STRING_SCAN_AFTER2:;
    case SG_STRING_SCAN_BOTH:
      return Sg_Values2(nullstr, Sg_CopyString(s));
    }
  }
  if (size1 >= size2) {
    const SgChar *ssp = ss1;
    for (i = 0; i < size1 - size2; i++) {
      if (memcmp(ssp, ss2, size2) == 0) {
	switch (retmode) {
	case SG_STRING_SCAN_INDEX: return Sg_MakeInteger(i);
	case SG_STRING_SCAN_BEFORE: return Sg_Substring(s, 0, i);
	case SG_STRING_SCAN_AFTER:  return Sg_Substring(s, i + size2, -1);
	case SG_STRING_SCAN_BEFORE2:
	  return Sg_Values2(Sg_Substring(s, 0, i),
			    Sg_Substring(s, i, -1));
	case SG_STRING_SCAN_AFTER2:
	  return Sg_Values2(Sg_Substring(s, 0, i + size2),
			    Sg_Substring(s, i + size2, -1));
	case SG_STRING_SCAN_BOTH:
	  return Sg_Values2(Sg_Substring(s, 0, i),
			    Sg_Substring(s, i + size2, -1));
	}
      }
      ssp++;
    }
  }
  if (size1 < size2) goto failed;
  if (size1 < 256 || size2 >= 256) {
    for (i = 0; i <= size1 - size2; i++) {
      if (memcmp(ss2, ss1 + i, size2) == 0) break;
    }
    if (i == size1 - size2 + 1) goto failed;
  } else {
    i = boyer_moore(ss1, size1, ss2, size2);
    if (i < 0) goto failed;
  }

  switch (retmode) {
  case SG_STRING_SCAN_INDEX: return Sg_MakeInteger(i);
  case SG_STRING_SCAN_BEFORE: return Sg_Substring(s, 0, i);
  case SG_STRING_SCAN_AFTER:  return Sg_Substring(s, i + size2, -1);
  case SG_STRING_SCAN_BEFORE2:
    return Sg_Values2(Sg_Substring(s, 0, i),
		      Sg_Substring(s, i, -1));
  case SG_STRING_SCAN_AFTER2:
    return Sg_Values2(Sg_Substring(s, 0, i + size2),
		      Sg_Substring(s, i + size2, -1));
  case SG_STRING_SCAN_BOTH:
    return Sg_Values2(Sg_Substring(s, 0, i),
		      Sg_Substring(s, i + size2, -1));
  }
 failed:
  if (retmode <= SG_STRING_SCAN_AFTER) {
    return SG_FALSE;
  } else {
    return Sg_Values2(SG_FALSE, SG_FALSE);
  }
}

SgObject Sg_StringScan(SgString *s1, SgString *s2, int retmode)
{
  return string_scan(s1, SG_STRING_VALUE(s2), SG_STRING_SIZE(s2), retmode);
}

SgObject Sg_StringScanChar(SgString *s1, SgChar ch, int retmode)
{
  SgChar buf[2];
  buf[0] = ch;
  buf[1] = '\0';
  return string_scan(s1, buf, 1, retmode);
}

SgObject Sg_Substring(SgString *x, int start, int end)
{
  int len = x->size;
  SgString *ret;
  SG_CHECK_START_END(start, end, len);

  ret = make_string(end - start);
  memcpy(ret->value, x->value + start, (end - start) * sizeof(SgChar));
  ret->value[end-start] = 0;
  return ret;
}

void Sg_StringSet(SgString *s, int k, SgChar c)
{
  if (SG_LITERAL_STRINGP(s)) {
    Sg_Error(UC("attemped to modify a immutable string %S"), s);
  }
  SG_STRING_VALUE_AT(s, k) = c;
}

void Sg_StringFill(SgString *s, SgChar c, int start, int end)
{
  int size = s->size, i;
  SG_CHECK_START_END(start, end, size);
  for (i = start; i < end; i++) {
    SG_STRING_VALUE_AT(s, i) = c;
  }
}

SgObject Sg_MaybeSubstring(SgString *s, int start, int end)
{
  if (start == 0 && end < 0) return SG_OBJ(s);
  return Sg_Substring(s, start, end);
}


#define STRING_HASH(hv, chars, size)				\
  do {								\
    int i_ = (size);						\
    (hv) = 0;							\
    while (i_-- > 0) {						\
      (hv) = ((hv) << 5) - (hv) + ((unsigned char)*chars++);	\
    }								\
  } while (0)

#if 0
static uint32_t string_hash(const SgHashCore *ht, intptr_t key)
{
  SgChar *p = (SgChar*)key;
  int size = (int)ustrlen(p);
  uint32_t hashval;
  STRING_HASH(hashval, p, size);
  return hashval;
}

static int string_compare(const SgHashCore *ht, intptr_t key, intptr_t entryKey)
{
  if (!SG_PTRP(entryKey)) return FALSE;
  else {
    const uint32_t *s1, *s2;
    for (s1 = (const uint32_t *)key, s2 = (const uint32_t *)entryKey;
	 *s1 == *s2 && *s1 != 0;
	 s1++, s2++);
    return *s1 - *s2 == 0;
  }
}
#endif

void Sg__InitString()
{
  Sg_InitMutex(&smutex, FALSE);
  /* stable = Sg_MakeHashTable(string_hash, string_compare, 4096); */
  stable = Sg_MakeHashTableSimple(SG_HASH_STRING, 4096);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
