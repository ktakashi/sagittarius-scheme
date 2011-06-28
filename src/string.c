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
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/string.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/unicode.h"
#include "sagittarius/pair.h"
#include "sagittarius/error.h"
#include "sagittarius/thread.h"

static SgString* make_string(int size)
{
  SgString *z = SG_NEW(SgString);
  SG_SET_HEADER(z, TC_STRING);
  z->size = size;
  z->value = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * (size + 1));
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
static SgHashTable STABLE = { MAKE_HDR_VALUE(TC_HASHTABLE), SG_HASH_GENERAL, { NULL } };
#define stable (&STABLE)

SgObject Sg_MakeString(const SgChar *value, SgStringType flag)
{
  SgObject r;
  SgString *z;
  if (flag == SG_LITERAL_STRING) {
    Sg_LockMutex(&smutex);
    r = Sg_HashTableRef(stable, SG_OBJ(value), SG_FALSE);
    Sg_UnlockMutex(&smutex);
    if (!SG_FALSEP(r)) {
      ASSERT(SG_STRINGP(r));
      return r;
    }
  }

  z = make_string(ustrlen(value));
  COPY_STRING(z, value, z->size, 0);
  z->value[z->size] = 0;

  /* store it if it's literal */
  if (flag == SG_LITERAL_STRING) {
    Sg_LockMutex(&smutex);
    SG_SET_HEADER_ATTRIBUTE(z, SG_MAKEBITS(1, STRING_LITERAL_SHIFT));
    r = Sg_HashTableSet(stable, SG_OBJ(value), SG_OBJ(z), SG_HASH_NO_OVERWRITE);
    Sg_UnlockMutex(&smutex);
  } else {
    r = SG_OBJ(z);
  }
  return r;
}

/* This method assumes given value as ASCII for now */
SgObject Sg_MakeStringC(const char *value)
{
  SgString *z;
  z = make_string(strlen(value));
  COPY_STRING(z, value, z->size, 0);
  z->value[z->size] = 0;
  return SG_OBJ(z);
}

SgObject Sg_ReserveString(size_t size, SgChar fill)
{
  SgString *z = make_string(size);
  size_t i;
  for (i = 0; i < size; i++) {
    z->value[i] = fill;
  }
  
  return SG_OBJ(z);
}

SgObject Sg_MakeEmptyString()
{
  SgString *z = SG_NEW(SgString);
  SG_SET_HEADER(z, TC_STRING);
  z->size = 0;
  z->value = NULL;
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
  r = make_string(len);
  /* append */
  SG_FOR_EACH(cp, args) {
    COPY_STRING(r, SG_STRING(SG_CAR(cp))->value, SG_STRING(SG_CAR(cp))->size, off);
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
  return Sg_MakeString(buf, SG_HEAP_STRING);
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


static uint32_t string_hash(const SgHashCore *ht, intptr_t key)
{
  SgChar *p = (SgChar*)key;
  int size = ustrlen(p);
  uint32_t hashval;
  STRING_HASH(hashval, p, size);
  return hashval;
}

static int string_compare(const SgHashCore *ht, intptr_t key, intptr_t entryKey)
{
  if (!SG_PTRP(entryKey)) return FALSE;
  else {
    SgChar *s1 = (SgChar*)key, *s2 = (SgChar*)entryKey;
    int size1 = ustrlen(s1), size2 = ustrlen(s2);
    return string_equal(s1, size1, s2, size2);
  }
}

void Sg__InitString()
{
  Sg_InitMutex(&smutex, FALSE);
  /* stable = Sg_MakeHashTable(string_hash, string_compare, 4096); */
  Sg_HashCoreInitGeneral(&stable->core, string_hash, string_compare, 4096, NULL);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
