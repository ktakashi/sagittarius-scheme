/* string.c                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/private/string.h"
#include "sagittarius/private/collection.h"
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/unicode.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/number.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/thread.h"
#include "sagittarius/private/values.h"
#include "sagittarius/private/writer.h"

static void string_print(SgObject o, SgPort *port, SgWriteContext *ctx)
{
  SgString *obj = SG_STRING(o);
  SG_PORT_LOCK_WRITE(port);
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_PutsUnsafe(port, obj);
  } else {
    SgChar *s = obj->value;
    long i, size = obj->size;
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
	      /* Issue #256, ÿ should have printed as it is */
	      /* ch == 0xff || */
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
  SG_PORT_UNLOCK_WRITE(port);
}

SG_DEFINE_BUILTIN_CLASS(Sg_StringClass, string_print, NULL, NULL, NULL,
			SG_CLASS_SEQUENCE_CPL);

#define ALLOC_TEMP_STRING SG_ALLOC_TEMP_STRING

static SgString* make_string(long size)
{
  SgString *z = SG_NEW_ATOMIC2(SgString *, SG_STRING_ALLOC_SIZE(size));
  SG_SET_CLASS(z, SG_CLASS_STRING);
  z->size = size;
  z->immutablep = FALSE;
  return z;
}

#define COPY_STRING(ret, src, size, offset)				\
  do {									\
    long i;								\
    for (i = 0; i < (size); i++) {					\
      ((ret)->value)[i + (offset)] = (src[i]);				\
    }									\
  } while (0)

static SgInternalMutex smutex;

#include "gc-incl.inc"

#ifdef USE_WEAK_STRING
# include "sagittarius/weak.h"
# define Sg_HashTableRef Sg_WeakHashTableRef
# define Sg_HashTableSet Sg_WeakHashTableSet
static SgWeakHashTable *stable = NULL;
#else
static SgHashTable *stable = NULL;
#endif

static SgObject makestring(const SgChar *value, SgStringType flag, long length)
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

  if (flag == SG_LITERAL_STRING || flag == SG_IMMUTABLE_STRING) {
    z->immutablep = TRUE;
  }
  /* store it if it's literal */
  if (flag == SG_LITERAL_STRING) {
    Sg_LockMutex(&smutex);
    r = Sg_HashTableSet(stable, SG_OBJ(z), SG_OBJ(z),
			SG_HASH_NO_OVERWRITE);
    Sg_UnlockMutex(&smutex);
  } else {
    r = SG_OBJ(z);
  }
  return r;
}

SgObject Sg_MakeString(const SgChar *value, SgStringType flag, long length)
{
  if (length < 0) {
    long len = (long)ustrlen(value);
    return makestring(value, flag, len);
  } else {
    return makestring(value, flag, length);
  }
}

/* This method assumes given value as ASCII for now */
SgObject Sg_MakeStringC(const char *value)
{
  SgString *z;
  z = make_string((long)strlen(value));
  COPY_STRING(z, value, z->size, 0);
  z->value[z->size] = 0;
  return SG_OBJ(z);
}

SgObject Sg_ReserveString(long size, SgChar fill)
{
  SgString *z = make_string(size);
  long i;
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

int Sg_LiteralStringP(SgString *s)
{
  /* TODO should we lock the table? */
  SgObject r;
  Sg_LockMutex(&smutex);
  r = Sg_HashTableRef(stable, SG_OBJ(s), SG_FALSE);
  Sg_UnlockMutex(&smutex);
  return SG_EQ(s, r);
}
/* converts given string to immutable string if it's not */
SgObject Sg_StringToIString(SgString *s, long start, long end)
{
  SgObject r;
  long size = SG_STRING_SIZE(s);
  SG_CHECK_START_END(start, end, size);
  
  if (start == 0 && end == size && SG_IMMUTABLE_STRINGP(s)) return s;
  
  r = Sg_Substring(s, start, end);
  SG_STRING(r)->immutablep = TRUE;
  return r;
}
/* mostly for cache */
SgObject Sg_StringIntern(SgString *s)
{
  SgObject r;
  Sg_LockMutex(&smutex);
  r = Sg_HashTableRef(stable, SG_OBJ(s), SG_FALSE);
  if (SG_FALSEP(r)) {
    SG_STRING(r)->immutablep = TRUE;
    r = Sg_HashTableSet(stable, SG_OBJ(r), SG_OBJ(r),
			SG_HASH_NO_OVERWRITE);
  }
  Sg_UnlockMutex(&smutex);
  return r; 
}


static int string_equal(SgChar *s1, long size1, SgChar *s2, long size2)
{
  if (size1 != size2) return FALSE;
  else {
    long i;
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

static inline int string_compare_rec(SgString *s1, SgString *s2, long len)
{
  long i;
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
  long s1_len = SG_STRING_SIZE(s1);
  long s2_len = SG_STRING_SIZE(s2);
  long len = (s1_len > s2_len) ? s2_len : s1_len;
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

SgObject Sg_StringAppendC(SgString *a, const SgChar *s, long sizey)
{
  long sizex = a->size;
  SgString *p = make_string(sizex + sizey);
  /* manual copy */
  COPY_STRING(p, a->value, sizex, 0);
  COPY_STRING(p, s, sizey, sizex);
  p->value[sizex + sizey] = '\0';

  return SG_OBJ(p);
}

SgObject Sg_StringAppend(SgObject args)
{
  long len = 0, off = 0;
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

SgObject Sg_StringToList(SgString *s, long start, long end)
{
  long size = SG_STRING_SIZE(s), i;
  const SgChar *buf = SG_STRING_VALUE(s);
  SgObject h = SG_NIL, t = SG_NIL;
  SG_CHECK_START_END(start, end, size);
  for (i = start; i < end; i++) {
    SG_APPEND1(h, t, SG_MAKE_CHAR(buf[i]));
  }
  return h;
}

SgObject Sg_ListToString(SgObject chars, long start, long end)
{
  SgObject cp, r;
  long len = 0, i;
  SgChar *buf;

  if (start < 0 || (end >= 0 && start > end)) {
    Sg_Error(UC("argument out of range (start %d, end %d)"), start, end);
  }

  i = start;
  chars = Sg_ListTail(chars, start, SG_UNBOUND);
  SG_FOR_EACH(cp, chars) {
    if (end >= 0 && i == end) break;
    if (!SG_CHARP(SG_CAR(cp))) {
      Sg_Error(UC("character required, but got %S"), SG_CAR(cp));
    }
    len++;
    i++;
  }
  if (len < (end - start)) {
    Sg_Error(UC("list is too short %S"), chars);
  }

  r = make_string(len);
  buf = SG_STRING_VALUE(r);
  i = start;
  SG_FOR_EACH(cp, chars) {
    if (end >= 0 && i == end) break;
    *buf++ = SG_CHAR_VALUE(SG_CAR(cp));
    i++;
  }
  *buf = 0;
  return r;
}

SgObject Sg_CopyString(SgString *a)
{
  /* TODO consider if src string was literal */
  SgString *s = make_string(a->size);
  COPY_STRING(s, a->value, a->size, 0);
  s->value[s->size] = '\0';
  return SG_OBJ(s);
}

static inline long boyer_moore(const SgChar *ss1, long siz1,
                              const SgChar *ss2, long siz2)
{
  long shift[256];
  long i, j, k;
  for (i = 0; i < 256; i++) { shift[i] = siz2; }
  for (j = 0; j < siz2-1; j++) {
    shift[(uint32_t)ss2[j]] = siz2-j-1;
  }
  for (i = siz2 - 1; i < siz1; i += shift[ss1[i]]) {
    for (j = siz2 - 1, k = i; j >= 0 && ss1[k] == ss2[j]; j--, k--)
      ;
    if (j == -1) return k+1;
  }
  return -1;
}

static SgObject string_scan(SgString *s, const SgChar *ss2,
			    long size2, int retmode)
{
  long i;
  const SgChar *ss1 = SG_STRING_VALUE(s);
  long size1 = SG_STRING_SIZE(s);
  const SgObject nullstr = SG_MAKE_STRING("");

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
      if (memcmp(ssp, ss2, size2 * sizeof(SgChar)) == 0) {
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
      if (memcmp(ss2, ss1 + i, size2 * sizeof(SgChar)) == 0) break;
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

SgObject Sg_StringSplitChar(SgString *s1, SgChar ch)
{
  /* we can't use values since this might be used before initialisation */
  SgObject pos = Sg_StringScanChar(s1, ch, SG_STRING_SCAN_INDEX);
  SgObject h = SG_NIL, t = SG_NIL, s = s1;

  while (!SG_FALSEP(pos)) {
    long p = SG_INT_VALUE(pos);
    SG_APPEND1(h, t, Sg_Substring(s, 0, p));
    s = Sg_Substring(s, p+1, SG_STRING_SIZE(s));
    pos = Sg_StringScanChar(s, ch, SG_STRING_SCAN_INDEX);
  }
  SG_APPEND1(h, t, s);
  return h;
}

SgObject Sg_Substring(SgString *x, long start, long end)
{
  long len = x->size;
  SgString *ret;
  SG_CHECK_START_END(start, end, len);

  ret = make_string(end - start);
  memcpy(ret->value, x->value + start, (end - start) * sizeof(SgChar));
  ret->value[end-start] = 0;
  return ret;
}

void Sg_StringFill(SgString *s, SgChar c, long start, long end)
{
  long size = s->size, i;
  SG_CHECK_START_END(start, end, size);
  for (i = start; i < end; i++) {
    SG_STRING_VALUE_AT(s, i) = c;
  }
}

SgObject Sg_MaybeSubstring(SgString *s, long start, long end)
{
  if (start == 0 && end < 0) return SG_OBJ(s);
  return Sg_Substring(s, start, end);
}


SgObject Sg_AsciiToString(const char *s, size_t len)
{
  SgObject ss = Sg_ReserveString(len, 0);
  size_t i;
  for (i = 0; i < len; i++) {
    SG_STRING_VALUE_AT(ss, i) = s[i];
  }
  return ss;
}

SgObject Sg_Utf8ToString(const char *s, size_t len)
{
  /* just forward */
  return Sg_Utf8sToUtf32s(s, len);
}

int Sg_IsString(SgObject obj)
{
  return SG_STRINGP(obj);
}
long Sg_StringLength(SgObject s)
{
  return SG_STRING_SIZE(s);
}
SgChar Sg_StringRef(SgObject s, long k)
{
  if (k > SG_STRING_SIZE(s) || k < 0) {
    Sg_AssertionViolation(SG_INTERN("string-ref"),
			  SG_MAKE_STRING("index out of bounds"),
			  SG_LIST2(s, SG_MAKE_INT(k)));
  }
  return SG_STRING_VALUE_AT(s, k);
}

void Sg_StringSet(SgObject s, long k, SgChar c)
{
  if (k < 0) {
    Sg_WrongTypeOfArgumentViolation(
      SG_INTERN("string-set!"),
      SG_MAKE_STRING("non negative exact integer"),
      SG_MAKE_INT(k),
      SG_LIST3(s, SG_MAKE_INT(k), SG_MAKE_CHAR(c)));
  }
  if (k > SG_STRING_SIZE(s)) {
    Sg_AssertionViolation(SG_INTERN("string-set!"),
			  SG_MAKE_STRING("index out of bounds"),
			  SG_LIST2(s, SG_MAKE_INT(k)));
  }
  if (SG_IMMUTABLE_STRINGP(s)) {
    Sg_AssertionViolation(
      SG_INTERN("string-set!"),
      SG_MAKE_STRING("attempted to modify an immutable string"),
      s);
  }
  SG_STRING_VALUE_AT(s, k) = c;
}


#ifdef USE_WEAK_STRING
DEFINE_DEBUG_DUMPER(string, stable)
#endif

void Sg__InitString()
{
  Sg_InitMutex(&smutex, FALSE);
#ifdef USE_WEAK_STRING
  /*  keys are refered by its values anyway */
  stable = Sg_MakeWeakHashTableSimple(SG_HASH_STRING, SG_WEAK_REMOVE_VALUE, 
				      4096, SG_FALSE);
#else
  stable = Sg_MakeHashTableSimple(SG_HASH_STRING, 4096);
#endif

#ifdef USE_WEAK_STRING
  ADD_DEBUG_DUMPER(string);
#endif
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
