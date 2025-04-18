/* hashtable.c                                     -*- mode:c; coding:utf-8; -*-
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/bytevector.h"
#include "sagittarius/private/collection.h"
#include "sagittarius/private/compare.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/number.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/keyword.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/vm.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/cache.h"
#include "sagittarius/private/generic.h"


typedef struct EntryRec
{
  intptr_t key;
  intptr_t value;
  struct EntryRec *next;
  SgHashVal hashValue;
} Entry;

#define BUCKETS(hc) ((Entry**)hc->buckets)

#define DEFAULT_BUCKET_COUNT 4
#define MAX_AVG_CHAIN_LIMIS  3
#define EXTEND_BITS          2
/* hash value must be 32 bit */
#define HASHMASK 0xffffffffUL

typedef Entry* SearchProc(SgHashCore *core, intptr_t key, 
			  SgDictOp op, int flags);

static unsigned long round2up(unsigned long val);
void hash_iter_init(SgHashCore *core, SgHashIter *itr);

/* hash functions */
#define STRING_HASH(hv, chars, size)				\
  do {								\
    long i_ = (size);						\
    (hv) = 0;							\
    while (i_-- > 0) {						\
      (hv) = ((hv) << 5) - (hv) + ((unsigned char)*chars++);	\
    }								\
  } while (0)

#define SMALL_INT_HASH(result, value)		\
  (result) = ((value)*2654435761UL)
#define ADDRESS_HASH(result, val)				\
  (result) = (SgHashVal)((SG_WORD(val) >> 3)*2654435761UL)

#define HASH2INDEX(tabsize, bits, hashval)			\
  (((hashval)+((hashval)>>(32-(bits)))) & ((tabsize) - 1))

#define COMBINE(hv1, hv2)  ((hv1)*5+(hv2))

SgHashVal Sg_EqHash(SgObject obj, SgHashVal bound)
{
  SgHashVal hashval;
  ADDRESS_HASH(hashval, obj);
  if (bound) return (hashval & HASHMASK) % bound;
  return hashval & HASHMASK;
}

SgHashVal Sg_EqvHash(SgObject obj, SgHashVal bound)
{
  SgHashVal hashval;
  if (SG_NUMBERP(obj)) {
    if (SG_INTP(obj)) {
      SMALL_INT_HASH(hashval, SG_INT_VALUE(obj));
    } else if (SG_BIGNUMP(obj)) {
      long i;
      unsigned long u = 0;
      long size = SG_BIGNUM_GET_COUNT(obj);
      for (i = 0; i < size; i++) {
	u += SG_BIGNUM(obj)->elements[i];
      }
      SMALL_INT_HASH(hashval, u);
    } else if (SG_FLONUMP(obj)) {
      hashval = (SgHashVal)(SG_FLONUM_VALUE(obj) * 2654435761UL);
    } else if (SG_RATIONALP(obj)) {
      SgHashVal h1 = Sg_EqvHash(SG_RATIONAL(obj)->numerator, bound);
      SgHashVal h2 = Sg_EqvHash(SG_RATIONAL(obj)->denominator, bound);
      hashval = COMBINE(h1, h2);
    } else {
      SgHashVal h1 = Sg_EqvHash(SG_COMPLEX(obj)->real, bound);
      SgHashVal h2 = Sg_EqvHash(SG_COMPLEX(obj)->imag, bound);
      hashval = COMBINE(h1, h2);
    }
  } else {
    ADDRESS_HASH(hashval, obj);
  }
  if (bound) return (hashval & HASHMASK) % bound;
  return hashval & HASHMASK;
}

static SgHashVal pair_hash(SgObject o, int level);
static SgHashVal vector_hash(SgObject o, int level);
static SgHashVal equal_hash_rec(SgObject obj, int level);

static SgHashVal level_hash(SgObject o, int level)
{
  if (SG_PAIRP(o)) return pair_hash(o, level);
  else if (SG_VECTORP(o)) return vector_hash(o, level);
  else return equal_hash_rec(o, level);
}

static SgHashVal pair_hash(SgObject o, int level)
{
  if (level == 0) return 0x08d;
  else return COMBINE(level_hash(SG_CAR(o), level-1),
		      level_hash(SG_CDR(o), level-1));
}

static SgObject compact_vector(SgObject vec, long len, long short_len)
{
  long selections = short_len - 5;
  long interval = (len-5)/(short_len-5);
  long fsp = 3 + interval/2, i, index;
  SgObject r = Sg_MakeVector(short_len, SG_FALSE);
  /* set 5 elements first */
  SG_VECTOR_ELEMENT(r,	0) = SG_VECTOR_ELEMENT(vec, 0);
  SG_VECTOR_ELEMENT(r,	1) = SG_VECTOR_ELEMENT(vec, 1);
  SG_VECTOR_ELEMENT(r,	2) = SG_VECTOR_ELEMENT(vec, 2);
  for (i = 3, index = fsp; i < selections+3; i++, index+=interval) {
    SG_VECTOR_ELEMENT(r, i) = SG_VECTOR_ELEMENT(vec, index);
  }
  SG_VECTOR_ELEMENT(r, i++) = SG_VECTOR_ELEMENT(vec, (len-2));
  SG_VECTOR_ELEMENT(r, i)   = SG_VECTOR_ELEMENT(vec, (len-1));
  return r;
}

static SgHashVal smoosh_vector(SgObject vec, long len, int level)
{
  long remain;
  SgHashVal result = 0xd80f;
  for (remain = len; remain > 0; remain--) {
    result = COMBINE(result, level_hash(SG_VECTOR_ELEMENT(vec, remain-1),
					level-1));
  }
  return result;
}
static SgHashVal vector_hash(SgObject obj, int level)
{
  if (level == 0) return 0xd80e;
  else {
    long breakn = 13, len = SG_VECTOR_SIZE(obj);
    SgHashVal hashval, h;
    SMALL_INT_HASH(hashval, len);
    if (len <= breakn) {
      h = smoosh_vector(obj, len, level);
    } else {
      obj = compact_vector(obj, len, breakn);
      h = smoosh_vector(obj, breakn, level);
    }
    return COMBINE(hashval, h);
  }
}

static SgHashVal object_hash(SgObject obj, int level)
{
  SgObject l = SG_MAKE_INT(level);
  SgObject r = Sg_Apply2(SG_OBJ(&Sg_GenericObjectHash), obj, l);
  SgHashVal hashval;  
  if (SG_EXACT_INTP(r)) {
    int oor;
    hashval = (SgHashVal)Sg_GetUIntegerClamp(r, SG_CLAMP_NONE, &oor);
    if (!oor) {
      return hashval;
    }
  }
  /* don't want to raise an error with hashing unless user raises it
     in Scheme world. */
  ADDRESS_HASH(hashval, obj);
  return hashval;
}

#define MAX_NESTING_LEVEL 4
static SgHashVal equal_hash_rec(SgObject obj, int level)
{
  SgHashVal hashval;
  if (!SG_PTRP(obj)) {
    SMALL_INT_HASH(hashval, (SgHashVal)SG_WORD(obj));
    return hashval;
  } else if (SG_NUMBERP(obj)) {
    return Sg_EqvHash(obj, 0);
  } else if (SG_STRINGP(obj)) {
    return Sg_StringHash(SG_STRING(obj), 0);
  } else if (SG_PAIRP(obj)) {
    return pair_hash(obj, MAX_NESTING_LEVEL); 
  } else if (SG_VECTORP(obj)) {
    return vector_hash(obj, MAX_NESTING_LEVEL); 
  } else if (SG_SYMBOLP(obj)) {
    return Sg_StringHash(SG_SYMBOL(obj)->name, 0);
  } else if (SG_KEYWORDP(obj)) {
    return Sg_StringHash(SG_KEYWORD_NAME(obj), 0);
  } else if (SG_BVECTORP(obj)) {
    /* TODO is this ok? */
    SgHashVal h = 0, h2;
    long i, size = SG_BVECTOR_SIZE(obj);
    for (i = 0; i < size; i++) {
      SMALL_INT_HASH(h2, SG_BVECTOR_ELEMENT(obj, i));
      h = COMBINE(h, h2);
    }
    return h;
  } else {
    return object_hash(obj, level);
  }
}

SgHashVal Sg_EqualHash(SgObject obj, SgHashVal bound)
{
  SgHashVal hash = equal_hash_rec(obj, MAX_NESTING_LEVEL);
  if (bound) return hash % bound;
  return hash;
}

SgHashVal Sg_StringHash(SgString *str, SgHashVal bound)
{
  SgHashVal hashval;
  const SgChar *p = str->value;
  STRING_HASH(hashval, p, str->size);
  if (bound == 0) return hashval;
  else return (hashval % bound);
}

/* accessor and so */
static Entry *insert_entry(SgHashCore *table,
			   intptr_t key,
			   SgHashVal hashval,
			   long index)
{
  Entry *e = SG_NEW(Entry);
  Entry **buckets = BUCKETS(table);
  e->key = key;
  if (table->create_entry) {
    table->create_entry(table, (SgHashEntry *)e);
  }
  e->value = 0;
  e->next = buckets[index];
  e->hashValue = hashval;
  buckets[index] = e;
  table->entryCount++;

  if (table->entryCount > table->bucketCount * MAX_AVG_CHAIN_LIMIS) {
    /* too many chains */
    /* extend the table */
    Entry **newb, *f;
    SgHashIter itr;
    long i, newsize = (table->bucketCount << EXTEND_BITS);
    long newbits = table->bucketsLog2Count + EXTEND_BITS;

    newb = SG_NEW_ARRAY(Entry*, newsize);
    for (i = 0; i < newsize; i++) newb[i] = NULL; /* initialize new buckets */

    hash_iter_init(table, &itr);
    while ((f = (Entry*)Sg_HashIterNext(&itr, NULL, NULL)) != NULL) {
      index = HASH2INDEX(newsize, newbits, f->hashValue);
      f->next = newb[index];
      newb[index] = f;
    }
    /* gc friendliness */
    for (i = 0; i < table->bucketCount; i++) table->buckets[i] = NULL;

    table->bucketCount = newsize;
    table->bucketsLog2Count = newbits;
    table->buckets = (void**)newb;
  }
  return e;
}

static Entry *delete_entry(SgHashCore *table,
			   Entry *entry, Entry *prev,
			   long index)
{
  if (prev) prev->next = entry->next;
  else table->buckets[index] = (void*)entry->next;
  table->entryCount--;
  ASSERT(table->entryCount >= 0);
  entry->next = NULL; /* GC friendliness */
  return entry;
}

#define FOUND(table, op, e, p, index)			\
  do {							\
    switch (op) {					\
    case SG_DICT_GET:;					\
    case SG_DICT_CREATE:;				\
      return e;						\
    case SG_DICT_DELETE:;				\
      return delete_entry(table, e, p, index);		\
    }							\
  } while(0)

#define NOTFOUND(table, op, key, hashval, index)	\
  do {							\
    if (op == SG_DICT_CREATE) {				\
      return insert_entry(table, key, hashval, index);	\
    } else {						\
      return NULL;					\
    }							\
  } while (0)



/* core initialize */
static void hash_core_init(SgHashCore *table,
			   SearchProc *access,
			   SgHashProc *hasher,
			   SgHashCompareProc *compare,
			   unsigned long initSize,
			   void* data)
{
  Entry **b;
  unsigned long i;

  if (initSize != 0) initSize = round2up(initSize);
  else initSize = DEFAULT_BUCKET_COUNT;

  b = SG_NEW_ARRAY(Entry*, initSize);
  table->buckets = (void**)b;
  table->bucketCount = initSize;
  table->entryCount = 0;
  table->access = access;
  table->hasher = hasher;
  table->compare = compare;
  table->data = data;
  table->generalHasher = SG_UNDEF;
  table->generalCompare = SG_UNDEF;
  for (i = initSize, table->bucketsLog2Count = 0; i > 1; i /= 2) {
    table->bucketsLog2Count++;
  }
  for (i = 0; i < initSize; i++) table->buckets[i] = NULL;
  table->create_entry = NULL;	/* default */
}

/** accessor function */
/* eq? */
static Entry *address_access(SgHashCore *table,
			     intptr_t key,
			     SgDictOp op,
			     int flags)
{
  SgHashVal hashval;
  unsigned long index;
  Entry *e, *p, **buckets = BUCKETS(table);

  ADDRESS_HASH(hashval, key);
  index = HASH2INDEX(table->bucketCount, table->bucketsLog2Count, hashval);

  for (e = buckets[index], p = NULL; e; p = e, e = e->next) {
    if (e->key == key) FOUND(table, op, e, p, index);
  }
  NOTFOUND(table, op, key, hashval, index);
}

static SgHashVal address_hash(const SgHashCore *ht, intptr_t obj)
{
  SgHashVal hashval;
  ADDRESS_HASH(hashval, obj);
  return hashval;
}

static int address_compare(const SgHashCore *ht, intptr_t key, intptr_t k2)
{
  return (key == k2);
}

/* eqv? and equal? */
static SgHashVal eqv_hash(const SgHashCore *table, intptr_t key)
{
  return Sg_EqvHash(SG_OBJ(key), 0);
}

static int eqv_compare(const SgHashCore *table, intptr_t key, intptr_t k2)
{
  return Sg_EqvP(SG_OBJ(key), SG_OBJ(k2));
}

static SgHashVal equal_hash(const SgHashCore *table, intptr_t key)
{
  return Sg_EqualHash(SG_OBJ(key), 0);
}

static int equal_compare(const SgHashCore *table, intptr_t key, intptr_t k2)
{
  return Sg_EqualP(SG_OBJ(key), SG_OBJ(k2));
}

/* string */
static Entry *string_access(SgHashCore *table,
			    intptr_t k,
			    SgDictOp op,
			    int flags)
{
  SgHashVal hashval;
  unsigned long index;
  long size;
  const SgChar *s;
  SgObject key = SG_OBJ(k);
  Entry *e, *p, **buckets;

  if (!SG_STRINGP(key)) {
    if (flags & SG_HASH_NO_ERROR) return NULL;
    Sg_Error(UC("Got non-string key %S to the string hashtable."), key);
  }
  
  s = SG_STRING(key)->value;
  size = SG_STRING(key)->size;
  STRING_HASH(hashval, s, size);
  index = HASH2INDEX(table->bucketCount, table->bucketsLog2Count, hashval);
  buckets = BUCKETS(table);

  for (e = buckets[index], p = NULL; e; p = e, e = e->next) {
    SgObject ee = SG_OBJ(e->key);
    if (Sg_StringEqual(SG_STRING(key), SG_STRING(ee))) {
      FOUND(table, op, e, p, index);
    }
  }
  NOTFOUND(table, op, k, hashval, index);
}

static SgHashVal string_hash(const SgHashCore *table, intptr_t key)
{
  return Sg_StringHash(SG_STRING(key), 0);
}

static int string_compare(const SgHashCore *table, intptr_t key, intptr_t k2)
{
  return Sg_StringEqual(SG_STRING(key), SG_STRING(k2));
}
/* general access */
static Entry* general_access(SgHashCore *table,
			     intptr_t key,
			     SgDictOp op,
			     int flags)
{
  SgHashVal hashval;
  unsigned long index;
  Entry *e, *p, **buckets;

  hashval = table->hasher(table, key);
  index = HASH2INDEX(table->bucketCount, table->bucketsLog2Count, hashval);
  buckets = BUCKETS(table);

  for (e = buckets[index], p = NULL; e; p = e, e = e->next) {
    if (table->compare(table, key, e->key)) FOUND(table, op, e, p, index);
  }
  NOTFOUND(table, op, key, hashval, index);
}

static SgHashVal general_hash(const SgHashCore *table, intptr_t key)
{
  SgObject hash, hasher = table->generalHasher;
  
  if (SG_SUBRP(hasher)) {
    SG_CALL_SUBR1(hash, hasher, SG_OBJ(key));
  } else {
    hash = Sg_Apply1(table->generalHasher, SG_OBJ(key));
  }
  if (!SG_EXACT_INTP(hash)) {
    Sg_Error(UC("%S is not an exact integer"), hash);
  }
  /* well the value must be either fixnum or bignum.
     To avoid overflow we use eqv-hash.
   */
  return Sg_EqvHash(hash, 0);
}

static int general_compare(const SgHashCore *table, intptr_t key, intptr_t k2)
{
  SgObject ret, compare = table->generalCompare;
  if (SG_SUBRP(compare)) {
    SG_CALL_SUBR2(ret, compare, SG_OBJ(key), SG_OBJ(k2));
  } else {
    ret = Sg_Apply2(table->generalCompare, SG_OBJ(key), SG_OBJ(k2));
  }
  return !SG_FALSEP(ret);
}


static int hash_core_predef_procs(SgHashType type,
				  SearchProc **access,
				  SgHashProc **hasher,
				  SgHashCompareProc **compare)
{
  switch (type) {
  case SG_HASH_EQ:
    *access = address_access;
    *hasher = address_hash;
    *compare = address_compare;
    return TRUE;
  case SG_HASH_EQV:
    *access = general_access;
    *hasher = eqv_hash;
    *compare = eqv_compare;
    return TRUE;
  case SG_HASH_EQUAL:
    *access = general_access;
    *hasher = equal_hash;
    *compare = equal_compare;
    return TRUE;
  case SG_HASH_STRING:
    *access = string_access;
    *hasher = string_hash;
    *compare = string_compare;
    return TRUE;
  case SG_HASH_GENERAL:
    *access = general_access;
    *hasher = general_hash;
    *compare = general_compare;
    return TRUE;
  default:
    return FALSE;
  }
}

void Sg_HashCoreInitSimple(SgHashCore *core,
			   SgHashType type,
			   long initSize,
			   void *data)
{
  SearchProc *access = NULL;
  SgHashProc *hasher = NULL;
  SgHashCompareProc *compare = NULL;
  if (hash_core_predef_procs(type, &access, &hasher, &compare) == FALSE) {
    Sg_Error(UC("wrong TYPE argument passed to Sg_HashCoreInitSimple: %d"),
	     type);
  }
  hash_core_init(core, access, hasher, compare, initSize, data);
}

void Sg_HashCoreInitGeneral(SgHashCore *core,
			    SgHashProc *hasher,
			    SgHashCompareProc *compare,
			    long initSize,
			    void *data)
{
  hash_core_init(core, general_access, hasher, compare, initSize, data);
}

int Sg_HashCoreTypeToProcs(SgHashType type, SgHashProc **hasher,
			   SgHashCompareProc **compare)
{
  SearchProc *access;
  return hash_core_predef_procs(type, &access, hasher, compare);
}

SgHashEntry* Sg_HashCoreSearch(SgHashCore *table, intptr_t key,
			       SgDictOp op, int flags)
{
  SearchProc *p = (SearchProc*)table->access;
  return (SgHashEntry*)p(table, key, op, flags);
}

void Sg_HashCoreCopy(SgHashTable *dstT, const SgHashTable *srcT)
{
  SgHashCore *dst = SG_HASHTABLE_CORE(dstT);
  const SgHashCore *src = SG_HASHTABLE_CORE(srcT);

  Entry **b = SG_NEW_ARRAY(Entry*, src->bucketCount);
  int i;
  Entry *e, *p, *s;

  for (i = 0; i < src->bucketCount; i++) {
    p = NULL;
    s = (Entry*)src->buckets[i];
    b[i] = NULL;
    while (s) {
      e = SG_NEW(Entry);
      e->key = s->key;
      SG_HASHTABLE_ENTRY_SET(dstT, (SgHashEntry *)e, SG_OBJ(s->value), 
			     SG_DICT_ON_COPY);
      /* e->value = s->value; */
      e->next = NULL;
      if (p) p->next = e;
      else b[i] = e;
      p = e;
      s = s->next;
    }
  }
  dst->bucketCount = dst->entryCount = 0;

  dst->buckets = (void**)b;
  dst->hasher = src->hasher;
  dst->compare = src->compare;
  dst->access = src->access;
  dst->generalHasher  =	src->generalHasher;
  dst->generalCompare =	src->generalCompare;
  dst->data = src->data;
  dst->bucketCount = src->bucketCount;
  dst->bucketsLog2Count = src->bucketsLog2Count;
  dst->entryCount = src->entryCount;
}

void Sg_HashCoreClear(SgHashCore *ht, long k)
{
  long i;
  for (i = 0; i < ht->bucketCount; i++) {
    ht->buckets[i] = NULL;
  }
  ht->entryCount = 0;
  /* TODO: do we need this? */
  if (k > 0) {
    Entry **b;
    ht->buckets = NULL;	/* gc friendliness */
    if (k == 0) k = DEFAULT_BUCKET_COUNT;
    else k = round2up(k);
    b = SG_NEW_ARRAY(Entry*, k);
    ht->buckets = (void**)b;
    ht->bucketCount = k;
    for (i = k, ht->bucketsLog2Count = 0; i > 1; i /= 2) {
      ht->bucketsLog2Count++;
    }
    for (i = 0; i < k; i++) ht->buckets[i] = NULL;
  }
}

SgHashEntry * hash_iter_next(SgHashIter *itr, SgObject *key, SgObject *value);
void hash_iter_init(SgHashCore *core, SgHashIter *itr)
{
  int i;
  itr->core = core;
  itr->iter_next = hash_iter_next;
  for (i = 0; i < core->bucketCount; i++) {
    if (core->buckets[i]) {
      itr->bucket = i;
      itr->next = core->buckets[i];
      return;
    }
  }
  itr->next = NULL;
}

void Sg_HashIterInit(SgObject table, SgHashIter *itr)
{
  SG_HASHTABLE_OPTABLE(table)->init_iter(table, itr);
}

SgHashEntry* Sg_HashIterNext(SgHashIter *itr, SgObject *key, SgObject *value)
{
  return itr->iter_next(itr, key, value);
}

static void hash_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgHashTable *ht = SG_HASHTABLE(obj);
  SG_PORT_LOCK_WRITE(port);
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
    Sg_Write(SG_HASHTABLE_CORE(ht)->generalHasher, port, ctx->mode);
    Sg_PutcUnsafe(port, ' ');
    Sg_Write(SG_HASHTABLE_CORE(ht)->generalCompare, port, ctx->mode);
    break;
  }
  /* Sg_PutcUnsafe(port, '>'); */
  Sg_Printf(port, UC(" %p>"), obj);
  SG_PORT_UNLOCK_WRITE(port);
}

/*
  caching hashtable is best effort. so if we can't it'll give up.
  the case we can't cache it:
     + General hashtable with subr
     + Entry contains something we can't cache

  cache structure
    + type (byte)
    + immutable? (byte)
    + size (int) enough?
      + if type == general
        + hasher
	+ compare
    + key value, so on
*/
static SgObject hash_cache_reader(SgPort *port, SgReadCacheCtx *ctx)
{
  SgHashTable *ht;
  int type, immutablep;
  long entryCount, i;
  SgObject count;
  type = Sg_GetbUnsafe(port);
  immutablep = Sg_GetbUnsafe(port);
  count = Sg_ReadCacheObject(port, ctx);
  ASSERT(SG_INTP(count));
  entryCount = SG_INT_VALUE(count);
  switch (type) {
  case SG_HASH_GENERAL: {
    SgObject hasher = Sg_ReadCacheObject(port, ctx);
    SgObject compare = Sg_ReadCacheObject(port, ctx);
    ht = SG_HASHTABLE(Sg_MakeHashTable(hasher, compare, entryCount));
    break;
  }
  default:
    ht = SG_HASHTABLE(Sg_MakeHashTableSimple(type, entryCount));
    break;
  }
  for (i = 0; i < entryCount; i++) {
    SgObject key = Sg_ReadCacheObject(port, ctx);
    SgObject value = Sg_ReadCacheObject(port, ctx);
    Sg_HashTableSet(ht, key, value, 0);
  }
  ht->immutablep = immutablep;
  return SG_OBJ(ht);
}

static SgObject hash_cache_scanner(SgObject obj, SgObject cbs,
				   SgWriteCacheCtx *ctx)
{
  SgHashTable *ht = SG_HASHTABLE(obj);
  SgHashIter iter;
  SgHashEntry *e;
  switch (ht->type) {
  default: break;
    /* we are only interested in general type */
  case SG_HASH_GENERAL:
    /* if one of these 2 is subr, then the cache will be failed. */
    cbs = Sg_WriteCacheScanRec(SG_HASHTABLE_CORE(ht)->generalHasher, cbs, ctx);
    cbs = Sg_WriteCacheScanRec(SG_HASHTABLE_CORE(ht)->generalCompare, cbs, ctx);
    break;
  }
  Sg_HashIterInit(ht, &iter);
  while ((e = Sg_HashIterNext(&iter, NULL, NULL)) != NULL) {
    cbs = Sg_WriteCacheScanRec(SG_HASH_ENTRY_KEY(e), cbs, ctx);
    cbs = Sg_WriteCacheScanRec(SG_HASH_ENTRY_VALUE(e), cbs, ctx);
  }
  return cbs;
}

static void hash_cache_writer(SgObject obj, SgPort *port,
			      SgWriteCacheCtx *ctx)
{
  SgHashTable *ht = SG_HASHTABLE(obj);
  SgHashIter iter;
  SgHashEntry *e;
  SgObject count = SG_MAKE_INT(SG_HASHTABLE_CORE(ht)->entryCount);
  Sg_PutbUnsafe(port, (int)ht->type);
  Sg_PutbUnsafe(port, (int)ht->immutablep);
  /* should we write 4 byte instead of object? */
  Sg_WriteObjectCache(count, port, ctx);
  switch (ht->type) {
  default: break;
    /* we are only interested in general type */
  case SG_HASH_GENERAL:
    /* if one of these 2 is subr, then the cache will be failed. */
    Sg_WriteObjectCache(SG_HASHTABLE_CORE(ht)->generalHasher, port, ctx);
    Sg_WriteObjectCache(SG_HASHTABLE_CORE(ht)->generalCompare, port, ctx);
    break;
  }
  Sg_HashIterInit(ht, &iter);
  while ((e = Sg_HashIterNext(&iter, NULL, NULL)) != NULL) {
    Sg_WriteObjectCache(SG_HASH_ENTRY_KEY(e), port, ctx);
    Sg_WriteObjectCache(SG_HASH_ENTRY_VALUE(e), port, ctx);
  }
}

#define DEFINE_CLASS_WITH_CACHE SG_DEFINE_BUILTIN_CLASS_WITH_CACHE

DEFINE_CLASS_WITH_CACHE(Sg_HashTableClass, 
			hash_cache_reader, hash_cache_scanner,
			hash_cache_writer,
			hash_print, NULL, NULL, NULL,
			SG_CLASS_DICTIONARY_CPL);

static SgHashTable * make_hashtable();

static SgObject hashtable_ref(SgObject table, SgHashEntry *e,  int flags)
{
  return SG_HASH_ENTRY_VALUE(e);
}

static SgObject hashtable_set(SgObject table, SgHashEntry *e, SgObject value,
			      int flags)
{
  if (e->value) {
    if (flags & SG_HASH_NO_OVERWRITE) return SG_HASH_ENTRY_VALUE(e);
    else {
      return SG_HASH_ENTRY_SET_VALUE(e, value);
    }
  } else {
    return SG_HASH_ENTRY_SET_VALUE(e, value);
  }
}

static SgObject hashtable_delete(SgObject table, SgObject key)
{
  SgHashEntry *e;

  e = Sg_HashCoreSearch(SG_HASHTABLE_CORE(table),
			(intptr_t)key,
			SG_DICT_DELETE,
			0);
  if (e && e->value) return SG_HASH_ENTRY_VALUE(e);
  else return SG_UNBOUND;
}

static SgObject hashtable_copy(SgObject src, int mutableP)
{
  SgHashTable *dst = make_hashtable();
  Sg_HashCoreCopy(dst, src);
  dst->type = SG_HASHTABLE_TYPE(src);
  if (!mutableP) {
    dst->immutablep = TRUE;
  }
  return SG_OBJ(dst);
}

static void hashtable_init_iter(SgObject table, SgHashIter *iter)
{
  hash_iter_init(SG_HASHTABLE_CORE(table), iter);
  iter->table = table;
}

SgHashEntry * hash_iter_next(SgHashIter *itr, SgObject *key, SgObject *value)
{
  Entry *e = (Entry*)itr->next;
  if (e != NULL) {
    if (e->next) itr->next = e->next;
    else {
      long i = itr->bucket + 1;
      for (; i < itr->core->bucketCount; i++) {
	if (itr->core->buckets[i]) {
	  itr->bucket = i;
	  itr->next = itr->core->buckets[i];
	  if (key) *key = SG_HASH_ENTRY_KEY(e);
	  if (value) *value = SG_HASH_ENTRY_VALUE(e);
	  return (SgHashEntry*)e;
	}
      }
      itr->next = NULL;
    }
    if (key) *key = SG_HASH_ENTRY_KEY(e);
    if (value) *value = SG_HASH_ENTRY_VALUE(e);
  }
  return (SgHashEntry*)e; /*NULL*/
}

static SgHashOpTable hashtable_operations = {
  hashtable_ref,
  hashtable_set,
  hashtable_delete,
  hashtable_copy,
  hashtable_init_iter,
};

static SgHashTable * make_hashtable()
{
  SgHashTable *z = SG_NEW(SgHashTable);
  SG_SET_CLASS(z, SG_CLASS_HASHTABLE);
  SG_HASHTABLE_OPTABLE(z) = &hashtable_operations;
  return z;
}

SgObject Sg_MakeHashTableSimple(SgHashType type, long initSize)
{
  SgHashTable *z = make_hashtable();
  return Sg_InitHashTableSimple(z, type, initSize);
}

SgObject Sg_InitHashTableSimple(SgHashTable *table, 
				SgHashType type, long initSize)
{
  if (type > SG_HASH_GENERAL) {
    Sg_Error(UC("Sg_MakeHashTableSimple: wrong type arg: %d"), type);
  }
  SG_SET_CLASS(table, SG_CLASS_HASHTABLE);
  SG_HASHTABLE_OPTABLE(table) = &hashtable_operations;
  Sg_HashCoreInitSimple(&table->core, type, initSize, NULL);
  table->type = type;
  table->immutablep = FALSE;
  return SG_OBJ(table);
}

SgObject Sg_MakeHashTable(SgObject hasher, SgObject compare, long initSize)
{
  SgHashTable *z = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_GENERAL,
						       initSize));
  z->core.generalHasher = hasher;
  z->core.generalCompare = compare;
  return SG_OBJ(z);
}

SgObject Sg_MakeHashTableWithComparator(SgObject comparator, long initSize)
{
  /* do some optimisation */
  if (comparator == Sg_EqComparator()) {
    return Sg_MakeHashTableSimple(SG_HASH_EQ, initSize);
  } else if (comparator == Sg_EqvComparator()) {
    return Sg_MakeHashTableSimple(SG_HASH_EQV, initSize);
  } else if (comparator == Sg_EqualComparator()) {
    return Sg_MakeHashTableSimple(SG_HASH_EQUAL, initSize);
  } else if (comparator == Sg_StringComparator()) {
    return Sg_MakeHashTableSimple(SG_HASH_STRING, initSize);
  } else {
    if (!SG_PROCEDUREP(SG_COMPARATOR(comparator)->hashFn) ||
	!SG_PROCEDUREP(SG_COMPARATOR(comparator)->eqFn)) {
      Sg_Error(UC("make-hashtable/comparator: comparator doesn't "
		  "have hash and/or equality procedure(s). %S"), comparator);
    }
    return Sg_MakeHashTable(SG_COMPARATOR(comparator)->hashFn,
			    SG_COMPARATOR(comparator)->eqFn,
			    initSize);
  }
}


SgObject Sg_HashTableCopy(SgHashTable *src, int mutableP)
{
  return SG_HASHTABLE_OPTABLE(src)->copy(src, mutableP);
}

SgObject Sg_HashTableRef(SgHashTable *table, SgObject key, SgObject fallback)
{
  SgHashEntry *e = Sg_HashCoreSearch(SG_HASHTABLE_CORE(table),
				     (intptr_t)key, SG_DICT_GET, 
				     0);
  if (!e) return fallback;
  return SG_HASHTABLE_OPTABLE(table)->ref(table, e, 0);
}

SgObject Sg_HashTableSet(SgHashTable *table, SgObject key, SgObject value,
			 int flags)
{
  SgHashEntry *e;
  if (SG_IMMUTABLE_HASHTABLE_P(table)) {
    Sg_Error(UC("attemp to modify immutable hashtable"));
    return SG_UNDEF;
  }

  e = Sg_HashCoreSearch(SG_HASHTABLE_CORE(table), (intptr_t)key,
			(flags & SG_HASH_NO_CREATE)
			? SG_DICT_GET
			: SG_DICT_CREATE,
			0);
  if (!e) return SG_UNBOUND;
  return SG_HASHTABLE_OPTABLE(table)->set(table, e, value, flags);
}

SgObject Sg_HashTableDelete(SgHashTable *table, SgObject key)
{
  if (SG_IMMUTABLE_HASHTABLE_P(table)) {
    Sg_Error(UC("attemp to modify immutable hashtable"));
    return SG_UNDEF;
  }
  return SG_HASHTABLE_OPTABLE(table)->remove(table, key);
}

SgObject Sg_HashTableAddAll(SgHashTable *dst, SgHashTable *src)
{
  SgObject keys;
  SgObject cp, key;

  if (SG_IMMUTABLE_HASHTABLE_P(dst)) {
    Sg_Error(UC("attemp to modify immutable hashtable"));
    return SG_UNDEF;
  }

  keys = Sg_HashTableKeys(src);
  SG_FOR_EACH(cp, keys) {
    key = SG_CAR(cp);
    Sg_HashTableSet(dst, key, Sg_HashTableRef(src, key, SG_UNBOUND), 0);
  }
  return keys;
}

SgObject Sg_HashTableKeys(SgHashTable *table)
{
  SgHashIter itr;
  SgObject h = SG_NIL, t = SG_NIL, k;
  Sg_HashIterInit(table, &itr);
  while (Sg_HashIterNext(&itr, &k, NULL) != NULL) {
    SG_APPEND1(h, t, k);
  }
  return h;
}

SgObject Sg_HashTableValues(SgHashTable *table)
{
  SgHashIter itr;
  SgObject h = SG_NIL, t = SG_NIL, v;
  Sg_HashIterInit(table, &itr);
  while (Sg_HashIterNext(&itr, NULL, &v) != NULL) {
    SG_APPEND1(h, t, v);
  }
  return h;
}

SgObject Sg_HashTableStat(SgHashTable *table)
{
  /* TODO */
  return SG_FALSE;
}

long Sg_HashTableSize(SgHashTable *table)
{
  return SG_HASHTABLE_CORE(table)->entryCount;
}

unsigned long round2up(unsigned long val)
{
  unsigned long n = 1;
  while (n < val) {
    n <<= 1;
    ASSERT(n > 1);      /* check overflow */
  }
  return n;
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
