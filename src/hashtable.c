/* -*- C -*- */
/*
 * hashtable.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/hashtable.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/collection.h"
#include "sagittarius/compare.h"
#include "sagittarius/error.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/number.h"
#include "sagittarius/symbol.h"
#include "sagittarius/keyword.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"
#include "sagittarius/writer.h"


typedef struct EntryRec
{
  intptr_t key;
  intptr_t value;
  struct EntryRec *next;
  uint32_t hashValue;
} Entry;

#define BUCKETS(hc) ((Entry**)hc->buckets)

#define DEFAULT_BUCKET_COUNT 4
#define MAX_AVG_CHAIN_LIMIS  3
#define EXTEND_BITS          2
/* hash value must be 32 bit */
#define HASHMASK 0xffffffffUL

typedef Entry* SearchProc(SgHashCore *core, intptr_t key, SgHashOp op);

static unsigned int round2up(unsigned int val);
static void hash_iter_init(SgHashCore *core, SgHashIter *itr);

/* hash functions */
#define STRING_HASH(hv, chars, size)				\
  do {								\
    int i_ = (size);						\
    (hv) = 0;							\
    while (i_-- > 0) {						\
      (hv) = ((hv) << 5) - (hv) + ((unsigned char)*chars++);	\
    }								\
  } while (0)

#define SMALL_INT_HASH(result, value)		\
  (result) = ((value)*2654435761UL)
#define ADDRESS_HASH(result, val)				\
  (result) = (uint32_t)((SG_WORD(val) >> 3)*2654435761UL)

#define HASH2INDEX(tabsize, bits, hashval)			\
  (((hashval)+((hashval)>>(32-(bits)))) & ((tabsize) - 1))

#define COMBINE(hv1, hv2)  ((hv1)*5+(hv2))

uint32_t Sg_EqHash(SgObject obj)
{
  uint32_t hashval;
  ADDRESS_HASH(hashval, obj);
  return hashval & HASHMASK;
}

uint32_t Sg_EqvHash(SgObject obj)
{
  uint32_t hashval;
  if (SG_NUMBERP(obj)) {
    if (SG_INTP(obj)) {
      SMALL_INT_HASH(hashval, SG_INT_VALUE(obj));
    } else if (SG_BIGNUMP(obj)) {
      unsigned int i;
      unsigned long u = 0;
      int size = SG_BIGNUM_GET_COUNT(obj);
      for (i = 0; i < size; i++) {
	u += SG_BIGNUM(obj)->elements[i];
      }
      SMALL_INT_HASH(hashval, u);
    } else if (SG_FLONUMP(obj)) {
      hashval = (unsigned long)(SG_FLONUM(obj)->value * 2654435761UL);
    } else if (SG_RATIONALP(obj)) {
      unsigned long h1 = Sg_EqvHash(SG_RATIONAL(obj)->numerator);
      unsigned long h2 = Sg_EqvHash(SG_RATIONAL(obj)->denominator);
      hashval = COMBINE(h1, h2);
    } else {
      unsigned long h1 = Sg_EqvHash(SG_COMPLEX(obj)->real);
      unsigned long h2 = Sg_EqvHash(SG_COMPLEX(obj)->imag);
      hashval = COMBINE(h1, h2);
    }
  } else {
    ADDRESS_HASH(hashval, obj);
  }
  return hashval & HASHMASK;
}

#define MAX_EQUAL_HASH_DEPTH 100
static uint32_t equal_hash_rec(SgObject obj, int depth, SgHashTable *seen)
{
  uint32_t hashval;
  if (depth == MAX_EQUAL_HASH_DEPTH) return 1;

#define REGISTER(obj)						\
  if (SG_UNBOUNDP(Sg_HashTableRef(seen, (obj), SG_UNBOUND))) {	\
    Sg_HashTableSet(seen, (obj), SG_TRUE, 0);			\
  }
#define CHECK(obj)						\
  if (!SG_FALSEP(Sg_HashTableRef(seen, (obj), SG_FALSE)))

  CHECK(obj) {
    ADDRESS_HASH(hashval, obj);
    return hashval;
  }

  if (!SG_PTRP(obj)) {
    SMALL_INT_HASH(hashval, (unsigned long)SG_WORD(obj));
    return hashval;
  } else if (SG_NUMBERP(obj)) {
    return Sg_EqvHash(obj);
  } else if (SG_STRINGP(obj)) {
    return Sg_StringHash(SG_STRING(obj), 0);
  } else if (SG_PAIRP(obj)) {
    unsigned long h = 0, h2;
    Sg_HashTableSet(seen, obj, SG_TRUE, 0);
    h = equal_hash_rec(SG_CAR(obj), depth+1, seen);
    REGISTER(SG_CAR(obj));
    CHECK(SG_CDR(obj)) {
      h2 = equal_hash_rec(SG_CDR(obj), depth+1, seen);
    } else {
      h2 = 1;
    }
    h = COMBINE(h, h2);
    return h;
  } else if (SG_VECTORP(obj)) {
    int i, size = SG_VECTOR_SIZE(obj);
    unsigned long h = 0, h2;
    Sg_HashTableSet(seen, obj, SG_TRUE, 0);
    for (i = 0; i < size; i++) {
      CHECK(SG_VECTOR_ELEMENT(obj, i)) continue;
      h2 = equal_hash_rec(SG_VECTOR_ELEMENT(obj, i), depth+1, seen);
      h = COMBINE(h, h2);
      REGISTER(SG_VECTOR_ELEMENT(obj, i));
    }
    return h;
  } else if (SG_SYMBOLP(obj)) {
    return Sg_StringHash(SG_SYMBOL(obj)->name, 0);
  } else if (SG_KEYWORDP(obj)) {
    return Sg_StringHash(SG_KEYWORD_NAME(obj), 0);
  } else if (SG_BVECTORP(obj)) {
    /* TODO is this ok? */
    unsigned long h = 0, h2;
    int i, size = SG_BVECTOR_SIZE(obj);
    for (i = 0; i < size; i++) {
      SMALL_INT_HASH(h2, SG_BVECTOR_ELEMENT(obj, i));
      h = COMBINE(h, h2);
    }
    return h;
  } else {
    ADDRESS_HASH(hashval, obj);
    return hashval;
  }
}

uint32_t Sg_EqualHash(SgObject obj)
{
  SgHashTable *ht = Sg_MakeHashTableSimple(SG_HASH_EQ, 0);
  return equal_hash_rec(obj, 0, ht);
}

uint32_t Sg_StringHash(SgString *str, uint32_t bound)
{
  uint32_t hashval;
  const SgChar *p = str->value;
  STRING_HASH(hashval, p, str->size);
  if (bound == 0) return hashval;
  else return (hashval % bound);
}

/* accessor and so */
static Entry *insert_entry(SgHashCore *table,
			   intptr_t key,
			   uint32_t hashval,
			   int index)
{
  Entry *e = SG_NEW(Entry);
  Entry **buckets = BUCKETS(table);
  e->key = key;
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
    int i, newsize = (table->bucketCount << EXTEND_BITS);
    int newbits = table->bucketsLog2Count + EXTEND_BITS;

    newb = SG_NEW_ARRAY(Entry*, newsize);
    for (i = 0; i < newsize; i++) newb[i] = NULL; /* initialize new buckets */

    hash_iter_init(table, &itr);
    while ((f = (Entry*)Sg_HashIterNext(&itr)) != NULL) {
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
			   int index)
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
    case SG_HASH_GET:;					\
    case SG_HASH_CREATE:;				\
      return e;						\
    case SG_HASH_DELETE:;				\
      return delete_entry(table, e, p, index);		\
    }							\
  } while(0)

#define NOTFOUND(table, op, key, hashval, index)	\
  do {							\
    if (op == SG_HASH_CREATE) {				\
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
			   unsigned int initSize,
			   void* data)
{
  Entry **b;
  unsigned int i;

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
}

/** accessor function */
/* eq? */
static Entry *address_access(SgHashCore *table,
			     intptr_t key,
			     SgHashOp op)
{
  uint32_t hashval, index;
  Entry *e, *p, **buckets = BUCKETS(table);

  ADDRESS_HASH(hashval, key);
  index = HASH2INDEX(table->bucketCount, table->bucketsLog2Count, hashval);

  for (e = buckets[index], p = NULL; e; p = e, e = e->next) {
    if (e->key == key) FOUND(table, op, e, p, index);
  }
  NOTFOUND(table, op, key, hashval, index);
}

static uint32_t address_hash(const SgHashCore *ht, intptr_t obj)
{
  uint32_t hashval;
  ADDRESS_HASH(hashval, obj);
  return hashval;
}

static int address_compare(const SgHashCore *ht, intptr_t key, intptr_t k2)
{
  return (key == k2);
}

/* eqv? and equal? */
static uint32_t eqv_hash(const SgHashCore *table, intptr_t key)
{
  return Sg_EqvHash(SG_OBJ(key));
}

static int eqv_compare(const SgHashCore *table, intptr_t key, intptr_t k2)
{
  return Sg_EqvP(SG_OBJ(key), SG_OBJ(k2));
}

static uint32_t equal_hash(const SgHashCore *table, intptr_t key)
{
  return Sg_EqualHash(SG_OBJ(key));
}

static int equal_compare(const SgHashCore *table, intptr_t key, intptr_t k2)
{
  return Sg_EqualP(SG_OBJ(key), SG_OBJ(k2));
}

/* string */
static Entry *string_access(SgHashCore *table,
			    intptr_t k,
			    SgHashOp op)
{
  uint32_t hashval, index;
  int size;
  const SgChar *s;
  SgObject key = SG_OBJ(k);
  Entry *e, *p, **buckets;

  if (!SG_STRINGP(key)) {
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

static uint32_t string_hash(const SgHashCore *table, intptr_t key)
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
			     SgHashOp op)
{
  uint32_t hashval, index;
  Entry *e, *p, **buckets;

  hashval = table->hasher(table, key);
  index = HASH2INDEX(table->bucketCount, table->bucketsLog2Count, hashval);
  buckets = BUCKETS(table);

  for (e = buckets[index], p = NULL; e; p = e, e = e->next) {
    if (table->compare(table, key, e->key)) FOUND(table, op, e, p, index);
  }
  NOTFOUND(table, op, key, hashval, index);
}

static uint32_t general_hash(const SgHashCore *table, intptr_t key)
{
  SgObject hash = Sg_Apply1(table->generalHasher, SG_OBJ(key));
  return Sg_GetIntegerClamp(hash, SG_CLAMP_NONE, NULL);
}

static int general_compare(const SgHashCore *table, intptr_t key, intptr_t k2)
{
  SgObject ret = Sg_Apply2(table->generalCompare, SG_OBJ(key), SG_OBJ(k2));
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
			   unsigned int initSize,
			   void *data)
{
  SearchProc *access = NULL;
  SgHashProc *hasher = NULL;
  SgHashCompareProc *compare = NULL;
  if (hash_core_predef_procs(type, &access, &hasher, &compare) == FALSE) {
    Sg_Error(UC("[internal error]: wrong TYPE argument passed to Sg_HashCoreInitSimple: %d"), type);
  }
  hash_core_init(core, access, hasher, compare, initSize, data);
}

void Sg_HashCoreInitGeneral(SgHashCore *core,
			    SgHashProc *hasher,
			    SgHashCompareProc *compare,
			    unsigned int initSize,
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
			       SgHashOp op)
{
  SearchProc *p = (SearchProc*)table->access;
  return (SgHashEntry*)p(table, key, op);
}

void Sg_HashCoreCopy(SgHashCore *dst, const SgHashCore *src)
{
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
      e->value = s->value;
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

void Sg_HashCoreClear(SgHashCore *ht, int k)
{
  int i;
  for (i = 0; i < ht->bucketCount; i++) {
    ht->buckets[i] = NULL;
  }
  ht->entryCount = 0;
  /* TODO: do we need this? */
  if (k > 0) {
    Entry **b;
    int i;
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

void hash_iter_init(SgHashCore *core, SgHashIter *itr)
{
  int i;
  itr->core = core;
  for (i = 0; i < core->bucketCount; i++) {
    if (core->buckets[i]) {
      itr->bucket = i;
      itr->next = core->buckets[i];
      return;
    }
  }
  itr->next = NULL;
}

void Sg_HashIterInit(SgHashCore *core, SgHashIter *itr)
{
  hash_iter_init(core, itr);
}

SgHashEntry* Sg_HashIterNext(SgHashIter *itr)
{
  Entry *e = (Entry*)itr->next;
  if (e != NULL) {
    if (e->next) itr->next = e->next;
    else {
      int i = itr->bucket + 1;
      for (; i < itr->core->bucketCount; i++) {
	if (itr->core->buckets[i]) {
	  itr->bucket = i;
	  itr->next = itr->core->buckets[i];
	  return (SgHashEntry*)e;
	}
      }
      itr->next = NULL;
    }
  }
  return (SgHashEntry*)e; /*NULL*/
}

static void hash_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgHashTable *ht = SG_HASHTABLE(obj);
  SG_PORT_LOCK(port);
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
  Sg_PutcUnsafe(port, '>');
  SG_PORT_UNLOCK(port);
}

SG_DEFINE_BUILTIN_CLASS(Sg_HashTableClass, hash_print, NULL, NULL, NULL,
			SG_CLASS_DICTIONARY_CPL);


SgObject Sg_MakeHashTableSimple(SgHashType type, int initSize)
{
  SgHashTable *z;
  if (type > SG_HASH_GENERAL) {
    Sg_Error(UC("Sg_MakeHashTableSimple: wrong type arg: %d"), type);
  }
  z = SG_NEW(SgHashTable);
  SG_SET_CLASS(z, SG_CLASS_HASHTABLE);
  Sg_HashCoreInitSimple(&z->core, type, initSize, NULL);
  z->type = type;
  return SG_OBJ(z);
}

SgObject Sg_MakeHashTable(SgHashProc *hasher, SgHashCompareProc *compre, int initSize)
{
  SgHashTable *z = SG_NEW(SgHashTable);
  SG_SET_CLASS(z, SG_CLASS_HASHTABLE);
  Sg_HashCoreInitGeneral(&z->core, hasher, compre, initSize, NULL);
  z->type = SG_HASH_GENERAL;
  return SG_OBJ(z);
}

SgObject Sg_MakeHashTableForScheme(SgObject hasher, SgObject compare, int initSize)
{
  SgHashTable *z = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_GENERAL, initSize));
  z->core.generalHasher = hasher;
  z->core.generalCompare = compare;
  return SG_OBJ(z);
}

SgObject Sg_HashTableCopy(SgHashTable *src, int mutableP)
{
  SgHashTable *dst = SG_NEW(SgHashTable);
  SG_SET_CLASS(dst, SG_CLASS_HASHTABLE);
  Sg_HashCoreCopy(SG_HASHTABLE_CORE(dst), SG_HASHTABLE_CORE(src));
  dst->type = src->type;
  if (!mutableP) {
    dst->immutablep = TRUE;
  }
  return SG_OBJ(dst);
}

SgObject Sg_HashTableRef(SgHashTable *table, SgObject key, SgObject fallback)
{
  SgHashEntry *e = Sg_HashCoreSearch(SG_HASHTABLE_CORE(table),
				     (intptr_t)key, SG_HASH_GET);
  if (!e) return fallback;
  else return SG_HASH_ENTRY_VALUE(e);
}

SgObject Sg_HashTableSet(SgHashTable *table, SgObject key, SgObject value, int flags)
{
  SgHashEntry *e;

  if (SG_IMMUTABLE_HASHTABLE_P(table)) {
    Sg_Error(UC("attemp to modify immutable hashtable"));
    return SG_UNDEF;
  }

  e = Sg_HashCoreSearch(SG_HASHTABLE_CORE(table),
			(intptr_t)key,
			(flags & SG_HASH_NO_CREATE) ? SG_HASH_GET: SG_HASH_CREATE);
  if (!e) return SG_UNBOUND;
  if (e->value) {
    if (flags & SG_HASH_NO_OVERWRITE) return SG_HASH_ENTRY_VALUE(e);
    else {
      SG_HASH_ENTRY_SET_VALUE(e, value);
      return SG_HASH_ENTRY_VALUE(e);
    }
  } else {
    return SG_HASH_ENTRY_SET_VALUE(e, value);
  }
}

SgObject Sg_HashTableDelete(SgHashTable *table, SgObject key)
{
  SgHashEntry *e;

  if (SG_IMMUTABLE_HASHTABLE_P(table)) {
    Sg_Error(UC("attemp to modify immutable hashtable"));
    return SG_UNDEF;
  }

  e = Sg_HashCoreSearch(SG_HASHTABLE_CORE(table),
			(intptr_t)key,
			SG_HASH_DELETE);
  if (e && e->value) return SG_HASH_ENTRY_VALUE(e);
  else return SG_UNBOUND;
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
  SgHashEntry *e;
  SgObject h = SG_NIL, t = SG_NIL;
  Sg_HashIterInit(SG_HASHTABLE_CORE(table), &itr);
  while ((e = Sg_HashIterNext(&itr)) != NULL) {
    SG_APPEND1(h, t, SG_HASH_ENTRY_KEY(e));
  }
  return h;
}

SgObject Sg_HashTableValues(SgHashTable *table)
{
  SgHashIter itr;
  SgHashEntry *e;
  SgObject h = SG_NIL, t = SG_NIL;
  Sg_HashIterInit(SG_HASHTABLE_CORE(table), &itr);
  while ((e = Sg_HashIterNext(&itr)) != NULL) {
    SG_APPEND1(h, t, SG_HASH_ENTRY_VALUE(e));
  }
  return h;
}

SgObject Sg_HashTableStat(SgHashTable *table)
{
  /* TODO */
  return SG_FALSE;
}

unsigned int round2up(unsigned int val)
{
  unsigned int n = 1;
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
