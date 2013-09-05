/* -*- C -*- */
/*
 * hashtable.h
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_HASHTABLE_H_
#define SAGITTARIUS_HASHTABLE_H_

#include "sagittariusdefs.h"
#include "clos.h"

typedef enum {
  SG_HASH_EQ,
  SG_HASH_EQV,
  SG_HASH_EQUAL,
  SG_HASH_STRING,
  SG_HASH_GENERAL,
} SgHashType;

typedef struct SgHashCoreRec SgHashCore;
typedef struct SgHashIterRec SgHashIter;

/* hasher */
typedef uint32_t SgHashProc(const SgHashCore *hc, intptr_t key);
/* tester */
typedef int      SgHashCompareProc(const SgHashCore *hc, intptr_t key,
				   intptr_t entryKey);

struct SgHashCoreRec
{
  void  **buckets;
  int     bucketCount;
  int     entryCount;
  int     bucketsLog2Count;
  void              *access;
  SgHashProc        *hasher;
  SgHashCompareProc *compare;
  SgObject generalHasher;	/* for make-hashtable */
  SgObject generalCompare; 	/* ditto */
  void   *data; 
};

struct SgHashIterRec
{
  SgHashCore *core;
  int         bucket;
  void       *next;
};

SG_CLASS_DECL(Sg_HashTableClass);
#define SG_CLASS_HASHTABLE (&Sg_HashTableClass)

struct SgHashTableRec
{
  SG_HEADER;
  char       immutablep;
  SgHashType type;
  SgHashCore core;
};

typedef struct SgHashEntryRec
{
  void *key;
  void *value;
} SgHashEntry;

typedef enum {
  SG_HASH_GET,
  SG_HASH_CREATE,
  SG_HASH_DELETE
} SgHashOp;

typedef enum {
  SG_HASH_NO_OVERWRITE = (1L<<0), /* do not overwrite the existing entry */
  SG_HASH_NO_CREATE    = (1L<<1)  /* do not create new one if no match */
} SgHashSetFlags;

#define SG_HASHTABLE_P(obj)    SG_XTYPEP(obj, SG_CLASS_HASHTABLE)
#define SG_HASHTABLE(obj)      ((SgHashTable*)(obj))
#define SG_HASHTABLE_CORE(obj) (&SG_HASHTABLE(obj)->core)
#define SG_HASH_ENTRY_KEY(e)   SG_OBJ((e)->key)
#define SG_HASH_ENTRY_VALUE(e) SG_OBJ((e)->value)
#define SG_HASH_ENTRY_SET_VALUE(e, v)		\
  SG_OBJ((e)->value = (void *)v)

#define SG_IMMUTABLE_HASHTABLE_P(obj)					\
  (SG_HASHTABLE_P(obj) && SG_HASHTABLE(obj)->immutablep)


SG_CDECL_BEGIN
/* hash core */
SG_EXTERN void Sg_HashCoreInitSimple(SgHashCore *core,
				     SgHashType type,
				     unsigned int initSize,
				     void *data);
SG_EXTERN void Sg_HashCoreInitGeneral(SgHashCore *core,
				      SgHashProc *hasher,
				      SgHashCompareProc *compare,
				      unsigned int initSize,
				      void *data);
SG_EXTERN int Sg_HashCoreTypeToProcs(SgHashType type, SgHashProc **hasher,
				     SgHashCompareProc **compare);
SG_EXTERN SgHashEntry* Sg_HashCoreSearch(SgHashCore *table, intptr_t key,
					 SgHashOp op);
SG_EXTERN void Sg_HashCoreCopy(SgHashCore *dst,
			       const SgHashCore *src);

SG_EXTERN void Sg_HashCoreClear(SgHashCore *ht, int k);

/* iterator */
SG_EXTERN void Sg_HashIterInit(SgHashCore *table,
			       SgHashIter *itr);
SG_EXTERN SgHashEntry* Sg_HashIterNext(SgHashIter *itr);

/* hasher */
SG_EXTERN uint32_t Sg_EqHash(SgObject obj);
SG_EXTERN uint32_t Sg_EqvHash(SgObject obj);
SG_EXTERN uint32_t Sg_EqualHash(SgObject obj);
SG_EXTERN uint32_t Sg_StringHash(SgString *str, uint32_t bound);

SG_EXTERN SgObject Sg_MakeHashTableSimple(SgHashType type, int initSize);
/* for c-string. see string.c */
SG_EXTERN SgObject Sg_MakeHashTable(SgHashProc *hasher, SgHashCompareProc *compre, int initSize);
SG_EXTERN SgObject Sg_MakeHashTableForScheme(SgObject hasher, SgObject compare, int initSize);

SG_EXTERN SgObject Sg_HashTableCopy(SgHashTable *table, int mutableP);

SG_EXTERN SgObject Sg_HashTableRef(SgHashTable *table, SgObject key, SgObject fallback);
SG_EXTERN SgObject Sg_HashTableSet(SgHashTable *table, SgObject key, SgObject value, int flags);
SG_EXTERN SgObject Sg_HashTableDelete(SgHashTable *table, SgObject key);

SG_EXTERN SgObject Sg_HashTableAddAll(SgHashTable *dst, SgHashTable *src);
SG_EXTERN SgObject Sg_HashTableKeys(SgHashTable *table);
SG_EXTERN SgObject Sg_HashTableValues(SgHashTable *table);
/* status for hash table */
SG_EXTERN SgObject Sg_HashTableStat(SgHashTable *table);

SG_CDECL_END

#endif /* SAGITTARIUS_HASHTABLE_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
