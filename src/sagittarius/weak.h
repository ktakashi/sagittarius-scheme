/* -*- C -*- */
/*
 * weak.h
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
#ifndef SAGITTARIUS_WEAK_H_
#define SAGITTARIUS_WEAK_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_WeakVectorClass);
SG_CLASS_DECL(Sg_WeakHashTableClass);
#define SG_CLASS_WEAK_VECTOR    (&Sg_WeakVectorClass)
#define SG_CLASS_WEAK_HASHTABLE (&Sg_WeakHashTableClass)
typedef struct SgWeakVectorRec
{
  SG_HEADER;
  int   size;
  void *pointers;		/* opaque */
} SgWeakVector;

#define SG_WEAK_VECTOR(obj)  ((SgWeakVector*)(obj))
#define SG_WEAK_VECTORP(obj) SG_XTYPEP(obj,SG_CLASS_WEAK_VECTOR)

/* weak box for weak hashtable */
SG_CLASS_DECL(Sg_WeakBoxClass);
typedef struct SgWeakBoxRec SgWeakBox;

#include "hashtable.h"

typedef enum {
  SG_WEAK_KEY   = (1L<<0),
  SG_WEAK_VALUE = (1L<<1),
  SG_WEAK_BOTH  = (SG_WEAK_KEY | SG_WEAK_VALUE)
} SgWeakness;

typedef struct SgWeakHashTableRec
{
  SG_HEADER;
  SgWeakness  weakness;
  SgHashType  type;
  SgHashCore  core;
  SgObject    defaultValue;
  SgHashProc        *hasher;
  SgHashCompareProc *compare;
  unsigned int goneEntries;
} SgWeakHashTable;

typedef struct SgWeakHashIterRec
{
  SgWeakHashTable *table;
  SgHashIter iter;
} SgWeakHashIter;

#define SG_WEAK_HASHTABLE(obj)      ((SgWeakHashTable*)obj)
#define SG_WEAK_HASHTABLE_P(obj)    SG_XTYPEP(obj, SG_CLASS_WEAK_HASHTABLE)
#define SG_WEAK_HASHTABLE_CORE(obj) (&SG_WEAK_HASHTABLE(obj)->core)

SG_CDECL_BEGIN

/* weak vector */
SG_EXTERN SgObject Sg_MakeWeakVector(int size);
SG_EXTERN SgObject Sg_WeakVectorRef(SgWeakVector *v, int index, SgObject fallback);
SG_EXTERN SgObject Sg_WeakVectorSet(SgWeakVector *v, int index, SgObject value);

/* weak box */
SG_EXTERN SgWeakBox* Sg_MakeWeakBox(void *value);
SG_EXTERN int        Sg_WeakBoxEmptyP(SgWeakBox *wbox);
SG_EXTERN void       Sg_WeakBoxSet(SgWeakBox *wbox, void *value);
SG_EXTERN void*      Sg_WeakBoxRef(SgWeakBox *wbox);

/* weak hash */
SG_EXTERN SgObject Sg_MakeWeakHashTableSimple(SgHashType type,
					      SgWeakness weakness,
					      int initSize,
					      SgObject defaultValue);
SG_EXTERN SgObject Sg_WeakHashTableCopy(SgWeakHashTable *table);
SG_EXTERN SgObject Sg_WeakHashTableRef(SgWeakHashTable *table,
				       SgObject key, SgObject fallback);
SG_EXTERN SgObject Sg_WeakHashTableSet(SgWeakHashTable *table,
				       SgObject key, SgObject value, int flag);
SG_EXTERN SgObject Sg_WeakHashTableDelete(SgWeakHashTable *table,
					  SgObject key);
SG_EXTERN SgObject Sg_WeakHashTableKeys(SgWeakHashTable *table);
SG_EXTERN SgObject Sg_WeakHashTableValues(SgWeakHashTable *table);

SG_EXTERN void     Sg_WeakHashIterInit(SgWeakHashIter *iter,
				       SgWeakHashTable *table);
SG_EXTERN int      Sg_WeakHashIterNext(SgWeakHashIter *iter,
				       SgObject *key, SgObject *value);


SG_CDECL_END

#endif /* SAGITTARIUS_WEAK_H_ */
