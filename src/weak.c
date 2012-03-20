/* -*- C -*- */
/*
 * weak.c
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
#include "sagittarius/weak.h"
#include "sagittarius/collection.h"
#include "sagittarius/core.h"
#include "sagittarius/error.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/writer.h"

static void wvector_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgWeakVector *wvec = SG_WEAK_VECTOR(obj);
  int size = wvec->size, i;
  Sg_Putuz(port, UC("#<weak-vector"));
  for (i = 0; i < size; i++) {
    Sg_Putc(port, ' ');
    Sg_Write(Sg_WeakVectorRef(wvec, i, SG_FALSE), port, ctx->mode);
  }
  Sg_Putc(port, '>');
}

SG_DEFINE_BUILTIN_CLASS(Sg_WeakVectorClass, wvector_print, NULL, NULL, NULL,
			SG_CLASS_SEQUENCE_CPL);

static void whash_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  /* dummy */
  Sg_Putuz(port, UC("#<weak-hashtable>"));
}
SG_DEFINE_BUILTIN_CLASS(Sg_WeakHashTableClass, whash_print, NULL, NULL, NULL,
			SG_CLASS_DICTIONARY_CPL);


static void weakvector_finalize(SgObject obj, void *data)
{
  int i;
  SgWeakVector *v = SG_WEAK_VECTOR(obj);
  SgObject *p = (SgObject*)v->pointers;
  for (i = 0; i < v->size; i++) {
    if (p[i] == NULL || SG_PTRP(p[i])) {
      Sg_UnregisterDisappearingLink((void **)&p[i]);
    }
    p[i] = SG_FALSE;
  }
}

SgObject Sg_MakeWeakVector(int size)
{
  int i;
  SgObject *p;
  SgWeakVector *v = SG_NEW(SgWeakVector);

  SG_SET_CLASS(v, SG_CLASS_WEAK_VECTOR);
  v->size = size;
  /* Allocate pointer array by ATOMIC, so that GC won't trace the
     pointers in it.
   */
  p = SG_NEW_ATOMIC2(SgObject *, size * sizeof(SgObject));
  for (i = 0; i < size; i++) p[i] = SG_FALSE;
  v->pointers = (void*)p;
  Sg_RegisterFinalizer(SG_OBJ(v), weakvector_finalize, NULL);
  return SG_OBJ(v);
}

SgObject Sg_WeakVectorRef(SgWeakVector *v, int index, SgObject fallback)
{
  SgObject *p;
  if (index < 0 || index >= v->size) {
    if (SG_UNBOUNDP(fallback)) {
      Sg_Error(UC("argument out of range: %d"), index);
    }
    return fallback;
  }
  p = (SgObject*)v->pointers;
  if (p[index] == NULL) {
    if (SG_UNBOUNDP(fallback)) return SG_FALSE;
    else return fallback;
  } else {
    return p[index];
  }
}

SgObject Sg_WeakVectorSet(SgWeakVector *v, int index, SgObject value)
{
  SgObject *p;
  if (index < 0 || index >= v->size) {
    Sg_Error(UC("argument out of range: %d"), index);
  }
  p = (SgObject*)v->pointers;
  /* unregister the location if it was registered before */
  if (p[index] == NULL || SG_PTRP(p[index])) {
    Sg_UnregisterDisappearingLink((void **)&p[index]);
  }
  p[index] = value;
  /* register the location if the value is a heap object */
  if (SG_PTRP(value)) {
    Sg_RegisterDisappearingLink((void **)&p[index], (void *)value);
  }
  return SG_UNDEF;
}

/* weak box is not an SgObject. just for wrapper */
struct SgWeakBoxRec
{
  void *ptr;
  int   registered;
};

static void wbox_setvalue(SgWeakBox *wbox, void *value)
{
  void *base = Sg_GCBase(value);
  wbox->ptr = value;
  if (base != NULL) {
    Sg_RegisterDisappearingLink((void *)&wbox->ptr, base);
    wbox->registered = TRUE;
  } else {
    wbox->registered = FALSE;
  }
}

SgWeakBox* Sg_MakeWeakBox(void *value)
{
  SgWeakBox *wbox = SG_NEW_ATOMIC(SgWeakBox);
  wbox_setvalue(wbox, value);
  return wbox;
}

int Sg_WeakBoxEmptyP(SgWeakBox *wbox)
{
  return (wbox->registered && wbox->ptr == NULL);
}

void Sg_WeakBoxSet(SgWeakBox *wbox, void *value)
{
  if (wbox->registered) {
    Sg_UnregisterDisappearingLink((void *)&wbox->ptr);
    wbox->registered = FALSE;
  }
  wbox_setvalue(wbox, value);
}

void* Sg_WeakBoxRef(SgWeakBox *wbox)
{
  return wbox->ptr;
}


#define MARK_GONE_ENTRY(ht, e) (ht->goneEntries++)

static uint32_t weak_key_hash(const SgHashCore *hc, intptr_t key)
{
  SgWeakHashTable *wh = SG_WEAK_HASHTABLE(hc->data);
  SgWeakBox *box = (SgWeakBox *)key;
  intptr_t realkey = (intptr_t)Sg_WeakBoxRef(box);

  if (Sg_WeakBoxEmptyP(box)) {
    return 0;
  } else {
    uint32_t k = wh->hasher(hc, realkey);
    return k;
  }
}

static int weak_key_compare(const SgHashCore *hc, intptr_t key,
			    intptr_t entryKey)
{
  SgWeakHashTable *wh = SG_WEAK_HASHTABLE(hc->data);
  SgWeakBox *box = (SgWeakBox *)key;
  intptr_t realkey = (intptr_t)Sg_WeakBoxRef(box);
  if (Sg_WeakBoxEmptyP(box)) {
    return FALSE;
  } else {
    return wh->compare(hc, key, realkey);
  }
}

SgObject Sg_MakeWeakHashTableSimple(SgHashType type,
				    SgWeakness weakness,
				    int initSize,
				    SgObject defaultValue)
{
  SgWeakHashTable *wh = SG_NEW(SgWeakHashTable);
  SG_SET_CLASS(wh, SG_CLASS_WEAK_HASHTABLE);
  wh->weakness = weakness;
  wh->type = type;
  wh->defaultValue = defaultValue;
  wh->goneEntries = 0;

  if (weakness & SG_WEAK_KEY) {
    if (!Sg_HashCoreTypeToProcs(type, &wh->hasher, &wh->compare)) {
      Sg_Error(UC("[internal error] Sg_MakeWeakHashTableSimple: unsupported type: %d"), type);
    }
    Sg_HashCoreInitGeneral(&wh->core, weak_key_hash, weak_key_compare,
			   initSize, wh);
  } else {
    Sg_HashCoreInitSimple(&wh->core, type, initSize, wh);
  }
  return SG_OBJ(wh);
}

SgObject Sg_WeakHashTableCopy(SgWeakHashTable *src)
{
  SgWeakHashTable *wh = SG_NEW(SgWeakHashTable);
  SG_SET_CLASS(wh, SG_CLASS_WEAK_HASHTABLE);
  wh->weakness = src->weakness;
  wh->type = src->type;
  wh->defaultValue = src->defaultValue;
  wh->hasher = src->hasher;
  wh->compare = src->compare;
  Sg_HashCoreCopy(&wh->core, &src->core);
  return SG_OBJ(wh);
}

SgObject Sg_WeakHashTableRef(SgWeakHashTable *table,
			     SgObject key, SgObject fallback)
{
  SgHashEntry *e = Sg_HashCoreSearch(SG_WEAK_HASHTABLE_CORE(table),
				     (intptr_t)key, SG_HASH_GET);

  if (!e) return fallback;
  if (table->weakness & SG_WEAK_VALUE) {
    void *val = Sg_WeakBoxRef((SgWeakBox*)e->value);
    if (Sg_WeakBoxEmptyP((SgWeakBox*)e->value)) return table->defaultValue;
    ASSERT(val != NULL);
    return SG_OBJ(val);
  } else {
    return SG_HASH_ENTRY_VALUE(e);
  }
}

SgObject Sg_WeakHashTableSet(SgWeakHashTable *table,
			     SgObject key, SgObject value, int flags)
{
  SgHashEntry *e;
  intptr_t proxy;

  if (table->weakness & SG_WEAK_KEY) {
    proxy = (intptr_t)Sg_MakeWeakBox(key);
  } else {
    proxy = (intptr_t)key;
  }

  e = Sg_HashCoreSearch(SG_WEAK_HASHTABLE_CORE(table), proxy,
			(flags & SG_HASH_NO_CREATE)
			   ? SG_HASH_GET: SG_HASH_CREATE);
  if (!e) return SG_UNBOUND;
  if (table->weakness & SG_WEAK_VALUE) {
    if (flags & SG_HASH_NO_OVERWRITE && e->value) {
      void *val = Sg_WeakBoxRef((SgWeakBox *)e->value);
      if (!Sg_WeakBoxEmptyP((SgWeakBox *)e->value)) {
	return SG_OBJ(val);
      }
    }
    SG_HASH_ENTRY_SET_VALUE(e, Sg_MakeWeakBox(value));
    return value;
  } else {
    if (flags & SG_HASH_NO_OVERWRITE && e->value) {
      return SG_HASH_ENTRY_VALUE(e);
    }
    SG_HASH_ENTRY_SET_VALUE(e, value);
    return value;
  }
}

SgObject Sg_WeakHashTableDelete(SgWeakHashTable *table,
				SgObject key)
{
  SgHashEntry *e = Sg_HashCoreSearch(SG_WEAK_HASHTABLE_CORE(table),
				     (intptr_t)key, SG_HASH_DELETE);
  if (e && e->value) {
    if (table->weakness & SG_WEAK_VALUE) {
      void *val = Sg_WeakBoxRef((SgWeakBox*)e->value);
      if (!Sg_WeakBoxEmptyP((SgWeakBox*)e->value))
	return SG_OBJ(val);
      else
	return SG_UNBOUND;
    } else {
      return SG_HASH_ENTRY_VALUE(e);
    }
  } else {
    return SG_UNBOUND;
  }
}

SgObject Sg_WeakHashTableKeys(SgWeakHashTable *table)
{
  SgWeakHashIter iter;
  SgObject h = SG_NIL, t = SG_NIL, k, v;
  Sg_WeakHashIterInit(&iter, table);
  while (Sg_WeakHashIterNext(&iter, &k, &v)) {
    SG_APPEND1(h, t, k);
  }
  return h;
}

SgObject Sg_WeakHashTableValues(SgWeakHashTable *table)
{
  SgWeakHashIter iter;
  SgObject h = SG_NIL, t = SG_NIL, k, v;
  Sg_WeakHashIterInit(&iter, table);
  while (Sg_WeakHashIterNext(&iter, &k, &v)) {
    SG_APPEND1(h, t, v);
  }
  return h;
}


void Sg_WeakHashIterInit(SgWeakHashIter *iter,
			 SgWeakHashTable *table)
{
  Sg_HashIterInit(SG_WEAK_HASHTABLE_CORE(table), &iter->iter);
  iter->table = table;
}

int Sg_WeakHashIterNext(SgWeakHashIter *iter,
			SgObject *key, SgObject *value)
{
  for (;;) {
    SgHashEntry *e = Sg_HashIterNext(&iter->iter);
    if (e == NULL) return FALSE;
    if (iter->table->weakness & SG_WEAK_KEY) {
      SgWeakBox *box = (SgWeakBox *)e->key;
      SgObject realkey = SG_OBJ(Sg_WeakBoxRef(box));
      if (Sg_WeakBoxEmptyP(box)) {
	MARK_GONE_ENTRY(iter->table, e);
	continue;
      }
      *key = realkey;
    } else {
      *key = (SgObject)e->key;
    }

    if (iter->table->weakness & SG_WEAK_VALUE) {
      SgWeakBox *box = (SgWeakBox *)e->value;
      SgObject realval = SG_OBJ(Sg_WeakBoxRef(box));
      if (Sg_WeakBoxEmptyP(box)) {
	*value = iter->table->defaultValue;
      } else {
	*value = realval;
      }
    } else {
      *value = (SgObject)e->value;
    }
    return TRUE;
  }
}

