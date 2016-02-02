/* weak.c                                          -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2016  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/string.h"
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

static SgClass *weak_hashtable_cpl[] = {
  SG_CLASS_HASHTABLE,
  SG_CLASS_DICTIONARY,
  SG_CLASS_COLLECTION,
  SG_CLASS_TOP,
  NULL,
};

SG_DEFINE_BUILTIN_CLASS(Sg_WeakHashTableClass, whash_print, NULL, NULL, NULL,
			weak_hashtable_cpl);


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
      Sg_Error(UC("weak-vector-ref: argument out of range: %d"), index);
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
    Sg_Error(UC("weak-vector-set!: argument out of range: %d"), index);
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

/* weak box is a SgObject. but not public */
static void wbox_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgWeakBox *wb = SG_WEAK_BOX(obj);
  /* Don't even try to print ptr as if it's a Scheme object. There is
     no guarantee!! */
  Sg_Printf(port, UC("#<weak-box %p:%d>"), wb->ptr, wb->registered);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_WeakBoxClass, wbox_print);


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
  SG_SET_CLASS(wbox, SG_CLASS_WEAK_BOX);
  wbox_setvalue(wbox, value);
  return wbox;
}

int Sg_WeakBoxEmptyP(SgWeakBox *wbox)
{
  /* if the content is static allocated, then we can't return
     empty. */
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
  SgWeakBox *box;
  intptr_t realkey;

  if (SG_WEAK_BOXP(key)) {
    box = (SgWeakBox *)key;
    if (Sg_WeakBoxEmptyP(box)) {
      return 0;
    }
    realkey = (intptr_t)Sg_WeakBoxRef(box);
  } else {
    realkey = key;
  }
  return wh->hasher(hc, realkey);
}

static int weak_key_compare(const SgHashCore *hc, intptr_t key,
			    intptr_t entryKey)
{
  SgWeakHashTable *wh = SG_WEAK_HASHTABLE(hc->data);
  SgWeakBox *box;  
  intptr_t realkey, realentrykey;
  if (SG_WEAK_BOXP(key)) {
    box = (SgWeakBox *)key;
    if (Sg_WeakBoxEmptyP(box)) return FALSE;
    realkey = (intptr_t)Sg_WeakBoxRef(box);
  } else {
    realkey = key;
  }
  /* entry key must always be weak box */
  box = (SgWeakBox *)entryKey;
  realentrykey = (intptr_t)Sg_WeakBoxRef(box);
  if (Sg_WeakBoxEmptyP(box)) {
    return FALSE;
  } else {
    return wh->compare(hc, realkey, realentrykey);
  }
}


/* Operations */
typedef struct gc_value_rec 
{
  SgWeakHashTable *table;
  intptr_t         key;
} gc_value_t;

static SgWeakHashTable * make_weak_hashtable(SgHashType type,
					     SgWeakness weakness,
					     SgObject defaultValue);
static void weak_hashtable_create_entry(SgHashCore *core, SgHashEntry *e);

static SgObject weak_hashtable_ref(SgObject table, SgHashEntry *e, int flags)
{
  if (SG_WEAK_HASHTABLE_WEAKNESS(table) & SG_WEAK_VALUE) {
    /* get value first so that it won't be GCed */
    void *val = Sg_WeakBoxRef((SgWeakBox*)e->value);
    if (Sg_WeakBoxEmptyP((SgWeakBox*)e->value)) 
      return SG_WEAK_HASHTABLE_DEFAULT_VALUE(table);
    ASSERT(val != NULL);
    return SG_OBJ(val);
  } else {
    return SG_HASH_ENTRY_VALUE(e);
  }
}

static SgObject weak_hashtable_delete_rec(SgObject table, SgObject key);
/* ugly solution for managing entry count of weak hash table. */
static void key_finalizer(SgObject z, void *data)
{
  /* when key is gone, means the entry is gone.
     so we want to decrease the entry count to avoid the unnecessary
     rehash operation.
   */
  SgWeakHashTable *table = SG_WEAK_HASHTABLE(data);
  SgObject e = weak_hashtable_delete_rec(table, z);
  /* maybe we shouldn't support SG_WEAK_REMOVE_BOTH */
  if (e && SG_UNBOUNDP(e)) {
    if ((table->weakness & SG_WEAK_VALUE) &&
	(table->weakness & SG_WEAK_REMOVE)) {
      Sg_UnregisterFinalizer(e);
    }
  }
  /* it's gone so decrease count manually... */
  if (!e) {
    SG_WEAK_HASHTABLE_CORE(data)->entryCount--;
  }
}

static void value_finalizer(SgObject z, void *data)
{
  /* when key is gone, means the entry is gone.
     so we want to decrease the entry count to avoid the unnecessary
     rehash operation.
  */
  SgWeakHashTable *table = ((gc_value_t *)data)->table;
  intptr_t key = ((gc_value_t *)data)->key;
  SgObject e = NULL;		/* dummy */
  SgHashEntry *et = Sg_HashCoreSearch(SG_WEAK_HASHTABLE_CORE(table),
				      (intptr_t)key, SG_DICT_GET, 
				      SG_HASH_NO_ERROR);

  if (et) e = weak_hashtable_ref(table, et, 0);

  /* ok, it's still there, so delete it */
  if (SG_EQ(e, z)) {
    e = weak_hashtable_delete_rec(table, SG_OBJ(key));
  }

  /* in case */
  /* maybe we shouldn't support SG_WEAK_REMOVE_BOTH */
  if (key && (table->weakness & SG_WEAK_KEY)) {
    Sg_UnregisterFinalizer(SG_OBJ(key));
    Sg_UnregisterDisappearingLink((void *)&((gc_value_t *)data)->key);
  }
  /* it's gone so decrease count manually... */
  if (!e) {
    SG_WEAK_HASHTABLE_CORE(data)->entryCount--;
  }
#if 0
  else {
    Sg_Printf(Sg_StandardErrorPort(), UC("%x, %S, %S"), table,key,z);
    fprintf(stderr, "[%p]\n", z);
#endif
}

static SgObject weak_hashtable_set(SgObject table,
				   SgHashEntry *e, SgObject value, int flags)
{ 
  if (SG_WEAK_HASHTABLE_WEAKNESS(table) & SG_WEAK_VALUE) {
    if (e->value && (flags & SG_HASH_NO_OVERWRITE)) {
      void *val = Sg_WeakBoxRef((SgWeakBox *)e->value);
      if (!Sg_WeakBoxEmptyP((SgWeakBox *)e->value)) {
	return SG_OBJ(val);
      }
    }
    if (SG_WEAK_HASHTABLE_WEAKNESS(table) & SG_WEAK_REMOVE) {
      gc_value_t *data;
      intptr_t key = e->key;
      void *base;
      if (SG_WEAK_BOXP(key)) {
	key = (intptr_t)Sg_WeakBoxRef(SG_WEAK_BOX(key));
      }
      base = Sg_GCBase(SG_OBJ(key));
      if (e->value) {
	/* not sure if we need this */
	void *val = Sg_WeakBoxRef((SgWeakBox *)e->value);
	if (!Sg_WeakBoxEmptyP((SgWeakBox *)e->value)) {
	  Sg_UnregisterFinalizer(val);
	}
      }
      data = SG_NEW(gc_value_t);
      data->table = table;
      data->key = key;
      if (base) {
	Sg_RegisterDisappearingLink((void *)&data->key, base);
      }
      Sg_RegisterFinalizer(value, value_finalizer, data);
    }
    if (e->value) {
      Sg_WeakBoxSet((SgWeakBox *)e->value, value);
    } else {
      (void)SG_HASH_ENTRY_SET_VALUE(e, Sg_MakeWeakBox(value));
    }
    return value;
  } else {
    if (flags & SG_HASH_NO_OVERWRITE && e->value) {
      return SG_HASH_ENTRY_VALUE(e);
    }
    return SG_HASH_ENTRY_SET_VALUE(e, value);
  }
}

static SgObject weak_hashtable_delete_rec(SgObject table, SgObject key)
{
  SgHashEntry *e = Sg_HashCoreSearch(SG_WEAK_HASHTABLE_CORE(table),
				     (intptr_t)key, SG_DICT_DELETE, 0);
  if (e && e->value) {
    if (SG_WEAK_HASHTABLE_WEAKNESS(table) & SG_WEAK_VALUE) {
      void *val = Sg_WeakBoxRef((SgWeakBox*)e->value);
      if (!Sg_WeakBoxEmptyP((SgWeakBox*)e->value))
	return SG_OBJ(val);
      else
	return SG_UNBOUND;
    } else {
      return SG_HASH_ENTRY_VALUE(e);
    }
  } else {
    return NULL;
  }
}

static SgObject weak_hashtable_delete(SgObject table, SgObject key)
{
  /* remove finalizer */
  SgObject v;
  if (SG_WEAK_HASHTABLE_WEAKNESS(table) & SG_WEAK_KEY) {
    Sg_UnregisterFinalizer(key);
  }
  v = weak_hashtable_delete_rec(table, key);
  if (v) {
    /* remove value finalizer if there is.
       NOTE: if it's gone, then it's removed anyway
     */
    if (!SG_UNBOUNDP(v)) {
      if ((SG_WEAK_HASHTABLE_WEAKNESS(table) & SG_WEAK_VALUE) &&
	  (SG_WEAK_HASHTABLE_WEAKNESS(table) & SG_WEAK_REMOVE)) {
	Sg_UnregisterFinalizer(v);
      }
    }
    return v;
  } else {
    return SG_UNBOUND;
  }
}

static SgObject weak_hashtable_copy(SgObject table, int mutableP)
{
  SgWeakHashTable *src = SG_WEAK_HASHTABLE(table);
  SgWeakHashTable *wh = make_weak_hashtable(SG_WEAK_HASHTABLE_TYPE(src),
					    src->weakness,
					    src->defaultValue);
  wh->hasher = src->hasher;
  wh->compare = src->compare;
  /* FIXME maybe we should initialise the core? */
  SG_WEAK_HASHTABLE_CORE(wh)->create_entry = weak_hashtable_create_entry;
  /* FIXME this is probably wrong since it just copies entries.
           the weak boxes would be shared between tables so if
	   one table is modified then other would also be modified
   */
  Sg_HashCoreCopy(SG_WEAK_HASHTABLE_CORE(wh), SG_WEAK_HASHTABLE_CORE(src));
  /* the data must be copied one */
  SG_WEAK_HASHTABLE_CORE(wh)->data = wh;
  return SG_OBJ(wh);  
}

extern SgHashEntry * hash_iter_next(SgHashIter *itr, SgObject *key, 
				    SgObject *value);
static SgHashEntry * weak_hash_iter_next(SgHashIter *iter, 
					 SgObject *key, SgObject *value)
{
  SgWeakHashTable *wh = SG_WEAK_HASHTABLE(iter->table);
  for (;;) {
    SgHashEntry *e = hash_iter_next(iter, NULL, NULL);
    if (e == NULL) return NULL;
    if (wh->weakness & SG_WEAK_KEY) {
      SgWeakBox *box = (SgWeakBox *)e->key;
      SgObject realkey = SG_OBJ(Sg_WeakBoxRef(box));
      if (Sg_WeakBoxEmptyP(box)) {
	MARK_GONE_ENTRY(wh, e);
	continue;
      }
      if (key) *key = realkey;
    } else {
      if (key) *key = (SgObject)e->key;
    }
    if (wh->weakness & SG_WEAK_VALUE) {
      SgWeakBox *box = (SgWeakBox *)e->value;
      SgObject realval = SG_OBJ(Sg_WeakBoxRef(box));
      if (Sg_WeakBoxEmptyP(box)) {
	if (value) *value = wh->defaultValue;
      } else {
	if (value) *value = realval;
      }
    } else {
      if (value) *value = (SgObject)e->value;
    }
    /* rather useless but required...*/
    return e;
  }
}
/* avoid infinite loop */
extern void hash_iter_init(SgHashCore *core, SgHashIter *itr);
static void weak_hashtable_init_iter(SgObject table, SgHashIter *iter)
{
  hash_iter_init(SG_WEAK_HASHTABLE_CORE(table), iter);
  iter->table = table;
  iter->iter_next = weak_hash_iter_next;
}


static SgHashOpTable weak_hashtable_operations = {
  weak_hashtable_ref,
  weak_hashtable_set,
  weak_hashtable_delete,
  weak_hashtable_copy,
  weak_hashtable_init_iter,
};

static SgWeakHashTable * make_weak_hashtable(SgHashType type,
					     SgWeakness weakness,
					     SgObject defaultValue)
{
  SgWeakHashTable *wh = SG_NEW(SgWeakHashTable);
  SG_SET_CLASS(wh, SG_CLASS_WEAK_HASHTABLE);
  wh->weakness = weakness;
  SG_WEAK_HASHTABLE_TYPE(wh) = type;
  wh->defaultValue = defaultValue;
  SG_HASHTABLE_OPTABLE(wh) = &weak_hashtable_operations;
  return wh;
}

static void weak_hashtable_create_entry(SgHashCore *core, SgHashEntry *e)
{
  SgObject table = SG_OBJ(core->data);
  if (SG_WEAK_HASHTABLE_WEAKNESS(table) & SG_WEAK_KEY) {
    SgObject key = SG_OBJ(e->key);
    e->key = (intptr_t)Sg_MakeWeakBox(key);
    /* needed for managing entryCount... */
    Sg_RegisterFinalizer(key, key_finalizer, table);
  }
}

SgObject Sg_MakeWeakHashTableSimple(SgHashType type,
				    SgWeakness weakness,
				    int initSize,
				    SgObject defaultValue)
{
  SgWeakHashTable *wh = make_weak_hashtable(type, weakness, defaultValue);

  if (weakness & SG_WEAK_KEY) {
    if (!Sg_HashCoreTypeToProcs(type, &wh->hasher, &wh->compare)) {
      Sg_Error(UC("Sg_MakeWeakHashTableSimple: unsupported type: %d"), type);
    }
    /* wh->keyStore = Sg_MakeWeakVector(initSize); */
    Sg_HashCoreInitGeneral(SG_WEAK_HASHTABLE_CORE(wh), weak_key_hash,
			   weak_key_compare, initSize, wh);
  } else {
    Sg_HashCoreInitSimple(SG_WEAK_HASHTABLE_CORE(wh), type, initSize, wh);
  }
  SG_WEAK_HASHTABLE_CORE(wh)->create_entry = weak_hashtable_create_entry;
  return SG_OBJ(wh);
}

SgObject Sg_MakeWeakHashTable(SgObject hasher,
			      SgObject compare,
			      SgWeakness weakness,
			      int initSize,
			      SgObject defaultValue)
{
  SgWeakHashTable *wh = 
    SG_WEAK_HASHTABLE(Sg_MakeWeakHashTableSimple(SG_HASH_GENERAL,
						 weakness,
						 initSize,
						 defaultValue));
  SG_WEAK_HASHTABLE_CORE(wh)->generalHasher = hasher;
  SG_WEAK_HASHTABLE_CORE(wh)->generalCompare = compare;
  return wh;
}

SgObject Sg_WeakHashTableCopy(SgWeakHashTable *src)
{
  return weak_hashtable_copy(src, TRUE);
}

SgObject Sg_WeakHashTableRef(SgWeakHashTable *table,
			     SgObject key, SgObject fallback)
{
  SgHashEntry *e = Sg_HashCoreSearch(SG_WEAK_HASHTABLE_CORE(table),
				     (intptr_t)key, SG_DICT_GET, 0);
  if (!e) return fallback;
  return weak_hashtable_ref(table, e, 0);
}

SgObject Sg_WeakHashTableSet(SgWeakHashTable *table,
			     SgObject key, SgObject value, int flags)
{
  SgHashEntry *e;

  if (SG_IMMUTABLE_HASHTABLE_P(table)) {
    Sg_Error(UC("attemp to modify immutable hashtable"));
    return SG_UNDEF;
  }
  e = Sg_HashCoreSearch(SG_WEAK_HASHTABLE_CORE(table), (intptr_t)key,
			(flags & SG_HASH_NO_CREATE)
			   ? SG_DICT_GET: SG_DICT_CREATE,
			0);

  if (!e) return SG_UNBOUND;
  return weak_hashtable_set(table, e, value, flags);
}

SgObject Sg_WeakHashTableDelete(SgWeakHashTable *table,
				SgObject key)
{
  if (SG_IMMUTABLE_HASHTABLE_P(table)) {
    Sg_Error(UC("attemp to modify immutable hashtable"));
    return SG_UNDEF;
  }
  return weak_hashtable_delete(table, key);
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
  weak_hashtable_init_iter(table, iter);
}

int Sg_WeakHashIterNext(SgWeakHashIter *iter,
			SgObject *key, SgObject *value)
{
  return weak_hash_iter_next(iter, key, value) != NULL;
}

/* for GC friendliness */
int Sg_WeakHashTableShrink(SgWeakHashTable *table)
{
  SgHashIter iter;
  SgHashEntry *e = NULL;
  int count = 0;
  Sg_HashIterInit(table, &iter);
  while ((e = Sg_HashIterNext(&iter, NULL, NULL)) != NULL) {
    /* feeling like this is actually useless.
       if the weak key is gone, how could we delete
       the entry? */
    if (table->weakness & SG_WEAK_KEY) {
      SgWeakBox *box = (SgWeakBox *)e->key;
      if (box && Sg_WeakBoxEmptyP(box)) {
	Sg_WeakHashTableDelete(table, SG_OBJ(e->key));
	count++;
	continue;
      }
    }
    if (table->weakness & SG_WEAK_VALUE) {
      SgWeakBox *box = (SgWeakBox *)e->value;
      if (box && Sg_WeakBoxEmptyP(box)) {
	Sg_WeakHashTableDelete(table, SG_OBJ(e->key));
	count++;
	continue;
      }
    }
  }
  return count;
}
