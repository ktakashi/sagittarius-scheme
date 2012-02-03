/* treemap.c                                              -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2011  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/treemap.h"
#include "sagittarius/collection.h"
#include "sagittarius/error.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/vm.h"
#include "sagittarius/writer.h"

static void treemap_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgTreeMap *tm = SG_TREEMAP(obj);
  Sg_Printf(port, UC("#<treemap %p (%d entries)>"), tm, tm->entryCount);
}

SG_DEFINE_BUILTIN_CLASS(Sg_TreeMapClass, treemap_print, NULL, NULL, NULL,
			SG_CLASS_ORDERED_DICTIONARY_CPL);

static SgTreeMap* make_treemap(int scm)
{
  SgTreeMap *tc = SG_NEW(SgTreeMap);
  SG_SET_CLASS(tc, SG_CLASS_TREE_MAP);
  if (scm) {
    tc->schemep = TRUE;
  }
  tc->entryCount = 0;
  return tc;
}

SgObject Sg_MakeGenericCTreeMap(SgTreeCompareProc *cmp,
				SgTreeRefProc *ref,
				SgTreeSetProc *set,
				SgTreeDeleteProc *remove,
				SgTreeCopyProc *copy,
				SgTreeIterInitProc *iter,
				SgTreeRefProc *higher,
				SgTreeRefProc *lower)
{
  SgTreeMap *tc = make_treemap(FALSE);
  ASSERT(cmp && ref && set && remove && copy && iter);
  SG_TREEMAP_C_PROC(tc, cmp) = cmp;
  SG_TREEMAP_C_PROC(tc, ref) = ref;
  SG_TREEMAP_C_PROC(tc, set) = set;
  SG_TREEMAP_C_PROC(tc, remove) = remove;
  SG_TREEMAP_C_PROC(tc, copy) = copy;
  SG_TREEMAP_C_PROC(tc, iter) = iter;
  SG_TREEMAP_C_PROC(tc, higher) = higher;
  SG_TREEMAP_C_PROC(tc, lower) = lower;
  tc->root = (intptr_t)NULL;
  return SG_OBJ(tc);
}

SgObject Sg_MakeGenericSchemeTreeMap(SgObject cmp,
				     SgObject ref,
				     SgObject set,
				     SgObject remove,
				     SgObject copy)
{
  SgTreeMap *tc = make_treemap(TRUE);
  SG_TREEMAP_SCM_PROC(tc, cmp) = cmp;
  SG_TREEMAP_SCM_PROC(tc, ref) = ref;
  SG_TREEMAP_SCM_PROC(tc, set) = set;
  SG_TREEMAP_SCM_PROC(tc, remove) = remove;
  SG_TREEMAP_SCM_PROC(tc, copy) = copy;
  tc->root = (intptr_t)SG_FALSE;
  return SG_OBJ(tc);
}

SgObject Sg_MakeDefaultTreeMap(SgTreeCompareProc *cmp)
{
  return Sg_MakeRBTreeMap(cmp);
}

SgObject Sg_TreeMapCopy(const SgTreeMap *src)
{
  if (SG_SCHEME_TREEMAP_P(src)) {
    SgTreeMap *tc = make_treemap(TRUE);
    SgObject root = Sg_Apply1(src->procs.scm.copy, SG_OBJ(src->root));
    /* for Scheme, we do not provide the interface to access
       internal procedures, so it needs to be copied here.
     */
    SG_TREEMAP_SCM_PROC(tc, cmp)    = SG_TREEMAP_SCM_PROC(src, cmp);
    SG_TREEMAP_SCM_PROC(tc, ref)    = SG_TREEMAP_SCM_PROC(src, ref);
    SG_TREEMAP_SCM_PROC(tc, set)    = SG_TREEMAP_SCM_PROC(src, set);
    SG_TREEMAP_SCM_PROC(tc, remove) = SG_TREEMAP_SCM_PROC(src, remove);
    SG_TREEMAP_SCM_PROC(tc, copy)   = SG_TREEMAP_SCM_PROC(src, copy);
    tc->root = (intptr_t)root;
    tc->entryCount = src->entryCount;
    return SG_OBJ(tc);
  } else {
    return SG_TREEMAP_C_PROC(src, copy)(src);
  }
}

SgTreeEntry* Sg_TreeMapCoreRef(SgTreeMap *tm, SgObject key)
{
  return SG_TREEMAP_C_PROC(tm, ref)(tm, key);
}

SgTreeEntry* Sg_TreeMapCoreSet(SgTreeMap *tm, SgObject key, SgObject value,
			   int flags)
{
  return SG_TREEMAP_C_PROC(tm, set)(tm, key, value, flags);
}

/* These APIs are mere dispatchers. */
SgObject Sg_TreeMapRef(SgTreeMap *tm, SgObject key,
		       SgObject fallback)
{
  if (SG_SCHEME_TREEMAP_P(tm)) {
    return Sg_Apply2(SG_TREEMAP_SCM_PROC(tm, ref), key, fallback);
  } else {
    SgTreeEntry *e = SG_TREEMAP_C_PROC(tm, ref)(tm, key);
    return (e) ? SG_OBJ(e->value) : fallback;
  }
}

SgObject Sg_TreeMapSet(SgTreeMap *tm, SgObject key, SgObject value,
		       int flags)
{
  if (SG_SCHEME_TREEMAP_P(tm)) {
    return Sg_Apply2(SG_TREEMAP_SCM_PROC(tm, set), key, value);
  } else {
    SgTreeEntry *e = SG_TREEMAP_C_PROC(tm, set)(tm, key, value, flags);
    return SG_OBJ(e->value);
  }
}

SgObject Sg_TreeMapDelete(SgTreeMap *tm, SgObject key)
{
  if (SG_SCHEME_TREEMAP_P(tm)) {
    return Sg_Apply1(SG_TREEMAP_SCM_PROC(tm, remove), key);
  } else {
    return SG_TREEMAP_C_PROC(tm, remove)(tm, key);
  }
}

/* iterator */
void Sg_TreeIterInit(SgTreeIter *iter,
		     SgTreeMap *tm, SgTreeEntry *start)
{
  SG_TREEMAP_C_PROC(tm, iter)(iter, tm, start);
}

SgTreeEntry* Sg_TreeIterNext(SgTreeIter *iter)
{
  return iter->next(iter);
}

int Sg_TreeIterHasNext(SgTreeIter *iter)
{
  return !iter->end;
}

SgTreeEntry* Sg_TreeMapHigherEntry(SgTreeMap *tm, SgObject key)
{
  if (!SG_SCHEME_TREEMAP_P(tm) &&
      SG_TREEMAP_C_PROC(tm, higher)) {
    return SG_TREEMAP_C_PROC(tm, higher)(tm, key);
  }
  Sg_ImplementationRestrictionViolation(SG_INTERN("treemap-higher"),
					SG_MAKE_STRING("given treemap does not "
						       "support higher "
						       "navigation."),
					tm);
  return NULL;			/* dummy */
}

SgTreeEntry* Sg_TreeMapLowerEntry(SgTreeMap *tm, SgObject key)
{
  if (!SG_SCHEME_TREEMAP_P(tm) &&
      SG_TREEMAP_C_PROC(tm, lower)) {
    return SG_TREEMAP_C_PROC(tm, lower)(tm, key);
  }
  Sg_ImplementationRestrictionViolation(SG_INTERN("treemap-lower"),
					SG_MAKE_STRING("given treemap does not "
						       "support lower "
						       "navigation."),
					tm);
  return NULL;			/* dummy */
}

int Sg_TreeMapEq(SgTreeMap *a, SgTreeMap *b)
{
  SgTreeIter ai, bi;
  SgTreeEntry *ae, *be;
  if (a->entryCount != b->entryCount) return FALSE;
  Sg_TreeIterInit(&ai, a, NULL);
  Sg_TreeIterInit(&bi, b, NULL);
  for (;;) {
    ae = Sg_TreeIterNext(&ai);
    be = Sg_TreeIterNext(&bi);
    if (ae == NULL) {
      if (be == NULL) return TRUE;
      else return FALSE;
    }
    if (be == NULL) return FALSE;
    if (ae->key != be->key || ae->value != be->value) return FALSE;
  }
}
