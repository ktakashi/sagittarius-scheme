/* clos.c                                                 -*- coding: utf-8; -*-
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
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/clos.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/charset.h"
#include "sagittarius/closure.h"
#include "sagittarius/code.h"
#include "sagittarius/codec.h"
#include "sagittarius/collection.h"
#include "sagittarius/core.h"
#include "sagittarius/error.h"
#include "sagittarius/generic.h"
#include "sagittarius/gloc.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/keyword.h"
#include "sagittarius/library.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/record.h"
#include "sagittarius/string.h"
#include "sagittarius/subr.h"
#include "sagittarius/symbol.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/treemap.h"
#include "sagittarius/unicode.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"
#include "sagittarius/weak.h"
#include "sagittarius/writer.h"
#include "sagittarius/builtin-keywords.h"

static void slot_acc_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgSlotAccessor *acc = SG_SLOT_ACCESSOR(obj);
  Sg_Printf(port, UC("#<slot-accessor %A:%A>"),
	    acc->klass?acc->klass->name:SG_UNDEF, acc->name);
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SlotAccessorClass, slot_acc_print);

SgClass *Sg_DefaultCPL[] = {
  SG_CLASS_TOP,
  NULL,
};
SgClass *Sg_ObjectCPL[] = {
  SG_CLASS_OBJECT,
  SG_CLASS_TOP,
  NULL,
};

static SgClass *Sg_MethodCPL[] = {
  SG_CLASS_METHOD,
  SG_CLASS_OBJECT,
  SG_CLASS_TOP,
  NULL
};

SG_DEFINE_ABSTRACT_CLASS(Sg_TopClass, NULL);

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_BoolClass, NULL);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_CharClass, NULL);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_UnknownClass, NULL);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_UndefinedClass, NULL);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_EOFObjectClass, NULL);

static void class_print(SgObject, SgPort *, SgWriteContext *);
static void generic_print(SgObject, SgPort *, SgWriteContext *);
static void method_print(SgObject, SgPort *, SgWriteContext *);
static void next_method_print(SgObject, SgPort *, SgWriteContext *);
/* allocate */
static SgObject class_allocate(SgClass *klass, SgObject initargs);
static SgObject generic_allocate(SgClass *klass, SgObject initargs);
static SgObject method_allocate(SgClass *klass, SgObject initargs);

/* compare */
static int object_compare(SgObject x, SgObject y, int equalp);

static SgSlotAccessor* lookup_slot_info(SgClass *klass, SgObject name,
					int raise);

static void init_class(SgClass *klass, const SgChar *name,
		       SgLibrary *lib, SgObject supers, SgSlotAccessor *specs,
		       int flags);

/* helper */
static SgObject class_array_to_names(SgClass **array, int len)
{
  SgObject h = SG_NIL, t = SG_NIL;
  int i;
  for (i = 0; i < len; i++, array++) {
    SG_APPEND1(h, t, (*array)->name);
  }
  return h;
}

static SgObject class_list_to_array(SgObject lst, int len)
{
  SgObject cp;
  SgClass **v, **vp;
  v = vp = SG_NEW_ARRAY(SgClass*, len+1);
  SG_FOR_EACH(cp, lst) {
    if (!Sg_TypeP(SG_CAR(cp), SG_CLASS_CLASS)) {
      Sg_Error(UC("list of classes required, but found non-class object"
		  " %S in %S"), SG_CAR(cp), lst);
    }
    *vp++ = SG_CLASS(SG_CAR(cp));
  }
  *vp = NULL;
  return v;
}

static SgObject class_list_to_names(SgClass **lst, int len)
{
  SgObject h = SG_NIL, t = SG_NIL;
  int i;
  for (i=0; i<len; i++, lst++) {
    SG_APPEND1(h, t, (*lst)->name);
  }
  return h;
}

SG_DEFINE_BASE_CLASS(Sg_ObjectClass, SgInstance,
		     NULL, NULL, NULL, Sg_ObjectAllocate,
		     SG_CLASS_DEFAULT_CPL);
SG_DEFINE_BASE_CLASS(Sg_ClassClass, SgClass,
		     class_print, NULL, NULL, class_allocate,
		     SG_CLASS_OBJECT_CPL);
SG_DEFINE_BASE_CLASS(Sg_GenericClass, SgGeneric,
		     generic_print, NULL, NULL, generic_allocate,
		     SG_CLASS_OBJECT_CPL);
SG_DEFINE_BASE_CLASS(Sg_MethodClass, SgMethod,
		     method_print, NULL, NULL, method_allocate,
		     Sg_MethodCPL);

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_NextMethodClass, next_method_print);

SgObject Sg_AllocateInstance(SgClass *klass)
{
  SgObject obj = SG_NEW2(SgObject, klass->coreSize);
  SgObject *slots;
  int i;

  switch (SG_CLASS_CATEGORY(klass)) {
  case SG_CLASS_BASE:
  case SG_CLASS_SCHEME:
    slots = SG_NEW_ARRAY(SgObject, klass->nfields);
    for (i = 0; i < klass->nfields; i++) {
      slots[i] = SG_UNBOUND;
    }
    SG_INSTANCE(obj)->slots = slots;
  }
  return obj;
}


static SgSlotAccessor *make_slot_accessor(SgClass *klass, SgObject name,
					  int index)
{
  SgSlotAccessor *ac = SG_NEW(SgSlotAccessor);
  SG_SET_CLASS(ac, SG_CLASS_SLOT_ACCESSOR);
  ac->name = name;
  ac->klass = klass;
  ac->index = index;
  ac->getter = NULL;
  ac->setter = NULL;
  ac->getterS = SG_FALSE;
  ac->setterS = SG_FALSE;
  return ac;
}

/*
  we need to calculate required arguments count, basically we just need to 
  -1 for call-next-method.

  NOTE: because of variable flatten in compiler, if a procedure has optional
        argument, the original required arguments count will be +1.
	ex) (lambda (a b . c) ...) ;; -> required argc = 3.
	so if the procedure has optional argument, we need to reduce 2, instead
	of 1.
 */
#define set_method_properties(m, proc)					\
  do {									\
    int opt = SG_PROCEDURE_OPTIONAL(proc), offset=1;			\
    SG_PROCEDURE_OPTIONAL(m) = opt;					\
    /* for call-next-method */						\
    if (opt) offset++;							\
    SG_PROCEDURE_REQUIRED(m) = SG_PROCEDURE_REQUIRED(proc)-offset;	\
  } while (0)

SgObject Sg_AddMethod(SgGeneric *generic, SgMethod *method)
{
  SgObject mp, pair;
  int reqs = SG_GENERIC_MAX_REQARGS(generic), replaced = FALSE, i;
  if (method->generic && method->generic != generic) {
    Sg_Error(UC("method %S already added to a generic function %S"),
	     method, method->generic);
  }
  if (!SG_FALSEP(Sg_Memq(SG_OBJ(method), SG_GENERIC_METHODS(generic)))) {
    Sg_Error(UC("method %S already appears in a method list of generid %S "
		"something wrong in MOP implementation?"),
	     method, method->generic);
  }
  method->generic = generic;
  /* pre-allcate cons pair to avoid triggering GC */
  pair = Sg_Cons(SG_OBJ(method), SG_GENERIC_METHODS(generic));
  if (SG_PROCEDURE_REQUIRED(method) > reqs) {
    reqs = SG_PROCEDURE_REQUIRED(method);
  }
  Sg_LockMutex(&generic->mutex);
  /* Check if a method with the same signature exists */
  SG_FOR_EACH(mp, SG_GENERIC_METHODS(generic)) {
    SgMethod *mm = SG_METHOD(SG_CAR(mp));
    if (SG_PROCEDURE_REQUIRED(method) == SG_PROCEDURE_REQUIRED(mm) &&
	SG_PROCEDURE_OPTIONAL(method) == SG_PROCEDURE_OPTIONAL(mm) &&
	SG_EQ(SG_METHOD_QUALIFIER(method), SG_METHOD_QUALIFIER(mm))) {
      SgClass **sp1 = SG_METHOD_SPECIALIZERS(method);
      SgClass **sp2 = SG_METHOD_SPECIALIZERS(mm);
      for (i = 0; i < SG_PROCEDURE_REQUIRED(method); i++) {
	if (sp1[i] != sp2[i]) break;
      }
      if (i == SG_PROCEDURE_REQUIRED(method)) {
	SG_SET_CAR(mp, SG_OBJ(method));
	replaced = TRUE;
	break;
      }
    }
  }
  if (!replaced) {
    SG_GENERIC_METHODS(generic) = pair;
    SG_GENERIC_MAX_REQARGS(generic) = reqs;
  }
  Sg_UnlockMutex(&generic->mutex);
  return SG_UNDEF;
}

SgObject Sg_RemoveMethod(SgGeneric *gf, SgMethod *m)
{
  SgObject mp;
  if (!SG_METHOD_GENERIC(m) || SG_METHOD_GENERIC(m) != gf) return SG_UNDEF;

  Sg_LockMutex(&gf->mutex);
  mp = SG_GENERIC_METHODS(gf);
  if (SG_PAIRP(mp)) {
    if (SG_EQ(SG_CAR(mp), SG_OBJ(m))) {
      SG_GENERIC_METHODS(gf) = SG_CDR(mp);
    } else {
      while (SG_PAIRP(SG_CDR(mp))) {
	if (SG_EQ(SG_CADR(mp), SG_OBJ(m))) {
	  SG_CDR(mp) = SG_CDDR(mp);
	  SG_METHOD_GENERIC(m) = NULL;
	  break;
	}
	mp = SG_CDR(mp);
      }
    }
  }
  SG_FOR_EACH(mp, SG_GENERIC_METHODS(gf)) {
    /* sync # of required selector */
    if (SG_PROCEDURE_REQUIRED(SG_CAR(mp)) > SG_GENERIC_MAX_REQARGS(gf)) {
      SG_GENERIC_MAX_REQARGS(gf) = SG_PROCEDURE_REQUIRED(SG_CAR(mp));
    }
  }
  Sg_UnlockMutex(&gf->mutex);
  return SG_UNDEF;
}

/* void Sg_AddDirectMethod(SgClass *klass, SgMethod *m) */
/* { */
/*   if (SG_CLASS_CATEGORY(klass) == SG_CLASS_SCHEME) { */
/*     SgObject p = Sg_Cons(SG_OBJ(m), SG_NIL); */
/*     Sg_LockMutex(&klass->mutex); */
/*     if (SG_FALSEP(Sg_Memq(klass->directMethods, SG_OBJ(m)))) { */
/*       SG_SET_CDR(p, klass->directMethods); */
/*       klass->directMethods = p; */
/*     } */
/*     Sg_UnlockMutex(&klass->mutex); */
/*   } */
/* } */

#define filter_in(r_, test_, l_)			\
  do {							\
    SgObject h__ = SG_NIL, t__ = SG_NIL, l__ = l_;	\
    while (1) {						\
      if (SG_NULLP(l__)) {				\
	r_ = h__;					\
	break;						\
      }							\
      if (test_(SG_CAR(l__))) {				\
	SG_APPEND1(h__, t__, SG_CAR(l__));		\
      }							\
      l__ = SG_CDR(l__);				\
    }							\
  } while (0)

/* 
   from A Monotonic Superclass Linearization for Dylan, Appendix B
   http://192.220.96.201/dylan/linearization-oopsla96.html

   C3 linearization
   sort of topological sort I guess. given list must be graphs like
   '((menu choice-widget object)
     (menu popup-mixin)
     (popup-mixin object))
   This indicates like this graph;
   
                <object>
                 /   \
   <choice-widget> ---\--------------+
         |            <popup-mixin>  |
       <menu>            |           |
           \             |           |
           <new-popup-menu> ---------+

   And the result will be like this list;
   (menu choice-widget popup-mixin object)

   This is the Scheme implementation without recursive
   (define (merge-lists sequence)
    (let loop ((rpr '()) ;; seed
	       (ri sequence))
      (if (for-all null? ri)
	  (reverse! rpr)
	  (letrec ((candidate (lambda (c)
				(define (tail? l) (memq c (tail l)))
				(and (not (exists tail? ri))
				     c)))
		   (candidate-at-head
		    (lambda (l)
		      (and (not (null? l))
			   (candidate (head l))))))
	    (let ((next (exists candidate-at-head ri)))
	      (if next
		  (letrec ((remove-next (lambda (l)
					  (if (eq? (head l) next) (tail l) l))))
		    (loop (cons next rpr)
				 (map remove-next ri)))
		  (error 'merge-lists "inconsistent precedence graph")))))))


   TODO It might be goot to export to scheme world, but I don't see any use
   case to use this one, instead of topological sort.
 */
static SgObject merge_lists(SgObject sequence)
{
  SgObject rpr = SG_NIL, next;
  int len = Sg_Length(sequence);
  SgObject *ri, *sp, *tp;
  /* never happen unless we export this function to the Scheme world */
  if (len < 0) Sg_Error(UC("bad list of sequence: %S"), sequence);
  ri = SG_NEW_ARRAY(SgObject, len);
  for (sp = ri; sp < ri+len; sp++, sequence=SG_CDR(sequence))  {
    *sp = SG_CAR(sequence);
  }

  for (;;) {
    /* (for-all null? ri) */
    for (sp = ri; sp < ri+len; sp++) {
      if (!SG_NULLP(*sp)) break;
    }
    if (sp == ri+len) return Sg_ReverseX(rpr);
    next = SG_FALSE;
    /* candidate-at-head */
    for (sp = ri; sp < ri+len; sp++) {
      SgObject c;
      if (!SG_PAIRP(*sp)) continue;
      c = SG_CAR(*sp);
      /* candidate */
      for (tp = ri; tp<ri+len; tp++) {
	if (!SG_PAIRP(*tp)) continue;
	if (!SG_FALSEP(Sg_Memq(c, SG_CDR(*tp)))) break;
      }
      if (tp != ri+len) continue;
      next = c;
      break;
    }
    if (SG_FALSEP(next)) return SG_FALSE;

    rpr = Sg_Cons(next, rpr);
    /* remove-next */
    for (sp = ri; sp<ri+len; sp++) {
      if (SG_PAIRP(*sp) && SG_EQ(next, SG_CAR(*sp))) {
	*sp = SG_CDR(*sp);
      }
    }
  }
  /* not reached */
}

static SgObject deletel(SgObject target, SgObject lst)
{
  SgObject h = SG_NIL, t = SG_NIL, cp;
  SG_FOR_EACH(cp, lst) {
    if (SG_EQ(SG_CAR(cp), target)) continue;
    SG_APPEND1(h, t, SG_CAR(cp));
  }
  return h;
}

SgObject Sg_ComputeCPL(SgClass *klass)
{
  SgObject seqh = SG_NIL, seqt = SG_NIL, ds, dp, result;
  /* a trick to ensure we have <object> <top> at the end of CPL */
  ds = deletel(SG_OBJ(SG_CLASS_OBJECT), klass->directSupers);
  ds = deletel(SG_OBJ(SG_CLASS_TOP), ds);
  ds = Sg_Append2(ds, SG_LIST1(SG_OBJ(SG_CLASS_OBJECT)));

  /* map(cpl-list, c-direct-superclasses */
  SG_FOR_EACH(dp, klass->directSupers) {
    if (!Sg_TypeP(SG_CAR(dp), SG_CLASS_CLASS)) {
      Sg_Error(UC("non-class found in direct superclass list: %S"),
	       klass->directSupers);
    }
    if (SG_CAR(dp) == SG_CLASS_OBJECT || SG_CAR(dp) == SG_CLASS_TOP) continue;
    SG_APPEND1(seqh, seqt, SG_CLASS(SG_CAR(dp))->cpl);
  }
  SG_APPEND1(seqh, seqt, SG_CLASS_OBJECT->cpl);
  SG_APPEND1(seqh, seqt, ds);

  result = merge_lists(seqh);
  if (SG_FALSEP(result)) {
    Sg_Error(UC("discrepancy found in class precedence lists of "
		"the superclasses: %S(%S)"), klass->directSupers, seqh);
  }
  /* add klass itsself */
  return Sg_Cons(SG_OBJ(klass), result);
}

SgObject Sg_ComputeSlots(SgClass *klass)
{
  SgObject slots = SG_NIL;
  SgObject cp, sp;
  SG_FOR_EACH(cp, klass->cpl) {
    ASSERT(Sg_TypeP(SG_CAR(cp), SG_CLASS_CLASS));
    SG_FOR_EACH(sp, SG_CLASS(SG_CAR(cp))->directSlots) {
      SgObject slot = SG_CAR(sp), snam, p;
      ASSERT(SG_PAIRP(slot));
      snam = SG_CAR(slot);
      p = Sg_Assq(snam, slots);
      if (SG_FALSEP(p)) {
	slots = Sg_Cons(Sg_CopyList(slot), slots);
      }
    }
  }
  return Sg_Reverse(slots);
}

SgObject Sg_ComputeGettersAndSetters(SgClass *klass, SgObject slots)
{
  SgObject h = SG_NIL, t = SG_NIL;
  /* direct-slots must be set before compute-getters-and-setters is called
     see clos/core.scm */
  SgObject sp, ds = klass->directSlots;
  int index = 0;
  SG_FOR_EACH(sp, slots) {
    SgSlotAccessor *ac = make_slot_accessor(klass, SG_CAAR(sp), index);
    SgObject rcpl = Sg_Reverse(SG_CDR(klass->cpl)), cp;
    SG_FOR_EACH(cp, rcpl) {
      SgObject check = Sg_Assq(SG_CAAR(sp), ds);
      if (SG_FALSEP(check)) {
	SgSlotAccessor *sac = lookup_slot_info(SG_CLASS(SG_CAR(cp)),
					       SG_CAAR(sp), FALSE);
	if (SG_UNDEFP(sac)) continue;
	if (sac->getter) ac->getter = sac->getter;
	if (sac->setter) ac->setter = sac->setter;
      }
    }

    SG_APPEND1(h, t, SG_OBJ(ac));
    index++;
  }
  return h;
}

int Sg_ApplicableP(SgObject c, SgObject arg)
{
  return !SG_FALSEP(Sg_Memq(c, SG_CLASS(Sg_ClassOf(arg))->cpl));
}

#define PREALLOC_SIZE 32

static SgObject compute_applicable_methods(SgGeneric *gf, SgObject *argv,
					   int argc, int applyargs)
{
  SgObject methods = SG_GENERIC_METHODS(gf), mp, ap;
  SgObject h = SG_NIL, t = SG_NIL;
  SgClass *typev_s[PREALLOC_SIZE], **typev = typev_s;
  int i, nsel;
  if (SG_NULLP(methods)) return SG_NIL;

  nsel = SG_GENERIC_MAX_REQARGS(gf);
  if (nsel > PREALLOC_SIZE) {
    typev = SG_NEW_ATOMIC2(SgClass**, sizeof(SgClass*)*nsel);
  }
  if (applyargs) argc--;
  for (i = 0; i < argc && nsel >= 0; i++, nsel--) {
    typev[i] = Sg_ClassOf(argv[i]);
  }
  if (applyargs && nsel) {
    SG_FOR_EACH(ap, argv[argc]) {
      if (--nsel >= 0) typev[i++] = Sg_ClassOf(SG_CAR(ap));
      argc++;
    }
  }

  SG_FOR_EACH(mp, methods) {
    SgMethod *m = SG_METHOD(SG_CAR(mp));
    SgClass **tp, **sp;
    int n;
    if (argc < SG_PROCEDURE_REQUIRED(m)) continue;
    if (!SG_PROCEDURE_OPTIONAL(m) && argc > SG_PROCEDURE_REQUIRED(m)) continue;
    for (tp = typev, sp = SG_METHOD_SPECIALIZERS(m), n = 0;
	 n < SG_PROCEDURE_REQUIRED(m);
	 tp++, sp++, n++) {
      if (!Sg_SubtypeP(*tp, *sp)) break;
    }
    if (n == SG_PROCEDURE_REQUIRED(m)) SG_APPEND1(h, t, SG_OBJ(m));
  }
  return h;
}

/*
  These functions must be generic, however for now we just put here
  and ignore the others.
 */
static int more_specific_p(SgClass *c1, SgClass *c2, SgClass *arg)
{
  SgClass **cpl;
  if (c1 == arg) return TRUE;
  if (c2 == arg) return FALSE;
  for (cpl = arg->cpa; *cpl; cpl++) {
    if (c1 == *cpl) return TRUE;
    if (c2 == *cpl) return FALSE;
  }
  Sg_Panic("internal error: couldn't determine more specific method.");
  return FALSE;			/* dummy */
}

static int method_more_specific(SgMethod *m1, SgMethod *m2,
				SgClass **targv, int argc)
{
  SgClass **spec1 = SG_METHOD_SPECIALIZERS(m1);
  SgClass **spec2 = SG_METHOD_SPECIALIZERS(m2);
  int i, xreq = SG_PROCEDURE_REQUIRED(m1), yreq = SG_PROCEDURE_REQUIRED(m2);
  for (i = 0; i < argc; i++) {
    if (!SG_EQ(spec1[i], spec2[i])) {
      return more_specific_p(*spec1, *spec2, targv[i]);
    }
  }
  if (xreq > yreq) return TRUE;
  if (xreq < yreq) return FALSE;
  if (SG_PROCEDURE_OPTIONAL(m2)) return TRUE;
  else return FALSE;
}

/* :around :before :after and :primary */
enum {
  PRIMARY_INDEX = 0,
  BEFORE_INDEX,
  AFTER_INDEX,
  AROUND_INDEX,
  QUALIFIER_COUNT
};

static SgObject* sort_method_by_qualifier(SgObject methods, SgObject *result)
{
  SgObject cp, art = SG_NIL, bt = SG_NIL, pt = SG_NIL, aft = SG_NIL;
  int i;
  for (i=0; i<QUALIFIER_COUNT; i++) result[i] = SG_NIL;
  SG_FOR_EACH(cp, methods) {
    SgMethod *m = SG_METHOD(SG_CAR(cp));
    if (SG_EQ(SG_METHOD_QUALIFIER(m), SG_KEYWORD_AROUND)) {
      SG_APPEND1(result[AROUND_INDEX], art, SG_OBJ(m));
    } else if (SG_EQ(SG_METHOD_QUALIFIER(m), SG_KEYWORD_BEFORE)) {
      SG_APPEND1(result[BEFORE_INDEX], bt, SG_OBJ(m));
    } else if (SG_EQ(SG_METHOD_QUALIFIER(m), SG_KEYWORD_PRIMARY)) {
      SG_APPEND1(result[PRIMARY_INDEX], pt, SG_OBJ(m));
    } else if (SG_EQ(SG_METHOD_QUALIFIER(m), SG_KEYWORD_AFTER)) {
      SG_APPEND1(result[AFTER_INDEX], aft, SG_OBJ(m));
    } else {
      /* invalid */
      Sg_Error(UC("wrong method-qualifier %S in method %S"),
	       SG_METHOD_QUALIFIER(m), m);
    }
  }
  return result;
}

static SgObject sort_primary_methods(SgObject methods, SgObject *argv, int argc,
				     int applyargs)
{
  SgObject array_s[PREALLOC_SIZE], *array = array_s;
  SgClass *targv_s[PREALLOC_SIZE], **targv = targv_s;
  int count = 0, len, step, i, j, tsize = argc;
  SgObject mp;
  /* for safety */
  if (SG_NULLP(methods)) return methods;

  len = Sg_Length(methods);
  /* TODO maybe we should use alloca */
  if (len >= PREALLOC_SIZE)  array = SG_NEW_ARRAY(SgObject, len);
  /* if this is apply call we need to expand the arguments */
  if (applyargs) {
    int n = Sg_Length(argv[argc-1]);
    if (n < 0) Sg_Error(UC("bad argument list: %S"), argv[argc-1]);
    tsize += n-1;
    argc--;
  }
  if (tsize >= PREALLOC_SIZE) targv = SG_NEW_ARRAY(SgClass*, tsize);

  SG_FOR_EACH(mp, methods) {
    if (!Sg_TypeP(SG_CAR(mp), SG_CLASS_METHOD)) {
      Sg_Error(UC("bad method in applicable method list: %S"), SG_CAR(mp));
    }
    array[count++] = SG_CAR(mp);
  }
  for (i=0; i <argc; i++) targv[i] = Sg_ClassOf(argv[i]);
  if (applyargs) {
    SgObject ap;
    SG_FOR_EACH(ap, argv[argc]) {
      targv[i++] = Sg_ClassOf(SG_CAR(ap));
    }
  }

  for (step = len/2; step > 0; step /=2) {
    for (i = step; i<len; i++) {
      for (j = i-step; j>=0; j -= step) {
	/* TODO, use generic method */
	if (method_more_specific(SG_METHOD(array[j]),
				 SG_METHOD(array[j+step]),
				 targv, tsize)) {
	  break;
	} else {
	  SgObject tmp = array[j+step];
	  array[j+step] = array[j];
	  array[j] = tmp;
	}
      }
    }
  }
  return Sg_ArrayToList(array, len);
}

/*
  creates a procedure and rest of next-methods.
  basically, only around must have next-methods, other must be called 
 */
static SgObject procedure_invoker(SgObject *args, int argc, void *data);
static SgObject invoke_cc(SgObject result, void **data)
{
  void **dvec = (void**)data[0];
  SgObject proc = SG_OBJ(dvec[0]);
  SgObject *args = (SgObject*)data[1];
  int argc = (int)data[2];
  /* store the result of :primary methods */
  if (dvec[2]) dvec[3] = result;
  if (SG_NULLP(proc)) return dvec[3]; /* no more methods */
  return procedure_invoker(args, argc, dvec);
}

static SgObject procedure_invoker(SgObject *args, int argc, void *data)
{
  void **dvec = (void**)data;
  SgObject proc;
  SgObject h = SG_NIL, t = SG_NIL;
  void *next[3];
  int i;
  /* retrive data */
  proc = SG_CAR(SG_OBJ(dvec[0]));
  dvec[0] = SG_CDR(SG_OBJ(dvec[0]));

  ASSERT(SG_METHODP(proc));
  next[0] = dvec;
  next[1] = SG_OBJ(args);
  next[2] = SG_OBJ(argc);

  if (SG_EQ(SG_METHOD_QUALIFIER(proc), SG_KEYWORD_PRIMARY)) {
    /* compute next-method */
    SgObject rest = SG_OBJ(dvec[1]);
    SG_APPEND1(h, t, Sg_MakeNextMethod(SG_METHOD_GENERIC(proc), rest,
				       args, argc, FALSE));
    dvec[2] = (void*)TRUE;
  } else {
    /* dummy, :before and :after can not have next-method. */
    SG_APPEND1(h, t, Sg_MakeNextMethod(SG_METHOD_GENERIC(proc),
				       SG_NIL,
				       args, argc, FALSE));
    dvec[2] = (void*)FALSE;
  }
  Sg_VMPushCC(invoke_cc, next, 3);
  for (i = 0; i < argc; i++) {
    SG_APPEND1(h, t, args[i]);
  }
  return Sg_VMApply(SG_METHOD_PROCEDURE(proc), h);
}

static SgObject compute_around_methods(SgObject around, SgObject before,
				       SgObject primary, SgObject after,
				       SgObject *argv, int argc, int applyargs)
{
  SgObject m, rest;
  SgObject proc, name = SG_UNDEF;
  SgObject result = SG_NIL, t = SG_NIL, mp;
  SgClass **specs = NULL;
  void **dvec;
  int req = -1, opt = -1;
  /* lazyness */
  primary = sort_primary_methods(primary, argv, argc, applyargs);
  /* if there is no primary method, then it must be an error */
  if (SG_NULLP(primary)) {
    return SG_NIL;
  }

  around  = sort_primary_methods(around, argv, argc, applyargs);
  before  = sort_primary_methods(before, argv, argc, applyargs);
  after   = sort_primary_methods(after, argv, argc, applyargs);

  /* on tiny clos for R6RS, after is called in reverse order */
  after = Sg_ReverseX(after);
  /* calculate before primary and after first */
  SG_FOR_EACH(mp, before) {
    SG_APPEND1(result, t, SG_CAR(mp));
  }
  mp = primary;
  req = SG_PROCEDURE_REQUIRED(SG_CAR(mp));
  opt = SG_PROCEDURE_OPTIONAL(SG_CAR(mp));
  name = SG_PROCEDURE_NAME(SG_CAR(mp));
  specs = SG_METHOD_SPECIALIZERS(SG_CAR(mp));
  SG_APPEND1(result, t, SG_CAR(mp));
  rest = SG_CDR(mp);
  /* primary next-method will be created in procedure_invoker */

  SG_FOR_EACH(mp, after) {
    SG_APPEND1(result, t, SG_CAR(mp));
  }
  /*
    data store vector.
    0: :before + first of :primary + :after
    1: the rest of :primary
    2: flag if invoke_cc needs to store the result or not
    3: the result of invoke_cc. this will be returned as a result of this
       generic method calling.
   */
  dvec = SG_NEW_ARRAY(void*, 4);
  dvec[0] = result;
  dvec[1] = rest;
  dvec[2] = (void*)FALSE;
  dvec[3] = SG_UNDEF;
  proc = Sg_MakeSubr(procedure_invoker, dvec, req, opt, name);
  m = method_allocate(SG_CLASS_METHOD, proc);
  SG_METHOD_PROCEDURE(m) = proc;
  SG_METHOD_SPECIALIZERS(m) = specs;
  SG_PROCEDURE_REQUIRED(m) = req;
  SG_PROCEDURE_OPTIONAL(m) = opt;
  SG_PROCEDURE_NAME(m) = name;

  if (SG_NULLP(around)) {
    return SG_LIST1(m);
  } else {
    /* calculate around.
       around method must have call-next-method and if it is not called,
       the method does not proceed to next. so we need to create a list
       which contains around method and the rest of result, like this;
       (:around m) ;; m is calculated methods.
       however, :around itself can be multiple, so it might be multiple
       lists, not only length 2 list.
     */
    SgObject h = SG_NIL, ap;
    t = SG_NIL;			/* reuse */
    SG_FOR_EACH(ap, around) {
      SG_APPEND1(h, t, SG_CAR(ap));
    }
    SG_APPEND1(h, t, m);
    return h;
  }
}

static SgObject sort_method(SgObject methods, SgObject *argv, int argc,
			    int applyargs)
{
  SgObject qualified_methods[QUALIFIER_COUNT];
  SgObject primary, before, after, around;
  sort_method_by_qualifier(methods, qualified_methods);
  primary = qualified_methods[PRIMARY_INDEX];
  before  = qualified_methods[BEFORE_INDEX];
  after   = qualified_methods[AFTER_INDEX];
  around  = qualified_methods[AROUND_INDEX];

  if (SG_NULLP(around) && SG_NULLP(before) && SG_NULLP(around)) {
    return sort_primary_methods(primary, argv, argc, applyargs);
  } else {
    /* dummy */
    return compute_around_methods(around, before, primary, after, argv, argc,
				  applyargs);
  }
}

SgObject Sg_ComputeMethods(SgGeneric *gf, SgObject *argv, int argc,
			   int applyargs)
{
  SgObject applicable = compute_applicable_methods(gf, argv, argc, applyargs);
  if (SG_NULLP(applicable)) return applicable;
  return sort_method(applicable, argv, argc, applyargs);
}

static SgSlotAccessor* lookup_slot_info(SgClass *klass, SgObject name,
					int raise)
{
  SgSlotAccessor **gNs = klass->gettersNSetters;
  SgObject cpl = klass->cpl;
  /* CPL never be '() */
  SgClass *tklass = SG_CAR(cpl);
  cpl = SG_CDR(cpl);
 entry:
  for (;*gNs;gNs++) {
    if (SG_EQ(name, (*gNs)->name)) {
      return *gNs;
    }
  }
  /* try tag */
  if (tklass != SG_CLASS_CLASS && !SG_NULLP(cpl)) {
    tklass = SG_CAR(cpl);
    cpl = SG_CDR(cpl);
    gNs = tklass->gettersNSetters;
    goto entry;
  }
  if (raise) {
    Sg_Error(UC("object of class %S doesn't have such slot: %S"), klass, name);
  }
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_SlotRef(SgObject obj, SgObject name)
{
  SgSlotAccessor *accessor = lookup_slot_info(Sg_ClassOf(obj), name, TRUE);
  if (accessor->getter) {
    return accessor->getter(obj);
  } else {
    /* scheme accessor, assume obj is instance */
    if (SG_FALSEP(accessor->getterS)) {
      return SG_INSTANCE(obj)->slots[accessor->index];
    } else {
      return Sg_Apply1(accessor->getterS, obj);
    }
  }
}

void Sg_SlotSet(SgObject obj, SgObject name, SgObject value)
{
  SgSlotAccessor *accessor = lookup_slot_info(Sg_ClassOf(obj), name, TRUE);
  if (accessor->setter) {
    accessor->setter(obj, value);
  } else {
    /* scheme accessor */
    if (SG_FALSEP(accessor->setterS)) {
      SG_INSTANCE(obj)->slots[accessor->index] = value;
    } else {
      Sg_Apply2(accessor->setterS, obj, value);
    }
  }
}

/* For now, these 2 are really simple */
SgObject Sg_SlotRefUsingAccessor(SgObject obj, SgSlotAccessor *ac)
{
  if (ac->getter) {
    return ac->getter(obj);
  } else {
    return SG_INSTANCE(obj)->slots[ac->index];
  }
}

void Sg_SlotSetUsingAccessor(SgObject obj, SgSlotAccessor *ac, SgObject value)
{
  if (ac->setter) {
    ac->setter(obj, value);
  } else {
    SG_INSTANCE(obj)->slots[ac->index] = value;
  }
}

SgClass* Sg_ClassOf(SgObject obj)
{
  if (!SG_PTRP(obj)) {
    if (SG_TRUEP(obj) || SG_FALSEP(obj)) return SG_CLASS_BOOL;
    if (SG_NULLP(obj)) return SG_CLASS_NULL;
    if (SG_CHARP(obj)) return SG_CLASS_CHAR;
    if (SG_INTP(obj)) return SG_CLASS_INTEGER;
    if (SG_EOFP(obj)) return SG_CLASS_EOF_OBJECT;
    if (SG_UNDEFP(obj)) return SG_CLASS_UNDEFINED_OBJECT;
    else return SG_CLASS_UNKNOWN;
  }
  if (SG_FLONUMP(obj)) return SG_CLASS_REAL;
  if (SG_PAIRP(obj)) return SG_CLASS_PAIR;
  return SG_CLASS_OF(obj);
}

int Sg_TypeP(SgObject obj, SgClass *type)
{
  return Sg_SubtypeP(Sg_ClassOf(obj), type);
}

int Sg_SubtypeP(SgClass *sub, SgClass *type)
{
  SgClass **p;
  if (sub == type) return TRUE;
  p = sub->cpa;
  while (*p) {
    if (*p++ == type) return TRUE;
  }
  return FALSE;
}

/* <class> */
static void class_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<class %A%s>"),
	    SG_CLASS(obj)->name,
	    /* for now */
	    UC(""));
}

static SgObject class_allocate(SgClass *klass, SgObject initargs)
{
  SgClass *instance = SG_ALLOCATE(SgClass, klass);
  SG_SET_CLASS(instance, klass);
  instance->allocate = NULL;
  instance->printer = NULL;
  instance->compare = object_compare;
  instance->serialize = NULL;
  instance->cpa = NULL;
  instance->nfields = 0;
  instance->coreSize = 0;
  instance->flags = SG_CLASS_SCHEME;
  instance->name = SG_FALSE;
  instance->directSupers = SG_NIL;
  instance->directSlots = SG_NIL;
  instance->gettersNSetters = SG_NIL;
  instance->cpl = SG_NIL;
  instance->fieldInitializers = SG_NIL;
  instance->creader = SG_FALSE;
  instance->cscanner = SG_FALSE;
  instance->cwriter = SG_FALSE;

  Sg_InitMutex(&instance->mutex, FALSE);
  Sg_InitCond(&instance->cv);
  /* should we add finalizer for mutex? */
  return SG_OBJ(instance);
}

static SgObject class_name(SgClass *klass)
{
  return klass->name;
}

static void class_name_set(SgClass *klass, SgObject name)
{
  klass->name = name;
}

static SgObject class_direct_supers(SgClass *klass)
{
  return klass->directSupers;
}

static void class_direct_supers_set(SgClass *klass, SgObject supers)
{
  /* TODO assertion */
  klass->directSupers = supers;
}

static SgObject class_direct_slots(SgClass *klass)
{
  return klass->directSlots;
}

static void class_direct_slots_set(SgClass *klass, SgObject slots)
{
  /* TODO assertion */
  klass->directSlots = slots;
}

static SgObject class_cpl(SgClass *klass)
{
  return klass->cpl;
}

/* subroutine for class_cpl_set. Scans klass' CPL and find out the
   suitable allocator function, C-struct core size and some flags*/
static void find_core_allocator(SgClass *klass)
{
  SgClass **p;
  SgClass *b = NULL;	/* the base calss klass gets the allocator func */
  int object_inherited = FALSE;

  klass->allocate = NULL;
  for (p = klass->cpa; *p; p++) {
    if (SG_CLASS_CATEGORY(*p) == SG_CLASS_BUILTIN) {
      Sg_Error(UC("class '%S' attempted to inherit from a builtin class "
		  "%S; you cannnot subclass a builtin class"), klass->name, *p);
    }
    if ((*p)->allocate == Sg_ObjectAllocate) {
      object_inherited = TRUE;
      continue;
    }
    if ((*p)->flags & SG_CLASS_APPLICABLE) {
      klass->flags |= SG_CLASS_APPLICABLE;
    }

    if (b &&
	SG_CLASS_CATEGORY(*p) == SG_CLASS_BASE &&
	b->allocate != (*p)->allocate) {
      /* found different C-defined class. */
      SgClass **bp = b->cpa;
      for (; *bp; bp++) {
	if (*bp == *p) break;
      }
      if (!*bp) {
	Sg_Error(UC("class '%S' attempted to inherit multiple C-defined "
		    "base class (%S and %S) which are not in a "
		    "superclass-superclass relathionship"),
		 klass->name, b, *p);
      }
      continue;
    }
    if (!b) {
      b = *p;
      klass->allocate = b->allocate;
      klass->coreSize = b->coreSize;
    }
  }
  if (!object_inherited) {
    Sg_Error(UC("class %S's precedence list doesn't have a base class: %S"),
	     klass->name, klass->cpl);
  }
  if (!klass->allocate) {
    klass->allocate = Sg_ObjectAllocate;
    klass->coreSize = sizeof(SgInstance);
  }
}

static void class_cpl_set(SgClass *klass, SgObject cpl)
{
  int len;
  SgObject cp;
  
  if (!SG_PAIRP(cpl)) goto err;
  if (SG_CAR(cpl) != SG_OBJ(klass)) goto err;

  cp = SG_CDR(cpl);
  if ((len = Sg_Length(cp)) < 0) goto err;
  klass->cpa = class_list_to_array(cp, len);
  if (klass->cpa[len-1] != SG_CLASS_TOP) goto err;
  klass->cpl = Sg_CopyList(cpl);
  find_core_allocator(klass);
  return;
 err:
  Sg_Error(UC("class precedence list must be a proper list of class "
	      "metaobject, beginning from the class itself owing the list, "
	      "and ending by the class <top>: %S"), cpl);
}

static SgObject class_slots_ref(SgClass *klass)
{
  return klass->slots;
}

static void class_slots_set(SgClass *klass, SgObject slots)
{
  klass->slots = slots;
}

static SgObject class_nfields(SgClass *klass)
{
  return SG_MAKE_INT(klass->nfields);
}

static void class_nfields_set(SgClass *klass, SgObject nfields)
{
  klass->nfields = SG_INT_VALUE(nfields);
}

static SgObject class_field_initializers(SgClass *klass)
{
  return klass->fieldInitializers;
}

static void class_field_initializers_set(SgClass *klass, SgObject initializers)
{
  klass->fieldInitializers = initializers;
}

static SgObject class_getters_n_setters(SgClass *klass)
{
  return Sg_ArrayToList((SgObject*)klass->gettersNSetters, klass->nfields);
}

static void class_getters_n_setters_set(SgClass *klass, SgObject getters)
{
  SgObject cp;
  if (!SG_LISTP(getters))
    Sg_Error(UC("proper list required, but got %S"), getters);

  SG_FOR_EACH(cp, getters) {
    if (!Sg_TypeP(SG_CAR(cp), SG_CLASS_SLOT_ACCESSOR)) {
      Sg_Error(UC("list of slot-accessor required, but got %S"), getters);
    }
  }

  klass->gettersNSetters = (SgSlotAccessor**)Sg_ListToArray(getters, TRUE);
}

static SgObject class_cache_reader(SgClass *klass)
{
  return klass->creader;
}

static void class_cache_reader_set(SgClass *klass, SgObject proc)
{
  if (!SG_PROCEDUREP(proc)) {
    Sg_Error(UC("procedure required, but got %S"), proc);
  }
  klass->creader = proc;
}

static SgObject class_cache_writer(SgClass *klass)
{
  return klass->cwriter;
}

static void class_cache_writer_set(SgClass *klass, SgObject proc)
{
  if (!SG_PROCEDUREP(proc)) {
    Sg_Error(UC("procedure required, but got %S"), proc);
  }
  klass->cwriter = proc;
}

static SgObject class_cache_scanner(SgClass *klass)
{
  return klass->cscanner;
}

static void class_cache_scanner_set(SgClass *klass, SgObject proc)
{
  if (!SG_PROCEDUREP(proc)) {
    Sg_Error(UC("procedure required, but got %S"), proc);
  }
  klass->cscanner = proc;
}


/* <object> */
SgObject Sg_ObjectAllocate(SgClass *klass, SgObject initargs)
{
  SgObject obj = Sg_AllocateInstance(klass);
  SG_SET_CLASS(obj, klass);
  return obj;
}

/* <generic> */
static void generic_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<generic %S (%d)>"),
	    SG_PROCEDURE_NAME(SG_GENERIC(obj)),
	    Sg_Length(SG_GENERIC_METHODS(obj)));
}

static SgObject generic_allocate(SgClass *klass, SgObject initargs)
{
  SgGeneric *gf = SG_ALLOCATE(SgGeneric, klass);
  SG_SET_CLASS(gf, klass);
  SG_PROCEDURE_INIT(gf, 0, 0, SG_PROC_GENERIC, SG_FALSE);
  SG_GENERIC_METHODS(gf) = SG_NIL;
  SG_GENERIC_FALLBACK(gf) = Sg_NoNextMethod;
  SG_GENERIC_DATA(gf) = NULL;
  SG_GENERIC_MAX_REQARGS(gf) = 0;
  Sg_InitMutex(SG_GENERIC_MUTEX(gf), FALSE);
  return SG_OBJ(gf);
}

static SgObject generic_name(SgGeneric *gf)
{
  return SG_PROCEDURE_NAME(gf);
}

static void generic_name_set(SgGeneric *gf, SgObject name)
{
  SG_PROCEDURE_NAME(gf) = name;
}

static SgObject generic_methods(SgGeneric *gf)
{
  return SG_GENERIC_METHODS(gf);
}


SgObject Sg_MakeBaseGeneric(SgObject name,
			    SgObject (*fallback)(SgObject *, int, SgGeneric *),
			    void *data)
{
  SgGeneric *gf = SG_GENERIC(generic_allocate(SG_CLASS_GENERIC, SG_NIL));
  SG_PROCEDURE_NAME(gf) = name;
  if (fallback) {
    SG_GENERIC_FALLBACK(gf) = fallback;
    SG_GENERIC_DATA(gf) = data;
  }
  return SG_OBJ(gf);
}

void Sg_InitBuiltinGeneric(SgGeneric *gf, const SgChar *name, SgLibrary *lib)
{
  SgObject s = Sg_Intern(Sg_MakeString(name, SG_LITERAL_STRING));
  SG_PROCEDURE_NAME(gf) = s;
  if (gf->fallback == NULL) {
    gf->fallback = Sg_NoNextMethod;
  }
  Sg_InitMutex(&gf->mutex, FALSE);
  Sg_InsertBinding(lib, s, SG_OBJ(gf));
}

void Sg_InitBuiltinMethod(SgMethod *m)
{
  SG_PROCEDURE_NAME(m)
    = Sg_Cons(SG_PROCEDURE_NAME(SG_METHOD_GENERIC(m)),
	      class_array_to_names(SG_METHOD_SPECIALIZERS(m),
				   SG_PROCEDURE_REQUIRED(m)));
  SG_METHOD_QUALIFIER(m) = SG_KEYWORD(SG_KEYWORD_PRIMARY);
  Sg_AddMethod(SG_METHOD_GENERIC(m), m);
}

SgObject Sg_InvalidApply(SgObject *argv, int argc, SgGeneric *gf)
{
  Sg_AssertionViolation(SG_INTERN("apply"),
			SG_MAKE_STRING("invalid application"),
			Sg_ArrayToList(argv, argc));
  return SG_UNDEF;
}

SgObject Sg_NoNextMethod(SgObject *argv, int argc, SgGeneric *gf)
{
  SgObject h = SG_NIL, t = SG_NIL, cp, args = Sg_ArrayToList(argv, argc);
  SG_FOR_EACH(cp, args) {
    SG_APPEND1(h, t, Sg_ClassOf(SG_CAR(cp)));
  }
  Sg_AssertionViolation(SG_INTERN("call-next-method"),
			Sg_Sprintf(UC("no applicable method for %S with "
				      "arguments %S"), SG_OBJ(gf), h),
			SG_NIL);
  return SG_UNDEF;		/* dummy */
}


/* <method> */
static SgObject method_allocate(SgClass *klass, SgObject initargs)
{
  SgMethod *instance = SG_ALLOCATE(SgMethod, klass);
  SG_SET_CLASS(instance, klass);
  SG_PROCEDURE_INIT(instance, 0, 0, SG_PROC_METHOD, SG_FALSE);
  SG_METHOD_PROCEDURE(instance) = SG_FALSE;
  SG_METHOD_SPECIALIZERS(instance) = NULL;
  SG_METHOD_GENERIC(instance) = NULL;
  return SG_OBJ(instance);
}

static void method_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<method %S%S>"),
	    SG_PROCEDURE_NAME(SG_METHOD(obj)),
	    SG_METHOD_QUALIFIER(obj));
}

static SgObject method_specializers(SgMethod *method)
{
  SgClass **specs = SG_METHOD_SPECIALIZERS(method);
  SgObject h = SG_NIL, t = SG_NIL;
  for (;*specs; specs++) {
    SG_APPEND1(h, t, SG_OBJ(*specs));
  }
  return h;
}

static void method_specializers_set(SgMethod *method, SgObject specs)
{
  SgClass **s;
  SgObject cp;
  if (!SG_LISTP(specs)) {
    Sg_Error(UC("proper list required, but got %S"), specs);
  }
  SG_FOR_EACH(cp, specs) {
    if (!Sg_TypeP(SG_CAR(cp), SG_CLASS_CLASS)) {
      Sg_Error(UC("list of class required, but got %S"), specs);
    }
  }
  s = (SgClass**)Sg_ListToArray(specs, TRUE);
  SG_METHOD_SPECIALIZERS(method) = s;
}

static SgObject method_name(SgMethod *method)
{
  return SG_PROCEDURE_NAME(method);
}


static void method_name_set(SgMethod *method, SgObject name)
{
  SG_PROCEDURE_NAME(method) = name;
}

static SgObject method_procedure(SgMethod *method)
{
  return SG_METHOD_PROCEDURE(method);
}

static void method_procedure_set(SgMethod *method, SgObject proc)
{
  if (!SG_CLOSUREP(proc) && !SG_SUBRP(proc)) {
    Sg_Error(UC("method procedure requires procedure but got %S"), proc);
  }
  SG_METHOD_PROCEDURE(method) = proc;
  set_method_properties(method, proc);
}

static SgObject method_required(SgMethod *method)
{
  return SG_MAKE_INT(method->common.required);
}

static SgObject method_optional(SgMethod *method)
{
  return SG_MAKE_BOOL(method->common.optional);
}

/* next method */
static void next_method_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgNextMethod *nm = SG_NEXT_METHOD(obj);
  SgObject args = Sg_ArrayToList(nm->argv, nm->argc);
  Sg_Printf(port, UC("#<next-method %S %S>"), nm->methods, args);
}

SgObject Sg_MakeNextMethod(SgGeneric *gf, SgObject methods,
			   SgObject *argv, int argc, int copyargs)
{
  SgNextMethod *nm = SG_NEW(SgNextMethod);
  SG_SET_CLASS(nm, SG_CLASS_NEXT_METHOD);
  SG_PROCEDURE_INIT(nm, 0, 0, SG_PROC_NEXT_METHOD, SG_FALSE);
  nm->generic = gf;
  nm->methods = methods;
  if (copyargs) {
    nm->argv = SG_NEW_ARRAY(SgObject, argc);
    memcpy(nm->argv, argv, sizeof(SgObject) * argc);
  } else {
    nm->argv = argv;
  }
  nm->argc = argc;
  return SG_OBJ(nm);
}

/* slot accessor */
static SgObject sa_getter(SgSlotAccessor *sa)
{
  return sa->getterS;
}
static void sa_getter_set(SgSlotAccessor *sa, SgObject proc)
{
  sa->getterS = proc;
}
static SgObject sa_setter(SgSlotAccessor *sa)
{
  return sa->setterS;
}
static void sa_setter_set(SgSlotAccessor *sa, SgObject proc)
{
  sa->setterS = proc;
}

/* static initializer */

/* slot initialization */
static SgSlotAccessor class_slots[] = {
  SG_CLASS_SLOT_SPEC("name",               0, class_name,
		     class_name_set),
  SG_CLASS_SLOT_SPEC("direct-supers",      1, class_direct_supers,
		     class_direct_supers_set),
  SG_CLASS_SLOT_SPEC("direct-slots",       2, class_direct_slots,
		     class_direct_slots_set),
  SG_CLASS_SLOT_SPEC("cpl",                3, class_cpl,
		     class_cpl_set),
  SG_CLASS_SLOT_SPEC("slots",              4, class_slots_ref,
		     class_slots_set),
  SG_CLASS_SLOT_SPEC("nfields",            5, class_nfields,
		     class_nfields_set),
  SG_CLASS_SLOT_SPEC("field-initializers", 6, class_field_initializers,
		     class_field_initializers_set),
  SG_CLASS_SLOT_SPEC("getters-n-setters",  7, class_getters_n_setters,
		     class_getters_n_setters_set),
  SG_CLASS_SLOT_SPEC("cache-reader",       8, class_cache_reader,
		     class_cache_reader_set),
  SG_CLASS_SLOT_SPEC("cache-scanner",      9, class_cache_scanner,
		     class_cache_scanner_set),
  SG_CLASS_SLOT_SPEC("cache-writer",      10, class_cache_writer,
		     class_cache_writer_set),
  { { NULL } }
};

static SgSlotAccessor generic_slots[] = {
  SG_CLASS_SLOT_SPEC("name",    0, generic_name, generic_name_set),
  SG_CLASS_SLOT_SPEC("methods", 1, generic_methods, NULL),
  { { NULL } }
};

static SgSlotAccessor method_slots[] = {
  SG_CLASS_SLOT_SPEC("specializers", 0, method_specializers, method_specializers_set),
  SG_CLASS_SLOT_SPEC("procedure", 1, method_procedure, method_procedure_set),
  SG_CLASS_SLOT_SPEC("name", 2, method_name, method_name_set),
  SG_CLASS_SLOT_SPEC("required", 3, method_required, NULL),
  SG_CLASS_SLOT_SPEC("optional", 4, method_optional, NULL),
  { { NULL } }
};

static SgSlotAccessor slot_accessor_slots[] = {
  SG_CLASS_SLOT_SPEC("getter",   0, sa_getter, sa_getter_set),
  SG_CLASS_SLOT_SPEC("setter",   1, sa_setter, sa_setter_set),
  { { NULL } }
};

static void initialize_builtin_cpl(SgClass *klass, SgObject supers)
{
  SgClass **p;
  SgObject h = SG_NIL, t = SG_NIL;
  SG_APPEND1(h, t, SG_OBJ(klass));
  for (p = klass->cpa; *p; p++) SG_APPEND1(h, t, SG_OBJ(*p));
  klass->cpl = h;
  if (SG_PAIRP(supers)) {
    SgObject cp, sp = supers;
    SG_FOR_EACH(cp, klass->cpl) {
      if (SG_EQ(SG_CAR(cp), SG_CAR(sp))) {
	sp = SG_CDR(sp);
	if (SG_NULLP(sp)) break;
      }
    }
    if (!SG_NULLP(sp)) {
      /* this happens in initialization of Sagittarius itself, so we can not
         handle any exception yet, but exit*/
      const char *cname = "(unnamed class)";
      if (SG_SYMBOLP(klass->name)) {
	cname = (const char*)Sg_Utf32sToUtf8s(SG_SYMBOL(klass->name)->name);
      }
      Sg_Panic("Class %s is being initialized with inconsistent super class "
	       "list. Must be an implementation error. Report to the author.",
	       cname);
    }
    klass->directSupers = supers;
  } else if (SG_PAIRP(SG_CDR(h))) {
    /* Default: take the next class of CPL as the only direct super */
    klass->directSupers = SG_LIST1(SG_CADR(h));
  } else {
    /* what is this? */
    klass->directSupers = SG_NIL;
  }
}

static void init_class(SgClass *klass, const SgChar *name,
		       SgLibrary *lib, SgObject supers,
		       SgSlotAccessor *specs, int flags)
{
  SgObject slots = SG_NIL, t = SG_NIL;
  SgObject acc = SG_NIL, sp;
  SgClass **super;

  if (klass->cpa == NULL) klass->cpa = SG_CLASS_DEFAULT_CPL;

  initialize_builtin_cpl(klass, supers);

  if (name && lib) {
    klass->name = Sg_Intern(Sg_MakeString(name, SG_LITERAL_STRING));
    Sg_InsertBinding(lib, SG_SYMBOL(klass->name), SG_OBJ(klass));
  }

  /* initialize direct slots */
  if (specs) {
    for (;specs->name; specs++) {
      SgObject snam = Sg_Intern(Sg_MakeStringC(specs->cname));
      specs->klass = klass;
      specs->name = snam;
      acc = Sg_Cons(SG_OBJ(&*specs), acc);
      SG_APPEND1(slots, t,
		 SG_LIST3(snam,
			  SG_KEYWORD_INIT_KEYWORD, 
			  Sg_MakeKeyword(SG_SYMBOL(snam)->name)));
    }
  }
  klass->directSlots = slots;

  /* compute other slots inherited from supers */
  for (super = klass->cpa; *super; super++) {
    SgSlotAccessor **dacc = (*super)->gettersNSetters;
    /* I think slot should have accessor info but for now */
    for (;dacc && *dacc; dacc++) {
      SgObject p = Sg_Assq((*dacc)->name, slots);
      if (SG_FALSEP(p)) {
	acc = Sg_Cons(acc, SG_OBJ(*dacc));
      }
    }
    SG_FOR_EACH(sp, (*super)->directSlots) {
      SgObject slot = SG_CAR(sp), snam, p;
      ASSERT(SG_PAIRP(slot));
      snam = SG_CAR(slot);
      p = Sg_Assq(snam, slots);
      if (SG_FALSEP(p)) {
	slots = Sg_Cons(Sg_CopyList(slot), slots);
      }
    }
  }
  klass->gettersNSetters = (SgSlotAccessor**)Sg_ListToArray(Sg_ReverseX(acc),
							    TRUE);
  klass->slots = slots;
  klass->nfields = Sg_Length(slots);
}

void Sg_InitStaticClass(SgClass *klass, const SgChar *name,
			SgLibrary *lib, SgSlotAccessor *specs, int flags)
{
  init_class(klass, name, lib, SG_FALSE, specs, flags);
}

/* 
   (make <class> (list <class>))
 */
static SgClass* make_implicit_meta(const SgChar *name, SgClass **cpa,
				   SgLibrary *lib)
{
  SgClass *meta = (SgClass*)class_allocate(SG_CLASS_CLASS, SG_NIL);
  SgObject s = Sg_Intern(Sg_MakeString(name, SG_LITERAL_STRING));
  static SgClass *metacpa[] = {
    SG_CLASS_CLASS,
    SG_CLASS_OBJECT,
    SG_CLASS_TOP,
    NULL
  };
  SgClass **metas = metacpa;
  SgClass **parent;
  int numExtraMetas = 0, i;

  for (parent = cpa; *parent; parent++) {
    if (SG_CLASS_OF(*parent) != SG_CLASS_CLASS) {
      numExtraMetas++;
    }
  }
  if (numExtraMetas) {
    metas = SG_NEW_ARRAY(SgClass*, numExtraMetas+4);
    for (i = 0, parent = cpa; *parent; parent++) {
      if (SG_CLASS_OF(*parent) != SG_CLASS_CLASS) {
	metas[i++] = SG_CLASS_OF(*parent);
      }
    }
    metas[i++] = SG_CLASS_CLASS;
    metas[i++] = SG_CLASS_OBJECT;
    metas[i++] = SG_CLASS_TOP;
    metas[i]   = NULL;
  }
  meta->name = s;
  meta->allocate = class_allocate;
  meta->printer = class_print;
  meta->cpa = metas;
  initialize_builtin_cpl(meta, SG_FALSE);
  Sg_InsertBinding(lib, SG_SYMBOL(s), SG_OBJ(meta));
  meta->slots = SG_CLASS_CLASS->slots;
  meta->gettersNSetters = SG_CLASS_CLASS->gettersNSetters;
  return meta;

}
				   

void Sg_InitStaticClassWithMeta(SgClass *klass, const SgChar *name,
				SgLibrary *lib, SgClass *meta,
				SgObject supers, SgSlotAccessor *specs,
				int flags)
{
  init_class(klass, name, lib, supers, specs, flags);
  if (meta) {
    SG_SET_CLASS(klass, meta);
  } else {
    int nlen;
    SgChar *metaname;
    nlen = (int)ustrlen(name);
    metaname = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * (nlen+6));
    if (name[nlen-1] == '>') {
      memcpy(metaname, name, (nlen-1)*sizeof(SgChar));
      memcpy(metaname+nlen-1, UC("-meta>"), 6*sizeof(SgChar));
    } else {
      memcpy(metaname, name, (nlen)*sizeof(SgChar));
      memcpy(metaname+nlen, UC("-meta"), 5*sizeof(SgChar));
    }
    SG_SET_CLASS(klass, make_implicit_meta(metaname, klass->cpa, lib));
  }
}

/* 
   builtin object initializer
   for now it the same as (lambda (call-next-method object . initargs) object)
 */
static SgObject builtin_initialize(SgObject *argv, int argc, SgGeneric *gf)
{
  ASSERT(argc >= 2);
  return argv[1];
}

/* builtin generics */
SG_DEFINE_GENERIC(Sg_GenericMake, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericAllocateInstance, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericInitialize, builtin_initialize, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeCPL, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeSlots, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeGetterAndSetter, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericAddMethod, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericRemoveMethod, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeApplicableMethods, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericObjectEqualP, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericObjectApply, Sg_InvalidApply, NULL);
SG_DEFINE_GENERIC(Sg_GenericObjectSetter, Sg_InvalidApply, NULL);
/* generic invocation */
SG_DEFINE_GENERIC(Sg_GenericComputeApplyGeneric, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeMethodMoreSpecificP, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeApplyMethods, Sg_NoNextMethod, NULL);


static SgObject allocate_impl(SgObject *args, int argc, void *data)
{
  SgClass *c = SG_CLASS(args[0]);
  if (c->allocate == NULL) {
    Sg_Error(UC("built-in class can't be allocated via allocate-instance: %S"),
	     SG_OBJ(c));
  }
  return c->allocate(c, args[1]);
}

SG_DEFINE_SUBR(allocate, 2, 0, allocate_impl, SG_FALSE, NULL);

static SgClass *class_allocate_SPEC[] = {
  SG_CLASS_CLASS, SG_CLASS_LIST
};

static SG_DEFINE_METHOD(class_allocate_rec, &Sg_GenericAllocateInstance,
			2, 0, class_allocate_SPEC, &allocate);

/* object-initialize */
static SgObject object_initialize_impl(SgObject *argv, int argc, void *data)
{
  SgObject obj = argv[0];
  SgObject initargs = argv[1];
  SgObject slots = Sg_ClassOf(obj)->slots;
  SgObject cp;
  if (SG_NULLP(slots)) return obj;
  SG_FOR_EACH(cp, slots) {
    SgObject slot = SG_CAR(cp);
    SgObject key  = Sg_Memq(SG_KEYWORD_INIT_KEYWORD, slot);

    /* (1) use init-keyword */
    if (!SG_FALSEP(key) && SG_PAIRP(SG_CDR(key)) &&
	SG_KEYWORDP(SG_CADR(key))) {
      SgObject v = Sg_GetKeyword(SG_CADR(key), initargs, SG_UNDEF);
      if (!SG_UNDEFP(v)) {
	Sg_SlotSet(obj, SG_CAR(slot), v);
	continue;
      }
      /* go through */
    }
    /* (2) use init-value */
    key = Sg_Memq(SG_KEYWORD_INIT_VALUE, slot);
    if (!SG_FALSEP(key)) {
      SgObject v = Sg_GetKeyword(SG_KEYWORD_INIT_VALUE, SG_CDR(slot), SG_UNDEF);
      if (!SG_UNDEFP(v)) {
	Sg_SlotSet(obj, SG_CAR(slot), v);
	continue;
      }
    }
    /* (2) use init-thunk */
    key = Sg_Memq(SG_KEYWORD_INIT_THUNK, slot);
    if (!SG_FALSEP(key)) {
      SgObject v = Sg_GetKeyword(SG_KEYWORD_INIT_THUNK, SG_CDR(slot), SG_UNDEF);
      if (!SG_UNDEFP(v)) {
	Sg_SlotSet(obj, SG_CAR(slot), Sg_Apply0(v));
	continue;
      }
    }
    
  }
  return SG_UNDEF;
}
SG_DEFINE_SUBR(object_initialize, 2, 0, object_initialize_impl, SG_FALSE, NULL);
static SgClass *object_initialize_SPEC[] = {
  SG_CLASS_OBJECT, SG_CLASS_LIST
};

static SG_DEFINE_METHOD(object_initialize_rec, &Sg_GenericInitialize,
			2, 0,
			object_initialize_SPEC,
			&object_initialize);

static int object_compare(SgObject x, SgObject y, int equalp)
{
  SgObject r;
  if (equalp) {
    r = Sg_Apply2(SG_OBJ(&Sg_GenericObjectEqualP), x, y);
    return (SG_FALSEP(r) ? -1: 0);
  } else {
    /* not supported yet */
    Sg_Error(UC("object %S and %S can't be ordered"), x, y);
    return 0;			/* dummy */
  }
}

/* fallback */
static SgObject object_equalp_impl(SgObject *argv, int argc, void *data)
{
  return SG_FALSE;
}

SG_DEFINE_SUBR(object_equalp_default, 2, 0, object_equalp_impl, SG_FALSE, NULL);
static SgClass *object_equalp_SPEC[] = {
  SG_CLASS_TOP, SG_CLASS_TOP
};
static SG_DEFINE_METHOD(object_equalp_rec, &Sg_GenericObjectEqualP,
			2, 0,
			object_equalp_SPEC,
			&object_equalp_default);


static SgObject method_initialize_impl(SgObject *argv, int argc, void *data)
{
  SgMethod *m = SG_METHOD(argv[0]);
  SgGeneric *g;
  SgObject initargs = argv[1];
  SgObject llist, quoli, generic, specs, body;
  SgClass **specarray;
  SgObject lp, h, t;
  int speclen = 0, req = 0, opt = 0, i;
  /* for sanity */
  ASSERT(SG_METHODP(m));
  /* get keyword arguments */
  llist   = Sg_GetKeyword(SG_KEYWORD_LAMBDA_LIST, initargs, SG_FALSE);
  quoli   = Sg_GetKeyword(SG_KEYWORD_QUALIFIER, initargs, SG_KEYWORD_PRIMARY);
  generic = Sg_GetKeyword(SG_KEYWORD_GENERIC, initargs, SG_FALSE);
  specs   = Sg_GetKeyword(SG_KEYWORD_SPECIALIZERS, initargs, SG_FALSE);
  body    = Sg_GetKeyword(SG_KEYWORD_PROCEDURE, initargs, SG_FALSE);

  if (!Sg_TypeP(generic, SG_CLASS_GENERIC)) {
    Sg_Error(UC("generic function required for :generic argument: %S"), 
	     generic);
  }
  g = SG_GENERIC(generic);
  if (!SG_CLOSUREP(body) && !SG_SUBRP(body)) {
    Sg_Error(UC("closure required for :body argument: %S"), body);
  }
  if ((speclen = Sg_Length(specs)) < 0) {
    Sg_Error(UC("invalid specializers list: %S"), specs);
  }
  specarray = class_list_to_array(specs, speclen);

  SG_FOR_EACH(lp, llist) req++;
  if (!SG_NULLP(lp)) opt++;

  if (SG_PROCEDURE_REQUIRED(body)+SG_PROCEDURE_OPTIONAL(body) != req+opt+1) {
    Sg_Error(UC("body doesn't match with lambda list: %S"), body);
  }
  if (speclen != req) {
    Sg_Error(UC("specializer list doesn't match with lambda list: %S"), specs);
  }
  SG_PROCEDURE_REQUIRED(m) = req;
  SG_PROCEDURE_OPTIONAL(m) = opt;
  SG_PROCEDURE_NAME(m) = Sg_Cons(SG_PROCEDURE_NAME(g),
				 class_list_to_names(specarray, speclen));
  SG_METHOD_GENERIC(m) = g;
  SG_METHOD_SPECIALIZERS(m) = specarray;
  SG_METHOD_PROCEDURE(m) = body;
  SG_METHOD_QUALIFIER(m) = quoli;
  /* mostly true */
  if (SG_CLOSUREP(body)) {
    h = t = SG_NIL;
    for (i=0; i<speclen; i++) {
      SG_APPEND1(h, t, specarray[i]->name);
    }
    SG_CODE_BUILDER(SG_CLOSURE(body)->code)->name
      = Sg_Cons(SG_PROCEDURE_NAME(g), h);
  }
  /* add direct methods? */
  return SG_OBJ(m);
}

SG_DEFINE_SUBR(method_initialize, 2, 0, method_initialize_impl, SG_FALSE, NULL);
static SgClass *method_initialize_SPEC[] = {
  SG_CLASS_METHOD, SG_CLASS_LIST
};

static SG_DEFINE_METHOD(method_initialize_rec, &Sg_GenericInitialize,
			2, 0,
			method_initialize_SPEC,
			&method_initialize);

/* compute-cpl */
static SgObject compute_cpl_impl(SgObject *args, int argc, void *data)
{
  return Sg_ComputeCPL(SG_CLASS(args[0]));
}

SG_DEFINE_SUBR(compute_cpl, 1, 0, compute_cpl_impl, SG_FALSE, NULL);

static SgClass *compute_cpl_SPEC[] = {
  SG_CLASS_CLASS
};

static SG_DEFINE_METHOD(compute_cpl_rec, &Sg_GenericComputeCPL,
			1, 0,
			compute_cpl_SPEC, &compute_cpl);

/* compute-slots */
static SgObject compute_slots_impl(SgObject *args, int argc, void *data)
{
  return Sg_ComputeSlots(SG_CLASS(args[0]));
}

SG_DEFINE_SUBR(compute_slots, 1, 0, compute_slots_impl, SG_FALSE, NULL);

static SgClass *compute_slots_SPEC[] = {
  SG_CLASS_CLASS
};

static SG_DEFINE_METHOD(compute_slots_rec, &Sg_GenericComputeSlots,
			1, 0,
			compute_slots_SPEC, &compute_slots);

/* compute-getters-and-setters */
static SgObject compute_gas_impl(SgObject *args, int argc, void *data)
{
  return Sg_ComputeGettersAndSetters(SG_CLASS(args[0]), args[1]);
}

SG_DEFINE_SUBR(compute_gas, 2, 0, compute_gas_impl, SG_FALSE, NULL);

static SgClass *compute_gas_SPEC[] = {
  SG_CLASS_CLASS, SG_CLASS_LIST
};

static SG_DEFINE_METHOD(compute_gas_rec, &Sg_GenericComputeGetterAndSetter,
			2, 0,
			compute_gas_SPEC, &compute_gas);

/* add-method */
static SgObject add_method_impl(SgObject *args, int argc, void *data)
{
  return Sg_AddMethod(SG_GENERIC(args[0]), SG_METHOD(args[1]));
}

SG_DEFINE_SUBR(add_method, 2, 0, add_method_impl, SG_FALSE, NULL);

static SgClass *add_method_SPEC[] = {
  SG_CLASS_GENERIC, SG_CLASS_METHOD
};

static SG_DEFINE_METHOD(add_method_rec, &Sg_GenericAddMethod,
			2, 0,
			add_method_SPEC, &add_method);
/* remove-method */
static SgObject remove_method_impl(SgObject *args, int argc, void *data)
{
  return Sg_RemoveMethod(SG_GENERIC(args[0]), SG_METHOD(args[1]));
}

SG_DEFINE_SUBR(remove_method, 2, 0, remove_method_impl, SG_FALSE, NULL);

static SgClass *remove_method_SPEC[] = {
  SG_CLASS_GENERIC, SG_CLASS_METHOD
};

static SG_DEFINE_METHOD(remove_method_rec, &Sg_GenericRemoveMethod,
			2, 0,
			remove_method_SPEC, &remove_method);

/* remove-method */
static SgObject compute_applicable_methods_impl(SgObject *args, int argc,
						void *data)
{
  SgGeneric *gf = SG_GENERIC(args[0]);
  SgObject argv = args[1];
  int size = Sg_Length(args[1]);
  if (size < 0) Sg_Error(UC("bad argument list: %S"), argv);

  return compute_applicable_methods(gf, &argv, 1, TRUE);
}

SG_DEFINE_SUBR(compute_applicable_methods_subr, 2, 0,
	       compute_applicable_methods_impl, SG_FALSE, NULL);

static SgClass *compute_applicable_methods_SPEC[] = {
  SG_CLASS_GENERIC, SG_CLASS_LIST
};

static SG_DEFINE_METHOD(compute_applicable_methods_rec,
			&Sg_GenericComputeApplicableMethods,
			2, 0,
			compute_applicable_methods_SPEC,
			&compute_applicable_methods_subr);

void Sg__InitClos()
{
  /* TODO library name */
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  static SgClass *nullcpa[1] = {NULL};

  SG_CLASS_TOP->cpa = nullcpa;
#define CINIT(cl, nam)					\
  Sg_InitStaticClassWithMeta(cl, UC(nam), lib, NULL, SG_FALSE, NULL, 0)

#define BINIT(cl, nam, slots) Sg_InitStaticClass(cl, UC(nam), lib, slots, 0)

  BINIT(SG_CLASS_CLASS,  "<class>", class_slots);
  BINIT(SG_CLASS_TOP,    "<top>", NULL);
  BINIT(SG_CLASS_OBJECT, "<object>", NULL);
  /* generic, method and next-method */
  BINIT(SG_CLASS_GENERIC,     "<generic>", generic_slots);
  BINIT(SG_CLASS_METHOD,      "<method>",  method_slots);
  BINIT(SG_CLASS_NEXT_METHOD, "<next-method>", NULL);
  BINIT(SG_CLASS_SLOT_ACCESSOR, "<slot-accessor>", slot_accessor_slots);
  /* set flags for above to make them applicable(procedure? returns #t) */
  SG_CLASS_GENERIC->flags |= SG_CLASS_APPLICABLE;
  SG_CLASS_METHOD->flags |= SG_CLASS_APPLICABLE;
  SG_CLASS_NEXT_METHOD->flags |= SG_CLASS_APPLICABLE;

  /* primitives */
  CINIT(SG_CLASS_BOOL,   "<boolean>");
  CINIT(SG_CLASS_CHAR,   "<char>");
  CINIT(SG_CLASS_EOF_OBJECT,   "<eof-object>");
  CINIT(SG_CLASS_UNDEFINED_OBJECT,   "<undefined-object>");
  CINIT(SG_CLASS_UNKNOWN,   "<unknown>");

  CINIT(SG_CLASS_CHAR_SET,  "<char-set>");
  CINIT(SG_CLASS_HASHTABLE, "<hashtable>");

  CINIT(SG_CLASS_LIST,      "<list>");
  CINIT(SG_CLASS_PAIR,      "<pair>");
  CINIT(SG_CLASS_NULL,      "<null>");

  /* number */
  CINIT(SG_CLASS_NUMBER,    "<number>");
  CINIT(SG_CLASS_COMPLEX,   "<complex>");
  CINIT(SG_CLASS_REAL,      "<real>");
  CINIT(SG_CLASS_RATIONAL,  "<rational>");
  CINIT(SG_CLASS_INTEGER,   "<integer>");

  /* string */
  CINIT(SG_CLASS_STRING,    "<string>");

  /* symbol */
  CINIT(SG_CLASS_SYMBOL,    "<symbol>");
  CINIT(SG_CLASS_GLOC,      "<gloc>");

  /* keyword */
  CINIT(SG_CLASS_KEYWORD,   "<keyword>");

  /* abstract collection */
  BINIT(SG_CLASS_COLLECTION, "<collection>", NULL);
  BINIT(SG_CLASS_SEQUENCE,   "<sequence>",   NULL);
  BINIT(SG_CLASS_DICTIONARY, "<dictionary>", NULL);
  BINIT(SG_CLASS_ORDERED_DICTIONARY, "<ordered-dictionary>", NULL);

  /* hashtable */
  CINIT(SG_CLASS_HASHTABLE, "<hashtable>");
  /* treemap */
  CINIT(SG_CLASS_TREE_MAP,  "<tree-map>");

  /* vector */
  CINIT(SG_CLASS_VECTOR,    "<vector>");
  /* bytevector */
  CINIT(SG_CLASS_BVECTOR,   "<bytevector>");
  /* weak */
  CINIT(SG_CLASS_WEAK_VECTOR,       "<weak-vector>");
  CINIT(SG_CLASS_WEAK_HASHTABLE,    "<weak-hashtable>");

  /* codec and transcoders */
  CINIT(SG_CLASS_CODEC,       "<codec>");
  CINIT(SG_CLASS_TRANSCODER,  "<transcoder>");

  /* Should we export this? this might be removed in future if I can rewrite
     record with CLOS. */
  /* record */
  CINIT(SG_CLASS_RECORD_TYPE, "<record-type>");

  /* we do not export values. this should not be first class object. */

  /* procedure */
  CINIT(SG_CLASS_PROCEDURE, "<procedure>");
  SG_CLASS_PROCEDURE->flags |= SG_CLASS_APPLICABLE;

#define GINIT(gf, nam)				\
  Sg_InitBuiltinGeneric(gf, UC(nam), lib)

  GINIT(&Sg_GenericMake, "make");
  GINIT(&Sg_GenericAllocateInstance, "allocate-instance");
  GINIT(&Sg_GenericInitialize, "initialize");
  GINIT(&Sg_GenericComputeCPL, "compute-cpl");
  GINIT(&Sg_GenericComputeSlots, "compute-slots");
  GINIT(&Sg_GenericComputeGetterAndSetter, "compute-getters-and-setters");
  GINIT(&Sg_GenericAddMethod, "add-method");
  GINIT(&Sg_GenericRemoveMethod, "remove-method");
  GINIT(&Sg_GenericComputeApplicableMethods, "compute-applicable-methods");
  GINIT(&Sg_GenericObjectEqualP, "object-equal?");
  GINIT(&Sg_GenericObjectApply, "object-apply");
  GINIT(&Sg_GenericObjectSetter, "setter of object-apply");
  GINIT(&Sg_GenericComputeApplyGeneric, "compute-apply-generic");
  GINIT(&Sg_GenericComputeMethodMoreSpecificP, "compute-method-more-specific?");
  GINIT(&Sg_GenericComputeApplyMethods, "compute-apply-methods");

  Sg_SetterSet(SG_PROCEDURE(&Sg_GenericObjectApply),
	       SG_PROCEDURE(&Sg_GenericObjectSetter),
	       TRUE);

  /* methods */
  Sg_InitBuiltinMethod(&class_allocate_rec);
  Sg_InitBuiltinMethod(&object_initialize_rec);
  Sg_InitBuiltinMethod(&method_initialize_rec);
  Sg_InitBuiltinMethod(&compute_cpl_rec);
  Sg_InitBuiltinMethod(&compute_slots_rec);
  Sg_InitBuiltinMethod(&compute_gas_rec);
  Sg_InitBuiltinMethod(&add_method_rec);
  Sg_InitBuiltinMethod(&remove_method_rec);
  Sg_InitBuiltinMethod(&compute_applicable_methods_rec);
  Sg_InitBuiltinMethod(&object_equalp_rec);
}
