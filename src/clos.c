/* clos.c                                                 -*- coding: utf-8; -*-
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
#include "sagittarius/private/clos.h"
#include "sagittarius/private/compare.h"
#include "sagittarius/private/bytevector.h"
#include "sagittarius/private/charset.h"
#include "sagittarius/private/closure.h"
#include "sagittarius/private/code.h"
#include "sagittarius/private/codec.h"
#include "sagittarius/private/collection.h"
#include "sagittarius/private/core.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/exceptions.h"
#include "sagittarius/private/generic.h"
#include "sagittarius/private/gloc.h"
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/instruction.h"
#include "sagittarius/private/keyword.h"
#include "sagittarius/private/library.h"
#include "sagittarius/private/number.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/record.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/subr.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/system.h"
#include "sagittarius/private/transcoder.h"
#include "sagittarius/private/treemap.h"
#include "sagittarius/private/unicode.h"
#include "sagittarius/private/values.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/vm.h"
#include "sagittarius/private/weak.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/builtin-keywords.h"

static void slot_acc_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgSlotAccessor *acc = SG_SLOT_ACCESSOR(obj);
  Sg_Printf(port, UC("#<slot-accessor %A:%A,%d>"),
	    acc->klass?acc->klass->name:SG_UNDEF, acc->name, acc->index);
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

static SgSlotAccessor* lookup_slot_info(SgClass *klass, SgObject name);

static void init_class(SgClass *klass, const SgChar *name,
		       SgLibrary *lib, SgObject supers, SgSlotAccessor *specs,
		       int flags);

static SgObject redefine_instance_class(SgObject obj, SgClass *old);

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

static SgObject class_list_to_array(SgObject lst, long len)
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
    if (Sg_TypeP(*lst, SG_CLASS_EQL_SPECIALIZER)) {
      SgObject cdr = SG_LIST1(SG_EQL_SPECIALIZER(*lst)->object);
      SgObject name;
      switch (SG_EQL_SPECIALIZER(*lst)->type) {
      case SG_EQ_SPECIALIZER: name = Sg_Cons(SG_INTERN("eq"), cdr);
      case SG_EQV_SPECIALIZER: name = Sg_Cons(SG_INTERN("eql"), cdr);
      case SG_EQUAL_SPECIALIZER: name = Sg_Cons(SG_INTERN("equal"), cdr);
      default: name = Sg_Cons(SG_INTERN("unknown"), cdr);
      }
      SG_APPEND1(h, t, name);
    } else {
      SG_APPEND1(h, t, (*lst)->name);
    }
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
		     SG_CLASS_OBJECT_CPL);

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_NextMethodClass, next_method_print);

static int actual_slots(SgClass *klass)
{
  SgSlotAccessor **ac = klass->gettersNSetters;
  int i, c = 0;

  for (i = 0; i < klass->nfields; i++) {
    if (ac[i]->index >= 0) c++;
  }
  return c;
}

SgObject Sg_AllocateInstance(SgClass *klass)
{
  SgObject obj = SG_NEW2(SgObject, klass->coreSize);
  SgObject *slots;
  int i, size;

  switch (SG_CLASS_CATEGORY(klass)) {
  case SG_CLASS_BASE:
  case SG_CLASS_SCHEME:
    size = actual_slots(klass);
    slots = SG_NEW_ARRAY(SgObject, size);
    for (i = 0; i < size; i++) {
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
  ac->name = SG_CAR(name);
  ac->klass = klass;
  ac->index = index;
  ac->getter = NULL;
  ac->setter = NULL;
  ac->getterS = SG_FALSE;
  ac->setterS = SG_FALSE;
  ac->boundP = SG_FALSE;
  ac->definition = name;		/* book keeping for redefinition */
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

static void set_method_debug_name(SgMethod *m, SgGeneric *g)
{
  SgObject body = SG_METHOD_PROCEDURE(m);
  SgClass **tmp, **specarray = SG_METHOD_SPECIALIZERS(m);
  int speclen = 0;
  for (tmp = specarray; *tmp; tmp++) speclen++;

  SG_PROCEDURE_NAME(m) = Sg_Cons(SG_PROCEDURE_NAME(g),
				 class_list_to_names(specarray, speclen));
  /* mostly true */
  if (SG_CLOSUREP(body)) {
    SG_CODE_BUILDER(SG_CLOSURE(body)->code)->name = SG_PROCEDURE_NAME(m);
  }
}

/*
  Some of CLOS operations, such as add-method, remove-method and
  redefining class needs to be aware of current environment so
  that it won't propagate the changes to other environments.
  Basically, if the current environment (library) is a child
  environment then we need to do some trick (or simply raises an
  error).
 */
static int in_global_context_p()
{
  /* if (Sg_MainThreadP()) return TRUE; */
  /* return Sg_VM()->state == IMPORTING; */
  return !SG_CHILD_LIBRARYP(Sg_VMCurrentLibrary());
}

/* some helpers */
static SgObject get_thead_local_methods(SgGeneric *gf)
{
  SgObject gslot = Sg_Assq(gf, SG_LIBRARY_GENERICS(Sg_VMCurrentLibrary()));
  if (SG_FALSEP(gslot)) return SG_NIL;
  return SG_CDDR(gslot);
}

static SgObject get_all_methods(SgGeneric *gf)
{
  SgObject ms = get_thead_local_methods(gf);
  if (SG_NULLP(ms)) return  SG_GENERIC_METHODS(gf);
  /* thread local first */
  return Sg_Append2(ms, SG_GENERIC_METHODS(gf));
}

static int generic_max_reqargs(SgGeneric *gf)
{
  SgObject gslot = Sg_Assq(gf, SG_LIBRARY_GENERICS(Sg_VMCurrentLibrary()));
  if (SG_FALSEP(gslot)) return SG_GENERIC_MAX_REQARGS(gf);
  return (int)(intptr_t)SG_CADR(gslot);
}

static int check_method(SgObject methods, SgMethod *method,
			int replaceP, int *errorP)
{
  SgObject mp;
  SG_FOR_EACH(mp, methods) {
    SgMethod *mm = SG_METHOD(SG_CAR(mp));
    if (SG_PROCEDURE_REQUIRED(method) == SG_PROCEDURE_REQUIRED(mm) &&
	SG_PROCEDURE_OPTIONAL(method) == SG_PROCEDURE_OPTIONAL(mm) &&
	SG_EQ(SG_METHOD_QUALIFIER(method), SG_METHOD_QUALIFIER(mm))) {
      SgClass **sp1 = SG_METHOD_SPECIALIZERS(method);
      SgClass **sp2 = SG_METHOD_SPECIALIZERS(mm);
      int required = SG_PROCEDURE_REQUIRED(method), i;
      for (i = 0; i < required; i++) {
	if (sp1[i] != sp2[i]) break;
      }
      if (i == required) {
	if (replaceP) {
	  SG_SET_CAR(mp, SG_OBJ(method));
	} else if (errorP) {
	  *errorP = TRUE;
	}
	return TRUE;
      }
    }
  }
  return FALSE;
}
/* TODO the code is now messy referctor me */
SgObject Sg_AddMethod(SgGeneric *generic, SgMethod *method)
{
  SgObject pair, whereToAdd, gslot = SG_NIL;
  int reqs = generic_max_reqargs(generic), replaced = FALSE, mainP;
  int errp = FALSE;
  if (method->generic && method->generic != generic) {
    Sg_Error(UC("method %S already added to a generic function %S"),
	     method, method->generic);
  }
  if (!SG_FALSEP(Sg_Memq(SG_OBJ(method), SG_GENERIC_METHODS(generic)))) {
    Sg_Error(UC("method %S already appears in a method list of generic %S "
		"something wrong in MOP implementation?"),
	     method, method->generic);
  }
  /* ok set it here :) */
  if (!method->generic) set_method_debug_name(method, generic);

  method->generic = generic;
  /* pre-allcate cons pair to avoid triggering GC */
  mainP = in_global_context_p();
  if (mainP) {
    whereToAdd = SG_GENERIC_METHODS(generic);
  } else {
    SgObject lib = Sg_VMCurrentLibrary();
    gslot = Sg_Assq(generic, SG_LIBRARY_GENERICS(lib));
    if (SG_FALSEP(gslot)) {
      gslot = SG_LIST2(generic, SG_OBJ(0));
      whereToAdd = SG_NIL;
      SG_LIBRARY_GENERICS(lib) = Sg_Cons(gslot, SG_LIBRARY_GENERICS(lib));
    } else {
      whereToAdd = SG_CDDR(gslot);
    }
  }
  pair = Sg_Cons(SG_OBJ(method), whereToAdd);
  if (SG_PROCEDURE_REQUIRED(method) > (unsigned int)reqs) {
    reqs = SG_PROCEDURE_REQUIRED(method);
  }
  Sg_LockMutex(&generic->mutex);
  /* Check if a method with the same signature exists */
  replaced = check_method(SG_GENERIC_METHODS(generic),method, mainP, &errp);
  if (errp) {
    Sg_UnlockMutex(&generic->mutex);
    /* we don't allow to replace the one defined in root vm */
    Sg_Error(UC("method %S of generic %S is defined in main thread, "
		"child thread can not replace it"),
	     method, method->generic);
  }
  if (!replaced) {
    if (mainP) {
      SG_GENERIC_METHODS(generic) = pair;
      SG_GENERIC_MAX_REQARGS(generic) = reqs;
    } else {
      replaced = check_method(SG_CDDR(gslot), method, TRUE, NULL);
      if (!replaced) {
	SG_SET_CDR(SG_CDR(gslot), pair);
	SG_SET_CAR(SG_CDR(gslot), SG_OBJ((intptr_t)reqs));
      }
    }
  }
  Sg_UnlockMutex(&generic->mutex);
  return SG_UNDEF;
}

/*
  remove-method is sort of otherway around.
  even the method exists on parent thread however it won't be
  removed from parent thread. child thread can't modify parent
  thread.
 */
/* TODO the code is now messy referctor me */
SgObject Sg_RemoveMethod(SgGeneric *gf, SgMethod *m)
{
  SgObject mp, gslot = SG_NIL;
  int mainP, maxReq = 0;
  if (!SG_METHOD_GENERIC(m) || SG_METHOD_GENERIC(m) != gf) return SG_UNDEF;

  Sg_LockMutex(&gf->mutex);
  mainP = in_global_context_p();
  if (mainP) {
    mp = SG_GENERIC_METHODS(gf);
  } else {
    gslot = Sg_Assq(gf, SG_LIBRARY_GENERICS(Sg_VMCurrentLibrary()));
    if (SG_FALSEP(gslot)) {
      mp = SG_NIL;
    } else {
      mp = SG_CDDR(gslot);
    }
  }
  if (SG_PAIRP(mp)) {
    if (SG_EQ(SG_CAR(mp), SG_OBJ(m))) {
      if (mainP) {
	SG_GENERIC_METHODS(gf) = SG_CDR(mp);
      } else {
	/* never be #f */
	SG_SET_CDR(SG_CDR(gslot), SG_CDR(mp));
      }
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
  if (mainP) {
    mp = SG_GENERIC_METHODS(gf);
    maxReq = SG_GENERIC_MAX_REQARGS(gf);
  } else {
    if (SG_FALSEP(gslot)) {
      mp = SG_NIL;
    } else {
      mp = SG_CDDR(gslot);
      maxReq = (int)(intptr_t)SG_CADR(gslot);
    }
  }
  SG_FOR_EACH(mp, mp) {
    /* sync # of required selector */
    if (SG_PROCEDURE_REQUIRED(SG_CAR(mp)) > (unsigned int)maxReq) {
      if (mainP) {
	SG_GENERIC_MAX_REQARGS(gf) = SG_PROCEDURE_REQUIRED(SG_CAR(mp));
      } else {
	intptr_t mr = SG_PROCEDURE_REQUIRED(SG_CAR(mp));
	SG_SET_CAR(SG_CDR(gslot), SG_OBJ(mr));
      }
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
  long len = Sg_Length(sequence);
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

/*
  compute-slots
  
  The computed slots' order is reversed order than before.
  e.g)
  (define-class <point> ()
    (x y))
  (define-class <point2> (<point>)
    (x1 y1))
  The <point2> of class-slots returns ((x) (y) (x1) (y1))
  so that the accessor always indicates the same position
  of instance slots. This makes slot accessing using class
  consistent.
 */
SgObject Sg_ComputeSlots(SgClass *klass)
{
  SgObject slots = SG_NIL;
  SgObject cp, sp;
  SG_FOR_EACH(cp, klass->cpl) {
    SgObject acc = SG_NIL;
    ASSERT(Sg_TypeP(SG_CAR(cp), SG_CLASS_CLASS));
    SG_FOR_EACH(sp, SG_CLASS(SG_CAR(cp))->directSlots) {
      SgObject slot = SG_CAR(sp);
      ASSERT(SG_PAIRP(slot));
      /* copy all slots */
      acc = Sg_Cons(Sg_CopyList(slot), acc);
    }
    if (!SG_NULLP(acc)) {
      slots = Sg_Append2X(Sg_ReverseX(acc), slots);
    }
  }
  return slots;
}

SgObject Sg_MakeSlotAccessor(SgClass *klass, SgObject slot, int index,
			     SgObject getter, SgObject setter, SgObject boundP)
{
  SgSlotAccessor *sac = make_slot_accessor(klass, slot, index);

  if (!SG_FALSEP(getter)) sac->getterS = getter;
  if (!SG_FALSEP(setter)) sac->setterS = setter;
  if (!SG_FALSEP(boundP)) sac->boundP  = boundP;
  return SG_OBJ(sac);
}

int Sg_ApplicableP(SgObject c, SgObject arg)
{
  return !SG_FALSEP(Sg_Memq(c, SG_CLASS(Sg_ClassOf(arg))->cpl));
}

#define PREALLOC_SIZE 32

static int compare_eql_specializer(SgObject sp, SgObject obj)
{
  SgObject object = SG_EQL_SPECIALIZER(sp)->object;
  switch (SG_EQL_SPECIALIZER(sp)->type) {
  case SG_EQ_SPECIALIZER: return SG_EQ(obj, object);
  case SG_EQV_SPECIALIZER: return Sg_EqvP(obj, object);
  case SG_EQUAL_SPECIALIZER: return Sg_EqualP(obj, object);
  default: return FALSE; /* unknown specializer, so no idea how to compare */
  }
}

int Sg_CompareEqlSpecializer(SgObject sp, SgObject obj)
{
  return compare_eql_specializer(sp, obj);
}

static int specializer_match(SgObject sp, SgObject obj)
{
  return (SG_EQL_SPECIALIZERP(sp) && compare_eql_specializer(sp, obj))
    || Sg_TypeP(obj, sp);
}

static SgObject compute_applicable_methods(SgGeneric *gf, SgObject *argv,
					   int argc, int applyargs)
{
  SgObject methods = get_all_methods(gf), mp;
  SgObject h = SG_NIL, t = SG_NIL;
  SgObject *args = argv;
  int nsel;

  if (SG_NULLP(methods)) return SG_NIL;

  nsel = generic_max_reqargs(gf);
  if (applyargs) argc--;
  if (applyargs && nsel) {
    int size = (int)Sg_Length(argv[argc]) + argc, i;
    SgObject ap;
    args = SG_NEW_ARRAY(SgObject, size);
    for (i = 0; i < argc; i++) {
      args[i] = argv[i];
    }
    SG_FOR_EACH(ap, argv[argc]) {
      if (--nsel >= 0) args[i++] = SG_CAR(ap);
    }
    argc = size;
  }

  SG_FOR_EACH(mp, methods) {
    SgMethod *m = SG_METHOD(SG_CAR(mp));
    SgClass **sp;
    SgObject *ap;
    unsigned int n;
    /* argument count check */
    if ((unsigned int)argc < SG_PROCEDURE_REQUIRED(m)) continue;
    if (!SG_PROCEDURE_OPTIONAL(m) &&
	(unsigned int)argc > SG_PROCEDURE_REQUIRED(m)) continue;
    /* type check */
    for (ap = args, sp = SG_METHOD_SPECIALIZERS(m), n = 0;
	 n < SG_PROCEDURE_REQUIRED(m); ap++, sp++, n++) {
      if (!specializer_match(*sp, *ap)) break;
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
  /* if we have eql specializer, then it's always more specific! */
  if (Sg_TypeP(c2, SG_CLASS_EQL_SPECIALIZER)) return FALSE;
  if (Sg_TypeP(c1, SG_CLASS_EQL_SPECIALIZER)) return TRUE;

  /* ok, non eql case. */
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
    if (!SG_EQ(spec1[i], spec2[i]) && spec1[i] && spec2[i]) {
      return more_specific_p(spec1[i], spec2[i], targv[i]);
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

static SgObject* sort_method_by_qualifier(SgObject methods, SgObject *result,
					  int checkType)
{
  SgObject cp, art = SG_NIL, bt = SG_NIL, pt = SG_NIL, aft = SG_NIL;
  int i;
  for (i=0; i<QUALIFIER_COUNT; i++) result[i] = SG_NIL;
  SG_FOR_EACH(cp, methods) {
    SgMethod *m = SG_METHOD(SG_CAR(cp));
    if (checkType && !SG_METHODP(m)) {
      Sg_Error(UC("method required but got %S"), m);
    }
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

  len = (int)Sg_Length(methods);
  /* TODO maybe we should use alloca */
  if (len >= PREALLOC_SIZE)  array = SG_NEW_ARRAY(SgObject, len);
  /* if this is apply call we need to expand the arguments */
  if (applyargs) {
    int n = (int)Sg_Length(argv[argc-1]);
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
  int argc = (int)(intptr_t)data[2];
  /* store the result of :primary methods */
  if (dvec[2]) dvec[3] = result;
  if (SG_NULLP(proc)) return dvec[3]; /* no more methods */
  return procedure_invoker(args, argc, dvec);
}

/* unpack argument for :before and :after qualifier.
   It's attempting to reuse the oargs' last position when the optional
   argument has only one length however DO NOT DO IT! It's the VM's stack
   so if you modify it, it causes invalid argument (not an error but
   unexpected value will be passed).
   e.g)
     ;; args of *1 will be '() but that's not what we want!
     (define-method hoge :before ((a <base>) b . args) args) ;; *1
     (define-method hoge :before ((a <sub>) b . args) args)
     (define-method hoge ((a <sub>) b . args) args)
     (hoge (make <sub>) 'b 'c)
   And we don't have to copy the args during the method creation since
   we know it's a valid pointer during method chain (VM stack) and
   we allocate when we unpack the argument :)
 */
static SgObject unpack_argument(SgObject proc, SgObject **oargs, int *oargc,
				SgObject rest)
{
  int argc = *oargc;
  SgObject *args = *oargs;
  SgObject opts = args[--argc];
  if (SG_NULLP(opts)) {
    /* easy */
    *oargc = argc;
    return Sg_MakeNextMethod(SG_METHOD_GENERIC(proc), rest, args, argc, FALSE);
  } else {
    int len = (int)Sg_Length(opts), i;
    int size = argc + len;
    SgObject *newargs = SG_NEW_ARRAY(SgObject, size), cp;
    for (i = 0; i < argc; i++) {
      newargs[i] = args[i];
    }
    SG_FOR_EACH(cp, opts) {
      newargs[argc++] = SG_CAR(cp);
    }
    *oargs = newargs;
    *oargc = argc;
    return Sg_MakeNextMethod(SG_METHOD_GENERIC(proc), rest, newargs,
			     argc, FALSE);
  }
  
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
  /* prepare call frame */
  next[0] = dvec;
  next[1] = SG_OBJ(args);
  next[2] = SG_OBJ((intptr_t)argc);
  if (SG_EQ(SG_METHOD_QUALIFIER(proc), SG_KEYWORD_PRIMARY)) {
    /* compute next-method */
    /* issue 119 check subr otherwise wrong number error will be raised */
    if (!SG_SUBRP(SG_METHOD_PROCEDURE(proc))) {
      SgObject rest = SG_OBJ(dvec[1]);
      /* unpack optional argument if the procedure accepts it
	 to avoid packing twice. */
      if (SG_PROCEDURE_OPTIONAL(SG_METHOD_PROCEDURE(proc))) {
	SgObject m = unpack_argument(proc, &args, &argc, rest);
	SG_APPEND1(h, t, m);
      } else {
	SG_APPEND1(h, t, Sg_MakeNextMethod(SG_METHOD_GENERIC(proc), rest,
					   args, argc, FALSE));
      }
    }
    dvec[2] = (void*)TRUE;
  } else {
    /* dummy, :before and :after can not have next-method. */
    if (SG_PROCEDURE_OPTIONAL(SG_METHOD_PROCEDURE(proc))) {
      SgObject m = unpack_argument(proc, &args, &argc, SG_NIL);
      SG_APPEND1(h, t, m);
    } else {
      SG_APPEND1(h, t, Sg_MakeNextMethod(SG_METHOD_GENERIC(proc), SG_NIL,
					 args, argc, FALSE));
    }
    dvec[2] = (void*)FALSE;
  }
  Sg_VMPushCC(invoke_cc, next, 3);
  for (i = 0; i < argc; i++) {
    SG_APPEND1(h, t, args[i]);
  }
  return Sg_VMApply(SG_METHOD_PROCEDURE(proc), h);
}

static SgObject compute_around_methods_rec(SgObject around, SgObject before,
					   SgObject primary, SgObject after)
{
  SgObject m, rest;
  SgObject proc, name = SG_UNDEF;
  SgObject result = SG_NIL, t = SG_NIL, mp;
  SgClass **specs = NULL;
  void **dvec;
  int req = -1, opt = -1;

  if (SG_NULLP(primary)) return SG_NIL;
  
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
  SG_METHOD_QUALIFIER(m) = SG_FALSE;
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

static SgObject compute_around_methods(SgObject around, SgObject before,
				       SgObject primary, SgObject after,
				       SgObject *argv, int argc, int applyargs)
{
  /* lazyness */
  primary = sort_primary_methods(primary, argv, argc, applyargs);
  /* if there is no primary method, then it must be an error */
  if (SG_NULLP(primary)) {
    return SG_NIL;
  }

  around  = sort_primary_methods(around, argv, argc, applyargs);
  before  = sort_primary_methods(before, argv, argc, applyargs);
  after   = sort_primary_methods(after, argv, argc, applyargs);

  return compute_around_methods_rec(around, before, primary, after);
}

static SgObject sort_method(SgObject methods, SgObject *argv, int argc,
			    int applyargs)
{
  SgObject qualified_methods[QUALIFIER_COUNT];
  SgObject primary, before, after, around;
  sort_method_by_qualifier(methods, qualified_methods, FALSE);
  primary = qualified_methods[PRIMARY_INDEX];
  before  = qualified_methods[BEFORE_INDEX];
  after   = qualified_methods[AFTER_INDEX];
  around  = qualified_methods[AROUND_INDEX];

  if (SG_NULLP(around) && SG_NULLP(before) && SG_NULLP(after)) {
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
  if (SG_NULLP(SG_CDR(applicable))) return applicable;
  return sort_method(applicable, argv, argc, applyargs);
}

#define VMSLOT_UNBOUND(klass, obj, slot)		\
  Sg_VMApply3(SG_OBJ(&Sg_GenericSlotUnbound),		\
	      SG_OBJ(klass), obj, slot)

#define VMSLOT_MISSING3(klass, obj, slot)                 \
  Sg_VMApply3(SG_OBJ(&Sg_GenericSlotMissing),		  \
	      SG_OBJ(klass), obj, slot)

#define VMSLOT_MISSING4(klass, obj, slot, val)		\
  Sg_VMApply4(SG_OBJ(&Sg_GenericSlotMissing),		\
	      SG_OBJ(klass), obj, slot, val)

#define SLOT_UNBOUND(klass, obj, slot)		\
  Sg_Apply3(SG_OBJ(&Sg_GenericSlotUnbound),	\
	    SG_OBJ(klass), obj, slot)

#define SLOT_MISSING3(klass, obj, slot)			  \
  Sg_Apply3(SG_OBJ(&Sg_GenericSlotMissing),		  \
	    SG_OBJ(klass), obj, slot)

#define SLOT_MISSING4(klass, obj, slot, val)		\
  Sg_Apply4(SG_OBJ(&Sg_GenericSlotMissing),		\
	    SG_OBJ(klass), obj, slot, val)


static SgSlotAccessor* lookup_slot_info(SgClass *klass, SgObject name)
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
  return NULL;		/* dummy */
}

/* FIXME this is really inefficient */
static SgObject c_getter_wrapper(SgObject *argv, int argc, void *data)
{
  if (argc != 1) {
    Sg_WrongNumberOfArgumentsViolation(SG_INTERN("slot getter"), 1, argc,
				       (argc > 0) ? argv[0] : SG_NIL);
  }
  return ((SgSlotGetterProc)data)(argv[0]);
}

static SgObject c_setter_wrapper(SgObject *argv, int argc, void *data)
{
  if (argc != 2) {
    Sg_WrongNumberOfArgumentsViolation(SG_INTERN("slot setter"), 2, argc,
				       (argc > 0) ? argv[0] : SG_NIL);
  }
  ((SgSlotSetterProc)data)(argv[0], argv[1]);
  return SG_UNDEF;
}


SgObject Sg_ComputeGetterAndSetter(SgClass *klass, SgObject slot)
{
  /* remove klass itself from cpl */
  SgObject rcpl = SG_CDR(klass->cpl), cp;
  SgObject ds = klass->directSlots, s = SG_CAR(slot);
  SgObject getter = SG_FALSE, setter = SG_FALSE;

  SG_FOR_EACH(cp, rcpl) {
    SgObject check = Sg_Assq(s, ds);
    if (SG_FALSEP(check)) {
      SgSlotAccessor *sac = lookup_slot_info(SG_CLASS(SG_CAR(cp)), s);

      if (!sac) continue;
      if (sac->getter && SG_FALSEP(getter)) {
	/* TODO, can we assume C getter is transparent? */
	getter = Sg_MakeSubr(c_getter_wrapper, sac->getter, 1, 0, s);
      }
      if (sac->setter && SG_FALSEP(setter)) {
	setter = Sg_MakeSubr(c_setter_wrapper, sac->setter, 2, 0, s);
      }
      if (!SG_FALSEP(getter) && !SG_FALSEP(setter)) break;
    }
  }
  /* by default bound? is not defined */
  return SG_LIST3(getter, setter, SG_FALSE);
}

static SgObject slot_boundp_cc(SgObject result, void **data)
{
  return SG_FALSEP(result) ? SG_FALSE: SG_TRUE;
}

static inline SgObject slot_ref_cc(SgObject result, void **data)
{
  SgObject obj = data[0];
  SgObject slot = data[1];
  int boundp = (int)(intptr_t)data[2];
  if (SG_UNBOUNDP(result) || SG_UNDEFP(result)) {
    if (boundp) return SG_FALSE;
    else return VMSLOT_UNBOUND(Sg_ClassOf(obj), obj, slot);
  } else {
    if (boundp) return SG_TRUE;
    else        return result;
  }
}

static SgObject slot_ref_rec(SgClass* klass, SgObject obj,
			     SgObject name, int boundp)
{
  SgSlotAccessor *accessor = lookup_slot_info(klass, name);
  void *data[3];
  if (accessor) {
    if (accessor->getter) {
      data[0] = obj;
      data[1] = name;
      data[2] = (void*)(intptr_t)boundp;
      return slot_ref_cc(accessor->getter(obj), data);
    } else {
      /* scheme accessor, assume obj is instance */
      if (boundp && SG_PROCEDUREP(accessor->boundP)) {
	data[0] = obj;
	data[1] = name;
	data[2] = (void*)(intptr_t)boundp;
	Sg_VMPushCC(slot_boundp_cc, data, 3);
	return Sg_VMApply1(accessor->boundP, obj);
      } else if (!SG_PROCEDUREP(accessor->getterS)) {
	SgObject val = SG_INSTANCE(obj)->slots[accessor->index];
	data[0] = obj;
	data[1] = name;
	data[2] = (void*)(intptr_t)boundp;
	return slot_ref_cc(val, data);
      } else {
	/* Hope this will be removed by compiler... */
	data[0] = obj;
	data[1] = name;
	data[2] = (void*)(intptr_t)boundp;
	Sg_VMPushCC(slot_ref_cc, data, 3);
	return Sg_VMApply1(accessor->getterS, obj);
      }
    }
  } else {
    return VMSLOT_MISSING3(klass, obj, name);
  }
}

static SgObject vmslot_ref_cc(SgObject result, void **data)
{
  return Sg_VMSlotRef(SG_OBJ(data[0]), SG_OBJ(data[1]));
}

SgObject Sg_VMSlotRef(SgObject obj, SgObject name)
{
  SgClass *klass = Sg_ClassOf(obj);
  if (!SG_FALSEP(klass->redefined)) {
    void *data[2];
    data[0] = obj;
    data[1] = name;
    Sg_VMPushCC(vmslot_ref_cc, data, 2);
    return redefine_instance_class(obj, klass);
  }
  return slot_ref_rec(klass, obj, name, FALSE);
}

static SgObject slot_set_rec(SgClass *klass, SgObject obj,
			     SgObject name, SgObject value)
{
  SgSlotAccessor *accessor = lookup_slot_info(klass, name);
  if (accessor) {
    if (accessor->setter) {
      accessor->setter(obj, value);
      return SG_UNDEF;
    } else {
      /* scheme accessor */
      if (!SG_PROCEDUREP(accessor->setterS)) {
	SG_INSTANCE(obj)->slots[accessor->index] = value;
	return SG_UNDEF;
      } else {
	return Sg_VMApply2(accessor->setterS, obj, value);
      }
    }
  } else {
    return VMSLOT_MISSING4(klass, obj, name, value);
  }
}

static SgObject vmslot_set_cc(SgObject result, void **data)
{
  return Sg_VMSlotSet(SG_OBJ(data[0]), SG_OBJ(data[1]), SG_OBJ(data[2]));
}

SgObject Sg_VMSlotSet(SgObject obj, SgObject name, SgObject value)
{
  SgClass *klass = Sg_ClassOf(obj);
  if (!SG_FALSEP(klass->redefined)) {
    void *data[3];
    data[0] = obj;
    data[1] = name;
    data[2] = value;
    Sg_VMPushCC(vmslot_set_cc, data, 3);
    return redefine_instance_class(obj, klass);
  }
  return slot_set_rec(klass, obj, name, value);
}

/* For now, these 2 are really simple */
SgObject Sg_SlotRefUsingAccessor(SgObject obj, SgSlotAccessor *ac)
{
  SgClass *klass = ac->klass;
  if (!SG_ISA(obj, klass)) {
    Sg_AssertionViolation(SG_INTERN("slot-ref-using-accessor"),
			  Sg_Sprintf(UC("object is not a type of %S"), klass),
			  obj);
  }
  if (ac->getter) {
    return ac->getter(obj);
  } else {
    if (ac->index < 0) {
      // virtual slot
      if (SG_PROCEDUREP(ac->getterS)) {
	return Sg_Apply1(ac->getterS, obj);
      } else {
	Sg_AssertionViolation(SG_INTERN("slot-ref-using-accessor"),
			      Sg_Sprintf(UC("Slot %A is virtual but no getter"),
					 ac->name),
			      obj);
      }
    }
    return SG_INSTANCE(obj)->slots[ac->index];
  }
}

int Sg_SlotBoundUsingAccessor(SgObject obj, SgSlotAccessor *ac)
{
  SgObject v = Sg_SlotRefUsingAccessor(obj, ac);
  return !(SG_UNBOUNDP(v) || SG_UNDEFP(v));
}

void Sg_SlotSetUsingAccessor(SgObject obj, SgSlotAccessor *ac, SgObject value)
{
  SgClass *klass = ac->klass;
  if (!SG_ISA(obj, klass)) {
    Sg_AssertionViolation(SG_INTERN("slot-set-using-accessor!"),
			  Sg_Sprintf(UC("object is not a type of %S"), klass),
			  obj);
  }
  if (ac->setter) {
    ac->setter(obj, value);
  } else {
    if (ac->index < 0) {
      // virtual slot
      if (SG_PROCEDUREP(ac->setterS)) {
	Sg_Apply2(ac->setterS, obj, value);
      } else {
	Sg_AssertionViolation(SG_INTERN("slot-set-using-accessor!"),
			      Sg_Sprintf(UC("Slot %A is virtual but no setter"),
					 ac->name),
			      obj);
      }
    } else {
      SG_INSTANCE(obj)->slots[ac->index] = value;
    }
  }
}

SgObject Sg_SlotRefUsingClass(SgClass *klass, SgObject obj, SgObject name)
{
  SgSlotAccessor *ac;
  if (!SG_ISA(obj, klass)) {
    Sg_Error(UC("object %S is not an instance of class %S"), obj, klass);
  }
  ac = lookup_slot_info(klass, name);
  if (!ac) Sg_Error(UC("class %S doesn't have slot named %S."), klass, name);
  return Sg_SlotRefUsingAccessor(obj, ac);
}

void Sg_SlotSetUsingClass(SgClass *klass, SgObject obj, SgObject name,
			  SgObject value)
{
  SgSlotAccessor *ac;
  if (!SG_ISA(obj, klass)) {
    Sg_Error(UC("object %S is not an instance of class %S"), obj, klass);
  }
  ac = lookup_slot_info(klass, name);
  if (!ac) Sg_Error(UC("class %S doesn't have slot named %S."), klass, name);
  Sg_SlotSetUsingAccessor(obj, ac, value);
}

int Sg_SlotBoundUsingClass(SgClass *klass, SgObject obj, SgObject name)
{
  SgSlotAccessor *ac = lookup_slot_info(klass, name);
  if (!ac) Sg_Error(UC("class %S doesn't have slot named %S."), klass, name);
  return !SG_UNBOUNDP(Sg_SlotRefUsingAccessor(obj, ac));
}

static SgObject vmslot_boundp_cc(SgObject result, void **data)
{
  return Sg_VMSlotBoundP(SG_OBJ(data[0]), SG_OBJ(data[1]));
}

SgObject Sg_VMSlotBoundP(SgObject obj, SgObject slot)
{
  SgClass *klass = Sg_ClassOf(obj);
  if (!SG_FALSEP(klass->redefined)) {
    void *data[2];
    data[0] = obj;
    data[1] = slot;
    Sg_VMPushCC(vmslot_boundp_cc, data, 2);
    return redefine_instance_class(obj, klass);
  }
  return slot_ref_rec(klass, obj, slot, TRUE);
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
#ifdef USE_IMMEDIATE_FLONUM
    if (SG_IFLONUMP(obj)) return SG_CLASS_REAL;
#endif
    else return SG_CLASS_UNKNOWN;
  }
  if (SG_FLONUMP(obj)) return SG_CLASS_REAL;
  if (SG_PAIRP(obj)) return SG_CLASS_PAIR;
  return SG_CLASS_OF(obj);
}

static SgObject vmclassof_cc(SgObject result, void **data)
{
  return Sg_VMClassOf(result);
}

SgObject Sg_VMClassOf(SgObject obj)
{
  /* for now */
  SgClass *klass = Sg_ClassOf(obj);
  if (!SG_FALSEP(klass->redefined)) {
    Sg_VMPushCC(vmclassof_cc, NULL, 0);
    return redefine_instance_class(obj, klass);
  }
  return SG_OBJ(klass);
}

static SgObject vmisa_cc(SgObject result, void **data)
{
  return Sg_VMIsA(SG_OBJ(data[0]), SG_CLASS(data[1]));
}

SgObject Sg_VMIsA(SgObject obj, SgClass *klass)
{
  /* for now */
  SgClass *k = Sg_ClassOf(obj);
  if (!SG_FALSEP(k->redefined)) {
    void *data[2];
    data[0] = obj;
    data[1] = klass;
    Sg_VMPushCC(vmisa_cc, data, 2);
    return redefine_instance_class(obj, k);
  }
  return SG_MAKE_BOOL(Sg_SubtypeP(k, klass));
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

static void class_finalize(SgObject obj, void *data)
{
  Sg_DestroyMutex(&SG_CLASS(obj)->mutex);
  Sg_DestroyCond(&SG_CLASS(obj)->cv);
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
  instance->directSubclasses = SG_NIL;
  instance->creader = SG_FALSE;
  instance->cscanner = SG_FALSE;
  instance->cwriter = SG_FALSE;
  instance->redefined = SG_FALSE;
  instance->library = SG_FALSE;

  Sg_InitMutex(&instance->mutex, FALSE);
  Sg_InitCond(&instance->cv);
  /* we may have redefinition and class may be GCed
     so add finalizer to destroy mutex */
  Sg_RegisterFinalizer(SG_OBJ(instance), class_finalize, NULL);
  return SG_OBJ(instance);
}

/* not in the header for now */
SgObject Sg_ClassAllocate(SgClass *klass, SgObject initargs)
{
  return class_allocate(klass, initargs);
}

/*
  <class> slot accessors

  I think it's safer to copy the all list slots in case of
  destructive operation such as set-car!. but for now I trust
  the users.
 */
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
  long len;
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
  /* TODO should we make nfields long? but who would list so many fields? */
  klass->nfields = (int)SG_INT_VALUE(nfields);
}

static SgObject class_direct_subclasses(SgClass *klass)
{
  return klass->directSubclasses;
}

/* 
   Now it's stored in reverse order but to show it in Scheme
   world we make it in proper order.
 */
static SgObject class_getters_n_setters(SgClass *klass)
{
  SgObject r = Sg_ArrayToList((SgObject*)klass->gettersNSetters,
			      klass->nfields);
  return Sg_ReverseX(r);
}

/*
  This is a bit confusing part.
  Since the computed slots are ascendant, the slot accessor must be
  set to reverse order so that the very bottom class's slot will be
  refer first.
 */
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
  getters = Sg_Reverse(getters);
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

static SgObject class_redefined(SgClass *klass)
{
  return klass->redefined;
}

static SgObject class_library(SgClass *klass)
{
  return klass->library;
}

static void class_library_set(SgClass *klass, SgObject lib)
{
  if (!SG_FALSEP(lib) && !SG_LIBRARYP(lib)) {
    Sg_Error(UC("library or #f required, but got %S"), lib);
  }
  klass->library = lib;
}

static SgObject class_initargs(SgClass *klass)
{
  return klass->initargs;
}

static void class_initargs_set(SgClass *klass, SgObject initargs)
{
  if (!SG_LISTP(initargs)) {
    Sg_Error(UC("list required, but got %S"), initargs);
  }
  klass->initargs = initargs;
}


void Sg_AddDirectSubclass(SgClass *super, SgClass *sub)
{
  /* built in classes can't have subclass.
     if we consider the base class, then <top> must have
     all the sub classes and that's basically the same as
     accepting builtin class. I think... so we only consider
     Scheme defined class. */
  if (SG_CLASS_CATEGORY(super) == SG_CLASS_SCHEME) {
    /* lock the class */
    Sg_LockMutex(&super->mutex);
    if (SG_FALSEP(Sg_Memq(sub, super->directSubclasses))) {
      super->directSubclasses = Sg_Cons(sub, super->directSubclasses);
    }
    Sg_UnlockMutex(&super->mutex);
  }
}

void Sg_RemoveDirectSubclass(SgClass *super, SgClass *sub)
{
  if (SG_CLASS_CATEGORY(super) == SG_CLASS_SCHEME) {
    /* lock the class */
    Sg_LockMutex(&super->mutex);
    /* should we make Sg_Remq and Sg_RemqX? */
    super->directSubclasses = deletel(sub, super->directSubclasses);
    Sg_UnlockMutex(&super->mutex);
  }
}

static SgObject redefine_instance_class(SgObject obj, SgClass *old)
{
  /* MT safe */
  SgObject newc;
  Sg_LockMutex(&old->mutex);
  while (!SG_ISA(old->redefined, SG_CLASS_CLASS)) {
    Sg_Wait(&old->cv, &old->mutex);
  }
  newc = old->redefined;
  Sg_UnlockMutex(&old->mutex);
  if (SG_CLASSP(newc)) {
    return Sg_VMApply2(&Sg_GenericChangeClass, obj, newc);
  } else {
    return SG_OBJ(old);
  }
}

/*
  to redefine a class, we need world lock.
  not sure how much trouble are there if i implement this lock very
  naive way like this. but for now.
 */
static struct {
  int dummy;
  SgInternalMutex mutex;
  SgInternalCond  cv;
} class_world_lock = {-1, };

static void lock_world()
{
  Sg_LockMutex(&class_world_lock.mutex);
}

static void unlock_world()
{
  Sg_UnlockMutex(&class_world_lock.mutex);
}

void Sg_StartClassRedefinition(SgClass *klass)
{
  SgVM *vm;
  if (SG_CLASS_CATEGORY(klass) != SG_CLASS_SCHEME) {
    Sg_Error(UC("builtin class can not redefined %S"), klass);
  }
  if (!in_global_context_p()) {
    /* now we need to check if the class is defined in the same environment.
       to detect the defined library we use simply the name of class the same
       way of define-class does (see lib/clos/user.scm).
       NOTE: the prediction is a bit naive. for example subclasses.
     */
    SgObject lib = Sg_VMCurrentLibrary();
    SgObject g = Sg_FindBinding(lib, klass->name, SG_FALSE);
    /* gloc won't be #f but in case. and if it is #f then assume it's defined
       somewhere else. */
    if (SG_FALSEP(g) || SG_GLOC(g)->library != lib) {
      Sg_Error(UC("Given class %S is defined in non child environment. "
		  "Child environment does not allow to change global class."),
	       klass);
    }
  }

  vm = Sg_VM();
  lock_world();
  Sg_LockMutex(&klass->mutex);
  if (SG_FALSEP(klass->redefined)) {
    klass->redefined = vm;
  }
  Sg_UnlockMutex(&klass->mutex);

  /* done for now */
}

void Sg_EndClassRedefinition(SgClass *klass, SgObject newklass)
{
  SgVM *vm;
  if (SG_CLASS_CATEGORY(klass) != SG_CLASS_SCHEME) return;
  if (!SG_FALSEP(newklass) && !SG_CLASSP(newklass)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("%end-class-redefinition!"),
				    SG_MAKE_STRING("class or #f"),
				    newklass, SG_LIST2(klass, newklass));
  }
  vm = Sg_VM();
  Sg_LockMutex(&klass->mutex);
  if (SG_EQ(klass->redefined, vm)) {
    klass->redefined = newklass;
    Sg_NotifyAll(&klass->cv);
  }
  Sg_UnlockMutex(&klass->mutex);

  unlock_world();
}

void Sg_ReplaceClassBinding(SgClass *oldklass, SgClass *newklass)
{
  if (!SG_LIBRARYP(oldklass->library)) return;
  if (!SG_SYMBOLP(oldklass->name)) return;
  Sg_InsertBinding(SG_LIBRARY(oldklass->library), oldklass->name,
		   SG_OBJ(newklass));
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
	    Sg_Length(SG_GENERIC_METHODS(obj)) + 
	    Sg_Length(get_thead_local_methods(SG_GENERIC(obj))));
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
  /* for MOP we need to consider all methods. */
  return get_all_methods(gf);
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
  SgObject s = Sg_Intern(Sg_String(name));
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
				      "class(es) %S of arguments"), 
				   SG_OBJ(gf), h),
			args);
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

static SgObject method_qualifier(SgMethod *method)
{
  return SG_METHOD_QUALIFIER(method);
}

static SgObject method_leaf(SgMethod *method)
{
  return SG_MAKE_BOOL(SG_METHOD_LEAF_P(method));
}

/* next method */
static void next_method_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgNextMethod *nm = SG_NEXT_METHOD(obj);
  SgObject args = Sg_ArrayToList(nm->argv, nm->argc);
  Sg_Printf(port, UC("#<next-method %S %S>"), nm->methods, args);
}

static SgObject next_method_has_nextP(SgNextMethod *nm)
{
  return SG_MAKE_BOOL(!SG_NULLP(nm->methods));
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
static SgObject sa_name(SgSlotAccessor *sa)
{
  return sa->name;
}
static SgObject sa_class(SgSlotAccessor *sa)
{
  return sa->klass;
}
static SgObject sa_definition(SgSlotAccessor *sa)
{
  return sa->definition;
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
  SG_CLASS_SLOT_SPEC("direct-subclasses",  6, class_direct_subclasses, NULL),
  SG_CLASS_SLOT_SPEC("getters-n-setters",  7, class_getters_n_setters,
		     class_getters_n_setters_set),
  SG_CLASS_SLOT_SPEC("cache-reader",       8, class_cache_reader,
		     class_cache_reader_set),
  SG_CLASS_SLOT_SPEC("cache-scanner",      9, class_cache_scanner,
		     class_cache_scanner_set),
  SG_CLASS_SLOT_SPEC("cache-writer",      10, class_cache_writer,
		     class_cache_writer_set),
  SG_CLASS_SLOT_SPEC("redefined",         11, class_redefined, NULL),
  SG_CLASS_SLOT_SPEC("defined-library",   12, class_library,
		     class_library_set),
  SG_CLASS_SLOT_SPEC("initargs",          13, class_initargs, 
		     class_initargs_set),
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
  SG_CLASS_SLOT_SPEC("qualifier", 5, method_qualifier, NULL),
  /* name is taken from Gauche */
  SG_CLASS_SLOT_SPEC("method-leaf", 6, method_leaf, NULL),
  { { NULL } }
};

static SgSlotAccessor slot_accessor_slots[] = {
  SG_CLASS_SLOT_SPEC("getter",   0, sa_getter, sa_getter_set),
  SG_CLASS_SLOT_SPEC("setter",   1, sa_setter, sa_setter_set),
  SG_CLASS_SLOT_SPEC("name",     2, sa_name,   NULL),
  SG_CLASS_SLOT_SPEC("class",    3, sa_class,  NULL),
  SG_CLASS_SLOT_SPEC("definition", 4, sa_definition,  NULL),
  { { NULL } }
};

static SgSlotAccessor next_method_slots[] = {
  SG_CLASS_SLOT_SPEC("next-method?",   0, next_method_has_nextP, NULL),
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

/* Fixup the index, the operation is done destructively for the list
   but not for the accessor. */
static void fixup_slot_accessor(SgObject accs)
{
  int index = (int)Sg_Length(accs) - 1;
  SgObject cp;
  SG_FOR_EACH(cp, accs) {
    SgSlotAccessor *acc = SG_SLOT_ACCESSOR(SG_CAR(cp));
    if (acc->index != index) {
      /* copy it. */
      SgSlotAccessor *n = SG_NEW(SgSlotAccessor);
      memcpy(n, acc, sizeof(SgSlotAccessor));
      n->index = index;		/* update index */
      SG_SET_CAR(cp, n);
    }
    index--;
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
    klass->name = Sg_Intern(Sg_String(name));
    klass->library = lib;
    Sg_InsertBinding(lib, SG_SYMBOL(klass->name), SG_OBJ(klass));
  }

  /* initialize direct slots */
  if (specs) {
    for (;specs->name; specs++) {
      SgObject snam = Sg_Intern(Sg_MakeStringC(specs->cname));
      SgObject slot = SG_LIST3(snam,
			       SG_KEYWORD_INIT_KEYWORD, 
			       Sg_MakeKeyword(SG_SYMBOL(snam)->name));
      specs->klass = klass;
      specs->name = snam;
      specs->definition = slot;
      acc = Sg_Cons(SG_OBJ(&*specs), acc);
      SG_APPEND1(slots, t, slot);
    }
  }
  klass->directSlots = slots;

  /* compute other slots inherited from supers */
  for (super = klass->cpa; *super; super++) {
    SgSlotAccessor **dacc = (*super)->gettersNSetters;
    /* I think slot should have accessor info but for now */
    SgObject tmp = SG_NIL;
    for (;dacc && *dacc; dacc++) {
      tmp = Sg_Cons(SG_OBJ(*dacc), tmp);
    }
    /* A (a b) <- B (c d)

       now acc is reverse order (d c) and super is (b a)
       append super to acc */
    if (!SG_NULLP(tmp)) {
      acc = Sg_Append2X(acc, tmp);
    }
    SG_FOR_EACH(sp, (*super)->directSlots) {
      SgObject slot = SG_CAR(sp);
      ASSERT(SG_PAIRP(slot));
      slots = Sg_Cons(Sg_CopyList(slot), slots);
    }
  }
  /* fixup slot index */
  fixup_slot_accessor(acc);
  klass->gettersNSetters = (SgSlotAccessor**)Sg_ListToArray(acc, TRUE);
  klass->slots = slots;
  klass->nfields = (int)Sg_Length(slots);
  
  /* do we need this? */
  if ((flags & SG_NO_INIT_MUTEX) == 0) {
    Sg_InitMutex(&klass->mutex, FALSE);
  }
  if ((flags & SG_NO_INIT_CV) == 0) {
    Sg_InitCond(&klass->cv);
  }
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
  SgObject s = Sg_Intern(Sg_String(name));
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

SgClass* Sg_BaseClassOf(SgClass *klass)
{
  SgClass **cpa = klass->cpa, *k;
  while ((k = *cpa++) != NULL) {
    if (SG_CLASS_CATEGORY(k) == SG_CLASS_BASE) return k;
  }
  return NULL;
}

/* %swap-class-and-slots
   Swap the class and slots.
   The instances must be either instance of scheme defined class or
   inherit the same base class.
 */
void Sg_SwapClassAndSlots(SgObject newInstance, SgObject oldInstance)
{
  SgClass *newKlass = Sg_ClassOf(newInstance);
  SgClass *oldKlass = Sg_ClassOf(oldInstance);
  SgClass *base, *tmp;
  SgObject *slots;
  
  if ((base = Sg_BaseClassOf(newKlass)) == NULL ||
      !SG_EQ(base, Sg_BaseClassOf(oldKlass))) {
    Sg_Error(UC("incompatible class swap: %S <-> %S"),
	     newInstance, oldInstance);
  }
  /*
    Now the instance memory structure is like this

    | 00 | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 |
    +----+----+----+----+----+----+----+----+----+
    |               Tag                          |
    +----+----+----+----+----+----+----+----+----+
    |               Slots                        |
    +----+----+----+----+----+----+----+----+----+
    |  C defined slots ...                       |
                      :
    |                                            |
    +----+----+----+----+----+----+----+----+----+

    The coreSize has at least sizeof(SgInstance).
    If the old class has some other C defined slot
    then it also has the size. As long as the base
    class is the same then coreSize is the same.
    So swap 'Class, 'Slots' and the rest of memory.
   */
  /* swap class */
  /* get the raw class, at this point it's safe (I believe) */
  tmp = SG_CLASS_OF(oldInstance);
  SG_SET_CLASS(oldInstance, SG_CLASS_OF(newInstance));
  SG_SET_CLASS(newInstance, tmp);
  /* swap slots */
  slots = SG_INSTANCE(oldInstance)->slots;
  SG_INSTANCE(oldInstance)->slots = SG_INSTANCE(newInstance)->slots;
  SG_INSTANCE(newInstance)->slots = slots;
  /* swap extra slots */
  if (base->coreSize > (int)sizeof(SgInstance)) {
    const intptr_t offset = sizeof(SgInstance);
    uint8_t *src = (uint8_t *)((intptr_t)oldInstance + offset);
    uint8_t *dst = (uint8_t *)((intptr_t)newInstance + offset);
    int count = base->coreSize - (int)offset;
    while (count--) {
      uint8_t tmp = *src;
      *dst++ = *src;
      *src++ = tmp;
    }
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
SG_DEFINE_GENERIC(Sg_GenericAddMethod, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericRemoveMethod, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericObjectEqualP, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericObjectCompare, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericObjectHash, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericObjectApply, Sg_InvalidApply, NULL);
SG_DEFINE_GENERIC(Sg_GenericObjectSetter, Sg_InvalidApply, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeGetterAndSetter, Sg_NoNextMethod, NULL);
/* generic invocation */
SG_DEFINE_GENERIC(Sg_GenericComputeApplyGeneric, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeMethodMoreSpecificP, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericComputeApplyMethods, Sg_NoNextMethod, NULL);
/* slot */
SG_DEFINE_GENERIC(Sg_GenericSlotUnbound, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericSlotMissing, Sg_NoNextMethod, NULL);
/* unbound-variable */
SG_DEFINE_GENERIC(Sg_GenericUnboundVariable, Sg_NoNextMethod, NULL);
/* change-class */
SG_DEFINE_GENERIC(Sg_GenericChangeClass, Sg_NoNextMethod, NULL);

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



SgObject Sg_VMSlotRefUsingSlotDefinition(SgObject obj, SgObject slot)
{
  SgSlotAccessor *ac = lookup_slot_info(Sg_ClassOf(obj), SG_CAR(slot));
  if (!ac) Sg_Error(UC("Unknown slot %S"), SG_CAR(slot));
  return Sg_SlotRefUsingAccessor(obj, ac);
}
SgObject Sg_VMSlotSetUsingSlotDefinition(SgObject obj, SgObject slot,
					 SgObject value)
{
  SgSlotAccessor *ac = lookup_slot_info(Sg_ClassOf(obj), SG_CAR(slot));
  if (!ac) Sg_Error(UC("Unknown slot %S"), SG_CAR(slot));
  Sg_SlotSetUsingAccessor(obj, ac, value);
  return SG_UNDEF;
}
SgObject Sg_VMSlotBoundUsingSlotDefinition(SgObject obj, SgObject slot)
{
  SgSlotAccessor *ac;
  if (!SG_PAIRP(slot)) {
    Sg_Error(UC("slot definition must be a list but got %S"), slot);
  }
  ac = lookup_slot_info(Sg_ClassOf(obj), SG_CAR(slot));
  if (!ac) Sg_Error(UC("Unknown slot %S"), SG_CAR(slot));
  return SG_MAKE_BOOL(Sg_SlotBoundUsingAccessor(obj, ac));
}

static SgObject slot_initialize_cc(SgObject result, void **data)
{
  SgObject obj = data[0];
  SgSlotAccessor *ac = SG_SLOT_ACCESSOR(data[1]);
  Sg_SlotSetUsingAccessor(obj, ac, result);
  return SG_UNDEF;
}

SgObject Sg_VMSlotInitializeUsingAccessor(SgObject obj, SgObject acc, 
					  SgObject initargs)
{
  SgSlotAccessor *ac = SG_SLOT_ACCESSOR(acc);
  SgObject slot = ac->definition;
  SgObject key  = Sg_Memq(SG_KEYWORD_INIT_KEYWORD, slot);

  /* (1) use init-keyword */
  if (!SG_FALSEP(key) && SG_PAIRP(SG_CDR(key)) &&
      SG_KEYWORDP(SG_CADR(key))) {
    SgObject v = Sg_GetKeyword(SG_CADR(key), initargs, SG_UNDEF);
    if (!SG_UNDEFP(v)) {
      Sg_SlotSetUsingAccessor(obj, ac, v);
      return SG_UNDEF;
    }
    /* go through */
  }
  /* (2) use init-value */
  key = Sg_Memq(SG_KEYWORD_INIT_VALUE, slot);
  if (!SG_FALSEP(key)) {
    SgObject v = Sg_GetKeyword(SG_KEYWORD_INIT_VALUE, SG_CDR(slot),
			       SG_UNDEF);
    if (!SG_UNDEFP(v)) {
      Sg_SlotSetUsingAccessor(obj, ac, v);
      return SG_UNDEF;
    }
  }
  /* (2) use init-thunk */
  key = Sg_Memq(SG_KEYWORD_INIT_THUNK, slot);
  if (!SG_FALSEP(key)) {
    SgObject v = Sg_GetKeyword(SG_KEYWORD_INIT_THUNK, SG_CDR(slot),
			       SG_UNDEF);
    if (!SG_UNDEFP(v)) {
      void *data[2];
      data[0] = obj;
      data[1] = ac;
      Sg_VMPushCC(slot_initialize_cc, data, 2);
      return Sg_VMApply0(v);
    }
  }
  return SG_UNDEF;
}

/* object-initialize */
static SgObject object_initialize_cc(SgObject result, void **data);

static SgObject object_initialize1(SgObject obj, SgObject slots,
				   SgObject initargs)
{
  void *next[3];
  if (SG_NULLP(slots)) return obj;
  next[0] = obj;
  next[1] = SG_CDR(slots);
  next[2] = initargs;
  Sg_VMPushCC(object_initialize_cc, next, 3);
  return Sg_VMSlotInitializeUsingAccessor(obj, SG_CAR(slots), initargs);
}

static SgObject object_initialize_cc(SgObject result, void **data)
{
  SgObject obj = data[0];
  SgObject slots = data[1];
  SgObject initargs = data[2];
  return object_initialize1(obj, slots, initargs);
}

static SgObject object_initialize_impl(SgObject *argv, int argc, void *data)
{
  SgObject obj = argv[0];
  SgObject initargs = argv[1];
  SgClass *klass = SG_CLASS(Sg_ClassOf(obj));
  SgObject slots = Sg_ReverseX(Sg_ArrayToList((SgObject*)klass->gettersNSetters,
					      klass->nfields));
  if (SG_NULLP(slots)) return obj;
  return object_initialize1(obj, slots, initargs);
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
    r = Sg_Apply2(SG_OBJ(&Sg_GenericObjectCompare), x, y);
    if (SG_INTP(r)) {
      long v = SG_INT_VALUE(r);
      if (v < 0) return -1;
      if (v > 0) return 1;
      return 0;
    }
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
static SG_DEFINE_METHOD(object_compare_rec, &Sg_GenericObjectCompare,
			2, 0,
			object_equalp_SPEC,
			&object_equalp_default);

int Sg_ObjectCompare(SgObject x, SgObject y)
{
  return object_compare(x, y, FALSE);
}

static SgObject object_hash_impl(SgObject *argv, int argc, void *data)
{
  /* this will do address hash trick */
  return SG_FALSE;
}

SG_DEFINE_SUBR(object_hash_default, 2, 0, object_hash_impl, SG_FALSE, NULL);
static SgClass *object_hash_SPEC[] = {
  SG_CLASS_TOP, SG_CLASS_TOP
};
static SG_DEFINE_METHOD(object_hash_rec, &Sg_GenericObjectHash,
			2, 0,
			object_hash_SPEC,
			&object_hash_default);

static int check_lref0(SgObject procedure)
{
  SgCodeBuilder *cb;
  int size, i;
  SgWord *code;
  
  if (!SG_CLOSUREP(procedure)) return FALSE;
  
  cb = SG_CODE_BUILDER(SG_CLOSURE(procedure)->code);
  size = cb->size;
  /* here we do rather naive way. thus, if it's referred it's called. */
  code = cb->code;
  
  for (i = 0; i < size; i++) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
    switch (info->number) {
    case LREF: case LREF_PUSH:
      /* we don't check LREF_CAR and LREF_CAR_PUSH since it'd be
         an error for. */
      if (INSN_VALUE1(code[i])) {
	continue;
      }
      return FALSE;
    }
    i += info->argc;
  }
  return TRUE;
}

static SgObject method_initialize_impl(SgObject *argv, int argc, void *data)
{
  SgMethod *m = SG_METHOD(argv[0]);
  SgGeneric *g = NULL;
  SgObject initargs = argv[1];
  SgObject llist, quoli, generic, specs, body;
  SgClass **specarray;
  SgObject lp;
  int speclen = 0, req = 0, opt = 0;
  /* for sanity */
  ASSERT(SG_METHODP(m));
  /* get keyword arguments */
  llist   = Sg_GetKeyword(SG_KEYWORD_LAMBDA_LIST, initargs, SG_FALSE);
  quoli   = Sg_GetKeyword(SG_KEYWORD_QUALIFIER, initargs, SG_KEYWORD_PRIMARY);
  generic = Sg_GetKeyword(SG_KEYWORD_GENERIC, initargs, SG_FALSE);
  specs   = Sg_GetKeyword(SG_KEYWORD_SPECIALIZERS, initargs, SG_FALSE);
  body    = Sg_GetKeyword(SG_KEYWORD_PROCEDURE, initargs, SG_FALSE);

  if (!SG_FALSEP(generic)) {
    g = SG_GENERIC(generic);
  }
  if (!SG_CLOSUREP(body) && !SG_SUBRP(body)) {
    Sg_Error(UC("closure required for :body argument: %S"), body);
  }
  if ((speclen = (int)Sg_Length(specs)) < 0) {
    Sg_Error(UC("invalid specializers list: %S"), specs);
  }
  SG_METHOD_LEAF_P(m) = check_lref0(body);
  
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

  SG_METHOD_GENERIC(m) = g;
  SG_METHOD_SPECIALIZERS(m) = specarray;
  SG_METHOD_PROCEDURE(m) = body;
  SG_METHOD_QUALIFIER(m) = quoli;
  if (g) {
    set_method_debug_name(m, g);
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

/* compute-getter-and-setter */
static SgObject compute_gas_impl(SgObject *args, int argc, void *data)
{
  return Sg_ComputeGetterAndSetter(SG_CLASS(args[0]), args[1]);
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

/* compute-method-more-specific? */
static SgObject more_specific_p_subr(SgObject *args, int argc, void *data)
{
  int i, r;
  SgClass **klass = (SgClass **)data;
  if (!SG_METHODP(args[0])) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("method-more-specific?"),
				    SG_INTERN("method"),
				    args[0], SG_LIST2(args[0], args[1]));
  }
  if (!SG_METHODP(args[1])) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("method-more-specific?"),
				    SG_INTERN("method"),
				    args[1], SG_LIST2(args[0], args[1]));
  }
  for (i = 0; klass[i]; i++);
  r = method_more_specific(SG_METHOD(args[0]), SG_METHOD(args[1]), 
			   klass, i);
  return SG_MAKE_BOOL(r);
}

static SgObject compute_method_more_specific_p(SgObject *args, int argc,
					       void *data)
{
  SgObject argv = args[1], cp;
  int len = (int)Sg_Length(argv), i;
  SgClass **klass = SG_NEW2(SgClass **, len);
  i = 0;
  SG_FOR_EACH(cp, argv) {
    klass[i++] = Sg_ClassOf(SG_CAR(cp));
  }
  return Sg_MakeSubr(more_specific_p_subr, klass, 2, 0,
		     SG_MAKE_STRING("more-specific?"));
}

SG_DEFINE_SUBR(compute_method_more_specific_p_subr, 2, 0, 
	       compute_method_more_specific_p, SG_FALSE, NULL);

static SgClass *compute_method_more_specific_SPEC[] = {
  SG_CLASS_GENERIC, SG_CLASS_LIST
};

static SG_DEFINE_METHOD(compute_method_more_specific_p_rec, 
			&Sg_GenericComputeMethodMoreSpecificP,
			2, 0,
			compute_method_more_specific_SPEC, 
			&compute_method_more_specific_p_subr);

SgObject Sg_ComputeApplicableMethods(SgObject gf, SgObject args)
{
  SgObject argv[1];
  argv[0] = args;
  if (Sg_TypeP(gf, SG_CLASS_GENERIC)) {
    return compute_applicable_methods(SG_GENERIC(gf), argv, 1, TRUE);
  } else {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("%compute-applicable-methods"),
				    SG_MAKE_STRING("sub type of generic"),
				    gf, SG_LIST1(gf));
    return SG_UNDEF;		/* dummy */
  }
}

SgObject Sg_VMSortMethodByQualifier(SgObject methods)
{
  SgObject qualified_methods[QUALIFIER_COUNT];
  sort_method_by_qualifier(methods, qualified_methods, TRUE);
  return Sg_Values4(qualified_methods[PRIMARY_INDEX],
		    qualified_methods[BEFORE_INDEX],
		    qualified_methods[AFTER_INDEX],
		    qualified_methods[AROUND_INDEX]);
}


SgObject Sg_VMComputeAroundMethods(SgObject around, SgObject before,
				   SgObject primary, SgObject after)
{
  return compute_around_methods_rec(around, before, primary, after);
}

static void eql_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  const SgChar *type;
  switch (SG_EQL_SPECIALIZER(o)->type) {
  case SG_EQ_SPECIALIZER: type = UC("eq"); break;
  case SG_EQV_SPECIALIZER: type = UC("eql"); break;
  case SG_EQUAL_SPECIALIZER: type = UC("equal"); break;
  default: type = UC("unknown"); break;
  }
  Sg_Printf(p, UC("#<eql-specializer (%s %S)>"), type,
	    SG_EQL_SPECIALIZER(o)->object);
}

static SgClass *Sg_ClassCPL[] = {
  SG_CLASS_CLASS,
  SG_CLASS_OBJECT,
  SG_CLASS_TOP,
  NULL
};

SG_DEFINE_BUILTIN_CLASS(Sg_EqlSpecializerClass, eql_printer, NULL, NULL, NULL,
			Sg_ClassCPL);

static SgObject make_eql_specializer(SgObject obj, SgEqlSpecializerType type)
{
  SgEqlSpecializer *z = SG_NEW(SgEqlSpecializer);
  SG_SET_CLASS(z, SG_CLASS_EQL_SPECIALIZER);
  z->object = obj;
  z->type = type;
  return SG_OBJ(z);
}
  

/* eql specializer stuff */
SgObject Sg_MakeEqSpecializer(SgObject obj)
{
  return make_eql_specializer(obj, SG_EQ_SPECIALIZER);
}

SgObject Sg_MakeEqlSpecializer(SgObject obj)
{
  return make_eql_specializer(obj, SG_EQV_SPECIALIZER);
}

SgObject Sg_MakeEqualSpecializer(SgObject obj)
{
  return make_eql_specializer(obj, SG_EQUAL_SPECIALIZER);
}

/* default slot-unbound and slot-missing methods */
static SgObject slot_unbound_subr(SgObject *argv, int argc, void *data)
{
  Sg_Error(UC("slot '%S' of object of class %S is unbound"),
	   argv[2], argv[0]);
  return SG_UNDEF;		/* dummy */
}

SG_DEFINE_SUBR(slot_unbound_subr_rec, 3, 0, slot_unbound_subr, SG_FALSE, NULL);
static SgClass *slot_unbound_SPEC[] = {
  SG_CLASS_CLASS,
  SG_CLASS_TOP,
  SG_CLASS_TOP
};
static SG_DEFINE_METHOD(slot_unbound_rec, &Sg_GenericSlotUnbound,
			3, 0, slot_unbound_SPEC, &slot_unbound_subr_rec);

/* slot-missing */
static SgObject slot_missing_subr(SgObject *argv, int argc, void *data)
{
  Sg_Error(UC("object of class %S doesn't have such slot: %S"),
	   argv[0], argv[2]);
  return SG_UNDEF;		/* dummy */
}

SG_DEFINE_SUBR(slot_missing_subr_rec, 3, 1, slot_missing_subr, SG_FALSE, NULL);
static SgClass *slot_missing_SPEC[] = {
  SG_CLASS_CLASS,
  SG_CLASS_TOP,
  SG_CLASS_TOP
};
static SG_DEFINE_METHOD(slot_missing_rec, &Sg_GenericSlotMissing,
			3, 1, slot_missing_SPEC, &slot_missing_subr_rec);

static SgObject unbound_variable_subr(SgObject *argv, int argc, void *data)
{
  SgObject h = SG_NIL, t = SG_NIL;
  SgObject lib = argv[1], variable = argv[0];
  SgObject message;
  SG_APPEND1(h, t, Sg_MakeUndefinedViolation());
  if (variable) {
    SG_APPEND1(h, t, Sg_MakeWhoCondition(variable));
  }
  message = Sg_Sprintf(UC("unbound variable %S in library %A"),
		       variable, SG_LIBRARY_NAME(lib));
  SG_APPEND1(h, t, Sg_MakeMessageCondition(message));
  Sg_Raise(Sg_Condition(h), FALSE);
  return SG_UNDEF;
}
SG_DEFINE_SUBR(unbound_variable_subr_rec, 3, 0, unbound_variable_subr,
	       SG_FALSE, NULL);
static SgClass *unbound_variable_SPEC[] = {
  SG_CLASS_TOP,
  SG_CLASS_TOP,
  SG_CLASS_TOP
};
static SG_DEFINE_METHOD(unbound_variable_rec, &Sg_GenericUnboundVariable,
			3, 0, unbound_variable_SPEC,
			&unbound_variable_subr_rec);

void Sg__InitClos()
{
  /* TODO library name */
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  static SgClass *nullcpa[1] = {NULL};

  /* init lock */
  Sg_InitMutex(&class_world_lock.mutex, TRUE);
  Sg_InitCond(&class_world_lock.cv);

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
  BINIT(SG_CLASS_NEXT_METHOD, "<next-method>", next_method_slots);
  BINIT(SG_CLASS_EQL_SPECIALIZER, "<eql-specializer>", NULL);
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

  /* library */
  CINIT(SG_CLASS_LIBRARY,   "<library>");

  /* abstract collection */
  BINIT(SG_CLASS_COLLECTION, "<collection>", NULL);
  BINIT(SG_CLASS_SEQUENCE,   "<sequence>",   NULL);
  BINIT(SG_CLASS_DICTIONARY, "<dictionary>", NULL);
  BINIT(SG_CLASS_ORDERED_DICTIONARY, "<ordered-dictionary>", NULL);

  /* treemap */
  CINIT(SG_CLASS_TREE_MAP,  "<tree-map>");

  /* vector */
  CINIT(SG_CLASS_VECTOR,    "<vector>");
  /* bytevector */
  CINIT(SG_CLASS_BVECTOR,   "<bytevector>");
  /* weak */
  CINIT(SG_CLASS_WEAK_VECTOR,       "<weak-vector>");
  CINIT(SG_CLASS_WEAK_HASHTABLE,    "<weak-hashtable>");
  CINIT(SG_CLASS_WEAK_BOX,          "<weak-box>");

  /* codec and transcoders */
  CINIT(SG_CLASS_CODEC,       "<codec>");
  CINIT(SG_CLASS_TRANSCODER,  "<transcoder>");

  /* procedure */
  CINIT(SG_CLASS_PROCEDURE, "<procedure>");
  SG_CLASS_PROCEDURE->flags |= SG_CLASS_APPLICABLE;

  /* code builder */
  CINIT(SG_CLASS_CODE_BUILDER, "<code-builder>");

#define GINIT(gf, nam)				\
  Sg_InitBuiltinGeneric(gf, UC(nam), lib)

  GINIT(&Sg_GenericMake, "make");
  GINIT(&Sg_GenericAllocateInstance, "allocate-instance");
  GINIT(&Sg_GenericInitialize, "initialize");
  GINIT(&Sg_GenericComputeCPL, "compute-cpl");
  GINIT(&Sg_GenericComputeSlots, "compute-slots");
  GINIT(&Sg_GenericAddMethod, "add-method");
  GINIT(&Sg_GenericRemoveMethod, "remove-method");
  GINIT(&Sg_GenericObjectEqualP, "object-equal?");
  GINIT(&Sg_GenericObjectCompare, "object-compare");
  GINIT(&Sg_GenericObjectHash, "object-hash");
  GINIT(&Sg_GenericObjectApply, "object-apply");
  GINIT(&Sg_GenericObjectSetter, "setter of object-apply");
  GINIT(&Sg_GenericComputeApplyGeneric, "compute-apply-generic");
  GINIT(&Sg_GenericComputeMethodMoreSpecificP, "compute-method-more-specific?");
  GINIT(&Sg_GenericComputeApplyMethods, "compute-apply-methods");
  GINIT(&Sg_GenericSlotUnbound, "slot-unbound");
  GINIT(&Sg_GenericSlotMissing, "slot-missing");
  GINIT(&Sg_GenericUnboundVariable, "unbound-variable");
  GINIT(&Sg_GenericComputeGetterAndSetter, "compute-getter-and-setter");
  GINIT(&Sg_GenericChangeClass, "change-class");

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
  /* Sg_InitBuiltinMethod(&compute_applicable_methods_rec); */
  Sg_InitBuiltinMethod(&compute_method_more_specific_p_rec);
  Sg_InitBuiltinMethod(&object_equalp_rec);
  Sg_InitBuiltinMethod(&object_compare_rec);
  Sg_InitBuiltinMethod(&object_hash_rec);
  Sg_InitBuiltinMethod(&slot_unbound_rec);
  Sg_InitBuiltinMethod(&slot_missing_rec);

  Sg_InitBuiltinMethod(&unbound_variable_rec);
}
