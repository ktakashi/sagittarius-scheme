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
#include "sagittarius/collection.h"
#include "sagittarius/error.h"
#include "sagittarius/generic.h"
#include "sagittarius/gloc.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/library.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/string.h"
#include "sagittarius/subr.h"
#include "sagittarius/symbol.h"
#include "sagittarius/treemap.h"
#include "sagittarius/unicode.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"
#include "sagittarius/weak.h"
#include "sagittarius/writer.h"


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
  return ac;
}

static SgObject make_class(SgObject supers);

SgObject Sg_MakeClass(SgObject supers, SgObject slots)
{
  return make_class(supers);
}

SgObject Sg_MakeGeneric()
{
  return generic_allocate(SG_CLASS_GENERIC, SG_NIL);
}

SgObject Sg_MakeMethod(SgObject specializers, SgObject procedure)
{
  SgObject *array, m, *a;
  array = Sg_ListToArray(specializers, TRUE);
  m = method_allocate(SG_CLASS_METHOD, SG_NIL);
  /* check */
  for (a = array; *a; a++) {
    if (!Sg_TypeP(*a, SG_CLASS_CLASS)) {
      Sg_Error(UC("<class> required but got %S"), *a);
    }
  }
  SG_METHOD_SPECIALIZERS(m) = (SgClass**)array;
  SG_METHOD_PROCEDURE(m) = procedure;
  return m;
}

static SgObject collect_unique_methods(SgGeneric *gf, SgMethod *m)
{
  SgObject h = SG_NIL, t = SG_NIL, cp;
  SG_FOR_EACH(cp, SG_GENERIC_METHODS(gf)) {
    SgMethod *gm = SG_METHOD(SG_CAR(cp));
    if (SG_EQ(gm, m)) continue;
    SG_APPEND1(h, t, gm);
  }
  SG_APPEND1(h, t, m);
  return h;
}

void Sg_AddMethod(SgGeneric *generic, SgMethod *method)
{
  SgObject methods = collect_unique_methods(generic, method);
  SG_GENERIC_METHODS(generic) = methods;
}

/* compute-std-cpl in tiny-clos */
static SgObject build_elements(SgClass *klass)
{
  SgObject result = SG_NIL, pending = SG_LIST1(klass);
  while (1) {
    SgObject next;
    if (SG_NULLP(pending)) return result;
    next = SG_CAR(pending);
    if (SG_FALSEP(Sg_Memq(next, result))) {
      result = Sg_Cons(next, result);
      pending = Sg_Append2(klass->directSupers, SG_CDR(pending));
    } else {
      pending = SG_CDR(pending);
    }
  }
}

static SgObject build_constraints(SgClass *klass)
{
  SgObject elements = build_elements(klass);
  SgObject this_one = SG_NIL, result = SG_NIL;
  while (1) {
    if (SG_NULLP(this_one) || SG_NULLP(SG_CDR(this_one))) {
      if (SG_NULLP(elements)) return result;
      ASSERT(SG_CLASSP(SG_CAR(elements)));
      this_one = Sg_Cons(SG_CAR(elements),
			 SG_CLASS(SG_CAR(elements))->directSupers);
      elements = SG_CDR(elements);
    } else {
      result = Sg_Cons(SG_LIST2(SG_CAR(this_one), SG_CADR(this_one)),
		       result);
      this_one = SG_CDR(this_one);
    }
  }
}

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

static SgObject tie_breaker(SgObject partial_cpl, SgObject min_elts)
{
  SgObject pcpl = Sg_Reverse(partial_cpl);
  while (1) {
    SgObject current_elt = SG_CAR(pcpl), ds_of_ce, common;
    ASSERT(SG_CLASSP(current_elt));
    ds_of_ce = SG_CLASS(current_elt)->directSupers;
#define filter_test(x) !SG_FALSEP(Sg_Memq((x), ds_of_ce))
    filter_in(common, filter_test, min_elts);
    if (SG_NULLP(common)) {
      if (SG_NULLP(SG_CDR(pcpl))) {
	Sg_Error(UC("tie-breaker: Nothing valid"));
	return SG_UNDEF;	/* dummy */
      }
      pcpl = SG_CDR(pcpl);
    } else {
      return SG_CAR(common);
    }
  }
}

static int cpl_every(SgObject x, SgObject constraints, SgObject result)
{
  SgObject cp;
  SG_FOR_EACH(cp, constraints) {
    SgObject constraint = SG_CAR(cp);
    if (SG_EQ(SG_CADR(constraint), x) &&
	SG_FALSEP(Sg_Memq(SG_CAR(constraint), result))) return FALSE;
  }
  return TRUE;
}

/* simple topological sort */
SgObject Sg_ComputeCPL(SgClass *klass)
{
  SgObject elements = build_elements(klass);
  SgObject constraints = build_constraints(klass);
  SgObject result = SG_NIL, t = SG_NIL;

  while (1) {
    SgObject can_go_in_now;
    if (SG_NULLP(elements)) return result;
#define can_go_test(x) cpl_every(x, constraints, result)
    filter_in(can_go_in_now, can_go_test, elements);

    if (SG_NULLP(can_go_in_now)) {
      Sg_Error(UC("compute-std-cpl: Invalid constraints"));
      return SG_UNDEF;		/* dummy */
    } else {
      SgObject choice;
      if (SG_NULLP(SG_CDR(can_go_in_now))) choice = SG_CAR(can_go_in_now);
      else choice = tie_breaker(result, can_go_in_now);
#define loop_test(x) !SG_EQ(x, choice)
      filter_in(elements, loop_test, elements);
      SG_APPEND1(result, t, choice);
    }
  }
}

SgObject Sg_ComputeSlots(SgClass *klass)
{
  SgObject slots = SG_NIL;
  SgObject cp, sp;
  SG_FOR_EACH(cp, klass->cpl) {
    ASSERT(SG_CLASSP(SG_CAR(cp)));
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
  SgObject sp;
  int index = 0;
  SG_FOR_EACH(sp, slots) {
    SgSlotAccessor *ac = make_slot_accessor(klass, SG_CAAR(sp), index);
    SG_APPEND1(h, t, SG_OBJ(ac));
    index++;
  }
  return h;
}

int Sg_ApplicableP(SgObject c, SgObject arg)
{
  return !SG_FALSEP(Sg_Memq(c, SG_CLASS(Sg_ClassOf(arg))->cpl));
}

static int method_every(SgObject method, SgObject *argv, int argc)
{
  SgClass **specs = SG_METHOD_SPECIALIZERS(method);
  for (; *specs; specs++) {
    int i;
    for (i = 0; i < argc; i++, specs++) {
      if (!Sg_ApplicableP(*specs, argv[i])) return FALSE;
    }
  }
  return TRUE;
}

#define PREALLOC_SIZE 32
/*
  These functions must be generic, however for now we just put here
  and ignore the others.
 */
static int more_specific_p(SgClass *c1, SgClass *c2, SgClass *arg)
{
  SgObject m1 = Sg_Memq(c1, arg->cpl);
  if (!SG_FALSEP(m1)) {
    return !SG_FALSEP(Sg_Memq(c2, m1));
  }
  return FALSE;
}

static int method_more_specific(SgMethod *m1, SgMethod *m2,
				SgClass **targv, int argc)
{
  SgClass **spec1 = SG_METHOD_SPECIALIZERS(m1);
  SgClass **spec2 = SG_METHOD_SPECIALIZERS(m2);
  int i;
  for (i = 0; i < argc; i++, spec1++, spec2++) {
    if (!*spec1) return TRUE;
    if (!*spec2) return FALSE;
    if (!SG_EQ(*spec1, *spec2)) {
      return more_specific_p(*spec1, *spec2, targv[i]);
    }
  }
  Sg_Error(UC("Fewer arguments than specializers"));
  return FALSE;			/* dummy */
}


static SgObject sort_method(SgObject methods, SgObject *argv, int argc)
{
  SgObject array_s[PREALLOC_SIZE], *array = array_s;
  SgClass *targv_s[PREALLOC_SIZE], **targv = targv_s;
  int count = 0, len = Sg_Length(methods), step, i, j;
  SgObject mp;

  /* TODO maybe we should use alloca */
  if (len >= PREALLOC_SIZE)  array = SG_NEW_ARRAY(SgObject, len);
  if (argc >= PREALLOC_SIZE) targv = SG_NEW_ARRAY(SgClass*, argc);

  SG_FOR_EACH(mp, methods) {
    if (!Sg_TypeP(SG_CAR(mp), SG_CLASS_METHOD)) {
      Sg_Error(UC("bad method in applicable method list: %S"), SG_CAR(mp));
    }
    array[count++] = SG_CAR(mp);
  }
  for (i=0; i <argc; i++) targv[i] = Sg_ClassOf(argv[i]);

  for (step = len/2; step > 0; step /=2) {
    for (i = step; i<len; i++) {
      for (j = i-step; j>=0; j -= step) {
	/* TODO, use generic method */
	if (method_more_specific(SG_METHOD(array[j]),
				 SG_METHOD(array[j+step]),
				 targv, argc)) {
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

SgObject Sg_ComputeMethods(SgGeneric *gf, SgObject *argv, int argc)
{
  SgObject applicable;
#define method_filter(x) method_every(x, argv, argc)
  filter_in(applicable, method_filter, SG_GENERIC_METHODS(gf));
  if (SG_NULLP(applicable)) return applicable;
  return sort_method(applicable, argv, argc);
}

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

  Sg_Error(UC("object of class %S doesn't have such slot: %S"), klass, name);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_SlotRef(SgObject obj, SgObject name)
{
  SgSlotAccessor *accessor = lookup_slot_info(Sg_ClassOf(obj), name);
  if (accessor->getter) {
    return accessor->getter(obj);
  } else {
    /* scheme accessor, assume obj is instance */
    return SG_INSTANCE(obj)->slots[accessor->index];
  }
}

void Sg_SlotSet(SgObject obj, SgObject name, SgObject value)
{
  SgSlotAccessor *accessor = lookup_slot_info(Sg_ClassOf(obj), name);
  if (accessor->setter) {
    return accessor->setter(obj, value);
  } else {
    /* scheme accessor */
    SG_INSTANCE(obj)->slots[accessor->index] = value;
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
  instance->compare = NULL; /* object_compare; */
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
  /* TODO check */
  klass->gettersNSetters = (SgSlotAccessor**)Sg_ListToArray(getters, TRUE);
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
  Sg_AddMethod(SG_METHOD_GENERIC(m), m);
}

SgObject Sg_NoNextMethod(SgObject *argv, int argc, SgGeneric *gf)
{
  Sg_AssertionViolation(SG_INTERN("call-next-method"),
			Sg_Sprintf(UC("no applicable method for %S with "
				      "arguments %S"), SG_OBJ(gf),
				   Sg_ArrayToList(argv, argc)),
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
  Sg_Printf(port, UC("#<method %S>"), SG_PROCEDURE_NAME(SG_METHOD(obj)));
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
  SgClass **s = (SgClass**)Sg_ListToArray(specs, TRUE);
  SG_METHOD_SPECIALIZERS(method) = s;
}

static SgObject method_procedure(SgMethod *method)
{
  return SG_METHOD_PROCEDURE(method);
}

static void method_procedure_set(SgMethod *method, SgObject proc)
{
  if (!SG_CLOSUREP(proc) || !SG_SUBRP(proc)) {
    Sg_Error(UC("method procedure requires procedure but got %S"), proc);
  }
  SG_METHOD_PROCEDURE(method) = proc;
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
  { { NULL } }
};

static SgSlotAccessor method_slots[] = {
  SG_CLASS_SLOT_SPEC("specializers",   0, method_specializers,
		     method_specializers_set),
  SG_CLASS_SLOT_SPEC("procedure",      1, method_procedure,
		     method_procedure_set),
  { { NULL } }
};

static SgObject make_class(SgObject supers)
{
  SgClass *klass = SG_CLASS(class_allocate(SG_CLASS_CLASS, SG_NIL));
  init_class(klass, NULL, NULL, supers, class_slots, 0);
  return SG_OBJ(klass);
}

static void initialize_builtin_cpl(SgClass *klass, SgObject supers)
{
  SgClass **p;
  SgObject h = SG_NIL, t = SG_NIL;
  SG_APPEND1(h, t, SG_OBJ(klass));
  for (p = klass->cpa; *p; p++) SG_APPEND1(h, t, SG_OBJ(*p));
  klass->cpl = h;
  if (SG_PAIRP(supers)) {
    /* for now don't support */
    ASSERT(FALSE);
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
      SG_APPEND1(slots, t, SG_LIST1(snam));
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

/* builtin generics */
SG_DEFINE_GENERIC(Sg_GenericMake, Sg_NoNextMethod, NULL);
SG_DEFINE_GENERIC(Sg_GenericAllocateInstance, Sg_NoNextMethod, NULL);

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
  BINIT(SG_CLASS_GENERIC,     "<generic>", NULL);
  BINIT(SG_CLASS_METHOD,      "<method>",  method_slots);
  BINIT(SG_CLASS_NEXT_METHOD, "<next-method>", NULL);
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
  CINIT(SG_CLASS_BVECTOR,   "<byte-vector>");
  /* weak */
  CINIT(SG_CLASS_WEAK_VECTOR,       "<weak-vector>");
  CINIT(SG_CLASS_WEAK_HASHTABLE,    "<weak-hashtable>");

  /* procedure */
  CINIT(SG_CLASS_PROCEDURE, "<procedure>");
  SG_CLASS_PROCEDURE->flags |= SG_CLASS_APPLICABLE;

#define GINIT(gf, nam)				\
  Sg_InitBuiltinGeneric(gf, UC(nam), lib)

  GINIT(&Sg_GenericMake, "make");
  GINIT(&Sg_GenericAllocateInstance, "allocate-instance");

  /* methods */
  Sg_InitBuiltinMethod(&class_allocate_rec);
}
