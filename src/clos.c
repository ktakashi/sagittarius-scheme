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
/* allocate */
static SgObject class_allocate(SgClass *klass, SgObject initargs);
static SgObject generic_allocate(SgClass *klass, SgObject initargs);
static SgObject method_allocate(SgClass *klass, SgObject initargs);

static void init_class(SgClass *klass, const SgChar *name,
		       SgLibrary *lib, SgObject supers, SgSlotAccessor *specs,
		       int flags);

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

/*
static SgSlotAccessor *make_slot_accessor(SgClass *klass, SgObject name)
{
  SgSlotAccessor *ac = SG_NEW(SgSlotAccessor);
  SG_SET_CLASS(SG_CLASS_SLOT_ACCESSOR);
  ac->name = name;
  ac->klass = klass;
  ac->getter = NULL;
  ac->setter = NULL;
  ac->getterS = Sg_MakeSubr
}

static SgSlotAccessor **slot_to_acc(SgClass *klass, SgObject slots)
{
  int size = Sg_Length(slots), i = 0;
  SgSlotAccessor **acc = SG_NEW_ARRAY(SgSlotAccessor*, size);
  SgObject cp;
  SG_FOR_EACH(cp, slots) {
    acc[i] = make_slot_accessor(klass, SG_CAR(cp));
  }  
}
*/

static SgObject make_class(SgObject supers);

SgObject Sg_MakeClass(SgObject supers, SgObject slots)
{
  /* SgClass *klass = SG_CLASS(class_allocate(SG_CLASS_CLASS, SG_NIL)); */
  /* SgSlotAccessor **acc = slot_to_acc(klass, slots); */
  /* init_class(klass, NULL, NULL, supers, acc, TRUE); */
  /* return SG_OBJ(klass); */
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
    if (!SG_CLASSP(*a)) {
      Sg_Error(UC("<class> required but got %S"), specializers);
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
    if (SG_NULLP(this_one) || SG_NULLP(result)) {
      if (SG_NULLP(elements)) return result;
      ASSERT(SG_CLASSP(SG_CAR(elements)));
      elements = SG_CDR(elements);
      this_one = Sg_Cons(SG_CAR(elements),
			 SG_CLASS(SG_CAR(elements))->directSupers);
    } else {
      this_one = SG_CDR(this_one);
      result = Sg_Cons(SG_LIST2(SG_CAR(this_one), SG_CADR(this_one)),
		       result);
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
    SgObject constraint;
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

int Sg_ApplicableP(SgObject c, SgObject arg)
{
  return !SG_FALSEP(Sg_Memq(c, SG_CLASS(Sg_ClassOf(c))->cpl));
}

static int method_every(SgObject method, SgObject *argv, int argc)
{
  int i;
  SgObject specs = SG_METHOD_SPECIALIZERS(method);
  for (i = 0; i < argc; i++) {
    if (!Sg_ApplicableP(SG_CAR(specs), argv[i])) return FALSE;
    /* sanity */
    if (SG_NULLP(specs)) return FALSE;
    specs = SG_CDR(specs);
  }
  return TRUE;
}

SgObject Sg_ComputeMethods(SgGeneric *gf, SgObject *argv, int argc)
{
  SgObject applicable;

#define method_filter(x) method_every(x, argv, argc)
  filter_in(applicable, method_filter, SG_GENERIC_METHODS(gf));

  /* todo sort */
  return applicable;
}


static SgSlotAccessor* lookup_slot_info(SgClass *klass, SgObject name)
{
  SgSlotAccessor **gNs = klass->gettersNSetters;
  SgClass *tklass = klass;
 entry:
  for (;*gNs;gNs++) {
    if (SG_EQ(name, (*gNs)->name)) {
      return *gNs;
    }
  }
  /* try tag */
  if (tklass != SG_CLASS_CLASS) {
    tklass = SG_CLASS_OF(klass);
    gNs = tklass->gettersNSetters;
    goto entry;
  }

  Sg_Error(UC("object of class %S doesn't have such slot: %S"), klass, name);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_SlotRef(SgObject obj, SgObject name)
{
  SgSlotAccessor *accessor = lookup_slot_info(obj, name);
  if (accessor->getter) {
    return accessor->getter(obj);
  } else {
    /* scheme accessor */
    return Sg_Apply1(accessor->getterS, obj);
  }
}

void Sg_SlotSet(SgObject obj, SgObject name, SgObject value)
{
  SgSlotAccessor *accessor = lookup_slot_info(obj, name);
  if (accessor->setter) {
    return accessor->setter(obj, value);
  } else {
    if (SG_FALSEP(accessor->setterS)) {
      Sg_Error(UC("slot %A in object of %S is not malleable"));
      return;
    }
    /* scheme accessor */
    Sg_Apply2(accessor->setterS, obj, value);
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

static SgObject class_direct_supers(SgClass *klass)
{
  return klass->directSupers;
}

static SgObject class_direct_slots(SgClass *klass)
{
  return klass->directSlots;
}

static SgObject class_cpl(SgClass *klass)
{
  return klass->cpl;
}

static SgObject class_slots_ref(SgClass *klass)
{
  return klass->slots;
}


static SgObject class_nfields(SgClass *klass)
{
  return SG_MAKE_INT(klass->nfields);
}

static SgObject class_field_initializers(SgClass *klass)
{
  return klass->fieldInitializers;
}

static SgObject class_getters_n_setters(SgClass *klass)
{
  return Sg_ArrayToList((SgObject*)klass->gettersNSetters, klass->nfields);
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

/* static initializer */

/* slot initialization */
static SgSlotAccessor class_slots[] = {
  SG_CLASS_SLOT_SPEC("name", class_name, NULL),
  SG_CLASS_SLOT_SPEC("direct-supers", class_direct_supers, NULL),
  SG_CLASS_SLOT_SPEC("direct-slots", class_direct_slots, NULL),
  SG_CLASS_SLOT_SPEC("cpl", class_cpl, NULL),
  SG_CLASS_SLOT_SPEC("slots", class_slots_ref, NULL),
  SG_CLASS_SLOT_SPEC("nfields", class_nfields, NULL),
  SG_CLASS_SLOT_SPEC("field-initializers", class_field_initializers, NULL),
  SG_CLASS_SLOT_SPEC("getters-n-setters", class_getters_n_setters, NULL),
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
  klass->gettersNSetters = (SgSlotAccessor**)Sg_ListToArray(acc, TRUE);
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
/* SG_DEFINE_GENERIC(Sg_GenericInitialize, Sg_NoNextMethod, NULL); */
/* SG_DEFINE_GENERIC(Sg_GenericAllocateInstance, Sg_NoNextMethod, NULL); */
/* SG_DEFINE_GENERIC(Sg_GenericComputeGetterAndSetter, Sg_NoNextMethod, NULL); */
/* SG_DEFINE_GENERIC(Sg_GenericComputeCPL, Sg_NoNextMethod, NULL); */
/* SG_DEFINE_GENERIC(Sg_GenericComputeSlots, Sg_NoNextMethod, NULL); */
/* SG_DEFINE_GENERIC(Sg_GenericComputeApplyGeneric, Sg_NoNextMethod, NULL); */
/* SG_DEFINE_GENERIC(Sg_GenericComputeMethods, Sg_NoNextMethod, NULL); */
/* SG_DEFINE_GENERIC(Sg_GenericComputeMethodMoreSpecificP, Sg_NoNextMethod, NULL); */
/* SG_DEFINE_GENERIC(Sg_GenericComputeApplyMethods, Sg_NoNextMethod, NULL); */

/* static SgObject object_initialize_impl(SgObject *args, int argc, void *data) */
/* { */
/*   return args[1]; */
/* } */
/* SG_DEFINE_SUBR(object_initialize, 3, 0, object_initialize_impl, SG_FALSE, NULL); */

/* static SgClass *object_initialize_SPEC[] = { */
/*   SG_CLASS_OBJECT */
/* }; */

/* static SG_DEFINE_METHOD(object_initialize_method, */
/* 			&Sg_GenericInitialize, 3, 0, */
/* 			object_initialize_SPEC, */
/* 			&object_initialize, NULL); */

/* static SgObject class_initialize_impl(SgObject *args, int argc, void *data) */
/* { */
  
/*   return args[1]; */
/* } */

/* SG_DEFINE_SUBR(class_initialize, 3, 0, class_initialize_impl, SG_FALSE, NULL); */

/* static SgClass *class_initialize_SPEC[] = { */
/*   SG_CLASS_CLASS */
/* }; */

/* static SG_DEFINE_METHOD(class_initialize_method, */
/* 			&Sg_GenericInitialize, 3, 0, */
/* 			class_initialize_SPEC, */
/* 			&class_initialize, NULL); */


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

#define GINIT(gf, nam)				\
  Sg_InitBuiltinGeneric(gf, UC(nam), lib)

  GINIT(&Sg_GenericMake, "make");
}
