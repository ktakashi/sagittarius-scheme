/* clos.h                                                 -*- coding: utf-8; -*-
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
#ifndef SAGITTARIUS_CLOS_H_
#define SAGITTARIUS_CLOS_H_

#include "sagittariusdefs.h"
#include "thread.h"

struct SgInstanceRec
{
  SgByte   *tag;
  SgObject *slots;
};
#define SG_INSTANCE(obj) ((SgInstance*)(obj))

#define SG_INSTANCE_HEADER SgInstance hdr

/* accessor */
SG_CLASS_DECL(Sg_SlotAccessorClass)
#define SG_CLASS_SLOT_ACCESSOR (&Sg_SlotAccessorClass)
typedef struct SgSlotAccessorRec SgSlotAccessor;
typedef SgObject (*SgSlotGetterProc)(SgObject);
typedef void     (*SgSlotSetterProc)(SgObject, SgObject);
struct SgSlotAccessorRec
{
  SG_HEADER;
  const char *cname;			       /* for static initilization */
  SgObject name;			       /* field name */
  SgClass *klass;			       /* slot class */
  SgSlotGetterProc getter;		       /* C getter */
  SgSlotSetterProc setter;		       /* C setter */
  SgObject getterS;
  SgObject setterS;
};
#define SG_SLOT_ACCESSOR(obj)  ((SgSlotAccessor*)(obj))
#define SG_SLOT_ACCESSORP(obj) SG_XTYPEP(obj, SG_CLASS_SLOT_ACCESSOR)

#define SG_CLASS_SLOT_SPEC(name, getter, setter)		\
  { { SG_CLASS2TAG(SG_CLASS_SLOT_ACCESSOR) },			\
      (name), SG_FALSE, NULL,					\
	(SgSlotGetterProc)(getter),				\
	(SgSlotSetterProc)(setter),				\
	SG_FALSE, SG_FALSE }


/* based on tiny clos. most of tricks are from Gauche */
/* define cprocs */
typedef void (*SgClassPrintProc)(SgObject obj, SgPort *port,
				 SgWriteContext *mode);
typedef int  (*SgClassCompareProc)(SgObject x, SgObject y, int equalP);
/* for external representation */
typedef int  (*SgClassSerializeProc)(SgObject obj, SgPort *port,
				     SgObject context);
typedef SgObject (*SgClassAllocateProc)(SgClass *klass, SgObject initargs);
/* For future use, define read/write own object cache */
typedef SgObject (*SgReadCacheProc)(SgPort *port);
typedef int      (*SgWriteCacheProc)(SgObject obj, SgPort *port);

struct SgClassRec
{
  union {
    SG_INSTANCE_HEADER;
    double align_dummy;
  } classHdr;

  SgClassPrintProc     printer;
  SgClassCompareProc   compare;
  SgClassSerializeProc serialize;
  SgClassAllocateProc  allocate;
  /* for future */
  /* SgReadCacheProc      cacheReader; */
  /* SgWriteCacheProc     cacheWriter; */

  SgClass **cpa;
  int       nfields;		/* need this? */
  int       coreSize;
  int       flags;

  /* scheme info */
  SgObject name;		/* class name (scheme) */
  SgObject directSupers;	/* list of classes */
  SgObject cpl;			/* list of classes */
  SgObject directSlots;		/* alist of name and definition */
  SgObject slots;		/* alist of name and definition */
  SgObject fieldInitializers;	/* list of initializers */
  SgSlotAccessor **gettersNSetters; /* array of accessors, NULL terminated */
  
  /* mutex */
  SgInternalMutex mutex;
  SgInternalCond  cv;
};

#define SG_CLASS(obj)  ((SgClass*)(obj))
#define SG_CLASSP(obj) SG_XTYPEP(obj, SG_CLASS_CLASS)

#define SG_CLASS_FLAGS(obj)    (SG_CLASS(obj)->flags)
#define SG_CLASS_CATEGORY(obj) (SG_CLASS_FLAGS(obj) & 3)

#define SG_ALLOCATE(klassname, klass)		\
  ((klassname*)Sg_AllocateInstance(klass))

enum {
  SG_CLASS_BUILTIN  = 0,
  SG_CLASS_ABSTRACT = 1,
  SG_CLASS_BASE     = 2,
  SG_CLASS_SCHEME   = 3,
};

/* built-in classes */
SG_CLASS_DECL(Sg_TopClass);
SG_CLASS_DECL(Sg_BoolClass);
SG_CLASS_DECL(Sg_CharClass);
SG_CLASS_DECL(Sg_ClassClass);
SG_CLASS_DECL(Sg_EOFObjectClass);
SG_CLASS_DECL(Sg_UndefinedClass);
SG_CLASS_DECL(Sg_UnknownClass);
SG_CLASS_DECL(Sg_ObjectClass);	/* base of Scheme-defined objects */

#define SG_CLASS_TOP   	    	  (&Sg_TopClass)
#define SG_CLASS_BOOL  	    	  (&Sg_BoolClass)
#define SG_CLASS_CHAR  	    	  (&Sg_CharClass)
#define SG_CLASS_CLASS 	    	  (&Sg_ClassClass)
#define SG_CLASS_EOF_OBJECT 	  (&Sg_EOFObjectClass)
#define SG_CLASS_UNDEFINED_OBJECT (&Sg_UndefinedClass)
#define SG_CLASS_UNKNOWN          (&Sg_UnknownClass)
#define SG_CLASS_OBJECT           (&Sg_ObjectClass)

extern SgClass *Sg_DefaultCPL[];
extern SgClass *Sg_ObjectCPL[];

#define SG_CLASS_DEFAULT_CPL   (Sg_DefaultCPL)
#define SG_CLASS_OBJECT_CPL    (Sg_ObjectCPL)

#define SG_DEFINE_CLASS_COMMON(cname, coreSize, flag, printer, compare, serialize, allocate, cpa) \
  SgClass cname = {							\
    {{ SG_CLASS_STATIC_TAG(Sg_ClassClass), NULL }},			\
    printer,								\
    compare,								\
    serialize,								\
    allocate,								\
    /* for future */							\
    /* reader, */							\
    /* writer, */							\
    cpa,								\
    0,				/* nfields */				\
    coreSize,			/* coreSize */				\
    flag,			/* flag */				\
    SG_FALSE,			/* name */				\
    SG_NIL,			/* directSupers */			\
    SG_NIL,			/* cpl */				\
    SG_NIL,			/* directSlots */			\
    SG_NIL,			/* slots */				\
    SG_NIL,			/* fieldInitializers */			\
    NULL			/* gettersNSetters */			\
  }

/* for now we do not add any cache reader, it's for future */
#define SG_DEFINE_BUILTIN_CLASS(cname, printer, compare, serialize, allocate, cpa) \
  SG_DEFINE_CLASS_COMMON(cname, 0, SG_CLASS_BUILTIN,			\
			 printer, compare, serialize, allocate, cpa)
#define SG_DEFINE_BUILTIN_CLASS_SIMPLE(cname, printer)			\
  SG_DEFINE_CLASS_COMMON(cname, 0, SG_CLASS_BUILTIN,			\
			 printer, NULL, NULL, NULL, NULL)

#define SG_DEFINE_ABSTRACT_CLASS(cname, cpa)				\
  SG_DEFINE_CLASS_COMMON(cname, 0, SG_CLASS_ABSTRACT,			\
			 NULL, NULL, NULL, NULL, cpa)

#define SG_DEFINE_BASE_CLASS(cname, ctype, printer, compare, serialize, allocate, cpa) \
  SG_DEFINE_CLASS_COMMON(cname, sizeof(ctype), SG_CLASS_BASE,	\
			 printer, compare, serialize, allocate, cpa)

#define argumentAsClass(index, tmp, var)				\
  castArgumentType(index, tmp, var, bytevector, SG_CLASSP, SG_CLASS)

SG_CDECL_BEGIN

/* API's for CLOS */
SG_EXTERN SgObject Sg_MakeClass(SgObject supers, SgObject slots);
SG_EXTERN SgObject Sg_MakeGeneric();
SG_EXTERN SgObject Sg_MakeMethod(SgObject specializers, SgObject procedure);
SG_EXTERN SgObject Sg_SlotRef(SgObject obj, SgObject name);
SG_EXTERN void     Sg_SlotSet(SgObject obj, SgObject name, SgObject value);

/* MOP looks like APIs */
SG_EXTERN SgClass* Sg_ClassOf(SgObject obj);
/* type check */
SG_EXTERN int      Sg_TypeP(SgObject obj, SgClass *type);
SG_EXTERN int      Sg_SubtypeP(SgClass *sub, SgClass *type);

/* just access to SgClass */
#define SG_CLASS_DIRECT_SUPERS(klass)  (SG_CLASS(klass)->directSupers)
#define SG_CLASS_DIRECT_SLOTS(klass)   (SG_CLASS(klass)->directSlots)
#define SG_CLASS_CPL(klass)            (SG_CLASS(klass)->cpl)
/* for C use */
#define SG_CLASS_CPA(klass)            (SG_CLASS(klass)->cpa)
#define SG_CLASS_SLOTS(klass)          (SG_CLASS(klass)->slots)

/* intercessory protocol */
SG_EXTERN SgObject Sg_AllocateInstance(SgClass *klass);
SG_EXTERN SgObject Sg_ComputeCPL(SgClass *klass);

SG_EXTERN int      Sg_ApplicableP(SgObject spec, SgObject args);

/* builtin class <object> */
SG_EXTERN SgObject Sg_ObjectAllocate(SgClass *klass, SgObject initargs);

/* internal for C. */
SG_EXTERN void     Sg_InitStaticClass(SgClass *klass, const SgChar *name,
				      SgLibrary *lib, SgSlotAccessor *specs,
				      int flags);
SG_EXTERN void     Sg_InitStaticClassWithMeta(SgClass *klass,
					      const SgChar *name,
					      SgLibrary *lib, SgClass *meta,
					      SgObject supers,
					      SgSlotAccessor *specs, int flags);
				      

SG_CDECL_END

#endif /* SAGITTARIUS_CLOS_H_ */
