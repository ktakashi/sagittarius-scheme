/* generic.h                                             -*- coding: utf-8; -*-
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
#ifndef SAGITTARIUS_GENERIC_H_
#define SAGITTARIUS_GENERIC_H_

#include "sagittariusdefs.h"
#include "thread.h"
#include "subr.h"
#include "clos.h"

/* generic */
SG_CLASS_DECL(Sg_GenericClass);
#define SG_CLASS_GENERIC (&Sg_GenericClass)

typedef struct SgGenericRec SgGeneric;
struct SgGenericRec
{
  SgProcedure common;
  SgObject methods;		/* list of applicable procedures */
  int      maxReqargs;
  /* generic defined in C can have C function for the very last method */
  SgObject (*fallback)(SgObject *argv, int argc, SgGeneric *gf);
  void     *data;
  SgInternalMutex mutex;
};
#define SG_GENERIC(obj)  ((SgGeneric*)(obj))
#define SG_GENERICP(obj) SG_XTYPEP(obj, SG_CLASS_GENERIC)
#define SG_GENERIC_METHODS(generic)    	(SG_GENERIC(generic)->methods)
#define SG_GENERIC_FALLBACK(generic)   	(SG_GENERIC(generic)->fallback)
#define SG_GENERIC_DATA(generic)       	(SG_GENERIC(generic)->data)
#define SG_GENERIC_MAX_REQARGS(generic)	(SG_GENERIC(generic)->maxReqargs)
#define SG_GENERIC_MUTEX(generic)	(&(SG_GENERIC(generic)->mutex))

#define SG_DEFINE_GENERIC(cvar, cfunc, data)				\
  SgGeneric cvar = {							\
    SG__PROCEDURE_INITIALIZER(SG_CLASS_STATIC_TAG(Sg_GenericClass),	\
			      0, 0, SG_PROC_GENERIC,			\
			      SG_FALSE, SG_FALSE),			\
    SG_NIL, 0, (cfunc), data						\
  }

/* method */
SG_CLASS_DECL(Sg_MethodClass);
#define SG_CLASS_METHOD (&Sg_MethodClass)

typedef struct SgMethodRec
{
  SgProcedure common;
  SgGeneric  *generic;
  SgKeyword  *qualifier;	/* :primary :around :before or :after */
  SgClass   **specializers;	/* list of class.
				   must be array to initialize statically. */
  SgObject    procedure;	/* subr or closuer.
				   (lambda (call-next-method generic) ...) */
} SgMethod;
#define SG_METHOD(obj)  ((SgMethod*)(obj))
#define SG_METHODP(obj) SG_XTYPEP(obj, SG_CLASS_METHOD)

#define SG_METHOD_GENERIC(method)      (SG_METHOD(method)->generic)
#define SG_METHOD_SPECIALIZERS(method) (SG_METHOD(method)->specializers)
#define SG_METHOD_PROCEDURE(method)    (SG_METHOD(method)->procedure)
#define SG_METHOD_QUALIFIER(method)    (SG_METHOD(method)->qualifier)

#define SG_DEFINE_METHOD(cvar, gf, req, opt, specs, proc)		\
  SgMethod cvar = {							\
    SG__PROCEDURE_INITIALIZER(SG_CLASS_STATIC_TAG(Sg_MethodClass),	\
			      req, opt, SG_PROC_METHOD,			\
			      SG_FALSE, SG_FALSE),			\
    gf, SG_KEYWORD(SG_FALSE), specs, proc				\
  }

SG_CLASS_DECL(Sg_NextMethodClass);
#define SG_CLASS_NEXT_METHOD (&Sg_NextMethodClass)

typedef struct SgNextMethodRec
{
  SgProcedure common;
  SgGeneric  *generic;
  SgObject    methods;
  SgObject   *argv;
  int         argc;
} SgNextMethod;
#define SG_NEXT_METHOD(obj)  ((SgNextMethod*)(obj))
#define SG_NEXT_METHODP(obj) SG_XTYPEP(obj, SG_CLASS_NEXT_METHOD)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeBaseGeneric(SgObject name,
				      SgObject (*fallback)(SgObject *, int, 
							   SgGeneric *),
				      void *data);

SG_EXTERN void     Sg_InitBuiltinGeneric(SgGeneric *gf, const SgChar *name,
					 SgLibrary *lib);
SG_EXTERN void     Sg_InitBuiltinMethod(SgMethod *m);
SG_EXTERN SgObject Sg_NoNextMethod(SgObject *argv, int argc, SgGeneric *gf);
SG_EXTERN SgObject Sg_InvalidApply(SgObject *argv, int argc, SgGeneric *gf);

/* needs to be here ... */
SG_EXTERN SgObject Sg_AddMethod(SgGeneric *generic, SgMethod *method);
SG_EXTERN SgObject Sg_RemoveMethod(SgGeneric *generic, SgMethod *method);
SG_EXTERN SgObject Sg_ComputeMethods(SgGeneric *gf, SgObject *argv, int argc);
SG_EXTERN SgObject Sg_MakeNextMethod(SgGeneric *gf, SgObject methods,
				     SgObject *argv, int argc, int copyargs);

/* I'm not sure if these should be usable from other shared object. */
/* The initialization protocol */
SG_EXTERN SgGeneric Sg_GenericMake;
SG_EXTERN SgGeneric Sg_GenericInitialize;
/* the instance structure protocol */
SG_EXTERN SgGeneric Sg_GenericAllocateInstance;
SG_EXTERN SgGeneric Sg_GenericComputeGetterAndSetter;
/* The class initialization protocol */
SG_EXTERN SgGeneric Sg_GenericComputeCPL;
SG_EXTERN SgGeneric Sg_GenericComputeSlots;
SG_EXTERN SgGeneric Sg_GenericAddMethod;
SG_EXTERN SgGeneric Sg_GenericRemoveMethod;

SG_EXTERN SgGeneric Sg_GenericComputeApplicableMethodsGeneric;
/* The generic invocation protocol */
/* SG_EXTERN SgGeneric Sg_GenericComputeApplyGeneric; */
/* SG_EXTERN SgGeneric Sg_GenericComputeMethods; */
/* SG_EXTERN SgGeneric Sg_GenericComputeMethodMoreSpecificP; */
/* SG_EXTERN SgGeneric Sg_GenericComputeApplyMethods; */

SG_EXTERN SgGeneric Sg_GenericObjectEqualP;
SG_EXTERN SgGeneric Sg_GenericObjectApply;

SG_CDECL_END

#endif /* SAGITTARIUS_GENERIC_H_ */
