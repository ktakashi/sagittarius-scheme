/* -*- C -*- */
/*
 * generic.h
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
#ifndef SAGITTARIUS_GENERIC_H_
#define SAGITTARIUS_GENERIC_H_

#include "sagittariusdefs.h"

/*
  SgGeneric.

  This is the base template for user defined types. The concept of user defined
  typeds are serializable. So the template should have printer(serializer) and
  constructor(deserializer). 
  A user type must be extendable, so it also needs to have parent classes. If
  a type extends other type and it doesn't have printer, use parent printer as
  default. So is constructor.
 */
struct SgGenericRec
{
  SG_HEADER;
  SgSymbol    *name;		/* type name */
  int          virtualP;	/* if this class is virtual.
				   this is acually for diamond inheritance.
				 */
  SgObject     printer;		/* serializer, must be closure or subr which
				   take this object and port(optional) as 
				   arguments */
  SgObject     reader;		/* reader for reader macro */
  SgObject     constructor;	/* constructor */
  SgObject     parents;		/* list of parent class */
  SgObject     fields;		/* list of field names */
};

struct SgInstanceRec
{
  SG_HEADER;
  SgGeneric   *generic;		/* generic information */
  SgObject     values;		/* filed hash table.
				   TODO it must be something else but for now
				 */
};

#define SG_GENERIC(obj)      ((SgGeneric*)(obj))
#define SG_GENERICP(obj)     (SG_PTRP(obj) && IS_TYPE(obj, TC_GENERIC))

#define SG_GENERIC_NAME(obj)        (SG_GENERIC(obj)->name)
#define SG_GENERIC_PRINTER(obj)     (SG_GENERIC(obj)->printer)
#define SG_GENERIC_READER(obj)      (SG_GENERIC(obj)->reader)
#define SG_GENERIC_CONSTRUCTOR(obj) (SG_GENERIC(obj)->constructor)
#define SG_GENERIC_FIELDS(obj)      (SG_GENERIC(obj)->fields)

#define SG_INSTANCE(obj)     ((SgInstance*)obj)
#define SG_INSTANCEP(obj)    (SG_PTRP(obj) && IS_TYPE(obj, TC_INSTANCE))

/* 
   For internal convenience.
   It initialize printer and constructor. So make sure somewhere it needs to be
   initialized with fields. Because we don't have static list initializer.
   NB: name must be in builtin symbols. Or else we can't initialize it.
 */
#define SG__STATIC_GENERIC_INIT(name, virtual, prin, read, ctr)		\
  { MAKE_HDR_VALUE(TC_GENERIC), (name), (virtual),			\
      (prin), (read), (ctr), SG_NIL, SG_NIL }
#define SG_STATIC_GENERIC_INIT(cvar, name, virtual, print, read, ctr)	\
  SgGeneric cvar = SG__STATIC_GENERIC_INIT(name, virtual, print, read, ctr)


/* I'm not sure if here is good location to put UserDef */
typedef void SgMetaObjectPrinter(SgPort *p, SgObject self);
typedef struct SgMetaObjectRec
{
  SG_HEADER;
  SgMetaObjectPrinter *printer;
} SgMetaObject;

/* this must be used for extension modules.
   so i think i don't have to consider VC's __impl_ problem.
 */
#define SG_META_OBJ(obj)   ((SgMetaObject *)obj)
#define SG_META_OBJ_P(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_USER_DEF)
#define SG_DECLARE_META_OBJ(meta) extern SgMetaObject meta
#define SG_SET_META_OBJ(obj, meta) (SG_HDR(obj) = (meta))
#define SG_META_OBJ_TYPE(obj, type) (SG_HDR(obj) == (type))
  

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeGeneric(SgSymbol *name, SgObject printer,
				  SgObject constructor, SgObject fields);
SG_EXTERN void     Sg_RegisterGeneric(SgSymbol *name, SgGeneric *generic, SgObject library);
SG_EXTERN SgObject Sg_CreateInstance(SgGeneric *generic);
SG_EXTERN SgObject Sg_RetrieveGeneric(SgSymbol *name, SgObject maybeLibrary);
/* simple accessor */
SG_EXTERN SgObject Sg_GenericRef(SgObject obj, SgObject name);
SG_EXTERN void     Sg_GenericSet(SgObject obj, SgObject name, SgObject value);
SG_EXTERN int      Sg_GenericHasField(SgObject obj, SgObject name);

/*
  for record, we need a vector like object. for now we expand instance type for this.
 */
SG_EXTERN SgObject Sg_MakeTuple(int size, SgObject fill, SgObject printer);
SG_EXTERN void     Sg_TupleListSet(SgObject tuple, SgObject lst);
SG_EXTERN void     Sg_TupleSet(SgObject tuple, int i, SgObject value);
SG_EXTERN SgObject Sg_TupleRef(SgObject tuple, int i, SgObject fallback);
SG_EXTERN int      Sg_TupleSize(SgObject tuple);

SG_CDECL_END

#endif /* SAGITTARIUS_GENERIC_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
