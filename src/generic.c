/* -*- C -*- */
/*
 * generic.c
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
#include "sagittarius/generic.h"
#include "sagittarius/error.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/library.h"
#include "sagittarius/pair.h"
#include "sagittarius/symbol.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"

static SgGeneric* make_generic()
{
  SgGeneric *g = SG_NEW(SgGeneric);
  SG_SET_HEADER(g, TC_GENERIC);
  return g;
}

static SgInstance* make_instance(SgGeneric *generic)
{
  SgInstance *ins = SG_NEW(SgInstance);
  SG_SET_HEADER(ins, TC_INSTANCE);
  ins->generic = generic;
  ins->values = Sg_MakeHashTableSimple(SG_HASH_EQ, 0);
  return ins;
}

SgObject Sg_MakeGeneric(SgSymbol *name, SgObject printer,
			SgObject constructor, SgObject fields)
{
  SgGeneric *g = make_generic();
  g->name = name;
  g->printer = printer;
  g->constructor = constructor;
  g->fields = fields;
  g->parents = SG_NIL;
  return SG_OBJ(g);
}

void Sg_RegisterGeneric(SgSymbol *name, SgGeneric *generic, SgObject library)
{
  SgLibrary *lib = SG_LIBRARY(Sg_FindLibrary(library, FALSE));
  Sg_AddGenerics(lib, name, generic);
}

SgObject Sg_CreateInstance(SgGeneric *generic)
{
  return make_instance(generic);
}

/* duplicated macro... ugly */
#define ENSURE_LIBRARY(o, e)						\
  if (SG_LIBRARYP(o)) {							\
    e = SG_LIBRARY(o);							\
  } else {								\
    e = Sg_FindLibrary((o), FALSE);					\
  }


SgObject Sg_RetrieveGeneric(SgSymbol *name, SgObject maybeLibrary)
{
  SgLibrary *lib;
  SgObject ret;
  if (!SG_FALSEP(maybeLibrary)) {
    ENSURE_LIBRARY(maybeLibrary, lib);
  } else {
    lib = SG_FALSE;
  }
  /* if it's not library, gets current library */
  if (!SG_LIBRARYP(lib)) {
    lib = Sg_VM()->currentLibrary;
  }
  ret = Sg_Assq(name, lib->generics);
  if (SG_FALSEP(ret) || !SG_GENERICP(SG_CDR(ret))) {
    Sg_Error(UC("%S is not registered as generic class."), name);
  }
  return SG_CDR(ret);
}

SgObject Sg_GenericRef(SgObject obj, SgObject name)
{
  if (!SG_INSTANCEP(obj)) {
    Sg_Error(UC("generic instance required, but got  %S"), obj);
  }
  /* TODO should i check if the object has the given field? */
  return Sg_HashTableRef(SG_INSTANCE(obj)->values, name, SG_UNDEF);
}

void Sg_GenericSet(SgObject obj, SgObject name, SgObject value)
{
  if (!SG_INSTANCEP(obj)) {
    Sg_Error(UC("generic instance required, but got  %S"), obj);
  }
  if (SG_FALSEP(Sg_Memq(name, SG_INSTANCE(obj)->generic->fields))) {
    Sg_Error(UC("generic instance does not have given field %S"), name);
  }

  Sg_HashTableSet(SG_INSTANCE(obj)->values, name, value, 0);
}

int Sg_GenericHasField(SgObject obj, SgObject name)
{
  if (!SG_INSTANCEP(obj)) {
    Sg_Error(UC("generic instance required, but got  %S"), obj);
  }
  return !SG_FALSEP(Sg_Memq(name, SG_INSTANCE(obj)->generic->fields));
}

static SgInstance* make_tuple(int size, SgObject fill, SgObject printer)
{
  SgGeneric *g = make_generic();
  SgInstance *i = SG_NEW(SgInstance);
  SG_SET_HEADER(i, TC_INSTANCE);
  g->name = SG_INTERN("tuple");
  g->printer = printer;
  g->constructor = SG_FALSE;
  g->fields = SG_NIL;
  g->parents = SG_NIL;
  i->generic = g;
  i->values = Sg_MakeVector(size, fill);
  return i;
}

SgObject Sg_MakeTuple(int size, SgObject fill, SgObject printer)
{
  return make_tuple(size, fill, printer);
}

void Sg_TupleListSet(SgObject tuple, SgObject lst)
{
  int i;
  SgObject cp = lst;
  SgVector *vec = SG_VECTOR(SG_INSTANCE(tuple)->values);
  for (i = 0, cp = lst; i < SG_VECTOR_SIZE(vec) && SG_PAIRP(cp); i++, cp = SG_CDR(cp)) {
    SG_VECTOR_ELEMENT(vec, i) = SG_CAR(cp);
  }
}

void Sg_TupleSet(SgObject tuple, int i, SgObject value)
{
  SgVector *vec = SG_VECTOR(SG_INSTANCE(tuple)->values);
  SG_VECTOR_ELEMENT(vec, i) = value;
}

SgObject Sg_TupleRef(SgObject tuple, int i, SgObject fallback)
{
  if (!SG_INSTANCEP(tuple) ||
      !SG_VECTORP(SG_INSTANCE(tuple)->values)) {
    return fallback;
  }
  return Sg_VectorRef(SG_INSTANCE(tuple)->values, i, fallback);
}

int Sg_TupleSize(SgObject tuple)
{
  return SG_VECTOR_SIZE(SG_INSTANCE(tuple)->values);
}
