/* -*- C -*- */
/*
 * macro.c
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
#include "sagittarius/macro.h"
#include "sagittarius/identifier.h"
#include "sagittarius/pair.h"
#include "sagittarius/vector.h"
#include "sagittarius/symbol.h"
#include "sagittarius/code.h"
#include "sagittarius/subr.h"
#include "sagittarius/vm.h"
#include "sagittarius/builtin-symbols.h"

SgObject Sg_MakeSyntax(SgSymbol *name, SgObject proc, int userDefined)
{
  SgSyntax *z = SG_NEW(SgSyntax);
  SG_SET_HEADER(z, TC_SYNTAX);
  z->name = name;
  z->proc = proc;
  z->userDefined = userDefined;
  return SG_OBJ(z);
}

SgObject Sg_MakeMacro(SgObject name, SgObject transformer, void *data, SgObject maybeLibrary)
{
  SgMacro *z = SG_NEW(SgMacro);
  SG_SET_HEADER(z, TC_MACRO);
  z->name = name;
  z->transformer = transformer;
  z->data = data;
  z->maybeLibrary = maybeLibrary;
  return SG_OBJ(z);
}

static SgObject unwrap_rec(SgObject form, SgObject history)
{
  SgObject newh;
  if (!SG_PTRP(form)) return form;
  if (!SG_FALSEP(Sg_Memq(form, history))) return form;

  if (SG_PAIRP(form)) {
    SgObject ca, cd;
    newh = Sg_Cons(form, history);
    ca = unwrap_rec(SG_CAR(form), newh);
    cd = unwrap_rec(SG_CDR(form), newh);
    if (ca == SG_CAR(form) && cd == SG_CDR(form)) {
      return form;
    } else {
      return Sg_Cons(ca, cd);
    }
  }
  if (SG_IDENTIFIERP(form)) {
    return SG_OBJ(SG_IDENTIFIER_NAME(form));
  }
  if (SG_VECTORP(form)) {
    int i, j, len = SG_VECTOR_SIZE(form);
    SgObject elt, *pelt = SG_VECTOR_ELEMENTS(form);
    newh = Sg_Cons(form, history);
    for (i = 0; i < len; i++, pelt++) {
      elt = unwrap_rec(*pelt, newh);
      if (elt != *pelt) {
	SgObject newvec = Sg_MakeVector(len, SG_FALSE);
	pelt = SG_VECTOR_ELEMENTS(form);
	for (j = 0; j < i; j++) {
	  SG_VECTOR_ELEMENT(newvec, j) = *pelt;
	}
	SG_VECTOR_ELEMENT(newvec, i) = elt;
	for (; j < len; j++) {
	  SG_VECTOR_ELEMENT(newvec, j) = unwrap_rec(*pelt, newh);
	}
	return newvec;
      }
    }
    return form;
  }
  return form;
}

static SgObject macro_tranform(SgObject *args, int argc, void *data_)
{
  SgObject macro, form, p1env, data;
  macro = args[0];
  form = args[1];
  p1env = args[2];
  data = args[3];

  return Sg_VMApply(data, SG_LIST1(Sg_Cons(form, p1env)));
}

static SG_DEFINE_SUBR(macro_tranform_Stub, 2, 0, macro_tranform, SG_FALSE, NULL);

SgObject Sg_MakeMacroTransformer(SgObject name, SgObject proc, SgObject library)
{
  if (SG_FALSEP(SG_PROCEDURE_NAME(&macro_tranform_Stub))) {
    SG_PROCEDURE_NAME(&macro_tranform_Stub) = Sg_MakeString(UC("macro-transform"), SG_LITERAL_STRING);
  }
  return Sg_MakeMacro(name, &macro_tranform_Stub, proc, library);
}

static SgObject macro_expand_rec(SgObject form, SgObject p1env, int onceP)
{
  SgObject expr;
  SG_FOR_EACH(expr, form) {
    if (SG_NULLP(expr)) return SG_NIL;
    else if (!SG_PAIRP(expr)) return expr;
    else if (SG_PAIRP(SG_CAR(expr))) {
      /* ((xx ...) ...) */
      return Sg_Cons(macro_expand_rec(SG_CAR(expr), p1env, onceP),
		     macro_expand_rec(SG_CDR(expr), p1env, onceP));
    } else {
      SgObject g = SG_FALSE, mac = SG_FALSE, syn = SG_FALSE,
	sym = SG_CAR(expr);
      if (SG_IDENTIFIERP(sym)) {
	g = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(sym),
			   SG_IDENTIFIER_NAME(sym));
      } else if (SG_EQ(sym, SG_SYMBOL_QUOTE)) {
	/* 'macro case. we need to avoid expantion for this. */
	return expr;
      } else if (SG_SYMBOLP(sym)) {
	g = Sg_FindBinding(Sg_VMCurrentLibrary(), sym);
      }
      if (SG_MACROP(g)) mac = g;
      if (SG_USER_DEFINED_SYNTXP(g)) syn = g;
      if (!SG_FALSEP(mac)) {
	SgObject applyArgs = SG_LIST4(mac, expr, p1env, SG_MACRO(mac)->data);
	SgObject ret = Sg_Apply(SG_MACRO(mac)->transformer, applyArgs);
	if (onceP) {
	  return ret;
	} else {
	  return macro_expand_rec(ret, p1env, onceP);
	}
      } else if (!SG_FALSEP(syn)) {
	if (SG_CODE_BUILDERP(SG_SYNTAX(syn)->proc)) {
	  SgObject ret = Sg_VMApply(SG_SYNTAX(syn)->proc, SG_LIST1(expr));
	  if (onceP) {
	    return ret;
	  } else {
	    return macro_expand_rec(ret, p1env, onceP);
	  }
	} else {
	  /* syntax that made by (syntax a) */
	  return Sg_Cons(SG_CAR(expr), macro_expand_rec(SG_CDR(expr), p1env, onceP));
	}
      } else {
	return Sg_Cons(SG_CAR(expr), macro_expand_rec(SG_CDR(expr), p1env, onceP));
      }
    }
  }
  /* not pair or null */
  return form;
}

SgObject Sg_MacroExpand(SgObject form, SgObject p1env, int onceP)
{
  return macro_expand_rec(form, p1env, onceP);
}

/* convert all identifier to symbol */
SgObject Sg_UnwrapSyntax(SgObject form)
{
  return unwrap_rec(form, SG_NIL);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
