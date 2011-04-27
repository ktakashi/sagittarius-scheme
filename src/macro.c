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
#include "sagittarius/gloc.h"
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
	for (j = 0; j < i; j++, pelt++) {
	  SG_VECTOR_ELEMENT(newvec, j) = *pelt;
	}
	SG_VECTOR_ELEMENT(newvec, i) = elt;
	for (; j < len; j++, pelt++) {
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
  /* TODO it's kinda waste of time if we compute each time. */
  /* NB: we don't use scheme apply(Sg_VMApply) to get macro transformer, because
         it contains HALT in it. If we use it, programme will be stopped. */
  data = Sg_Apply0(args[3]);
  if (SG_MACROP(data)) {
    return Sg_Apply4(SG_MACRO(data)->transformer,
		     data, form, p1env, SG_MACRO(data)->data);
  } else {
    return Sg_Apply1(data, Sg_Cons(form, p1env));
  }
}

static SG_DEFINE_SUBR(macro_tranform_Stub, 2, 0, macro_tranform, SG_FALSE, NULL);

SgObject Sg_MakeMacroTransformer(SgObject name, SgObject proc, SgObject library)
{
  if (SG_FALSEP(SG_PROCEDURE_NAME(&macro_tranform_Stub))) {
    SG_PROCEDURE_NAME(&macro_tranform_Stub) = Sg_MakeString(UC("macro-transform"), SG_LITERAL_STRING);
  }
  return Sg_MakeMacro(name, &macro_tranform_Stub, proc, library);
}

static SgObject macro_expand_cc(SgObject result, void **data)
{
  SgObject env = SG_OBJ(data[0]);
  return Sg_MacroExpand(result, env, FALSE);
}

SgObject Sg_MacroExpand(SgObject expr, SgObject p1env, int onceP)
{
  SgObject sym, op;
  SgMacro *mac;

  if (!SG_PAIRP(expr)) return expr;
  if (SG_PAIRP(SG_CAR(expr))) {
    return Sg_Cons(Sg_MacroExpand(SG_CAR(expr), p1env, onceP),
		   Sg_MacroExpand(SG_CDR(expr), p1env, onceP));
  }
  op = SG_CAR(expr);
  if (SG_MACROP(op)) {
    mac = SG_MACRO(op);
  } else if (!SG_SYMBOLP(op) && !SG_IDENTIFIERP(op)) {
    return expr;
  } else {
    mac = NULL;
    sym = op;
    if (SG_MACROP(sym)) {
      /* never happen i guess */
      mac = SG_MACRO(sym);
    } else {
      SgObject g = NULL;
      if (SG_IDENTIFIERP(sym)) {
	g = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(sym),
			   SG_IDENTIFIER_NAME(sym),
			   SG_FALSE);
      } else if (SG_SYMBOLP(sym)) {
	g = Sg_FindBinding(SG_VECTOR_ELEMENT(p1env, 0),
			   sym,
			   SG_FALSE);
			   
      }
      if (!SG_FALSEP(g)) {
	SgObject gval = SG_GLOC_GET(SG_GLOC(g));
	if (SG_MACROP(gval)) {
	  mac = SG_MACRO(gval);
	}
      }
    }
  }
  if (mac) {
    if (!onceP) {
      void *data[1];
      data[0] = p1env;
      Sg_VMPushCC(macro_expand_cc, data, 1);
    }
    expr = Sg_Apply4(mac->transformer, mac, expr, p1env, mac->data);
  }
  /* not pair or null */
  return expr;
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
