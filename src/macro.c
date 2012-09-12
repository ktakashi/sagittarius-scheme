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
#include "sagittarius/clos.h"
#include "sagittarius/identifier.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/vector.h"
#include "sagittarius/symbol.h"
#include "sagittarius/code.h"
#include "sagittarius/library.h"
#include "sagittarius/subr.h"
#include "sagittarius/reader.h"
#include "sagittarius/vm.h"
#include "sagittarius/gloc.h"
#include "sagittarius/writer.h"
#include "sagittarius/builtin-symbols.h"

static void syntax_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<syntax %A>"), SG_SYNTAX(obj)->name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SyntaxClass, syntax_print);

SgObject Sg_MakeSyntax(SgSymbol *name, SgObject proc, int userDefined)
{
  SgSyntax *z = SG_NEW(SgSyntax);
  SG_SET_CLASS(z, SG_CLASS_SYNTAX);
  z->name = name;
  z->proc = proc;
  z->userDefined = userDefined;
  return SG_OBJ(z);
}

static void macro_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<macro %A>"), SG_MACRO(obj)->name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_MacroClass, macro_print);

SgObject Sg_MakeMacro(SgObject name, SgObject transformer, 
		      void *data, SgObject env, SgObject maybeLibrary)
{
  SgMacro *z = SG_NEW(SgMacro);
  SG_SET_CLASS(z, SG_CLASS_MACRO);
  z->name = name;
  z->transformer = transformer;
  z->data = data;
  z->env = env;
  z->maybeLibrary = maybeLibrary;
  return SG_OBJ(z);
}

static SgObject unrename_symbol(SgObject name, int reversep)
{
  /* bit more check */
  if (reversep &&
      SG_UNINTERNED_SYMBOL(name) &&
      SG_SYMBOL(name)->flags & SG_SYMBOL_REVERSIBLE) {
    /* finds ` which is mark of separator */
    SgString *s = SG_SYMBOL(name)->name;
      
    SgChar *buf = SG_STRING_VALUE(s), tmp[256];
    int len, size = SG_STRING_SIZE(s), i;
    for (len = 0; len < size; len++, buf++) {
      if (*buf == '`') break;
    }
    ASSERT(len != size);	/* something wrong */
    if (len >= 256) {
      buf = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * (len+1));
    } else {
      buf = tmp;
    }
    /* copy */
    for (i = 0; i < len; i++) buf[i] = SG_STRING_VALUE_AT(s, i);
    buf[i] = 0;
    /* the copied string must be literal for symbol */
    s = SG_STRING(Sg_MakeStringEx(buf, SG_LITERAL_STRING, len));
    return Sg_Intern(s);
  } else {
    return name;
  }
}

static SgObject unwrap_rec(SgObject form, SgObject history, int reversep)
{
  SgObject newh;
  if (!SG_PTRP(form)) return form;
  if (!SG_FALSEP(Sg_Memq(form, history))) return form;

  if (Sg_ConstantLiteralP(form)) return form;

  if (SG_PAIRP(form)) {
    SgObject ca, cd;
    newh = Sg_Cons(form, history);
    ca = unwrap_rec(SG_CAR(form), newh, reversep);
    cd = unwrap_rec(SG_CDR(form), newh, reversep);
    if (ca == SG_CAR(form) && cd == SG_CDR(form)) {
      return form;
    } else {
      return Sg_Cons(ca, cd);
    }
  }
  if (SG_IDENTIFIERP(form)) {
    return unrename_symbol(SG_IDENTIFIER_NAME(form), reversep);
  }
  if (SG_SYMBOLP(form)) {
    return unrename_symbol(form, reversep);
  }
  if (SG_VECTORP(form)) {
    int i, j, len = SG_VECTOR_SIZE(form);
    SgObject elt, *pelt = SG_VECTOR_ELEMENTS(form);
    newh = Sg_Cons(form, history);
    for (i = 0; i < len; i++, pelt++) {
      elt = unwrap_rec(*pelt, newh, reversep);
      if (elt != *pelt) {
	SgObject newvec = Sg_MakeVector(len, SG_FALSE);
	pelt = SG_VECTOR_ELEMENTS(form);
	for (j = 0; j < i; j++, pelt++) {
	  SG_VECTOR_ELEMENT(newvec, j) = *pelt;
	}
	SG_VECTOR_ELEMENT(newvec, i) = elt;
	for (; j < len; j++, pelt++) {
	  SG_VECTOR_ELEMENT(newvec, j) = unwrap_rec(*pelt, newh, reversep);
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
  SgVM *vm = Sg_VM();
  SgObject macro, form, p1env, mac_env;
  SgObject ue_save = vm->usageEnv;
  SgObject me_save = vm->macroEnv;
  SgObject data;
  SgObject result;
  macro = args[0];
  ASSERT(SG_MACROP(macro));
  form = args[1];
  p1env = args[2];

  data = Sg_Apply0(args[3]);
  /* data = args[3]; */
  mac_env = SG_MACRO(macro)->env;

  vm->usageEnv = p1env;
  vm->macroEnv = mac_env;
  if (SG_MACROP(data)) {
    /* variable transformer */
    result = Sg_Apply4(SG_MACRO(data)->transformer,
		       data, form, mac_env, SG_MACRO(data)->data);
  } else {
    result = Sg_Apply1(data, form);
  }
  vm->usageEnv = ue_save;
  vm->macroEnv = me_save;
  return result;
}

static SG_DEFINE_SUBR(macro_tranform_Stub, 4, 0, macro_tranform, SG_FALSE, NULL);

SgObject Sg_MakeMacroTransformer(SgObject name, SgObject proc,
				 SgObject env, SgObject library)
{
  if (SG_FALSEP(SG_PROCEDURE_NAME(&macro_tranform_Stub))) {
    SG_PROCEDURE_NAME(&macro_tranform_Stub) = SG_MAKE_STRING("macro-transform");
  }
  return Sg_MakeMacro(name, &macro_tranform_Stub, proc, env, library);
}

static SgObject macro_expand_cc(SgObject result, void **data)
{
  SgObject env = SG_OBJ(data[0]);
  return Sg_MacroExpand(result, env, FALSE);
}


static SgObject p1env_lookup(SgVector *p1env, SgObject name, SgObject lookup_as)
{
  int name_identp = SG_IDENTIFIERP(name);
  SgObject frames = SG_VECTOR_ELEMENT(p1env, 1);
  SgObject fp, vp, cgen_62;
  SG_FOR_EACH(fp, frames) {
    if ((name_identp && SG_IDENTIFIER_ENVS(name) == fp)) {
      name=SG_OBJ(SG_IDENTIFIER_NAME(name));
    }
    if (SG_CAAR(fp) > lookup_as) {
      continue;
    }
    SG_FOR_EACH(cgen_62,SG_CDAR(fp)) {
      vp = SG_CAR(cgen_62);
      if (SG_EQ(name, SG_CAR(vp))) {
	return SG_CDR(vp);
      }
    }
  }
  return SG_FALSE;
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
      } else {
	g = p1env_lookup(SG_VECTOR(p1env), sym, SG_MAKE_INT(2));
	if (SG_MACROP(g)) {
	  mac = SG_MACRO(g);
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
  } else if (!onceP) {
    /* do recursively */
    expr = Sg_Cons(op,
		   Sg_MacroExpand(SG_CDR(expr), p1env, onceP));
  }
  /* not pair or null */
  return expr;
}

/* convert all identifier to symbol */
SgObject Sg_UnwrapSyntax(SgObject form)
{
  return unwrap_rec(form, SG_NIL, FALSE);
}

SgObject Sg_UnwrapSyntaxWithReverse(SgObject form)
{
  return unwrap_rec(form, SG_NIL, TRUE);
}

static SgObject macro_name(SgMacro *m)
{
  return m->name;
}

static SgObject macro_trans(SgMacro *m)
{
  return m->transformer;
}

/* be careful */
static SgObject macro_data(SgMacro *m)
{
  return m->data;
}
static SgObject macro_env(SgMacro *m)
{
  return m->env;
}

static SgObject macro_library(SgMacro *m)
{
  return m->maybeLibrary;
}

static SgSlotAccessor macro_slots[] = {
  SG_CLASS_SLOT_SPEC("name", 0, macro_name, NULL),
  SG_CLASS_SLOT_SPEC("transformer", 1, macro_trans, NULL),
  SG_CLASS_SLOT_SPEC("data", 2, macro_data, NULL),
  SG_CLASS_SLOT_SPEC("env", 3, macro_env, NULL),
  SG_CLASS_SLOT_SPEC("library", 4, macro_library, NULL),
  { { NULL } }
};

void Sg__InitMacro()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  Sg_InitStaticClass(SG_CLASS_MACRO, UC("<macro>"), lib, macro_slots, 0);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
