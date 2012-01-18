/* -*- C -*- */
/*
 * identifier.c
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
#include "sagittarius/identifier.h"
#include "sagittarius/symbol.h"
#include "sagittarius/library.h"
#include "sagittarius/pair.h"
#include "sagittarius/vector.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/writer.h"
#include "sagittarius/port.h"

static void id_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgIdentifier *id = SG_IDENTIFIER(obj);
  Sg_Putuz(port, UC("#<identifier "));
  Sg_Write(id->name, port, ctx->mode);
  Sg_Putc(port, '#');
  Sg_Write(id->library->name, port, ctx->mode);
#if 1
  if (SG_WRITE_MODE(ctx) == SG_WRITE_WRITE) {
    char buf[50];
    snprintf(buf, sizeof(buf), "(%p)", id);
    Sg_Putz(port, buf);
  }
#endif
  Sg_Putc(port, '>');
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_IdentifierClass, id_print);

static SgObject get_binding_frame(SgObject var, SgObject env)
{
  SgObject frame, fp;
  SG_FOR_EACH(frame, env) {
    if (!SG_PAIRP(SG_CAR(frame))) continue;
    SG_FOR_EACH(fp, SG_CDAR(frame)) {
      if (SG_CAAR(fp) == var) return frame;
    }
  }
  return SG_NIL;
}

SgObject Sg_MakeIdentifier(SgSymbol *symbol, SgObject envs, SgLibrary *library)
{
  SgIdentifier *id = SG_NEW(SgIdentifier);
  SG_SET_CLASS(id, SG_CLASS_IDENTIFIER);
  id->name = symbol;
  id->library = library;
  id->envs = (envs == SG_NIL) ? SG_NIL : get_binding_frame(SG_OBJ(symbol), envs);
  return SG_OBJ(id);
}

SgObject Sg_CopyIdentifier(SgIdentifier *id)
{
  return Sg_MakeIdentifier(id->name, id->envs, id->library);
}

/* TODO this is almost the same as one in vmlib.stub */
static SgObject p1env_lookup(SgObject form, SgVector *p1env, int lookup_as)
{
  SgObject frames = SG_VECTOR_ELEMENT(p1env, 1);
  SgObject fp, vp, vtmp;
  SG_FOR_EACH(fp, frames) {
    if (SG_INT_VALUE(SG_CAAR(fp)) > lookup_as) continue;
    SG_FOR_EACH(vtmp, SG_CDAR(fp)) {
      vp = SG_CAR(vtmp);
      if (SG_EQ(form, SG_CAR(vp))) {
	return fp;
      }
    }
  }
  return SG_NIL;
}

static SgObject wrap_rec(SgObject form, SgVector *p1env, SgHashTable *seen, int lexicalP)
{
  if (SG_NULLP(form)) {
    return form;
  } else if (SG_PAIRP(form)) {
    return Sg_Cons(wrap_rec(SG_CAR(form), p1env, seen, lexicalP),
		   wrap_rec(SG_CDR(form), p1env, seen, lexicalP));
  } else if (SG_VECTORP(form)) {
    return Sg_ListToVector(wrap_rec(Sg_VectorToList(form, 0, -1), p1env, seen, lexicalP), 0, -1);
  } else if (SG_SYMBOLP(form)) {
    /* lookup from p1env.
       exists: we need to wrap with the env which contains this symbol.
       not exist: we just need to wrap it.
     */
    /* TODO lexical? */
    SgObject id = Sg_HashTableRef(seen, form, SG_FALSE);
    if (SG_FALSEP(id)) {
      SgObject env = p1env_lookup(form, p1env, 0);
      if (SG_NULLP(env) && !lexicalP) {
	id = Sg_MakeIdentifier(form,
			       SG_VECTOR_ELEMENT(p1env, 1),
			       SG_VECTOR_ELEMENT(p1env, 0));
      } else if (!SG_NULLP(env)) {
	id = Sg_MakeIdentifier(form, env, SG_VECTOR_ELEMENT(p1env, 0));
      } else {
	/* if it's partial wrap and symbol is not lexical bounded, just return */
	return form;
      }
      Sg_HashTableSet(seen, form, id, 0);
      return id;
    } else {
      return id;
    }

  } else {
    return form;
  }
}

/* wrap form to identifier */
SgObject Sg_WrapSyntax(SgObject form, SgVector *p1env, SgObject seen, int lexicalP)
{
  if (!seen) {
    seen = Sg_MakeHashTableSimple(SG_HASH_EQ, 0);
  }
  ASSERT(SG_HASHTABLE_P(seen));
  return wrap_rec(form, p1env, SG_HASHTABLE(seen), lexicalP);
}

/* originally from chibi scheme */
int Sg_IdentifierEqP(SgObject e1, SgObject id1, SgObject e2, SgObject id2)
{
  SgObject lam1 = SG_FALSE, lam2 = SG_FALSE;
  SgLibrary *lib1 = NULL, *lib2 = NULL;
  int both = 0;
  /* short cut */
  if (SG_EQ(id1, id2)) return TRUE;

  /* strip p1env to frames*/
  e1 = (SG_VECTORP(e1))
    ? SG_VECTOR_ELEMENT(e1, 1)
    : (SG_PAIRP(e1))
    ? e1
    : SG_NIL;
  e2 = (SG_VECTORP(e2))
    ? SG_VECTOR_ELEMENT(e2, 1)
    : (SG_PAIRP(e2))
    ? e2
    : SG_NIL;

  if (SG_IDENTIFIERP(id1)) {
    e1 = SG_IDENTIFIER_ENVS(id1);
    lib1 = SG_IDENTIFIER_LIBRARY(id1);
    id1 = SG_IDENTIFIER_NAME(id1);
    both++;
  }
  if (SG_IDENTIFIERP(id2)) {
    e2 = SG_IDENTIFIER_ENVS(id2);
    lib2 = SG_IDENTIFIER_LIBRARY(id2);
    id2 = SG_IDENTIFIER_NAME(id2);
    both++;
  }
  /* lam1 = Sg_Assq(id1, e1); */
  lam1 = get_binding_frame(id1, e1);
  if (!SG_NULLP(lam1)) {
    lam1 = SG_CDR(lam1);
  }
  /* lam2 = Sg_Assq(id2, e2); */
  lam2 = get_binding_frame(id2, e2);
  if (!SG_NULLP(lam2)) {
    lam2 = SG_CDR(lam2);
  }
  if (both != 2) {
    lib1 = lib2 = NULL;
  }
  return (id1 == id2) && (lam1 == lam2) && (lib1 == lib2);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
