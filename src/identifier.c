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
#include "sagittarius/reader.h"
#include "sagittarius/vm.h"

static void id_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgIdentifier *id = SG_IDENTIFIER(obj);
  Sg_Putuz(port, UC("#<identifier "));
  Sg_Write(id->name, port, ctx->mode);
  Sg_Putc(port, '#');
  if (SG_LIBRARYP(id->library)) {
    Sg_Write(id->library->name, port, 0);
  } else {
    Sg_Write(SG_INTERN(":toplevel"), port, 0);
  }
#if 1
  if (SG_WRITE_MODE(ctx) == SG_WRITE_WRITE ||
      SG_WRITE_MODE(ctx) == SG_WRITE_SHARED) {
    char buf[50];
    snprintf(buf, sizeof(buf), "(%p)", id);
    Sg_Putz(port, buf);
  }
  /* Sg_Write(id->envs, port, SG_WRITE_SHARED); */
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

static SgIdentifier* make_identifier()
{
  SgIdentifier *id = SG_NEW(SgIdentifier);
  SG_SET_CLASS(id, SG_CLASS_IDENTIFIER);
  id->pending = FALSE;		/* for sanity */
  return id;
}

SgObject Sg_MakeIdentifier(SgObject id_or_sm, SgObject envs, SgLibrary *library)
{
  SgIdentifier *id = make_identifier();
  id->name = (SG_IDENTIFIERP(id_or_sm))
    ? SG_IDENTIFIER_NAME(id_or_sm) : id_or_sm;
  id->library = library;
  id->envs = (envs == SG_NIL)
    ? SG_NIL : SG_IDENTIFIERP(id_or_sm)
    ? envs   : get_binding_frame(SG_OBJ(id_or_sm), envs);
  return SG_OBJ(id);
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

void Sg__InitIdentifier()
{
  /* For future we might want to make identifier <object> to use slot-ref
     but for now.*/
  SgLibrary *clib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  Sg_InitStaticClass(SG_CLASS_IDENTIFIER, UC("<identifier>"), clib, NULL, 0);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
