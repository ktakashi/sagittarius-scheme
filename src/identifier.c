/* identifier.c                                    -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
    Sg_Write(id->library->name, port, SG_WRITE_DISPLAY);
  }
#if 1
  if (SG_WRITE_MODE(ctx) == SG_WRITE_WRITE ||
      SG_WRITE_MODE(ctx) == SG_WRITE_SHARED) {
    char buf[50];
    Sg_Putc(port, ' ');
    Sg_Write(SG_IDENTIFIER_IDENTITY(id), port, SG_WRITE_WRITE);
    snprintf(buf, sizeof(buf), " (%p):%d", id, SG_IDENTIFIER_PENDING(id));
    Sg_Putz(port, buf);
  }
  /* Sg_Write(id->envs, port, SG_WRITE_SHARED); */
#endif
  Sg_Putc(port, '>');
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_IdentifierClass, id_print);

static SgIdentifier* make_identifier()
{
  SgIdentifier *id = SG_NEW(SgIdentifier);
  SG_SET_CLASS(id, SG_CLASS_IDENTIFIER);
  id->pending = FALSE;		/* for sanity */
  return id;
}

/* should we move this to vm.c so that SG_CURRENT_IDENTITY can be hidden? */
/*
  This procedure actually does 2 things depending on the given arguments.
  1. creating global identifier
  2. renaming the given identifier.
  TODO the second part should be separated but for now.
 */
SgObject Sg_MakeIdentifier(SgObject id_or_sm, SgObject envs, SgLibrary *library)
{
  SgIdentifier *id = make_identifier();
  id->name = (SG_IDENTIFIERP(id_or_sm))
    ? SG_IDENTIFIER_NAME(id_or_sm) : id_or_sm;
  id->library = library;

  if (SG_IDENTIFIERP(id_or_sm)) {
    id->envs = Sg_Cons(envs, SG_IDENTIFIER_ENVS(id_or_sm));
  } else {
    /* just wrap it */
    if (SG_NULLP(envs)) {
      id->envs = SG_NIL;
    } else {
      id->envs = SG_LIST1(envs);
    }
  }
  if (SG_NULLP(envs)) {
    SG_IDENTIFIER_IDENTITY(id) = SG_FALSE; /* global */
  } else {
    if (SG_IDENTIFIERP(id_or_sm)) {
      SG_IDENTIFIER_IDENTITY(id) = Sg_Cons(SG_CURRENT_IDENTITY, 
					   SG_IDENTIFIER_IDENTITY(id_or_sm));
    } else {
      /* fake it as if renamed from global identifier */
      SG_IDENTIFIER_IDENTITY(id) = Sg_Cons(SG_CURRENT_IDENTITY, SG_FALSE);
    }
  }
  return SG_OBJ(id);
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
