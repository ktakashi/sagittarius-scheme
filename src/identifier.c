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
  SG_SET_HEADER(id, TC_IDENTIFIER);
  id->name = symbol;
  id->library = library;
  id->envs = (envs == SG_NIL) ? SG_NIL : get_binding_frame(SG_OBJ(symbol), envs);
  return SG_OBJ(id);
}

SgObject Sg_CopyIdentifier(SgIdentifier *id)
{
  return Sg_MakeIdentifier(id->name, id->envs, id->library);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
