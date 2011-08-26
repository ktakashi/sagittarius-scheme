/* -*- C -*- */
/*
 * identifier.h
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
#ifndef SAGITTARIUS_IDENTIFIER_H_
#define SAGITTARIUS_IDENTIFIER_H_

#include "sagittariusdefs.h"

struct SgIdentifierRec
{
  SG_HEADER;
  SgSymbol  *name;
  SgObject   envs;
  SgLibrary *library;
  SgObject   renamed;
  SgObject   transformersEnv;
};

#define SG_IDENTIFIER(obj)   	    ((SgIdentifier*)(obj))
#define SG_IDENTIFIERP(obj)  	    (SG_PTRP(obj) && IS_TYPE(obj, TC_IDENTIFIER))
#define SG_IDENTIFIER_NAME(obj)     (SG_IDENTIFIER(obj)->name)
#define SG_IDENTIFIER_ENVS(obj)     (SG_IDENTIFIER(obj)->envs)
#define SG_IDENTIFIER_LIBRARY(obj)  (SG_IDENTIFIER(obj)->library)
#define SG_IDENTIFIER_RENAMED(obj)  (SG_IDENTIFIER(obj)->renamed)
#define SG_IDENTIFIER_TRANSFORMERS_ENV(obj)  (SG_IDENTIFIER(obj)->transformersEnv)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeIdentifier(SgSymbol *symbol, SgObject envs, SgLibrary *library);
SG_EXTERN SgObject Sg_CopyIdentifier(SgIdentifier *id);
SG_EXTERN SgObject Sg_WrapSyntax(SgObject form, SgVector *p1env, SgObject seen, int lexicalP);
SG_EXTERN int      Sg_IdentifierEqP(SgObject use_env, SgObject x, SgObject mac_env, SgObject y);

SG_CDECL_END

#endif /* SAGITTARIUS_IDENTIFIER_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
