/* -*- C -*- */
/*
 * gloc.h
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
#ifndef SAGITTARIUS_GLOC_H_
#define SAGITTARIUS_GLOC_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_GlocClass);
#define SG_CLASS_GLOC (&Sg_GlocClass)

struct SgGlocRec
{
  SG_HEADER;
  SgSymbol   *name;
  SgLibrary  *library;
  SgObject    value;
  unsigned int constant : 1;	/* TRUE if this is constant value */
};

#define SG_GLOC(obj)  ((SgGloc*)obj)
#define SG_GLOCP(obj) SG_XTYPEP(obj, SG_CLASS_GLOC)

#define SG_GLOC_GET(gloc)      ((gloc)->value)
#define SG_GLOC_SET(gloc, val) ((gloc)->value = (val))

SG_CDECL_BEGIN

SG_EXTERN int      Sg_GlocConstP(SgGloc *g);

SG_EXTERN SgObject Sg_MakeGloc(SgSymbol *name, SgLibrary *library);

SG_CDECL_END

#endif /* SAGITTARIUS_GLOC_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/

