/* -*- C -*- */
/*
 * keyword.h
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
#ifndef SAGITTARIUS_KEYWORD_H_
#define SAGITTARIUS_KEYWORD_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_KeywordClass);
#define SG_CLASS_KEYWORD (&Sg_KeywordClass)

struct SgKeywordRec
{
  SG_HEADER;
  SgString *name;
};

#define SG_KEYWORD(obj)      ((SgKeyword*)(obj))
#define SG_KEYWORDP(obj)     (SG_HPTRP(obj) && SG_XTYPEP(obj, SG_CLASS_KEYWORD))
#define SG_KEYWORD_NAME(obj) (SG_KEYWORD(obj)->name)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeKeyword(SgString *name);
SG_EXTERN SgObject Sg_GetKeyword(SgObject key, SgObject list,
				 SgObject fallback);

SG_CDECL_END

#endif /* SAGITTARIUS_KEYWORD_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
