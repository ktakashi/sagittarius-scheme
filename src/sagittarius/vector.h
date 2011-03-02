// -*- C -*-
/*
 * vector.h
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
#ifndef SAGITTARIUS_VECTOR_H_
#define SAGITTARIUS_VECTOR_H_

#include "sagittariusdefs.h"

struct SgVectorRec
{
  SG_HEADER;
  int size;
  SgObject elements[1];
};

#define SG_VECTOR(obj)        	  ((SgVector*)obj)
#define SG_VECTORP(obj)       	  (SG_PTRP(obj) && IS_TYPE(obj, TC_VECTOR))
#define SG_VECTOR_SIZE(obj)   	  (SG_VECTOR(obj)->size)
#define SG_VECTOR_ELEMENTS(obj)   (SG_VECTOR(obj)->elements)
#define SG_VECTOR_ELEMENT(obj, i) (SG_VECTOR(obj)->elements[i])

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeVector(int size, SgObject fill);
SG_EXTERN SgObject Sg_VectorRef(SgVector *vec, int i, SgObject fallback);
SG_EXTERN SgObject Sg_VectorSet(SgVector *vec, int i, SgObject obj);
SG_EXTERN SgObject Sg_VectorFill(SgVector *vec, SgObject fill, int start, int end);

SG_EXTERN SgObject Sg_ListToVector(SgObject l, int start, int end);
SG_EXTERN SgObject Sg_VectorToList(SgVector *v, int start, int end);
SG_EXTERN SgObject Sg_VectorCopy(SgVector *vec, int start, int end, SgObject fill);

SG_CDECL_END

#endif /* SAGITTARIUS_VECTOR_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
