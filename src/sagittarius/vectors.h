/* vectorss.h                                      -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
 */
#ifndef SAGITTARIUS_VECTORS_H_
#define SAGITTARIUS_VECTORS_H_

#include "sagittarius/platform.h"

SG_CDECL_BEGIN

/**
   Make a vector.

   @param size the size of the vector
   @param fill filling of the vector
 */
SG_EXTERN SgObject Sg_MakeVector(long size, SgObject fill);
/**
   Check if the object is a vector or not.

   @param obj an object to check
   @return 1 object is a vector, 0 object is not a vector
 */
SG_EXTERN int Sg_IsVector(SgObject obj);
/**
   Get the length of a vector.

   @param vec a vector object
   @return the length of the vector
 */
SG_EXTERN long Sg_VectorLength(SgObject vec);

/**
   Get element from a vector.

   @param vec vector object
   @param i   index
   @param fallback 
 */
SG_EXTERN SgObject Sg_VectorRef(SgObject vec, long i, SgObject fallback);
/**
   Set an object into the specified location of a vector.

   @param vec vector object
   @param i index
   @param obj object to set
 */
SG_EXTERN SgObject Sg_VectorSet(SgObject vec, long i, SgObject obj);
/**
   Fill a vector with given object.

   @param vec vector object
   @param fill object to fill
   @param start start position (inclusive)
   @param end   end position (exclusive)
 */
SG_EXTERN SgObject Sg_VectorFill(SgObject vec, SgObject fill,
				 long start, long end);
/**
   Convert a list to a vector.

   @param l a list object
   @param start start position (inclusive)
   @param end end position (exclusive)
 */
SG_EXTERN SgObject Sg_ListToVector(SgObject l,  long start, long end);
/**
   Convert a vector to a list.

   @param v a vector object
   @param start start position (inclusive)
   @param end end param (exclusive)
 */
SG_EXTERN SgObject Sg_VectorToList(SgObject v, long start, long end);
/**
   Copy a vector with size of (end - start).

   @param vec a vector object
   @param start start position (inclusive)
   @param end end param (exclusive)
   @param fill filler if (end - start) is bigger the size of the vector
 */
SG_EXTERN SgObject Sg_VectorCopy(SgObject vec, long start, long end,
				 SgObject fill);
/**
   Concatenate a list of vectors to one vector.

   @param vecList a list of vectors
 */
SG_EXTERN SgObject Sg_VectorConcatenate(SgObject vecList);
/**
   Make a copy of a vector with reversed elements

   @param vec a vector object
   @param start start position (inclusive)
   @param end end position
   @return a reversed vector.
 */
SG_EXTERN SgObject Sg_VectorReverse(SgObject vec, long start, long end);

SG_CDECL_END
#endif	/* SAGITTARIUS_VECTORS_H_ */
