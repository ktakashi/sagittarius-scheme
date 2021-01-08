/* lists.h                                     -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_LISTS_H_
#define SAGITTARIUS_LISTS_H_

#include "sagittarius/platform.h"

SG_CDECL_BEGIN

/**
   Construct a pair.
   @param car the first part
   @param cdr the second part
   @return a pair object
 */
SG_EXTERN SgObject Sg_Cons(SgObject car, SgObject cdr);
/**
   Get the first part of the given pair.
   
   @param obj a pair object
   @return the first part of the pair
 */
SG_EXTERN SgObject Sg_Car(SgObject obj);
/**
   Get the second part of the given pair.

   @param obj a pair object
   @return the second part of the pair
 */
SG_EXTERN SgObject Sg_Cdr(SgObject obj);
/**
   Get the length of the given list.
   If the return value is negative, then the given obj is not a list.

   @param obj
   @return 0>= length of the pair
           -1  dotted list
	   -2  circular list
 */
SG_EXTERN long     Sg_Length(SgObject obj);
/**
   Converts given object array to a list.

   @param array an array of objects
   @param nelts number of array elements
   @return a list
 */
SG_EXTERN SgObject Sg_ArrayToList(SgObject *array, int nelts);
/**
   Concatenates the given list of list into one list.
   This is the same as (apply append '((...) (...) ...)).

   @param args a list of list.
   @return a list
 */
SG_EXTERN SgObject Sg_Append(SgObject args);
/**
   Returns a reversed list.

   @param list a list
   @return a newly create list with reversed elements.
 */
SG_EXTERN SgObject Sg_Reverse(SgObject list);

SG_CDECL_END
#endif	/* SAGITTARIUS_LISTS_H_ */
