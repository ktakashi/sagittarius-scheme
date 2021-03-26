/* strings.h                                     -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_STRINGS_H_
#define SAGITTARIUS_STRINGS_H_

#include "sagittarius/platform.h"

SG_CDECL_BEGIN

/**
   Make a string object from ASCII string

   @param s C char of ascii
   @return A string object
 */
SG_EXTERN SgObject Sg_AsciiToString(const char *s, size_t len);
/**
   Make a string object from UTF-8 array

   @param s C char of UTF-8
   @return A string object
 */
SG_EXTERN SgObject Sg_Utf8ToString(const char *s, size_t len);
/**
   Check if given object is a string.
   @param obj an object
   @return 1 obj is a string, 0 obj is not a string
 */
SG_EXTERN int      Sg_IsString(SgObject obj);
/**
   Returns the length of given string
   @param s A string object
   @return Length of the string object
 */
SG_EXTERN long     Sg_StringLength(SgObject s);
/**
   Returns the element of index k of the given string s.
   @param s A string object
   @param k Index
   @return A UCS32 charactor 
 */
SG_EXTERN SgChar   Sg_StringRef(SgObject s, long k);
/**
   Sets the character c into the string of index k
   @param s A string object
   @param k Index
   @param c A UCS32 character
 */
SG_EXTERN void     Sg_StringSet(SgObject s, long k, SgChar c);

SG_CDECL_END
#endif	/* SAGITTARIUS_STRINGS_H_ */
