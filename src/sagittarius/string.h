// -*- C -*-
/*
 * string.h
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
#ifndef STRING_SAGITTARIUS_H_
#define STRING_SAGITTARIUS_H_

#include "sagittariusdefs.h"

/*
  String class must contain UCS4 char array as its body.

  string header info
  ........ ........ .....L.. ....0111 : L: literal
 */
struct SgStringRec
{
  SG_HEADER;
  int     size;
  SgChar *value;
};

typedef enum {
  SG_LITERAL_STRING,
  SG_HEAP_STRING,
} SgStringType;

#define STRING_LITERAL_SHIFT   	11
#define STRING_LITERAL_BIT     ((uintptr_t)1 << STRING_LITERAL_SHIFT)
#define READ_STRING_MAX_SIZE   	2048
#define SG_STRINGP(obj)        	(SG_PTRP(obj) && IS_TYPE(obj, TC_STRING))
#define SG_STRING(obj)         	((SgString*)(obj))
#define SG_LITERAL_STRINGP(obj) (SG_STRINGP(obj) && (SG_HDR(obj) & STRING_LITERAL_BIT))

#define SG_STRING_SIZE(obj)     (SG_STRING(obj)->size)
#define SG_STRING_VALUE(obj)    (SG_STRING(obj)->value)
#define SG_STRING_VALUE_AT(obj, index)    (SG_STRING(obj)->value[index])

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeStringC(const char *value);
SG_EXTERN SgObject Sg_MakeString(const SgChar *value, SgStringType flag);
SG_EXTERN SgObject Sg_ReserveString(size_t size);

SG_EXTERN SgObject Sg_ListToString(SgObject obj);

/* compare */
SG_EXTERN int Sg_StringEqual(SgString *s1, SgString *s2);

/* concat */
SG_EXTERN SgObject Sg_StringAppend2(SgString *a, SgString *b);
SG_EXTERN SgObject Sg_StringAppendC(SgString *a, const SgChar *s, int size);
SG_EXTERN SgObject Sg_StringAppend(SgObject args);
SG_EXTERN SgObject Sg_CopyString(SgString *a);

/* modify */
SG_EXTERN SgObject Sg_Substring(SgString *x, int start, int end);

SG_CDECL_END

#endif /* STRING_SAGITTARIUS_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
