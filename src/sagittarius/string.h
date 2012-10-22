/* -*- C -*- */
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
#include "clos.h"

SG_CLASS_DECL(Sg_StringClass);
#define SG_CLASS_STRING (&Sg_StringClass)

struct SgStringRec
{
  SG_HEADER;
  unsigned int literalp: 1;
  int size             : (SIZEOF_INT*CHAR_BIT-1);
  SgChar value[1];
};

typedef enum {
  SG_LITERAL_STRING,
  SG_HEAP_STRING,
} SgStringType;

typedef enum {
  SG_STRING_SCAN_INDEX,		/* return index */
  SG_STRING_SCAN_BEFORE,
  SG_STRING_SCAN_AFTER,
  SG_STRING_SCAN_BEFORE2,
  SG_STRING_SCAN_AFTER2,
  SG_STRING_SCAN_BOTH
} SgStringScanType;

#define READ_STRING_MAX_SIZE  2048
#define SG_STRINGP(obj)       SG_XTYPEP(obj, SG_CLASS_STRING)
#define SG_STRING(obj)         	((SgString*)(obj))
#define SG_LITERAL_STRINGP(obj) (SG_STRINGP(obj) && SG_STRING(obj)->literalp)

#define SG_STRING_SIZE(obj)     (SG_STRING(obj)->size)
#define SG_STRING_VALUE(obj)    (SG_STRING(obj)->value)
#define SG_STRING_VALUE_AT(obj, index)    (SG_STRING(obj)->value[index])

#define SG_MAKE_STRING(str) SG_STRING(Sg_MakeString(UC(str), SG_LITERAL_STRING))

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeStringC(const char *value);
SG_EXTERN SgObject Sg_MakeString(const SgChar *value, SgStringType flag);
SG_EXTERN SgObject Sg_MakeStringEx(const SgChar *value, SgStringType flag,
				   int length);

SG_EXTERN SgObject Sg_ReserveString(int size, SgChar fill);
/* this is for get-string-n related not for c use */
SG_EXTERN SgObject Sg_MakeEmptyString();

SG_EXTERN SgObject Sg_StringToList(SgString *s, int start, int end);
SG_EXTERN SgObject Sg_ListToString(SgObject obj, int start, int end);

/* compare */
SG_EXTERN int 	   Sg_StringEqual(SgString *s1, SgString *s2);
SG_EXTERN int 	   Sg_StringCompare(SgString *s1, SgString *s2);

/* concat */
SG_EXTERN SgObject Sg_StringAppend2(SgString *a, SgString *b);
SG_EXTERN SgObject Sg_StringAppendC(SgString *a, const SgChar *s, int size);
SG_EXTERN SgObject Sg_StringAppend(SgObject args);
SG_EXTERN SgObject Sg_CopyString(SgString *a);

SG_EXTERN SgChar   Sg_StringRef(SgString *s, int k);
/* search */
SG_EXTERN SgObject Sg_StringScan(SgString *s1, SgString *s2, int retmode);
SG_EXTERN SgObject Sg_StringScanChar(SgString *s1, SgChar ch, int retmode);
/* split */
SG_EXTERN SgObject Sg_StringSplitChar(SgString *s1, SgChar ch);

/* modify */
SG_EXTERN SgObject Sg_Substring(SgString *x, int start, int end);
SG_EXTERN void     Sg_StringSet(SgString *s, int k, SgChar c);
SG_EXTERN void     Sg_StringFill(SgString *s, SgChar c, int start, int end);
/* for srfi-13 */
SG_EXTERN SgObject Sg_MaybeSubstring(SgString *s, int start, int end);

SG_CDECL_END

#endif /* STRING_SAGITTARIUS_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
