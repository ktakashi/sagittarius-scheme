/* -*- C -*- */
/*
 * charset.h
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
#ifndef SAGITTARIUS_CHARSET_HPP_
#define SAGITTARIUS_CHARSET_HPP_

/* For SRFI14 */
#include "sagittariusdefs.h"

#define SG_CHAR_SET_SMALL_CHARS 128

struct SgCharSetRec
{
  SG_HEADER;
  /* it's better if we just use 8 array of uint32_t
     but i'm lazy...
   */
  char    small[SG_CHAR_SET_SMALL_CHARS];
  SgTreeMap *large;
};

#define SG_CHAR_SET(obj)   ((SgCharSet *)obj)
#define SG_CHAR_SET_P(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_CHAR_SET))


/* predefined character set API */
enum {
  SG_CHAR_SET_ALNUM,
  SG_CHAR_SET_ALPHA,
  SG_CHAR_SET_BLANK,
  SG_CHAR_SET_CNTRL,
  SG_CHAR_SET_DIGIT,
  SG_CHAR_SET_GRAPH,
  SG_CHAR_SET_LOWER,
  SG_CHAR_SET_UPPER,
  SG_CHAR_SET_SYMBL,
  SG_CHAR_SET_PRINT,
  SG_CHAR_SET_PUNCT,
  SG_CHAR_SET_SPACE,
  SG_CHAR_SET_XDIGIT,
  SG_CHAR_SET_ASCII,
  SG_CHAR_SET_WORD,
  SG_CHAR_SET_NUM_PREDEFINED_SETS
};

SG_CDECL_BEGIN

/* API name were borrowed from Gauche */
SG_EXTERN SgObject Sg_MakeEmptyCharSet();
SG_EXTERN SgObject Sg_CharSetCopy(SgCharSet *src);
SG_EXTERN int      Sg_CharSetEq(SgCharSet *x, SgCharSet *y);
SG_EXTERN int      Sg_CharSetLe(SgCharSet *x, SgCharSet *y);
SG_EXTERN SgObject Sg_CharSetAddRange(SgCharSet *cs,
				      SgChar from, SgChar to);
SG_EXTERN SgObject Sg_CharSetAdd(SgCharSet *dest, SgCharSet *src);
SG_EXTERN SgObject Sg_CharSetComplement(SgCharSet *cs);
SG_EXTERN SgObject Sg_StringToCharSet(SgString *input, int error_p);
SG_EXTERN int      Sg_CharSetContains(SgCharSet *cs, SgChar c);
SG_EXTERN SgObject Sg_GetStandardCharSet(int id);
SG_EXTERN SgObject Sg_CharSetRanges(SgCharSet *cs);

SG_CDECL_END

#endif /* SAGITTARIUS_CHARSET_HPP_ */
