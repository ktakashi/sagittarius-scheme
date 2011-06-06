// -*- C -*-
/*
 * unicode.h
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
#ifndef SAGITTARIUS_UNICODE_H_
#define SAGITTARIUS_UNICODE_H_

#include "sagittariusdefs.h"

typedef enum {
  Lu,
  Ll,
  Lt,
  Lm,
  Lo,
  Mn,
  Mc,
  Me,
  Nd,
  Nl,
  No,
  Ps,
  Pe,
  Pi,
  Pf,
  Pd,
  Pc,
  Po,
  Sc,
  Sm,
  Sk,
  So,
  Zs,
  Zp,
  Zl,
  Cc,
  Cf,
  Cs,
  Co,
  Cn
} GeneralCategory;


SG_CDECL_BEGIN

SG_EXTERN int 	 Sg_Ucs4ConstituentP(SgChar c);
SG_EXTERN int 	 Sg_Ucs4SubsequentP(SgChar c);
SG_EXTERN int 	 Sg_Ucs4WhiteSpaceP(SgChar c);
SG_EXTERN int 	 Sg_Ucs4IntralineWhiteSpaceP(SgChar c);
SG_EXTERN int 	 Sg_ConvertUcs4ToUtf8(SgChar c, uint8_t utf8[4], ErrorHandlingMode mode);
SG_EXTERN int 	 Sg_ConvertUcs4ToUtf16(SgChar c, uint8_t utf8[4], ErrorHandlingMode mode, int littelp);
SG_EXTERN SgChar Sg_ConvertUtf8ToUcs4(SgPort *port, ErrorHandlingMode mode);
SG_EXTERN SgChar Sg_ConvertUtf16ToUcs4(SgPort *port, ErrorHandlingMode mode, SgCodec *codec, int checkBOMNow);
SG_EXTERN SgChar Sg_EnsureUcs4(SgChar c);

/* string convertion */
SG_EXTERN SgObject Sg_Utf8sToUtf32s(const char *s, int len);
SG_EXTERN SgObject Sg_Utf16sToUtf32s(const char *s, int len);
SG_EXTERN char*    Sg_Utf32sToUtf8s(const SgString *s);

/* char case */
SG_EXTERN SgChar   Sg_CharUpCase(SgChar ch);
SG_EXTERN SgChar   Sg_CharDownCase(SgChar ch);
SG_EXTERN SgChar   Sg_CharTitleCase(SgChar ch);
SG_EXTERN SgChar   Sg_CharFoldCase(SgChar ch);

/* char condition */
SG_EXTERN int      Sg_CharAlphabeticP(SgChar ch);
SG_EXTERN int      Sg_CharNumericP(SgChar ch);
SG_EXTERN int      Sg_CharUpperCaseP(SgChar ch);
SG_EXTERN int      Sg_CharLowerCaseP(SgChar ch);
SG_EXTERN int      Sg_CharTitleCaseP(SgChar ch);

/* string */
SG_EXTERN SgObject Sg_StringUpCase(SgString *str);
SG_EXTERN SgObject Sg_StringDownCase(SgString *str);
SG_EXTERN SgObject Sg_StringTitleCase(SgString *str);
SG_EXTERN SgObject Sg_StringFoldCase(SgString *str);

SG_EXTERN SgObject Sg_StringNormalizeNfd(SgString *str);
SG_EXTERN SgObject Sg_StringNormalizeNfkd(SgString *str);
SG_EXTERN SgObject Sg_StringNormalizeNfc(SgString *str);
SG_EXTERN SgObject Sg_StringNormalizeNfkc(SgString *str);

SG_EXTERN GeneralCategory Sg_CharGeneralCategory(SgChar ch);
SG_EXTERN SgObject Sg_CategroyToSymbol(GeneralCategory cate);

/* These are not a part of method for sagittarius scheme object. */
SG_EXTERN size_t ustrcspn(const SgChar *s1, const char *s2);
SG_EXTERN int    ustrcmp(const SgChar *s1, const char *s2);
SG_EXTERN int    ustrncmp(const SgChar *s1, const char *s2, size_t size);
SG_EXTERN size_t ustrlen(const SgChar *s1);

SG_CDECL_END

#endif /* SAGITTARIUS_UNICODE_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
