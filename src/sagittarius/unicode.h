/* unicode.h                                       -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
  SG_GENERAL_CATEGORY_Lu,
  SG_GENERAL_CATEGORY_Ll,
  SG_GENERAL_CATEGORY_Lt,
  SG_GENERAL_CATEGORY_Lm,
  SG_GENERAL_CATEGORY_Lo,
  SG_GENERAL_CATEGORY_Mn,
  SG_GENERAL_CATEGORY_Mc,
  SG_GENERAL_CATEGORY_Me,
  SG_GENERAL_CATEGORY_Nd,
  SG_GENERAL_CATEGORY_Nl,
  SG_GENERAL_CATEGORY_No,
  SG_GENERAL_CATEGORY_Ps,
  SG_GENERAL_CATEGORY_Pe,
  SG_GENERAL_CATEGORY_Pi,
  SG_GENERAL_CATEGORY_Pf,
  SG_GENERAL_CATEGORY_Pd,
  SG_GENERAL_CATEGORY_Pc,
  SG_GENERAL_CATEGORY_Po,
  SG_GENERAL_CATEGORY_Sc,
  SG_GENERAL_CATEGORY_Sm,
  SG_GENERAL_CATEGORY_Sk,
  SG_GENERAL_CATEGORY_So,
  SG_GENERAL_CATEGORY_Zs,
  SG_GENERAL_CATEGORY_Zp,
  SG_GENERAL_CATEGORY_Zl,
  SG_GENERAL_CATEGORY_Cc,
  SG_GENERAL_CATEGORY_Cf,
  SG_GENERAL_CATEGORY_Cs,
  SG_GENERAL_CATEGORY_Co,
  SG_GENERAL_CATEGORY_Cn
} SgGeneralCategory;


SG_CDECL_BEGIN

SG_EXTERN int 	 Sg_Ucs4ConstituentP(SgChar c);
SG_EXTERN int 	 Sg_Ucs4SubsequentP(SgChar c);
SG_EXTERN int 	 Sg_Ucs4WhiteSpaceP(SgChar c);
SG_EXTERN int 	 Sg_Ucs4IntralineWhiteSpaceP(SgChar c);
SG_EXTERN int 	 Sg_ConvertUcs4ToUtf8(SgChar c, uint8_t utf8[4],
				      SgErrorHandlingMode mode);
SG_EXTERN int 	 Sg_ConvertUcs4ToUtf16(SgChar c, uint8_t utf8[4],
				       SgErrorHandlingMode mode, int littelp);
SG_EXTERN SgChar Sg_ConvertUtf8ToUcs4(SgPort *port, SgErrorHandlingMode mode);
SG_EXTERN SgChar Sg_ConvertUtf16ToUcs4(SgPort *port, SgErrorHandlingMode mode,
				       SgCodec *codec, int checkBOMNow);
SG_EXTERN SgChar Sg_EnsureUcs4(SgChar c);

SG_EXTERN int64_t Sg_ConvertUtf8BufferToUcs4(SgCodec *codec,
					     uint8_t *u8buf, int64_t u8size,
					     SgChar *buf, int64_t size,
					     SgPort *port,
					     SgErrorHandlingMode mode,
					     int checkBOM);
SG_EXTERN int64_t Sg_ConvertUtf16BufferToUcs4(SgCodec *codec,
					      uint8_t *u8buf, int64_t u8size,
					      SgChar *buf, int64_t size,
					      SgPort *port,
					      SgErrorHandlingMode mode,
					      int checkBOM);

/* string convertion */
SG_EXTERN SgObject Sg_Utf8sToUtf32s(const char *s, int len);
SG_EXTERN SgObject Sg_Utf16sToUtf32s(const char *s, int len);
SG_EXTERN char*    Sg_Utf32sToUtf8s(const SgString *s);

/* might be convenient? */
SG_EXTERN wchar_t* Sg_StringToWCharTs(SgObject s);
SG_EXTERN SgObject Sg_WCharTsToString(wchar_t *s, int len);

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
SG_EXTERN SgObject Sg_StringTitleCase(SgString *str, int useSpecialCasing);
SG_EXTERN SgObject Sg_StringFoldCase(SgString *str);

SG_EXTERN SgObject Sg_StringNormalizeNfd(SgString *str);
SG_EXTERN SgObject Sg_StringNormalizeNfkd(SgString *str);
SG_EXTERN SgObject Sg_StringNormalizeNfc(SgString *str);
SG_EXTERN SgObject Sg_StringNormalizeNfkc(SgString *str);

SG_EXTERN SgGeneralCategory Sg_CharGeneralCategory(SgChar ch);
SG_EXTERN SgObject Sg_CategroyToSymbol(SgGeneralCategory cate);

SG_EXTERN SgObject Sg_DigitValue(SgChar ch);

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
