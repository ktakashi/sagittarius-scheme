
// -*- C -*-
/*
 * unicode.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/unicode.h"
#include "sagittarius/error.h"
#include "sagittarius/port.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/codec.h"
#include "sagittarius/string.h"

#include "../unicode/lexeme.inc"

int Sg_Ucs4Constituent(SgChar c)
{
  int offset = c / 8;
  int bit = 1 << (c & 7);
  return (constituent[offset] & bit) != 0;
}

int Sg_Ucs4Subsequent(SgChar c)
{
  int offset = c / 8;
  int bit = 1 << (c & 7);
  return (subsequent[offset] & bit) != 0;
}

int Sg_Ucs4WhiteSpace(SgChar c)
{
  if (c == 0x0020) return TRUE;                   //; White_Space # Zs       SPACE
  if (c >= 0x0009 && c <= 0x000d) return TRUE;    //; White_Space # Cc   [5] <control-0009>..<control-000D>
  if (c <= 0x007F) return FALSE;
  if (c >= 0x2000 && c <= 0x200a) return TRUE;    //; White_Space # Zs  [11] EN QUAD..HAIR SPACE
  switch (c) {
  case 0x0085:    //; White_Space # Cc       <control-0085>
  case 0x00A0:    //; White_Space # Zs       NO-BREAK SPACE
  case 0x1680:    //; White_Space # Zs       OGHAM SPACE MARK
  case 0x180E:    //; White_Space # Zs       MONGOLIAN VOWEL SEPARATOR
  case 0x2028:    //; White_Space # Zl       LINE SEPARATOR
  case 0x2029:    //; White_Space # Zp       PARAGRAPH SEPARATOR
  case 0x202F:    //; White_Space # Zs       NARROW NO-BREAK SPACE
  case 0x205F:    //; White_Space # Zs       MEDIUM MATHEMATICAL SPACE
  case 0x3000:    //; White_Space # Zs       IDEOGRAPHIC SPACE
    return TRUE;
  }
  return FALSE;
}

int Sg_Ucs4IntralineWhiteSpace(SgChar c)
{
  if (c == 0x0020) return TRUE;                   //; White_Space # Zs       SPACE
  if (c == 0x0009) return TRUE;                   //; White_Space # Cc   [5] <control-0009>
  if (c <= 0x007F) return FALSE;
  if (c >= 0x2000 && c <= 0x200a) return TRUE;    //; White_Space # Zs  [11] EN QUAD..HAIR SPACE
  switch (c) {
  case 0x00A0:    //; White_Space # Zs       NO-BREAK SPACE
  case 0x1680:    //; White_Space # Zs       OGHAM SPACE MARK
  case 0x180E:    //; White_Space # Zs       MONGOLIAN VOWEL SEPARATOR
  case 0x202F:    //; White_Space # Zs       NARROW NO-BREAK SPACE
  case 0x205F:    //; White_Space # Zs       MEDIUM MATHEMATICAL SPACE
  case 0x3000:    //; White_Space # Zs       IDEOGRAPHIC SPACE
    return TRUE;
  }
  return FALSE;
}

int Sg_ConvertUcs4ToUtf8(SgChar ucs4, uint8_t utf8[4], ErrorHandlingMode mode)
{
  if (ucs4 < 0x80) {
    utf8[0] = ucs4;
    return 1;
  } else if (ucs4 < 0x800) {
    utf8[0] = ((ucs4 >>  6) & 0x1f) | 0xc0;
    utf8[1] = ((ucs4      ) & 0x3f) | 0x80;
    return 2;
  } else if (ucs4 < 0x10000) {
    utf8[0] = ((ucs4 >> 12) & 0x0f) | 0xe0;
    utf8[1] = ((ucs4 >>  6) & 0x3f) | 0x80;
    utf8[2] = ((ucs4      ) & 0x3f) | 0x80;
    return 3;
  } else if (ucs4 < 0x200000) {
    utf8[0] = ((ucs4 >> 18) & 0x07) | 0xf0;
    utf8[1] = ((ucs4 >> 12) & 0x3f) | 0x80;
    utf8[2] = ((ucs4 >>  6) & 0x3f) | 0x80;
    utf8[3] = ((ucs4      ) & 0x3f) | 0x80;
    return 4;
  } else {
    if (mode == SG_RAISE_ERROR) {
      Sg_Error(UC("%s:%d %x\n"),  UC(__FILE__), __LINE__, ucs4);
      return 0;
    } else if (mode == SG_REPLACE_ERROR) {
      utf8[0] = 0xff;
      utf8[1] = 0xfd;
      return 2;
    } else {
      if (mode == SG_IGNORE_ERROR) {
	return 0;
      } else {
	Sg_Error(UC("invalid error handling mode"));
      }
    }
  }
  return 0; /* dummy */
}

int Sg_ConvertUcs4ToUtf16(SgChar ucs4, uint8_t utf8[4], ErrorHandlingMode mode, int littlep)
{
#define put2byte(buf, in)			\
  if (littlep) {				\
    (buf)[0] = (uint8_t)(in);			\
    (buf)[1] = (uint8_t)((in) >> 8);		\
  } else {					\
    (buf)[0] = (uint8_t)((in) >> 8);		\
    (buf)[1] = (uint8_t)(in);			\
  }

  if (ucs4 > 0x10FFFF) {
    if (mode == SG_RAISE_ERROR) {
      Sg_Error(UC("character out of utf16 range %s:%d %x\n"),  UC(__FILE__), __LINE__, ucs4);
      return 0;
    } else if (mode == SG_REPLACE_ERROR) {
      utf8[0] = 0xff;
      utf8[1] = 0xfd;
      return 2;
    } else {
      if (mode == SG_IGNORE_ERROR) {
	return 0;
      } else {
	Sg_Error(UC("invalid error handling mode"));
      }
    }
  }
  if (ucs4 < 0x10000) {
    put2byte(utf8, ucs4);
    return 2;
  } else {
    // http://unicode.org/faq/utf_bom.html#utf16-3
    const uint16_t HI_SURROGATE_START = 0xD800;
    uint16_t X = (uint16_t) ucs4;
    SgChar   U = (ucs4 >> 16) & ((1 << 5) - 1);
    uint16_t W = (uint16_t) U - 1;
    uint16_t HiSurrogate = HI_SURROGATE_START | (W << 6) | X >> 10;
    uint16_t LoSurrogate;
    const uint16_t LO_SURROGATE_START = 0xDC00;
    X = (uint16_t)ucs4;
    LoSurrogate = (uint16_t) (LO_SURROGATE_START | (X & ((1 << 10) - 1)));
    put2byte(utf8 + 0, HiSurrogate);
    put2byte(utf8 + 2, LoSurrogate);
    return 4;
  }
}

#define decodeError(ignore)						\
  if (mode == SG_RAISE_ERROR) {						\
    Sg_Error(UC("Invalid encode %s:%x\n"),  UC(__FILE__), __LINE__);	\
  } else if (mode == SG_REPLACE_ERROR) {				\
    *ucs4 = 0xFFFD;							\
    return 2;								\
  } else {								\
    ASSERT(mode == SG_IGNORE_ERROR);					\
    return (ignore);							\
  }

int Sg_ConvertUtf8ToUcs4(uint8_t utf8[4], SgChar *ucs4, ErrorHandlingMode mode)
{
  SgChar sv;

  if (utf8[0] < 0x80) {
    sv = utf8[0];
    if (sv >= 0x80) { decodeError(-1); }
    *ucs4 = sv;
    return 1;
  } else if (utf8[0] < 0xc2) {
    decodeError(-1);
  } else if (utf8[0] < 0xe0) {
    if ((utf8[1] < 0x80) | (utf8[1] > 0xbf)) { decodeError(-2); }
    sv = ((utf8[0] & 0x1f) << 6) + (utf8[1] & 0x3f);
    if ((sv < 0x80) | (sv > 0x7FF)) { decodeError(-2); }
    *ucs4 = sv;
    return 2;
  } else if (utf8[0] < 0xf0) {
    if ((utf8[1] < 0x80) | (utf8[1] > 0xbf)) { decodeError(-2); }
    if ((utf8[2] < 0x80) | (utf8[2] > 0xbf)) { decodeError(-3); }
    sv = ((utf8[0] & 0x0f) << 12) + ((utf8[1] & 0x3f) << 6) + (utf8[2] & 0x3f);
    if ((sv < 0x800) | (sv > 0xFFFF)) { decodeError(-3); }
    if ((sv >= 0xD800) & (sv <= 0xDFFF)) { decodeError(-3); }
    *ucs4 = sv;
    return 3;
  } else if (utf8[0] < 0xf8) {
    if ((utf8[1] < 0x80) | (utf8[1] > 0xbf)) { decodeError(-2); }
    if ((utf8[2] < 0x80) | (utf8[2] > 0xbf)) { decodeError(-3); }
    if ((utf8[3] < 0x80) | (utf8[3] > 0xbf)) { decodeError(-4); }
    sv = ((utf8[0] & 0x07) << 18) + ((utf8[1] & 0x3f) << 12) + ((utf8[2] & 0x3f) << 6) + (utf8[3] & 0x3f);
    if ((sv < 0x10000) | (sv > 0x10FFFF)) { decodeError(-4); }
    *ucs4 = sv;
    return 4;
  } else {
    decodeError(-1);
  }
  return 0;			/* dummy */
}

int Sg_ConvertUtf16ToUcs4(uint8_t utf8[4], SgChar *ucs4, ErrorHandlingMode mode, int littlep)
{
  uint16_t hi;
  uint16_t lo;
  SgChar   X;
  SgChar   W;
  SgChar   U;
  SgChar   C;
  uint16_t val1, val2;
  int a, b, c, d;

  a = utf8[0];
  b = utf8[1];
  val1 = littlep ? ((b << 8) | a) : ((a << 8) | b);
  if (val1 < 0xD800 || val1 > 0xDFFF) {
    *ucs4 = val1;
    return 2;
  }
  c = utf8[2];
  if (EOF == c) {
    decodeError(-3);
  }
  d = utf8[3];
  if (EOF == d) {
    decodeError(-4);
  }
  val2 = littlep ? ((d << 8) | c) : ((c << 8) | d);
  // http://unicode.org/faq/utf_bom.html#utf16-3
  hi = val1;
  lo = val2;
  X = (hi & ((1 << 6) -1)) << 10 | (lo & ((1 << 10) -1));
  W = (hi >> 6) & ((1 << 5) - 1);
  U = W + 1;
  C = U << 16 | X;
  *ucs4 = c;
  return 4;
}

SgChar Sg_EnsureUcs4(SgChar c)
{
  ASSERT(c >= 0);
  if (c > 0x10ffff) Sg_Error(UC("code point out of range, U+%X"), c);
  if (c >= 0xd800 && c <= 0xdfff) Sg_Error(UC("code point in excluded range, U+%X"), c);
  return c;
}

SgObject Sg_Utf8sToUtf32s(const char *s, int len)
{
  SgPort *p = Sg_MakeByteArrayInputPort((uint8_t *)s, len);
  SgTranscoder *t = Sg_MakeTranscoder(Sg_MakeUtf8Codec(), LF, SG_IGNORE_ERROR);
  SgPort *sp = Sg_MakeStringOutputPort(len * sizeof(SgChar));
  int i;
  for (i = 0; i < len; i++) {
    Sg_Putc(sp, t->getChar(t, p));
  }
  return Sg_GetStringFromStringPort(sp);
}

SgObject Sg_Utf16sToUtf32s(const char *s, int len)
{
  SgPort *p = Sg_MakeByteArrayInputPort((uint8_t *)s, len);
  SgTranscoder *t = Sg_MakeTranscoder(Sg_MakeUtf16Codec(UTF_16CHECK_BOM), LF, SG_IGNORE_ERROR);
  SgPort *sp = Sg_MakeStringOutputPort(len * sizeof(SgChar));
  int i;
  for (i = 0; i < len; i++) {
    Sg_Putc(sp, t->getChar(t, p));
  }
  return Sg_GetStringFromStringPort(sp);
}

char* Sg_Utf32sToUtf8s(SgString *s)
{
  SgPort *p = Sg_MakeByteArrayOutputPort(s->size + sizeof(SgChar));
  SgTranscoder *t = Sg_MakeTranscoder(Sg_MakeUtf8Codec(), LF, SG_IGNORE_ERROR);
  int i, len = s->size;
  SgChar *value = s->value;
  for (i = 0; i < len; i++) {
    t->putChar(t, p, *(value + i));
  }
  t->putChar(t, p, '\0');
  return (char*)Sg_GetByteArrayFromBinaryPort(p);
}

size_t ustrcspn(const SgChar *s1, const char *s2)
{
  register const SgChar *ss1 = s1;
  register int c;
  register const char *ss2;
  for (; (c = *ss1) != '\0'; ss1++) {
    for (ss2 = s2; *ss2 != '\0'; ss2++) {
      if (c == *ss2) {
        goto quit;
      }
    }
  }
quit:
  return ss1 - s1;
}

int ustrcmp(const SgChar *s1, const char *s2)
{
  register const uint32_t *ss1;
  register const unsigned char *ss2;
  for (ss1 = (const uint32_t *)s1,
	 ss2 = (const unsigned char *)s2;
       *ss1 == *ss2 && *ss1 != 0;
       ss1++, ss2++)
    ;
  return *ss1 - *ss2;
}

int ustrncmp(const SgChar *s1,
	     const char *s2, size_t n)
{
  register const uint32_t *ss1;
  register const unsigned char *ss2, *t;
  for (ss1 = (const unsigned int *)s1,
	 ss2 = (const unsigned char *)s2,
	 t = ss1 + n;
       ss1 != t && *ss1 == *ss2 && *ss1 != 0;
       ss1++, ss2++)
    ;
  return *ss1 - *ss2;
}


size_t ustrlen(const SgChar *value)
{
  /* TODO naive? */
  int count = 0;
  for (; *value ;value++, count++);
  return count;
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
