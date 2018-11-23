/* unicode.c                                       -*- mode:c; coding:utf-8; -*-
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
#include <wchar.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/unicode.h"
#include "sagittarius/error.h"
#include "sagittarius/port.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/codec.h"
#include "sagittarius/number.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/writer.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/hashtable.h"

#include "shortnames.incl"

#include "../unicode/lexeme.inc"

int Sg_Ucs4ConstituentP(SgChar c)
{
  int offset = c / 8;
  int bit = 1 << (c & 7);
  return (constituent[offset] & bit) != 0;
}

int Sg_Ucs4SubsequentP(SgChar c)
{
  int offset = c / 8;
  int bit = 1 << (c & 7);
  return (subsequent[offset] & bit) != 0;
}

/* !!!fuck!!! 
   seems starting Unicode 6.3.0, U+180E is Cf. Who the hell decided that!
*/
int Sg_Ucs4WhiteSpaceP(SgChar c)
{
  /*; White_Space # Zs       SPACE */
  if (c == 0x0020) return TRUE;
  /*; White_Space # Cc   [5] <control-0009>..<control-000D> */
  if (c >= 0x0009 && c <= 0x000d) return TRUE;
  if (c <= 0x007F) return FALSE;
  /*; White_Space # Zs  [11] EN QUAD..HAIR SPACE */
  if (c >= 0x2000 && c <= 0x200a) return TRUE;
  switch (c) {
  case 0x0085:    /*; White_Space # Cc       <control-0085> */
  case 0x00A0:    /*; White_Space # Zs       NO-BREAK SPACE */
  case 0x1680:    /*; White_Space # Zs       OGHAM SPACE MARK */
  /* starting Unicode 6.3.0 this is Cf... */
  /* case 0x180E: */    /*; White_Space # Zs       MONGOLIAN VOWEL SEPARATOR */
  case 0x2028:    /*; White_Space # Zl       LINE SEPARATOR */
  case 0x2029:    /*; White_Space # Zp       PARAGRAPH SEPARATOR */
  case 0x202F:    /*; White_Space # Zs       NARROW NO-BREAK SPACE */
  case 0x205F:    /*; White_Space # Zs       MEDIUM MATHEMATICAL SPACE */
  case 0x3000:    /*; White_Space # Zs       IDEOGRAPHIC SPACE */
    return TRUE;
  }
  return FALSE;
}

int Sg_Ucs4IntralineWhiteSpaceP(SgChar c)
{
  /*; White_Space # Zs       SPACE */
  if (c == 0x0020) return TRUE;
  /*; White_Space # Cc   [5] <control-0009> */
  if (c == 0x0009) return TRUE;
  if (c <= 0x007F) return FALSE;
  /*; White_Space # Zs  [11] EN QUAD..HAIR SPACE */
  if (c >= 0x2000 && c <= 0x200a) return TRUE;
  switch (c) {
  case 0x00A0:    /*; White_Space # Zs       NO-BREAK SPACE */
  case 0x1680:    /*; White_Space # Zs       OGHAM SPACE MARK */
  /* ditto */
  /* case 0x180E: */    /*; White_Space # Zs       MONGOLIAN VOWEL SEPARATOR */
  case 0x202F:    /*; White_Space # Zs       NARROW NO-BREAK SPACE */
  case 0x205F:    /*; White_Space # Zs       MEDIUM MATHEMATICAL SPACE */
  case 0x3000:    /*; White_Space # Zs       IDEOGRAPHIC SPACE */
    return TRUE;
  }
  return FALSE;
}

int Sg_ConvertUcs4ToUtf8(SgChar ucs4, uint8_t utf8[4], SgErrorHandlingMode mode)
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
      Sg_IOError(SG_IO_ENCODE_ERROR, SG_INTERN("convert-ucs4-to-utf8"),
		 Sg_Sprintf(UC("character out of utf8 range %s:%d %x"),
			    UC(__FILE__), __LINE__, ucs4),
		 SG_UNDEF, SG_UNDEF);
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

int Sg_ConvertUcs4ToUtf16(SgChar ucs4, uint8_t utf8[4],
			  SgErrorHandlingMode mode,
			  int littlep)
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
      Sg_IOError(SG_IO_ENCODE_ERROR, SG_INTERN("convert-ucs4-to-utf16"),
		 Sg_Sprintf(UC("character out of utf16 range %s:%d %x"),
			    UC(__FILE__), __LINE__, ucs4),
		 SG_UNDEF, SG_UNDEF);
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
    /* http://unicode.org/faq/utf_bom.html#utf16-3 */
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

#define decodeError(_r, who)						\
  if (mode == SG_RAISE_ERROR) {						\
    Sg_IOError(SG_IO_DECODE_ERROR, who,					\
	       Sg_Sprintf(UC("Invalid encode %s:%x\n"),			\
			  UC(__FILE__), __LINE__),			\
	       SG_UNDEF, port);						\
  } else if (mode == SG_REPLACE_ERROR) {				\
    (_r) = 0xFFFD;							\
    goto end;								\
  } else {								\
    ASSERT(mode == SG_IGNORE_ERROR);					\
    goto retry;								\
  }

static inline int isUtf8Tail(uint8_t b)
{
    return (0x80 <= b && b <= 0xbf);
}

#define utf8_reader(_r, u8reader, codec, port, mode, checkBOM, data)	\
  do {									\
    int f;								\
    uint8_t first;							\
  retry:								\
    f = u8reader(data, TRUE);						\
    if (f == EOF) {							\
      (_r) = EOF;							\
      goto end;								\
    }									\
    first = (uint8_t)(f & 0xff);					\
    /* UTF8-1(ascii) = %x00-7F */					\
    if (first < 0x80) {							\
      (_r) = first;							\
      goto end;								\
      /* UTF8-2 = %xC2-DF UTF8-tail */					\
    } else if (0xc2 <= first && first <= 0xdf) {			\
      uint8_t second = u8reader(data, TRUE);				\
      if (isUtf8Tail(second)) {						\
	(_r) = ((first & 0x1f) << 6) | (second & 0x3f);			\
	goto end;							\
      } else {								\
	u8reader(data, FALSE);						\
	decodeError(_r, SG_INTERN("convert-utf8-to-ucs4"));		\
      }									\
      /* UTF8-3 = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /  */	\
      /*          %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )    */	\
    } else if (0xe0 <= first && first <= 0xef) {			\
      int second = u8reader(data, TRUE);				\
      int third =  u8reader(data, TRUE);				\
      if (!isUtf8Tail(third)) {						\
	goto err3;							\
      } else if ((0xe0 == first && 0xa0 <= second && second <= 0xbf)    || \
		 (0xed == first && 0x80 <= second && second <= 0x9f)    || \
		 (0xe1 <= first && first <= 0xec && isUtf8Tail(second)) || \
		 ((0xee == first || 0xef == first) && isUtf8Tail(second))) { \
	SgChar c = (((first & 0xf) << 12) | ((second & 0x3f) << 6)	\
		    | (third & 0x3f));					\
	(_r) = c;							\
	goto end;							\
      }									\
    err3:								\
      if (second != EOF) u8reader(data, FALSE);				\
      if (third != EOF)  u8reader(data, FALSE);				\
      decodeError(_r, SG_INTERN("convert-utf8-to-ucs4"));		\
      /* UTF8-4 = %xF0 %x90-BF 2( UTF8-tail ) */			\
      /*        / %xF1-F3 3( UTF8-tail )      */			\
      /*        / %xF4 %x80-8F 2( UTF8-tail ) */			\
    } else if (0xf0 <= first && first <= 0xf4) {			\
      int second = u8reader(data, TRUE);				\
      int third =  u8reader(data, TRUE);				\
      int fourth = u8reader(data, TRUE);				\
      if (!isUtf8Tail(third) || !isUtf8Tail(fourth)) {			\
	goto err4;							\
      } else if ((0xf0 == first && 0x90 <= second && second <= 0xbf)     || \
		 (0xf4 == first && 0x80 <= second && second <= 0x8f)     || \
		 (0xf1 <= first && first <= 0xf3 && isUtf8Tail(second))) { \
	(_r) = (((first & 0x7) << 18) | ((second & 0x3f) << 12) |	\
		((third & 0x3f) << 6) | (fourth & 0x3f));		\
	goto end;							\
      }									\
    err4:								\
      if (second != EOF) u8reader(data, FALSE);				\
      if (third != EOF)  u8reader(data, FALSE);				\
      if (fourth != EOF) u8reader(data, FALSE);				\
      decodeError(_r, SG_INTERN("convert-utf8-to-ucs4"));		\
    } else {								\
      decodeError(_r, SG_INTERN("convert-utf8-to-ucs4"));		\
    }									\
    (_r) = ' ';								\
  end:;									\
  } while(0)

static inline int port_u8_reader(void *data, int getP)
{
  if (getP) {
    return Sg_GetbUnsafe(SG_PORT(data));
  } else {
    if (Sg_HasSetPortPosition(SG_PORT(data))) {
      Sg_SetPortPosition(SG_PORT(data), -1, SG_CURRENT);
    }
    return -1;
  }
}

SgChar Sg_ConvertUtf8ToUcs4(SgPort *port, SgErrorHandlingMode mode)
{
  SgChar r;
  utf8_reader(r, port_u8_reader, NULL, port, mode, FALSE, (void*)port);
  return r;
}

/* we only support proper replacement on the ports which have
   set-port-position! */
typedef struct
{
  int64_t  pos;
  uint8_t *buf;
  int64_t  buf_size;
  SgPort  *port;
} u8_reader_ctx;
static inline int buffer_u8_reader(void *data, int getP)
{
  u8_reader_ctx *ctx = (u8_reader_ctx *)data;
  if (ctx->pos < ctx->buf_size) {
    if (getP) {
      return ctx->buf[ctx->pos++];
    }
    ctx->pos--;
    return -1;
  } else {
    if (getP) {
      if (ctx->port) {
	return Sg_GetbUnsafe(ctx->port);
      } else
	return EOF;
    } else {
      /* error case. revert back to previous position. 
	 it might be slow but it's error recovery, so
	 we don't care that much.
	 TODO: custom port doesn't accept SG_CURENT...
       */      
      if (ctx->pos == ctx->buf_size && ctx->port &&
	  Sg_HasSetPortPosition(ctx->port)) {
	Sg_SetPortPosition(ctx->port, -1, SG_CURRENT);
      } else {
	ctx->pos--;
      }
      return -1;
    }
  }
}

#define DEFINE_BUFFER_CONVERTOR(cname, char_reader)			\
  int64_t cname(SgCodec *codec, uint8_t *u8buf, int64_t u8size,		\
		SgChar *buf, int64_t size, SgPort *port,		\
		SgErrorHandlingMode mode, int checkBOM)			\
  {									\
    int64_t i;								\
    u8_reader_ctx ctx;							\
    ctx.pos = 0;							\
    ctx.buf = u8buf;							\
    ctx.buf_size = u8size;						\
    ctx.port = port;							\
    for (i = 0; i < size; i++) {					\
      SgChar r;								\
      char_reader(r, buffer_u8_reader, codec, port, mode, checkBOM, &ctx); \
      if (r == EOF) return i;						\
      buf[i] = r;							\
    }									\
    return i;								\
  }

DEFINE_BUFFER_CONVERTOR(Sg_ConvertUtf8BufferToUcs4, utf8_reader);

#define utf16_reader(_r, u8reader, codec, port, mode, checkBOMNow, data) \
  do {									\
    SgChar   C;								\
    uint16_t val1, val2;						\
    int a, b, c, d;							\
  retry:								\
    a = u8reader(data, TRUE);						\
    b = u8reader(data, TRUE);						\
    if (a == EOF) {							\
      (_r) = EOF;							\
      goto end;								\
    }									\
    if (b == EOF) {							\
      decodeError(_r, SG_INTERN("convert-utf16-to-ucs4"));		\
    }									\
    if (checkBOMNow && SG_CODEC_ENDIAN(codec) == UTF_16CHECK_BOM) {	\
      if (a == 0xFE && b == 0xFF) {					\
	SG_CODEC_BUILTIN(codec)->littlep = FALSE;			\
	checkBOMNow = FALSE;						\
	goto retry;							\
      } else if (a == 0xFF && b == 0xFE) {				\
	SG_CODEC_BUILTIN(codec)->littlep = TRUE;			\
	checkBOMNow = FALSE;						\
	goto retry;							\
      } else {								\
	SG_CODEC_BUILTIN(codec)->littlep = FALSE;			\
      }									\
    }									\
    val1 = SG_CODEC_BUILTIN(codec)->littlep?((b << 8)|a):((a<<8)|b);	\
    if (val1 < 0xD800 || 0xDFFF < val1) {				\
      (_r) = val1;							\
      goto end;								\
    }									\
    c = u8reader(data, TRUE);						\
    if (EOF == c) {							\
      decodeError(_r, SG_INTERN("convert-utf16-to-ucs4"));		\
    }									\
    d = u8reader(data, TRUE);						\
    if (EOF == d) {							\
      decodeError(_r, SG_INTERN("convert-utf16-to-ucs4"));		\
    }									\
    val2 = SG_CODEC_BUILTIN(codec)->littlep?((d<<8)|c):((c<<8)|d);	\
    /* http://unicode.org/faq/utf_bom.html#utf16-4 */			\
    C = ((val1-0xD800) << 10) + (val2-0xDC00) + 0x010000;		\
    if ((0x10FFFF < C) || (0xD800 <= C && C <= 0xDFFF)) {		\
      decodeError(_r, SG_INTERN("convert-utf16-to-ucs4"));		\
    }									\
    (_r) = C;								\
  end:;									\
  } while (0)

SgChar Sg_ConvertUtf16ToUcs4(SgPort *port, SgErrorHandlingMode mode,
			     SgCodec *codec, int checkBOMNow)
{
  SgChar r;
  utf16_reader(r, port_u8_reader, codec, port, mode, checkBOMNow, port);
  return r;
}

DEFINE_BUFFER_CONVERTOR(Sg_ConvertUtf16BufferToUcs4, utf16_reader);

SgChar Sg_EnsureUcs4(SgChar c)
{
  ASSERT(c >= 0);
  if (c > 0x10ffff)
    Sg_Error(UC("code point out of range, U+%X"), c);
  if (c >= 0xd800 && c <= 0xdfff)
    Sg_Error(UC("code point in excluded range, U+%X"), c);
  return c;
}

SgObject Sg_Utf8sToUtf32s(const char *s, int len)
{
  /* we know utf8 length will be less than ucs4 */
  SgObject ss = Sg_ReserveString(len, 0);
  int64_t r = Sg_ConvertUtf8BufferToUcs4(Sg_MakeUtf8Codec(), (uint8_t*)s, len,
					 SG_STRING_VALUE(ss), len, NULL,
					 SG_IGNORE_ERROR, FALSE);
  SG_STRING_SIZE(ss) = (int)r;
  return ss;
}

SgObject Sg_Utf16sToUtf32s(const char *s, int len)
{
  /* we know utf16->ucs32 less than len. (well actuall len/2 is enough) */
  SgObject ss = Sg_ReserveString(len/2, 0);
  int64_t r = Sg_ConvertUtf16BufferToUcs4(Sg_MakeUtf16Codec(UTF_16CHECK_BOM),
					  (uint8_t*)s, len,
					  SG_STRING_VALUE(ss), len/2, NULL,
					  SG_IGNORE_ERROR, FALSE);
  SG_STRING_SIZE(ss) = (int)r;
  return ss;
}

char* Sg_Utf32sToUtf8s(const SgString *s)
{
  char *r;
  int count = 0, i;

  /* calculate returning size first */
  for (i = 0; i < SG_STRING_SIZE(s); i++) {
    SgChar ucs4 = SG_STRING_VALUE_AT(s, i);
    count += ((ucs4 < 0x80)     ? 1 :
	      (ucs4 < 0x800)    ? 2 :
	      (ucs4 < 0x10000)  ? 3 :
	      (ucs4 < 0x200000) ? 4 : 0);
  }
  /* conversion */
  r = SG_NEW_ATOMIC2(char *, count+1);
  count = 0;
  for (i = 0; i < SG_STRING_SIZE(s); i++) {
    count += Sg_ConvertUcs4ToUtf8(SG_STRING_VALUE_AT(s, i), 
				  (uint8_t*)(r+count),
				  SG_IGNORE_ERROR);
  }
  *(r+count) = '\0';
  return r;
}

wchar_t* Sg_StringToWCharTs(SgObject s)
{
  int size = SG_STRING_SIZE(s);
  SgBytePort out;
  SgTranscodedPort tp;
#if SIZEOF_WCHAR_T == 2
# if WORDS_BIGENDIAN
  SgCodec *codec = Sg_MakeUtf16Codec(UTF_16BE);
# else
  SgCodec *codec = Sg_MakeUtf16Codec(UTF_16LE);
# endif
#else
  SgCodec *codec = Sg_MakeUtf32Codec(UTF_32USE_NATIVE_ENDIAN);
#endif
  SgTranscoder *tcoder = Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR);

  Sg_InitByteArrayOutputPort(&out, sizeof(wchar_t) * (size + 1));
  Sg_InitTranscodedPort(&tp, SG_PORT(&out), tcoder, SG_OUTPUT_PORT);

  Sg_TranscoderWrite(tcoder, SG_PORT(&tp), 
		     SG_STRING_VALUE(s), SG_STRING_SIZE(s));
  Sg_TranscoderPutc(tcoder, SG_PORT(&tp), '\0');
  return (wchar_t*)Sg_GetByteArrayFromBinaryPort(&out);
}

SgObject Sg_WCharTsToString(wchar_t *s, int size)
{
#define BUF_SIZ 256
  /* TODO this is a bit inefficient */
  /* size_t size = wcslen(s); */
#if SIZEOF_WCHAR_T == 2
# if WORDS_BIGENDIAN
  SgCodec *codec = Sg_MakeUtf16Codec(UTF_16BE);
# else
  SgCodec *codec = Sg_MakeUtf16Codec(UTF_16LE);
# endif
#else
  SgCodec *codec = Sg_MakeUtf32Codec(UTF_32USE_NATIVE_ENDIAN);
#endif
  SgTranscoder *transcoder = Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR);
  SgObject bin = Sg_MakeByteArrayInputPort((uint8_t *)s, size*sizeof(wchar_t));
  SgObject tin = Sg_MakeTranscodedPort(bin, transcoder);
  SgObject accum = Sg_MakeStringOutputPort(size);
  int64_t total_size = 0;
  int64_t len;
  SgChar buf[BUF_SIZ];
  int read_size = BUF_SIZ;

  for (;;) {
    int rest;
    len = Sg_ReadsUnsafe(tin, buf, read_size);
    if (len < read_size) break;
    Sg_WritesUnsafe(accum, buf, len);
    total_size += len;
    rest = (int)(size - total_size);
    len = 0;
    if (rest <= 0) break;
    if (rest < read_size) read_size = rest;
  }
  if (len != 0) {
    Sg_WritesUnsafe(accum, buf, len);
  }
  return Sg_GetStringFromStringPort(accum);
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
#if 0
  register const uint32_t *ss1;
  register const unsigned char *ss2, *t;
  for (ss1 = (const unsigned int *)s1,
	 ss2 = (const unsigned char *)s2,
	 t = ss1 + n;
       ss1 != t && *ss1 == *ss2 && *ss1 != 0;
       ss1++, ss2++)
    ;
  return *ss1 - *ss2;
#endif
  size_t i;
  for(i = 0; i < n; i++) {
    if(s1[i] ^ s2[i]) return s1[i] - s2[i];
  }
  return 0;
}


size_t ustrlen(const SgChar *value)
{
  /* TODO naive? */
  int count = 0;
  for (; *value ;value++, count++);
  return count;
}

#define CASE_OFFSET 0x20

#include "../unicode/general-category-1.inc"
#include "../unicode/general-category-2.inc"
#include "../unicode/numeric-property.inc"
#include "../unicode/other-alphabetic.inc"
#include "../unicode/other-lowercase.inc"
#include "../unicode/other-uppercase.inc"
#include "../unicode/simple-lowercase.inc"
#include "../unicode/simple-titlecase.inc"
#include "../unicode/simple-uppercase.inc"
#include "../unicode/canonical-class.inc"
#include "../unicode/compose.inc"
#include "../unicode/compatibility.inc"

#include "../unicode/case-folding.inc"
#include "../unicode/special-casing-lower.inc"
#include "../unicode/special-casing-upper.inc"
/* not used so for now make compiler shut */
#include "../unicode/special-casing-title.inc"
#include "../unicode/decompose.inc"

#define DECLARE_SIMPLE_CASE(name, how)				\
  static SgChar name (SgChar ch)				\
  {								\
    const int size = array_sizeof(SG_CPP_CAT(s_, name));	\
    int i;							\
    for (i = 0; i < size; i++) {				\
      if (SG_CPP_CAT(s_, name)[i].in == ch) {			\
	return (SgChar)(SG_CPP_CAT(s_, name)[i].out);		\
      }								\
    }								\
    return how? ch: 0;						\
  }

DECLARE_SIMPLE_CASE(simple_uppercase, TRUE);
DECLARE_SIMPLE_CASE(simple_lowercase, TRUE);
DECLARE_SIMPLE_CASE(simple_titlecase, TRUE);
DECLARE_SIMPLE_CASE(canonical_class, FALSE);
DECLARE_SIMPLE_CASE(compose, FALSE);

#define DECLARE_BOOL_CASE(name)					\
  static int name (SgChar ch)					\
  {								\
    const int size = array_sizeof(SG_CPP_CAT(s_, name));	\
    int i;							\
    for (i = 0; i < size; i++) {				\
      if (SG_CPP_CAT(s_, name)[i].in == ch) {			\
	return TRUE;						\
      }								\
    }								\
    return FALSE;						\
  }

DECLARE_BOOL_CASE(compatibility);

#define DECLARE_OTHER_CASE_PRED(name, lo, hi)				\
  static int SG_CPP_CAT(name, _property_p) (SgChar ch)			\
  {									\
    if ((lo) <= ch && ch <= (hi)) {					\
      const int size = array_sizeof(SG_CPP_CAT(s_, name));		\
      int i;								\
      for (i = 0; i < size; i++) {					\
	if (SG_CPP_CAT(s_, name)[i].in <= ch &&				\
	    SG_CPP_CAT(s_, name)[i].out <= ch) {			\
	  return TRUE;							\
	}								\
      }									\
    }									\
    return FALSE;							\
  }
DECLARE_OTHER_CASE_PRED(other_alphabetic, 0x345, 0x10A0F);
/* since unicode 6.1.0, 0xAA is categorised in Lo */
DECLARE_OTHER_CASE_PRED(other_lowercase, 0xAA, 0x24E9);
DECLARE_OTHER_CASE_PRED(other_uppercase, 0x2160, 0x24CF);
#if 0
static int other_alphabetic_property_p(SgChar ch)
{
  if (0x345 <= ch && ch <= 0x10A0F) {
    const int size = array_sizeof(s_other_alphabetic);
    int i;
    for (i = 0; i < size; i++) {
      if (s_other_alphabetic[i].in <= ch &&
	  s_other_alphabetic[i].out <= ch) {
	return TRUE;
      }
    }
  }
  return FALSE;
}
#endif

SgObject Sg_DigitValue(SgChar ch)
{
  if ('0' <= ch && ch <= '9') return SG_MAKE_INT(ch - '0');
  else {
    const int size = array_sizeof(s_numeric_property);
    int i;
    for (i = 0; i < size; i++) {
      if (s_numeric_property[i].in == (int32_t)ch) {
	SgObject nume = Sg_MakeIntegerFromS64(s_numeric_property[i].nume);
	int64_t deno = s_numeric_property[i].deno;
	if (deno == 1LL) {
	  return nume;
	} else {
	  return Sg_MakeRational(nume, Sg_MakeIntegerFromS64(deno));
	}
      }
    }
    return SG_FALSE;;
  }
}

SgChar Sg_CharUpCase(SgChar ch)
{
  if (ch < 'a') return ch;
  else if (ch > 'z') return simple_uppercase(ch);
  else return ch - CASE_OFFSET;
}

SgChar Sg_CharDownCase(SgChar ch)
{
  if (ch < 'A') return ch;
  else if (ch <= 'Z') return ch + CASE_OFFSET;
  else if (ch > 'z') return simple_lowercase(ch);
  else return ch;
}

SgChar Sg_CharTitleCase(SgChar ch)
{
  return simple_titlecase(ch);
}

SgChar Sg_CharFoldCase(SgChar ch)
{
  if (ch <= 'z') return Sg_CharDownCase(ch);
  else if (ch == 0x130 ||
	   ch == 0x131) return ch;
  else return Sg_CharDownCase(Sg_CharUpCase(ch));
}

int Sg_CharAlphabeticP(SgChar ch)
{
  if ('a' <= ch && ch <= 'z') return TRUE;
  else if ('A' <= ch && ch <= 'Z') return TRUE;
  else if (0x80 <= ch) {
    switch (Sg_CharGeneralCategory(ch)) {
    case Lu: case Ll: case Lt: case Lm: case Lo: case Nl:
      return TRUE;
    case Mn: case Mc: case So:
      return other_alphabetic_property_p(ch);
    default:
      return FALSE;
    }
  }
  else return FALSE;
}

int Sg_CharNumericP(SgChar ch)
{
  if ('0' <= ch && ch <= '9') return TRUE;
  else if (0x80 <= ch) {
    /* handle special cases
       U+F96B, U+F973, U+F978, U+F9B2, U+F9D1, U+F9D3, U+F9FD, U+2F890
       these are kanji numbers but categorised in Lo.
       NB: digit-value can convert it because it has numeric property
           to make R7RS compliant, we need to handle it separately
     */
    switch (ch) {
    case 0xF96B: 		/* CJK COMPATIBILITY IDEOGRAPH 3  */
    case 0xF973:		/* CJK COMPATIBILITY IDEOGRAPH 10 */
    case 0xF978:		/* CJK COMPATIBILITY IDEOGRAPH 2  */
    case 0xF9B2:		/* CJK COMPATIBILITY IDEOGRAPH 0  */
    case 0xF9D1:		/* CJK COMPATIBILITY IDEOGRAPH 6  */
    case 0xF9D3:		/* CJK COMPATIBILITY IDEOGRAPH 6  */
    case 0xF9FD:		/* CJK COMPATIBILITY IDEOGRAPH 10 */
    case 0x2F890:		/* CJK COMPATIBILITY IDEOGRAPH 9  */
      return TRUE;
    default:
      /* OK, check category */
      switch (Sg_CharGeneralCategory(ch)) {
      case Nd: case Nl: case No: return TRUE;
      default: return FALSE;
      }
    }
  }
  else return FALSE;
}

int Sg_CharUpperCaseP(SgChar ch)
{
  if ('A' <= ch && ch <= 'Z') return TRUE;
  else if (0x80 <= ch) {
    switch (Sg_CharGeneralCategory(ch)) {
    case Lu:
      return TRUE;
    case Nl: case So:
      return other_uppercase_property_p(ch);
    default:
      return FALSE;
    }
  }
  else return FALSE;
}

int Sg_CharLowerCaseP(SgChar ch)
{
  if ('a' <= ch && ch <= 'z') return TRUE;
  else if (0x80 <= ch) {
    switch (Sg_CharGeneralCategory(ch)) {
    case Ll:
      return TRUE;
    case Lm: case Mn: case Nl: case So: case Lo:
      return other_lowercase_property_p(ch);
    default:
      return FALSE;
    }
  }
  else return FALSE;
}

int Sg_CharTitleCaseP(SgChar ch)
{
  return Sg_CharGeneralCategory(ch) == Lt;
}

static SgHashTable *general_category = NULL;

SgGeneralCategory Sg_CharGeneralCategory(SgChar ch)
{
#if 0
  const int cate1_size = array_sizeof(s_general_category_1);
  const int cate2_size = array_sizeof(s_general_category_2);
  int i;

  for (i = 0; i < cate1_size; i++) {
    if (s_general_category_1[i].in == ch) return s_general_category_1[i].out;
  }
  for (i = 0; i < cate2_size; i++) {
    if (s_general_category_2[i].in == ch) return s_general_category_2[i].out;
  }
#endif
  SgObject c;
  c = Sg_HashTableRef(general_category, SG_MAKE_CHAR(ch), SG_FALSE);
  if (!SG_FALSEP(c)) {
    return SG_INT_VALUE(c);
  }
  if (0x3400 <= ch && ch <= 0x4DB5) return Lo;
  else if (0x4E00 <= ch && ch <= 0x9FBB) return Lo;
  else if (0xAC00 <= ch && ch <= 0xD7A3) return Lo;
  else if (0xD800 <= ch && ch <= 0xDB7F) return Cs;
  else if (0xDB80 <= ch && ch <= 0xDBFF) return Cs;
  else if (0xDC00 <= ch && ch <= 0xDFFF) return Cs;
  else if (0xE000 <= ch && ch <= 0xF8FF) return Co;
  else if (0x20000 <= ch && ch <= 0x2A6D6) return Lo;
  else if (0xF0000 <= ch && ch <= 0xFFFFD) return Co;
  else if (0x100000 <= ch && ch <= 0x10FFFD) return Co;
  else return Cn;
}

SgObject Sg_CategroyToSymbol(SgGeneralCategory cate)
{
#define CASE_INTERN(c)			\
  case c : return SG_INTERN(#c)

  switch (cate) {
  CASE_INTERN(Lu);
  CASE_INTERN(Ll);
  CASE_INTERN(Lt);
  CASE_INTERN(Lm);
  CASE_INTERN(Lo);
  CASE_INTERN(Mn);
  CASE_INTERN(Mc);
  CASE_INTERN(Me);
  CASE_INTERN(Nd);
  CASE_INTERN(Nl);
  CASE_INTERN(No);
  CASE_INTERN(Ps);
  CASE_INTERN(Pe);
  CASE_INTERN(Pi);
  CASE_INTERN(Pf);
  CASE_INTERN(Pd);
  CASE_INTERN(Pc);
  CASE_INTERN(Po);
  CASE_INTERN(Sc);
  CASE_INTERN(Sm);
  CASE_INTERN(Sk);
  CASE_INTERN(So);
  CASE_INTERN(Zs);
  CASE_INTERN(Zp);
  CASE_INTERN(Zl);
  CASE_INTERN(Cc);
  CASE_INTERN(Cf);
  CASE_INTERN(Cs);
  CASE_INTERN(Co);
  CASE_INTERN(Cn);
  }
  /* never happen */
  return SG_INTERN("Cn");
}

#define DECLARE_SPECIAL_CASING(name)				\
  static int name (SgChar ch)					\
  {								\
    const int size = array_sizeof(SG_CPP_CAT(s_, name));	\
    int i;							\
    if (ch < SG_CPP_CAT(s_, name)[0].in) return -1;		\
    if (ch > SG_CPP_CAT(s_, name)[size-1].in) return -1;	\
    /* TODO maybe binary search? */				\
    for (i = 0; i < size; i++) {				\
      if (SG_CPP_CAT(s_, name)[i].in == ch) return i;		\
    }								\
    return -1;							\
  }

DECLARE_SPECIAL_CASING(special_casing_upper);
DECLARE_SPECIAL_CASING(special_casing_lower);
DECLARE_SPECIAL_CASING(special_casing_title);
DECLARE_SPECIAL_CASING(case_folding);
DECLARE_SPECIAL_CASING(decompose);

static int final_sigma_p(int index, SgString *in, SgPort *out)
{
  SgChar ch;
  int size = SG_STRING_SIZE(in);
  if (size <= index + 1) {
    return Sg_PortPosition(out) != 0;
  }
  ch = SG_STRING_VALUE_AT(in, index + 1);
  if (Sg_CharAlphabeticP(ch)) return FALSE;
  else if (Sg_Ucs4WhiteSpaceP(ch)) return TRUE;
  else if (Sg_CharGeneralCategory(ch) == Pd) return TRUE;
  else {
    int i = index;
    for (; i < size; i++) {
      ch = SG_STRING_VALUE_AT(in, i);
      if (Sg_CharAlphabeticP(ch)) return FALSE;
      else if (Sg_Ucs4WhiteSpaceP(ch)) return TRUE;
      else if (Sg_CharGeneralCategory(ch) == Pd) return TRUE;
    }
    return Sg_PortPosition(out) != 0;
  }
}

SgObject Sg_StringUpCase(SgString *str)
{
  int i, size = SG_STRING_SIZE(str);
  SgPort *out;
  SgStringPort tp;
  SgObject newS;

  Sg_InitStringOutputPort(&tp, size);
  out = SG_PORT(&tp);
  for (i = 0; i < size; i++) {
    int r = special_casing_upper(SG_STRING_VALUE_AT(str, i));
    if (r >= 0) {
      const int up_size = array_sizeof(s_special_casing_upper[r].out);
      int j;
      for (j = 0; j < up_size; j++) {
	if (s_special_casing_upper[r].out[j] == 0) break;
	Sg_PutcUnsafe(out, s_special_casing_upper[r].out[j]);
      } 
    } else {
      Sg_PutcUnsafe(out, Sg_CharUpCase(SG_STRING_VALUE_AT(str, i)));
    }
  }
  newS = Sg_GetStringFromStringPort(&tp);
  SG_CLEAN_STRING_PORT(&tp);
  if (Sg_StringEqual(str, newS)) {
    newS = NULL;
    return str;
  } else {
    return newS;
  }
}

static void special_casing_char_downcase(SgPort *out, SgChar ch, SgChar lastCh,
					 int finalSigmaP)
{
  if (ch == 0x03A3) { 	/* greek capital letter sigma */
    if (Sg_Ucs4WhiteSpaceP(lastCh)) {
      Sg_PutcUnsafe(out, 0x03C3);
    } else {
      if (finalSigmaP) {
	Sg_PutcUnsafe(out, 0x03C2); /* greek small letter final sigma */
      } else {
	Sg_PutcUnsafe(out, 0x03C3); /* greek small letter sigma */
      }
    }
  } else {
    int r = special_casing_lower(ch);
    if (r >= 0) {
      const int up_size = array_sizeof(s_special_casing_lower[r].out);
      int j;
      for (j = 0; j < up_size; j++) {
	if (s_special_casing_lower[r].out[j] == 0) break;
	Sg_PutcUnsafe(out, s_special_casing_lower[r].out[j]);
      }
    } else {
      Sg_PutcUnsafe(out, Sg_CharDownCase(ch));
    }
  }
}

SgObject Sg_StringDownCase(SgString *str)
{
  int i, size = SG_STRING_SIZE(str);
  SgPort *out;
  SgStringPort tp;
  SgObject newS;
  SgChar ch, lastCh = ' ';

  Sg_InitStringOutputPort(&tp, size);
  out = SG_PORT(&tp);
  for (i = 0; i < size; i++, lastCh = ch) {
    ch = SG_STRING_VALUE_AT(str, i);
    special_casing_char_downcase(out, ch, lastCh, final_sigma_p(i, str, out));
  }
  newS = Sg_GetStringFromStringPort(&tp);
  SG_CLEAN_STRING_PORT(&tp);
  if (Sg_StringEqual(str, newS)) {
    newS = NULL;
    return str;
  } else {
    return newS;
  }
}

static int titlecase_first_char(int index, SgString *in, SgPort *out,
				int useSpecialCasing);

static int downcase_subsequence(int index, SgString *in, SgPort *out,
				int useSpecialCasing)
{
  int i, size = SG_STRING_SIZE(in);
  SgChar ch, lastCh = ' ';
  for (i = index; i < size; i++, lastCh = ch) {
    ch = SG_STRING_VALUE_AT(in, i);
    switch (Sg_CharGeneralCategory(ch)) {
      case Ll: case Lu: case Lt:
	if (useSpecialCasing) {
	  special_casing_char_downcase(out, ch, lastCh, 
				       final_sigma_p(i, in, out));
	} else {
	  Sg_PutcUnsafe(out, Sg_CharDownCase(ch));
	}
	break;
    case Po: case Pf:
      if (ch == 0x0027 ||   /* mid letter # Po apostrophe */
	  ch == 0x003A ||   /* mid letter # Po colon */
	  ch == 0x00B7 ||   /* mid letter # Po middle dot */
	  ch == 0x05F4 ||   /* mid letter # Po hebrew punctuation gershayim */
	  ch == 0x2019 ||   /* mid letter # Po right single quotation mark */
	  ch == 0x2027) {   /* mid letter # Po hyphenation point */
	Sg_PutcUnsafe(out, ch);
      } else {
	Sg_PutcUnsafe(out, ch);
	i++;
	i += titlecase_first_char(i, in, out, useSpecialCasing);
      }
      break;
    case Nd:
      Sg_PutcUnsafe(out, ch);
      break;
    default:
      Sg_PutcUnsafe(out, ch);
      i++;
      i += titlecase_first_char(i, in, out, useSpecialCasing);
    }
  }
  return i - index;
}

static int titlecase_first_char(int index, SgString *in, SgPort *out,
				int useSpecialCasing)
{
  int i, size = SG_STRING_SIZE(in);
  SgChar ch;
  for (i = index; i < size; i++) {
    ch = SG_STRING_VALUE_AT(in, i);
    switch (Sg_CharGeneralCategory(ch)) {
    case Ll: case Lu: case Lt:
      if (useSpecialCasing) {
	int r = special_casing_title(ch);
	if (r >= 0) {
	  const int title_size = array_sizeof(s_special_casing_title[r].out);
	  int j;
	  for (j = 0; j < title_size; j++) {
	    if (s_special_casing_title[r].out[j] == 0) break;
	    Sg_PutcUnsafe(out, s_special_casing_title[r].out[j]);
	  }
	} else {
	  Sg_PutcUnsafe(out, Sg_CharTitleCase(ch));
	}
      } else {
	Sg_PutcUnsafe(out, Sg_CharTitleCase(ch));
      }
      i++;
      i += downcase_subsequence(i, in, out, useSpecialCasing);
      break;
    default:
      Sg_PutcUnsafe(out, ch);
      break;
    }
  }
  return i - index;
}

SgObject Sg_StringTitleCase(SgString *str, int useSpecialCasing)
{
  int size = SG_STRING_SIZE(str);
  SgPort *out;
  SgStringPort tp;
  SgObject newS;

  Sg_InitStringOutputPort(&tp, size);
  out = SG_PORT(&tp);
  titlecase_first_char(0, str, out, useSpecialCasing);

  newS = Sg_GetStringFromStringPort(&tp);
  SG_CLEAN_STRING_PORT(&tp);
  if (Sg_StringEqual(str, newS)) {
    newS = NULL;
    return str;
  } else {
    return newS;
  }
}

SgObject Sg_StringFoldCase(SgString *str)
{
  int i, size = SG_STRING_SIZE(str);
  SgPort *out;
  SgStringPort tp;
  SgObject newS;

  Sg_InitStringOutputPort(&tp, size);
  out = SG_PORT(&tp);
  for (i = 0; i < size; i++) {
    int r = case_folding(SG_STRING_VALUE_AT(str, i));
    if (r >= 0) {
      const int up_size = array_sizeof(s_case_folding[r].out);
      int j;
      for (j = 0; j < up_size; j++) {
	if (s_case_folding[r].out[j] == 0) break;
	Sg_PutcUnsafe(out, s_case_folding[r].out[j]);
      } 
    } else {
      Sg_PutcUnsafe(out, SG_STRING_VALUE_AT(str, i));
    }
  }
  newS = Sg_GetStringFromStringPort(&tp);
  SG_CLEAN_STRING_PORT(&tp);
  if (Sg_StringEqual(str, newS)) {
    newS = NULL;
    return str;
  } else {
    return newS;
  }
}

#define SBase 0xAC00
#define LBase 0x1100
#define VBase 0x1161
#define TBase 0x11A7
#define LCount 19
#define VCount 21
#define TCount 28
#define NCount (VCount * TCount)
#define SCount (LCount * NCount)

/* helper, utf32 native is easy to implement without using transcoder */
static SgByteVector* string2bytevector(SgObject s)
{
  SgByteVector* bv = Sg_MakeByteVector(SG_STRING_SIZE(s)*sizeof(SgChar), 0);
  int i;
  for (i = 0; i < SG_STRING_SIZE(s); i++) {
    uint8_t *tmp = &SG_BVECTOR_ELEMENT(bv, i*4);
    *(uint32_t *)tmp = SG_STRING_VALUE_AT(s, i);
  }
  return bv;
}

static SgObject bytevector2string(SgByteVector *bv)
{
  SgString *s = Sg_ReserveString(SG_BVECTOR_SIZE(bv)/sizeof(SgChar), ' ');
  int i;
  for (i = 0; i < SG_STRING_SIZE(s); i++) {
    uint8_t *tmp = &SG_BVECTOR_ELEMENT(bv, i*4);
    SG_STRING_VALUE_AT(s, i) = *(uint32_t*)tmp;
  }
  return SG_OBJ(s);
}

static void recursive_decomposition(int canonicalP, SgChar sv, SgPort *out)
{
  int dindex = decompose(sv), sindex = sv - SBase, i;

  if (dindex >= 0 && !(canonicalP && compatibility(sv))) {
    const int size = array_sizeof(s_decompose[dindex].out);
    for (i = 0; i < size; i++) {
      SgChar ch = s_decompose[dindex].out[i];
      if (ch == 0) break;
      recursive_decomposition(canonicalP, ch, out);
    }
  } else if (-1 < sindex && sindex < SCount) {
    int L = LBase + (sindex / NCount);
    int V = VBase + ((sindex % NCount) / TCount);
    int T = TBase + (sindex % TCount);
    Sg_PutcUnsafe(out, L);
    Sg_PutcUnsafe(out, V);
    if (T != TBase) {
      Sg_PutcUnsafe(out, T);
    }
  } else {
    Sg_PutcUnsafe(out, sv);
  }
}

static SgByteVector* decompose_rec(SgString *in, int canonicalP)
{
  int i, size = SG_STRING_SIZE(in);
  SgPort *out;
  SgStringPort tp;
  SgObject r;
  Sg_InitStringOutputPort(&tp, size);
  out = SG_PORT(&tp);
  for (i = 0; i < size; i++) {
    SgChar ch = SG_STRING_VALUE_AT(in, i);
    recursive_decomposition(canonicalP, ch, out);
  }
  r = Sg_GetStringFromStringPort(&tp);
  SG_CLEAN_STRING_PORT(&tp);
  return string2bytevector(r);
}

static SgByteVector* sort_combining_marks(SgByteVector *bv)
{
  int last = SG_BVECTOR_SIZE(bv) - 4, i;
  for (i = 0; i < last;) {
    uint32_t this = Sg_ByteVectorU32NativeRef(bv, i);
    uint32_t next = Sg_ByteVectorU32NativeRef(bv, i + 4);
    int32_t this_cc = canonical_class(this);
    int32_t next_cc = canonical_class(next);
    if (this_cc > 0 &&
	next_cc > 0 &&
	this_cc > next_cc) {
      Sg_ByteVectorU32NativeSet(bv, i, next);
      Sg_ByteVectorU32NativeSet(bv, i + 4, this);
      if (i >= 4) {
	i -= 4;
      } else {
	i = 4;
      }
    } else {
      i += 4;
    }
  }
  return bv;
}

SgObject Sg_StringNormalizeNfd(SgString *str)
{
  SgByteVector *bv = decompose_rec(str, TRUE);
  sort_combining_marks(bv);
  return bytevector2string(bv);
}

SgObject Sg_StringNormalizeNfkd(SgString *str)
{
  SgByteVector *bv = decompose_rec(str, FALSE);
  sort_combining_marks(bv);
  return bytevector2string(bv);
}

static int32_t pair_wise_composition(uint32_t first, uint32_t second)
{
  if (first > 0x10FFFF  || second > 0x10FFFF) {
    return -1;
  } else {
    int32_t lindex = first - LBase;
    int32_t vindex = second - VBase;
    int32_t sindex = first - SBase;
    int32_t tindex = second - TBase;
    if ((-1 < lindex && lindex < LCount) &&
	(-1 < vindex && vindex < VCount)) {
      return SBase + (TCount * (vindex + (lindex * VCount)));
    } else if ((-1 < sindex && sindex < SCount) &&
	       (-1 < tindex && tindex < TCount) &&
	       ((sindex % TCount) == 0)) {
      return first + tindex;
    } else {
      int64_t val = (first * 0x10000) + second;
      int32_t r = compose((SgChar)val);
      return r ? r : -1;
    }
  }
}

static SgObject compose_rec(SgByteVector *bv)
{
  SgByteVector *out;
  int size = SG_BVECTOR_SIZE(bv);
  uint32_t first = Sg_ByteVectorU32NativeRef(bv, 0);
  int32_t first_cc = (canonical_class(first) == 0) ? 0 : 256;
  
  uint32_t starter = first;
  int32_t starter_cc = first_cc;
  int starter_pos, comp_pos, i;

  for (i = 4, comp_pos = 4, starter_pos = 0;; i += 4) {
    if (i >= size) {
      /* out = Sg_MakeByteVector(comp_pos, 0); */
      SG_ALLOC_TEMP_BVECTOR(out, comp_pos);
      Sg_ByteVectorCopyX(bv, 0, out, 0, comp_pos);
      return bytevector2string(out);
    } else {
      uint32_t this = Sg_ByteVectorU32NativeRef(bv, i);
      int32_t this_cc = canonical_class(this), composit;
      if ((starter_cc == 0 || starter_cc < this_cc) &&
	  (composit = pair_wise_composition(starter, this)) >= 0) {
	ASSERT(composit >= 0);
	Sg_ByteVectorU32NativeSet(bv, starter_pos, composit);
	starter = composit;
	starter_cc = canonical_class(composit);
      } else {
	Sg_ByteVectorU32NativeSet(bv, comp_pos, this);
	if (this_cc == 0) {
	  starter = this;
	  starter_pos = comp_pos;
	}
	starter_cc = this_cc;
	comp_pos += 4;
      }
    }
  }
}

SgObject Sg_StringNormalizeNfc(SgString *str)
{
  SgByteVector *bv = decompose_rec(str, TRUE);
  sort_combining_marks(bv);
  return compose_rec(bv);
}

SgObject Sg_StringNormalizeNfkc(SgString *str)
{
  SgByteVector *bv = decompose_rec(str, FALSE);
  sort_combining_marks(bv);
  return compose_rec(bv);
}

void Sg__InitUnicode()
{
  size_t i;
  const size_t size_1 = array_sizeof(s_general_category_1);
  const size_t size_2 = array_sizeof(s_general_category_2);
  general_category = Sg_MakeHashTableSimple(SG_HASH_EQV, (int)(size_1+size_2));
  for (i = 0; i < size_1; i++) {
    Sg_HashTableSet(general_category,
		    SG_MAKE_CHAR(s_general_category_1[i].in),
		    SG_MAKE_INT(s_general_category_1[i].out),
		    0);
  }
  for (i = 0; i < size_2; i++) {
    Sg_HashTableSet(general_category,
		    SG_MAKE_CHAR(s_general_category_2[i].in),
		    SG_MAKE_INT(s_general_category_2[i].out),
		    0);
  }
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
