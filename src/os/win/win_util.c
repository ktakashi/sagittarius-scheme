/* -*- C -*- */
/*
 * win_util.c
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

#include <sagittarius/transcoder.h>
#include <sagittarius/codec.h>
#include <sagittarius/string.h>
#include <sagittarius/port.h>

/* from mosh */
static const wchar_t* utf32ToUtf16(SgString *path)
{
  int size = SG_STRING_SIZE(path);
  SgPort *out = Sg_MakeByteArrayOutputPort(sizeof(wchar_t) * (size + 1));
  SgCodec *codec = Sg_MakeUtf16Codec(UTF_16LE);
  SgTranscoder *tcoder = Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR);
  SgPort *tp = Sg_MakeTranscodedOutputPort(out, tcoder);

  Sg_TranscoderWrite(tcoder, tp, SG_STRING_VALUE(path), SG_STRING_SIZE(path));
  Sg_TranscoderPutc(tcoder, tp, '\0');
  return (const wchar_t*)Sg_GetByteArrayFromBinaryPort(out);
}

static inline int isLead(SgChar c) { return (c & 0xfffffc00) == 0xd800; }
static inline int isTrail(SgChar c) { return (c & 0xfffffc00) == 0xdc00; }
static SgString* utf16ToUtf32(wchar_t *s)
{
  const SgChar offset = (0xd800 << 10UL) + 0xdc00 - 0x10000;
  size_t i = 0, n = wcslen(s);
  SgObject out = Sg_MakeStringOutputPort(n);
  while (i < n) {
    SgChar c0 = s[i++];
    if (isLead(c0)) {
      SgChar c1;
      if (i < n && isTrail((c1 = s[i]))) {
	i++;
	c0 = (c0 << 10) + c1 - offset;
      } else {
	return Sg_MakeString(UC("bad char"), SG_LITERAL_STRING);
      }
    }
    Sg_PutcUnsafe(out, c0);
  }
  return Sg_GetStringFromStringPort(out);
}

static SgString* utf16ToUtf32WithRegion(wchar_t *s, wchar_t *e)
{
  const SgChar offset = (0xd800 << 10UL) + 0xdc00 - 0x10000;
  SgObject out = Sg_MakeStringOutputPort((e - s) * 2);
  while (s < e) {
    SgChar c0 = *s++;
    if (isLead(c0)) {
      SgChar c1;
      if (s < e && isTrail((c1 = *s))) {
	s++;
	c0 = (c0 << 10) + c1 - offset;
      } else {
	return Sg_MakeString(UC("bad char"), SG_LITERAL_STRING);
      }
    }
    Sg_PutcUnsafe(out, c0);
  }
  return Sg_GetStringFromStringPort(out);
}

static SgObject get_last_error(DWORD e)
{
#define MSG_SIZE 128
  wchar_t msg[MSG_SIZE];
  int size = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
			    0, 
			    e,
			    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			    msg,
			    MSG_SIZE,
			    NULL);
  if (size > 2 && msg[size - 2] == '\r') {
    msg[size - 2] = 0;
    size -= 2;
  }
  return utf16ToUtf32(msg);
#undef MSG_SIZE
}
