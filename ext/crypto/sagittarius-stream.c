/* sagittarius-stream.c                          -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2023  Takashi Kato <ktakashi@ymail.com>
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
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-stream.h"

static void stream_state_printer(SgObject self, SgPort *port,
				 SgWriteContext *ctx)
{
  const SgChar *cipher;
  switch (SG_STREAM_CIPHER_STATE_CIPHER(self)) {
  case CHACHA:           cipher = UC("chacha"); break;
  case SALSA20:          cipher = UC("salsa20"); break;
  case XSALSA20:         cipher = UC("xsalsa20"); break;
  case SOSEMANUK:        cipher = UC("sosemanuk"); break;
  case RABBIT:           cipher = UC("rabbit"); break;
  case RC4:              cipher = UC("rc4"); break;
  case SOBER128:         cipher = UC("sober128"); break;
  case CHACHA20POLY1305: cipher = UC("chacha20-poly1305"); break;
  default: cipher = UC("unknown"); break;
  }
  Sg_Printf(port, UC("#<%s-cipher-state>"), cipher);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_StreamCipherStateClass, stream_state_printer);

SgObject Sg_MakeStreamCipherState(SgStreamCipher cipher)
{
  SgStreamCipherState *z = SG_NEW(SgStreamCipherState);
  SG_SET_CLASS(z, SG_CLASS_STREAM_CIPHER_STATE);
  z->cipher = cipher;
  return SG_OBJ(z);
}

void Sg_InitStream(SgLibrary *lib)
{
#define CIPHER_CONST(name, value)					\
  Sg_MakeBinding(lib, SG_INTERN(#name), SG_MAKE_INT(value), TRUE)

  CIPHER_CONST(*stream-cipher:chacha*,            CHACHA);
  CIPHER_CONST(*stream-cipher:salsa20*,           SALSA20);
  CIPHER_CONST(*stream-cipher:xsalsa20*,          XSALSA20);
  CIPHER_CONST(*stream-cipher:sosemanuk*,         SOSEMANUK);
  CIPHER_CONST(*stream-cipher:rabbit*,            RABBIT);
  CIPHER_CONST(*stream-cipher:rc4*,               RC4);
  CIPHER_CONST(*stream-cipher:sober128*,          SOBER128);
  CIPHER_CONST(*stream-cipher:chacha20-poly1305*, CHACHA20POLY1305);
  
  Sg_InitStaticClass(SG_CLASS_STREAM_CIPHER_STATE,
		     UC("<stream-cipher-state>"), lib, NULL, 0);
}
