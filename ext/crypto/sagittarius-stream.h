/* sagittarius-stream.h                          -*- mode: c; coding: utf-8; -*-
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

#ifndef SAGITTARIUS_STREAM_H_
#define SAGITTARIUS_STREAM_H_

#ifdef noreturn
# define noreturn_save noreturn
# undef noreturn
#endif
#include <sagittarius.h>
#include <tomcrypt.h>

#ifdef noreturn_save
# define noreturn noreturn_save
#endif

typedef enum {
  CHACHA,
  SALSA20,
  XSALSA20,
  SOSEMANUK,
  RABBIT,
  RC4,
  SOBER128,
  CHACHA20POLY1305
} SgStreamCipher;

/* builtin stream cipher state */
typedef struct {
  SG_HEADER;
  SgStreamCipher cipher;
  union {
    chacha_state    chacha;
    salsa20_state   salsa20; /* also for xsalsa20 */
    sosemanuk_state sosemanuk;
    rabbit_state    rabbit;
    rc4_state       rc4;
    sober128_state  sober128;
    chacha20poly1305_state chacha20poly1305;
  };
} SgStreamCipherState;

SG_CLASS_DECL(Sg_StreamCipherStateClass);
#define SG_CLASS_STREAM_CIPHER_STATE (&Sg_StreamCipherStateClass)

#define SG_STREAM_CIPHER_STATE(obj)   ((SgStreamCipherState *) obj)
#define SG_STREAM_CIPHER_STATE_P(obj) SG_XTYPEP(obj, SG_CLASS_STREAM_CIPHER_STATE)
#define SG_STREAM_CIPHER_STATE_CIPHER(obj) SG_STREAM_CIPHER_STATE(obj)->cipher

SgObject Sg_MakeStreamCipherState(SgStreamCipher cipher);

void Sg_InitStream(SgLibrary *lib);

#endif	/* SAGITTARIUS_STREAM_H_ */
