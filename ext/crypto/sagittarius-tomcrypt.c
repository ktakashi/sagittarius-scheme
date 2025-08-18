/* sagittarius-tomcrypt.c                        -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2022  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius-cipher.h"
#include "sagittarius-digest.h"
#include "sagittarius-random.h"
#include "sagittarius-mac.h"
#include "sagittarius-stream.h"

extern void Sg__Init_kdf(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__tomcrypt()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__tomcrypt);

  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius crypto tomcrypt)"),
				  FALSE));
  Sg_InitCipher(lib);
  Sg_InitDigest(lib);
  Sg_InitRandom(lib);
  Sg_InitMac(lib);
  Sg_InitStream(lib);
  Sg__Init_kdf(lib);
}
