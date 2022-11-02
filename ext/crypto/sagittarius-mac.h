/* sagittarius-mac.h                             -*- mode: c; coding: utf-8; -*-
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

#ifndef SAGITTARIUS_MAC_H_
#define SAGITTARIUS_MAC_H_

/* need DIGEST_DESCRIPTORP anyway :) */
#include "sagittarius-digest.h"

typedef struct {
  SG_HEADER;
  int md;
  hmac_state state;
} SgHmacState;

SG_CLASS_DECL(Sg_HmacStateClass);
#define SG_CLASS_HMAC_STATE (&Sg_HmacStateClass)

#define SG_HMAC_STATE(obj)   ((SgHmacState *)obj)
#define SG_HMAC_STATE_P(obj) SG_XTYPEP(obj, SG_CLASS_HMAC_STATE)
#define SG_HMAC_STATE_MD(obj) SG_HMAC_STATE(obj)->md
#define SG_HMAC_STATE_STATE(obj) SG_HMAC_STATE(obj)->state

SgObject Sg_MakeHmacState(int md);
void Sg_InitMac(SgLibrary *lib);

#endif	/* SAGITTARIUS_MAC_H_ */
