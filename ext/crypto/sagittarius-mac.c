/* sagittarius-mac.c                             -*- mode: c; coding: utf-8; -*-
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
#include "sagittarius-mac.h"

static void hmac_state_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  const char *cname = DIGEST_DESCRIPTOR_NAME(SG_HMAC_STATE_MD(self));
  SgObject name = Sg_MakeStringC(cname);
  Sg_Printf(port, UC("#<hmac-state %A>"), name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_HmacStateClass, hmac_state_printer);

SgObject Sg_MakeHmacState(int md)
{
  SgHmacState *state = SG_NEW(SgHmacState);
  SG_SET_CLASS(state, SG_CLASS_HMAC_STATE);
  state->md = md;
  return SG_OBJ(state);
}

extern void Sg__Init_mac(SgLibrary *lib);

void Sg_InitMac(SgLibrary *lib)
{
  Sg__Init_mac(lib);
  Sg_InitStaticClass(SG_CLASS_HMAC_STATE, UC("<hmac-state>"), lib, NULL, 0);
}
