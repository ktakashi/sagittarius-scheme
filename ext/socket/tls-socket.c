/* tls-socket.c                                    -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2018  Takashi Kato <ktakashi@ymail.com>
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
#include "tls-socket.h"

static SgObject tls_socket_getter(SgTLSSocket *s)
{
  return s->socket;
}
static SgSlotAccessor tls_socket_slots[] = {
  SG_CLASS_SLOT_SPEC("raw-socket",   0, tls_socket_getter, NULL),
  {{ NULL }}
};

static void tls_socket_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgTLSSocket *socket = SG_TLS_SOCKET(self);
  Sg_Printf(port, UC("#<tls-socket %S>"), socket->socket);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_TLSSocketClass, tls_socket_printer);

extern void Sg__Init_tls_socket_stub(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__tls_socket()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__tls_socket);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius tls-socket)"),
				  TRUE));
  Sg_InitStaticClassWithMeta(SG_CLASS_TLS_SOCKET, UC("<tls-socket>"), lib, NULL,
			     SG_FALSE, tls_socket_slots, 0);
  Sg_InitTLSImplementation();
  Sg__Init_tls_socket_stub(lib);
}
