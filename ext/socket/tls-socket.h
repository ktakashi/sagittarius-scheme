/* tls-socket.h                                    -*- mode:c; coding:utf-8; -*-
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

#ifndef TLS_SOCKET_H_
#define TLS_SOCKET_H_

#include "sagittarius-socket.h"

typedef struct SgTLSSocketRec
{
  SG_HEADER;
  SgSocket *socket;
  void *data;
} SgTLSSocket;

SG_CLASS_DECL(Sg_TLSSocketClass);
#define SG_CLASS_TLS_SOCKET  (&Sg_TLSSocketClass)
#define SG_TLS_SOCKET(obj)   ((SgTLSSocket *)obj)
#define SG_TLS_SOCKET_P(obj) SG_XTYPEP(obj, SG_CLASS_TLS_SOCKET)

SG_CDECL_BEGIN

SG_EXTERN SgTLSSocket* Sg_SocketToTLSSocket(SgSocket *socket,
					    /* list of bytevectors */
					    SgObject certificates,
					    /* encoded private key */
					    SgByteVector *privateKey);
SG_EXTERN int       Sg_TLSSocketConnect(SgTLSSocket *tlsSocket,
					SgObject domainName,
					SgObject alpn);
SG_EXTERN SgObject  Sg_TLSSocketAccept(SgTLSSocket *tlsSocket, int handshake);
SG_EXTERN SgObject  Sg_TLSServerSocketHandshake(SgTLSSocket *tlsSocket);
SG_EXTERN void      Sg_TLSSocketShutdown(SgTLSSocket *tlsSocket, int how);
SG_EXTERN void      Sg_TLSSocketClose(SgTLSSocket *tlsSocket);
SG_EXTERN int       Sg_TLSSocketOpenP(SgTLSSocket *tlsSocket);
SG_EXTERN int       Sg_TLSSocketReceive(SgTLSSocket *tlsSocket, uint8_t *data,
					int size, int flags);
SG_EXTERN int       Sg_TLSSocketSend(SgTLSSocket *tlsSocket, uint8_t *data,
				     int size, int flags);
SG_EXTERN void      Sg_InitTLSImplementation();

SG_CDECL_END

#endif
