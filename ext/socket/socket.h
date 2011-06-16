/* -*- C -*- */
/*
 * socket.h
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
#ifndef SAGITTARIUS_SOCKET_H_
#define SAGITTARIUS_SOCKET_H_

#include <sagittarius.h>
#ifdef _WIN32
# include <winsock2.h>
# include <ws2tcpip.h>
# include <wspiapi.h>
# pragma comment(lib, "ws2_32.lib")
# pragma comment(lib, "iphlpapi.lib")
# define snprintf _snprintf
#else
# include <sys/socket.h>
# include <netdb.h>
# include <unistd.h>
# include <errno.h>
#endif

typedef enum {
  SG_SOCKET_CLIENT,
  SG_SOCKET_SERVER,
} SgSocketType;

typedef struct SgSocketRec
{
  SG_META_HEADER;
  int socket;			/* fd */
  int lastError;
  SgSocketType type;
  SgString *address;		/* for print */
} SgSocket;

SG_DECLARE_META_OBJ(Sg_SocketMeta);
#define SG_META_SOCKET   (&Sg_SocketMeta)
#define SG_SOCKET(obj)   ((SgSocket*)obj)
#define SG_SOCKET_P(obj) SG_META_OBJ_TYPE_P(obj, SG_META_SOCKET)

SG_CDECL_BEGIN

SgSocket* Sg_CreateClientSocket(const SgString *node,
				const SgString *service,
				int ai_family,
				int ai_socktype,
				int ai_flags,
				int ai_protocol);
SgSocket* Sg_CreateServerSocket(const SgString *service,
				int ai_family,
				int ai_socktype,
				int ai_protocol);

int       Sg_SocketReceive(SgSocket *socket, uint8_t *data, int size, int flags);
int       Sg_SocketSend(SgSocket *socket, uint8_t *data, int size, int flags);
SgSocket* Sg_SocketAccept(SgSocket *socket);
void      Sg_SocketShutdown(SgSocket *socket, int how);
void      Sg_SocketClose(SgSocket *socket);
int       Sg_SocketOpenP(SgSocket *socket);

SgObject  Sg_MakeSocketPort(SgSocket *socket);
void      Sg_ShutdownPort(SgPort *port);

SG_CDECL_END

#endif /* SAGITTARIUS_SOCKET_HPP_ */
