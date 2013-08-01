/* -*- C -*- */
/*
 * socket.h
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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

#ifdef _WIN32
# include <winsock2.h>
# include <ws2tcpip.h>
# pragma comment(lib, "ws2_32.lib")
#if 0
# include <wspiapi.h>
# pragma comment(lib, "iphlpapi.lib")
#endif
# define snprintf _snprintf
/* what a crap!! */
# define SHUT_RD   SD_RECEIVE 
# define SHUT_WR   SD_SEND 
# define SHUT_RDWR SD_BOTH 
#else
# include <sys/socket.h>
# include <netdb.h>
# include <unistd.h>
# include <errno.h>
# include <fcntl.h>
# include <arpa/inet.h>
# include <sys/select.h>
#endif
#include <sagittarius.h>

typedef enum {
  SG_SOCKET_UNKNOWN,
  SG_SOCKET_CLIENT,
  SG_SOCKET_SERVER,
} SgSocketType;

#ifndef _WIN32
typedef int SOCKET;
#endif

typedef struct SgSocketRec
{
  SG_HEADER;
  SOCKET socket;		/* fd */
  int lastError;
  SgSocketType type;
  SgString *address;		/* for print */
} SgSocket;

SG_CLASS_DECL(Sg_SocketClass);
#define SG_CLASS_SOCKET (&Sg_SocketClass)
#define SG_SOCKET(obj)  ((SgSocket*)obj)
#define SG_SOCKETP(obj) SG_XTYPEP(obj, SG_CLASS_SOCKET)

typedef struct SgAddrinfoRec
{
  SG_HEADER;
  struct addrinfo *ai;
} SgAddrinfo;

SG_CLASS_DECL(Sg_AddrinfoClass);
#define SG_CLASS_ADDRINFO (&Sg_AddrinfoClass)
#define SG_ADDRINFO(obj)  ((SgAddrinfo*)obj)
#define SG_ADDRINFOP(obj) SG_XTYPEP(obj, SG_CLASS_ADDRINFO)

typedef enum {
  None,
  IPv4,
  IPv6
} INET_TYPE;
typedef struct SgIpAddressRec
{
  SG_HEADER;
  SgObject ip;
  INET_TYPE type;
} SgIpAddress;

SG_CLASS_DECL(Sg_IpAddressClass);
#define SG_CLASS_IP_ADDRESS  (&Sg_IpAddressClass)
#define SG_IP_ADDRESS(obj)   ((SgIpAddress*)obj)
#define SG_IP_ADDRESS_P(obj) SG_XTYPEP(obj, SG_CLASS_IP_ADDRESS)

typedef struct SgSocketInfoRec
{
  SG_HEADER;
  SgObject  hostname;		/* string */
  SgObject  ipaddress;
  int       port;
} SgSocketInfo;

SG_CLASS_DECL(Sg_SocketInfoClass);
#define SG_CLASS_SOCKET_INFO (&Sg_SocketInfoClass)
#define SG_SOCKET_INFO(obj)  ((SgSocketInfo*)obj)
#define SG_SOCKET_INFO_P(obj) SG_XTYPEP(obj, SG_CLASS_SOCKET_INFO)

SG_CDECL_BEGIN

SG_EXTERN SgAddrinfo* Sg_MakeAddrinfo();
SG_EXTERN SgAddrinfo* Sg_GetAddrinfo(SgObject node, SgObject service,
				     SgAddrinfo *hints);

SG_EXTERN SgObject  Sg_CreateSocket(int family, int socktype, int protocol);

SG_EXTERN SgObject  Sg_SocketSetopt(SgSocket *socket, int level,
				    int opname, SgObject value);
SG_EXTERN SgObject  Sg_SocketGetopt(SgSocket *socket, int level,
				    int opname, int rsize);

SG_EXTERN SgObject  Sg_SocketConnect(SgSocket *socket, SgAddrinfo* addrinfo);
SG_EXTERN SgObject  Sg_SocketBind(SgSocket *socket, SgAddrinfo* addrinfo);
SG_EXTERN SgObject  Sg_SocketListen(SgSocket *socket, int backlog);

SG_EXTERN int       Sg_SocketReceive(SgSocket *socket, uint8_t *data,
				     int size, int flags);
SG_EXTERN int       Sg_SocketSend(SgSocket *socket, uint8_t *data,
				  int size, int flags);
SG_EXTERN SgSocket* Sg_SocketAccept(SgSocket *socket);
SG_EXTERN void      Sg_SocketShutdown(SgSocket *socket, int how);
SG_EXTERN void      Sg_SocketClose(SgSocket *socket);
SG_EXTERN int       Sg_SocketOpenP(SgSocket *socket);

/* misc */
SG_EXTERN SgObject  Sg_SocketErrorMessage(SgSocket *socket);

SG_EXTERN int       Sg_SocketNonblocking(SgSocket *socket);
SG_EXTERN int       Sg_SocketBlocking(SgSocket *socket);

SG_EXTERN SgObject  Sg_MakeSocketPort(SgSocket *socket, int close);
SG_EXTERN SgObject  Sg_MakeSocketInputPort(SgSocket *socket);
SG_EXTERN SgObject  Sg_MakeSocketOutputPort(SgSocket *socket);
SG_EXTERN void      Sg_ShutdownPort(SgPort *port, int how);

/* select */
SG_EXTERN SgObject  Sg_SocketSelect(SgObject reads, SgObject writes,
				    SgObject errors, SgObject timeout);

/* misc */
SG_EXTERN SgObject  Sg_SocketPeer(SgObject socket);
SG_EXTERN SgObject  Sg_SocketName(SgObject socket);
SG_EXTERN SgObject  Sg_SocketInfo(SgObject socket);
SG_EXTERN SgObject  Sg_IpAddressToString(SgObject ip);

SG_CDECL_END

#endif /* SAGITTARIUS_SOCKET_HPP_ */
