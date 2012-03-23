/* -*- C -*- */
/*
 * socket.c
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
#include <sys/types.h>
#include <string.h>
#include <signal.h>
/* we assume _WIN32 is only VC */
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
#define EINTR WSAEINTR
#endif

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "socket.h"

static void socket_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgSocket *socket = SG_SOCKET(self);
  const SgChar *type = (socket->type == SG_SOCKET_CLIENT)
    ? UC("client") : UC("server");
  Sg_Printf(port, UC("#<socket %s %S>"), type, socket->address);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SocketClass, socket_printer);


#ifdef _WIN32
#define last_error WSAGetLastError()
#else
#define last_error errno
#endif

#define set_last_error(sock)			\
  do {						\
    (sock)->lastError = last_error;		\
  } while (0)


static void socket_finalizer(SgObject self, void *data)
{
  Sg_SocketClose(SG_SOCKET(self));
}

static SgSocket* make_socket(int fd, SgSocketType type, SgString *address)
{
  SgSocket *s = SG_NEW(SgSocket);
  SG_SET_CLASS(s, SG_CLASS_SOCKET);
  s->socket = fd;
  s->type = type;
  s->address = address;
  s->lastError = 0;
  Sg_RegisterFinalizer(s, socket_finalizer, NULL);
  return s;
}

static SgString* get_address_string(const struct sockaddr *addr,
				    socklen_t addrlen)
{
  int ret;
  char host[NI_MAXHOST];
  char serv[NI_MAXSERV];
  char name[NI_MAXSERV + NI_MAXHOST + 1];
  do {
    ret = getnameinfo(addr,
		      addrlen,
		      host, sizeof(host),
		      serv, sizeof(serv), NI_NUMERICSERV);
  } while (EAI_AGAIN == ret);
  snprintf(name, sizeof(name), "%s:%s", host, serv);
  return SG_STRING(Sg_MakeStringC(name));
}

SgSocket* Sg_CreateClientSocket(SgString *node,
				SgString *service,
				int ai_family,
				int ai_socktype,
				int ai_flags,
				int ai_protocol)
{
    struct addrinfo hints, *result, *p;
    int ret, lastError = 0;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = ai_family;
    hints.ai_socktype = ai_socktype;
    hints.ai_flags = ai_flags;
    hints.ai_protocol = ai_protocol;
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;

    ASSERT(!((ai_flags & AI_PASSIVE) && node == NULL));
    do {
      const char * cnode = (node != NULL) ? Sg_Utf32sToUtf8s(node) : NULL;
      const char * csrv  = (service != NULL) ? Sg_Utf32sToUtf8s(service) : NULL;
      ret = getaddrinfo(cnode, csrv, &hints, &result);
    } while (EAI_AGAIN == ret);


    if (ret != 0) {
      Sg_IOError((SgIOErrorType)-1, SG_INTERN("create-client-socket"), 
		 Sg_GetLastErrorMessageWithErrorCode(ret),
		 SG_FALSE, SG_LIST2(SG_OBJ(node), SG_OBJ(service)));
      return NULL;
    }

    for (p = result; p != NULL; p = p->ai_next) {
      const int fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
      if (-1 == fd) {
	lastError = last_error;
	continue;
      }
      if (connect(fd, p->ai_addr, p->ai_addrlen) != -1) {
	SgString *addressString = get_address_string(p->ai_addr, p->ai_addrlen);
	freeaddrinfo(result);
	return make_socket(fd, SG_SOCKET_CLIENT, addressString);
      } else {
	lastError = last_error;
#ifdef _WIN32
	shutdown(fd, SD_SEND);
	closesocket(fd);
#else
	close(fd);
#endif
      }
    }
    freeaddrinfo(result);
    Sg_IOError((SgIOErrorType)-1, SG_INTERN("create-client-socket"), 
	       Sg_GetLastErrorMessageWithErrorCode(last_error),
	       SG_FALSE, SG_LIST2(SG_OBJ(node), SG_OBJ(service)));
    return NULL;
}

SgSocket* Sg_CreateServerSocket(SgString *service,
				int ai_family,
				int ai_socktype,
				int ai_protocol)
{
    struct addrinfo hints, *result, *p;
    int ret, lastError = 0;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = ai_family;
    hints.ai_socktype = ai_socktype;
    hints.ai_flags = AI_PASSIVE;
    hints.ai_protocol = ai_protocol;
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;

    do {
      const char * csrv  = (service != NULL) ? Sg_Utf32sToUtf8s(service) : NULL;
      ret = getaddrinfo(NULL, csrv, &hints, &result);
    } while (EAI_AGAIN == ret);


    if (ret != 0) {
      Sg_IOError((SgIOErrorType)-1, SG_INTERN("create-server-socket"), 
		 Sg_GetLastErrorMessageWithErrorCode(last_error),
		 SG_FALSE, SG_NIL);
      return NULL;
    }

    for (p = result; p != NULL; p = p->ai_next) {
      const int fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
      const int TRADITIONAL_BACKLOG = 5;
      int optValue = 1;
      SgString *addressString;
      if (-1 == fd) {
	lastError = last_error;
	continue;
      }
      if(setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char*)&optValue,
		    sizeof(optValue)) == -1) {
#ifdef _WIN32
	shutdown(fd, SD_SEND);
	closesocket(fd);
#else
	close(fd);
#endif
	lastError = last_error;
	continue;
      }

      if (bind(fd, p->ai_addr, p->ai_addrlen) == -1) {
#ifdef _WIN32
	shutdown(fd, SD_SEND);
	closesocket(fd);
#else
	close(fd);
#endif
	lastError = last_error;
	continue;
      }

      if (p->ai_socktype == SOCK_STREAM) {
	if (listen(fd, TRADITIONAL_BACKLOG) == -1) {
#ifdef _WIN32
	  shutdown(fd, SD_SEND);
	  closesocket(fd);
#else
	  close(fd);
#endif
	lastError = last_error;	  
	continue;
	}
      }

      addressString = get_address_string(p->ai_addr, p->ai_addrlen);
      freeaddrinfo(result);
      return make_socket(fd, SG_SOCKET_SERVER, addressString);
    }
    freeaddrinfo(result);
    Sg_IOError((SgIOErrorType)-1, SG_INTERN("create-server-socket"), 
	       Sg_GetLastErrorMessageWithErrorCode(last_error),
	       SG_FALSE, SG_NIL);
    return NULL;
}


int Sg_SocketReceive(SgSocket *socket, uint8_t *data, int size, int flags)
{
  int count = 0, osize = size;
  ASSERT(Sg_SocketOpenP(socket));
  for (;;) {
    const int ret = recv(socket->socket, (char*)data, size, flags);
    if (ret == -1) {
      if (errno == EINTR) {
	continue;
      } else {
	Sg_IOError((SgIOErrorType)-1, SG_INTERN("socket-recv"), 
		   Sg_GetLastErrorMessageWithErrorCode(last_error),
		   SG_FALSE, SG_NIL);
	return ret;
      }
    }
    if (ret == 0) return count;
    count += ret;
    if (count >= osize)
      return count;
    data += ret;
    size -= ret;
  }
}

int Sg_SocketSend(SgSocket *socket, uint8_t *data, int size, int flags)
{
  int rest = size;
  int sizeSent = 0;

  ASSERT(Sg_SocketOpenP(socket));
  while (rest > 0) {
    const int ret = send(socket->socket, (char*)data, size, flags);
    if (ret == -1) {
      if (errno == EINTR) {
	continue;
      } else {
	Sg_IOError((SgIOErrorType)-1, SG_INTERN("socket-send"), 
		   Sg_GetLastErrorMessageWithErrorCode(last_error),
		   SG_FALSE, SG_NIL);
	return ret;
      }
    }
    sizeSent += ret;
    rest -= ret;
    data += ret;
    size -= ret;
  }
  return sizeSent;
}

SgSocket* Sg_SocketAccept(SgSocket *socket)
{
  struct sockaddr_storage addr;
  socklen_t addrlen = sizeof(addr);
  int fd = -1;
  ASSERT(socket->socket != -1);

  for (;;) {
    fd = accept(socket->socket, (struct sockaddr *)&addr, &addrlen);
    if (-1 == fd) {
      if (errno == EINTR) {
	continue;
      } else {
	/* setLastError(); */
	return NULL;
      }
    } else {
      break;
    }
  }
  return make_socket(fd, SG_SOCKET_SERVER,
		     get_address_string((struct sockaddr *)&addr, addrlen));
}

void Sg_SocketShutdown(SgSocket *socket, int how)
{
  if (!Sg_SocketOpenP(socket)) {
    return;
  }
  shutdown(socket->socket, how);
}

void Sg_SocketClose(SgSocket *socket)
{
  if (!Sg_SocketOpenP(socket)) {
    return;
  }
#ifdef _WIN32
  shutdown(socket->socket, SD_SEND);
  closesocket(socket->socket);
#else
  close(socket->socket);
#endif
  socket->socket = -1;

}

int Sg_SocketOpenP(SgSocket *socket)
{
  return socket->socket != -1;
}

static SgPort* make_port(enum SgPortDirection d, enum SgPortType t,
			 enum SgBufferMode m)
{
  SgPort *z = SG_NEW(SgPort);
  SG_SET_CLASS(z, SG_CLASS_PORT);
  z->direction = d;
  z->type = t;
  z->bufferMode = m;
  Sg_InitMutex(&z->lock, TRUE);
  return z;
}

static SgBinaryPort* make_binary_port(enum SgBinaryPortType t, SgSocket *socket)
{
  SgBinaryPort *z = SG_NEW(SgBinaryPort);
  z->type = t;
  z->buffer = NULL;
  z->bufferSize = 0;
  z->bufferIndex = 0;
  z->position = 0;
  z->dirty = EOF;
  z->closed = SG_BPORT_OPEN;
  z->src.custom.data = (void *)socket;
  z->src.custom.position = NULL;
  z->src.custom.setPosition = NULL;
  return z;
}

#define SG_PORT_SOCKET(p) SG_SOCKET(SG_BINARY_PORT(p)->src.custom.data)

static void socket_flush(SgObject self)
{
}

static int socket_open(SgObject self)
{
  return Sg_SocketOpenP(SG_PORT_SOCKET(self));
}

static int socket_close(SgObject self)
{
  if (!SG_PORT(self)->closed) {
    SG_PORT(self)->closed = TRUE;
    Sg_SocketClose(SG_PORT_SOCKET(self));
  }
  return SG_PORT(self)->closed;
}

static int socket_get_u8(SgObject self)
{
  if (SG_PORT_HAS_U8_AHEAD(self)) {
    uint8_t buf = SG_PORT_U8_AHEAD(self);
    SG_PORT_U8_AHEAD(self) = EOF;
    return buf;
  } else {
    uint8_t c;
    const int ret = Sg_SocketReceive(SG_PORT_SOCKET(self), &c, 1, 0);
    if (0 == ret) {
      return EOF;
    } else if (-1 == ret) {
      Sg_IOReadError(SG_INTERN("get-u8"),
		     Sg_GetLastErrorMessageWithErrorCode(SG_PORT_SOCKET(self)->lastError),
		     self);
      return -1;
    } else {
      return c;
    }
  }
}

static int socket_look_ahead_u8(SgObject self)
{
  const uint8_t ret = socket_get_u8(self);
  SG_PORT_U8_AHEAD(self) = ret;
  return ret;
}

static int64_t socket_read_u8(SgObject self, uint8_t *buf, int64_t size)
{
  /* we need to read eagarly, or else something wrong happen. */
  int readSize = 0, offset = 0;
  if (SG_PORT_HAS_U8_AHEAD(self) && size > 0) {
    buf[0] = SG_PORT_U8_AHEAD(self);
    SG_PORT_U8_AHEAD(self) = EOF;
    buf++;
    size--;
    readSize++;
    offset++;
  }
  for (;;) {
    int now = Sg_SocketReceive(SG_PORT_SOCKET(self), buf + offset, size, 0);
    if (-1 == now) {
      Sg_IOReadError(SG_INTERN("read-u8"),
		     Sg_GetLastErrorMessageWithErrorCode(SG_PORT_SOCKET(self)->lastError),
		     self);
      return -1;
    }
    size -= now;
    readSize += now;
    offset += now;
    if (now == 0) return readSize;
    if (size == 0) break;
    /* loop */
  }
  return readSize;
}

static int64_t socket_read_u8_all(SgObject self, uint8_t **buf)
{
  uint8_t read_buf[1024];
  SgObject buffer = Sg_MakeByteArrayOutputPort(1024);
  int mark = 0;
  for (;;) {
    int read_size = Sg_SocketReceive(SG_PORT_SOCKET(self), read_buf, 1024, 0);
    if (-1 == read_size) {
      Sg_IOReadError(SG_INTERN("read-u8-all"),
		     Sg_GetLastErrorMessageWithErrorCode(SG_PORT_SOCKET(self)->lastError),
		     self);
      return -1;
    } else if (0 == read_size) {
      break;
    } else {
      Sg_WritebUnsafe(SG_PORT(buffer), read_buf, 0, read_size);
      mark += read_size;
    }
  }
  *buf = Sg_GetByteArrayFromBinaryPort(SG_PORT(buffer));
  return mark;
}


static int64_t socket_put_u8_array(SgObject self, uint8_t *v, int64_t size)
{
  int64_t written_size = Sg_SocketSend(SG_PORT_SOCKET(self), v, size, 0);
  if (-1 == written_size) {
    Sg_IOWriteError(SG_INTERN("read-u8"),
		    Sg_GetLastErrorMessageWithErrorCode(SG_PORT_SOCKET(self)->lastError),
		    self);
    return -1;
  }
  return written_size;
}


static int64_t socket_put_u8(SgObject self, uint8_t v)
{
  return socket_put_u8_array(self, &v, 1);
}

SgObject Sg_MakeSocketPort(SgSocket *socket)
{
  SgPort *z = make_port(SG_IN_OUT_PORT, SG_BINARY_PORT_TYPE, SG_BUFMODE_NONE);
  SgBinaryPort *b = make_binary_port(SG_CUSTOM_BINARY_PORT_TYPE, socket);

  z->closed = FALSE;
  z->flush = socket_flush;
  z->close = socket_close;
  z->impl.bport = b;

  b->open = socket_open;
  b->getU8 = socket_get_u8;
  b->lookAheadU8 = socket_look_ahead_u8;
  b->readU8 = socket_read_u8;
  b->readU8All = socket_read_u8_all;
  b->putU8 = socket_put_u8;
  b->putU8Array = socket_put_u8_array;

  b->bufferWriter = NULL;
  SG_PORT_U8_AHEAD(z) = EOF;
  return SG_OBJ(z);
}

void Sg_ShutdownPort(SgPort *port)
{
  if (SG_BINARY_PORT(port)->type != SG_BINARY_CUSTOM_PORT_TYPE ||
      !SG_SOCKETP(SG_PORT_SOCKET(port))) {
    Sg_Error(UC("socket port required but got %S"), port);
  }
  if (!Sg_PortClosedP(port)) {
    Sg_FlushPort(port);
    Sg_SocketShutdown(SG_PORT_SOCKET(port), 1);
  }
}

extern void Sg__Init_sagittarius_socket_impl();

#ifdef _WIN32
static void finish_winsock(void *data)
{
  WSACleanup();
}
#endif

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__socket()
{
  SgLibrary *lib;
#ifdef _WIN32
  WSADATA wsaData;
  WSAStartup(2, &wsaData);
  Sg_AddCleanupHandler(finish_winsock, NULL);
#endif
  SG_INIT_EXTENSION(sagittarius__socket);
  Sg__Init_sagittarius_socket_impl();
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius socket impl)"),
				  FALSE));

  /* for multithreading issue, we do not add this cond-feature.
     we always support this anyway */
  /* Sg_AddCondFeature(UC("sagittarius.socket")); */
  Sg_InitStaticClassWithMeta(SG_CLASS_SOCKET, UC("<socket>"), lib, NULL,
			     SG_FALSE, NULL, 0);
  /* from Ypsilon */
#define ARCH_CCONST(name)					\
  Sg_MakeBinding(lib, SG_SYMBOL(SG_INTERN(#name)), SG_MAKE_INT(name), TRUE)
#define ARCH_CFALSE(name)			\
  Sg_MakeBinding(lib, SG_SYMBOL(SG_INTERN(#name)), SG_FALSE, TRUE)
#ifdef AF_UNSPEC
  ARCH_CCONST(AF_UNSPEC);
#else
  ARCH_CFALSE(AF_UNSPEC);
#endif
#ifdef AF_INET
  ARCH_CCONST(AF_INET);
#else
  ARCH_CFALSE(AF_INET);
#endif
#ifdef AF_INET6
  ARCH_CCONST(AF_INET6);
#else
  ARCH_CFALSE(AF_INET6);
#endif
#ifdef SOCK_STREAM
  ARCH_CCONST(SOCK_STREAM);
#else
  ARCH_CFALSE(SOCK_STREAM);
#endif
#ifdef SOCK_DGRAM
  ARCH_CCONST(SOCK_DGRAM);
#else
  ARCH_CFALSE(SOCK_DGRAM);
#endif
#ifdef SOCK_RAW
  ARCH_CCONST(SOCK_RAW);
#else
  ARCH_CFALSE(SOCK_RAW);
#endif
#ifdef SOCK_RDM
  ARCH_CCONST(SOCK_RDM);
#else
  ARCH_CFALSE(SOCK_RDM);
#endif
#ifdef SOCK_SEQPACKET
  ARCH_CCONST(SOCK_SEQPACKET);
#else
  ARCH_CFALSE(SOCK_SEQPACKET);
#endif
#ifdef AI_PASSIVE
  ARCH_CCONST(AI_PASSIVE);
#else
  ARCH_CFALSE(AI_PASSIVE);
#endif
#ifdef AI_CANONNAME
  ARCH_CCONST(AI_CANONNAME);
#else
  ARCH_CFALSE(AI_CANONNAME);
#endif
#ifdef AI_NUMERICHOST
  ARCH_CCONST(AI_NUMERICHOST);
#else
  ARCH_CFALSE(AI_NUMERICHOST);
#endif
#ifdef AI_V4MAPPED
  ARCH_CCONST(AI_V4MAPPED);
#else
  ARCH_CFALSE(AI_V4MAPPED);
#endif
#ifdef AI_ALL
  ARCH_CCONST(AI_ALL);
#else
  ARCH_CFALSE(AI_ALL);
#endif
#ifdef AI_ADDRCONFIG
  ARCH_CCONST(AI_ADDRCONFIG);
#else
  ARCH_CFALSE(AI_ADDRCONFIG);
#endif
#ifdef SHUT_RD
  ARCH_CCONST(SHUT_RD);
#else
  ARCH_CFALSE(SHUT_RD);
#endif
#ifdef SHUT_WR
  ARCH_CCONST(SHUT_WR);
#else
  ARCH_CFALSE(SHUT_WR);
#endif
#ifdef SHUT_RDWR
  ARCH_CCONST(SHUT_RDWR);
#else
  ARCH_CFALSE(SHUT_RDWR);
#endif
#ifdef MSG_OOB
  ARCH_CCONST(MSG_OOB);
#else
  ARCH_CFALSE(MSG_OOB);
#endif
#ifdef MSG_PEEK
  ARCH_CCONST(MSG_PEEK);
#else
  ARCH_CFALSE(MSG_PEEK);
#endif
#ifdef MSG_DONTROUTE
  ARCH_CCONST(MSG_DONTROUTE);
#else
  ARCH_CFALSE(MSG_DONTROUTE);
#endif
#ifdef MSG_CTRUNC
  ARCH_CCONST(MSG_CTRUNC);
#else
  ARCH_CFALSE(MSG_CTRUNC);
#endif
#ifdef MSG_PROBE
  ARCH_CCONST(MSG_PROBE);
#else
  ARCH_CFALSE(MSG_PROBE);
#endif
#ifdef MSG_TRUNC
  ARCH_CCONST(MSG_TRUNC);
#else
  ARCH_CFALSE(MSG_TRUNC);
#endif
#ifdef MSG_DONTWAIT
  ARCH_CCONST(MSG_DONTWAIT);
#else
  ARCH_CFALSE(MSG_DONTWAIT);
#endif
#ifdef MSG_EOR
  ARCH_CCONST(MSG_EOR);
#else
  ARCH_CFALSE(MSG_EOR);
#endif
#ifdef MSG_WAITALL
  ARCH_CCONST(MSG_WAITALL);
#else
  ARCH_CFALSE(MSG_WAITALL);
#endif
#ifdef MSG_FIN
  ARCH_CCONST(MSG_FIN);
#else
  ARCH_CFALSE(MSG_FIN);
#endif
#ifdef MSG_SYN
  ARCH_CCONST(MSG_SYN);
#else
  ARCH_CFALSE(MSG_SYN);
#endif
#ifdef MSG_CONFIRM
  ARCH_CCONST(MSG_CONFIRM);
#else
  ARCH_CFALSE(MSG_CONFIRM);
#endif
#ifdef MSG_RST
  ARCH_CCONST(MSG_RST);
#else
  ARCH_CFALSE(MSG_RST);
#endif
#ifdef MSG_ERRQUEUE
  ARCH_CCONST(MSG_ERRQUEUE);
#else
  ARCH_CFALSE(MSG_ERRQUEUE);
#endif
#ifdef MSG_NOSIGNAL
  ARCH_CCONST(MSG_NOSIGNAL);
#else
  ARCH_CFALSE(MSG_NOSIGNAL);
#endif
#ifdef MSG_MORE
  ARCH_CCONST(MSG_MORE);
#else
  ARCH_CFALSE(MSG_MORE);
#endif
#ifdef MSG_EOF
  ARCH_CCONST(MSG_EOF);
#else
  ARCH_CFALSE(MSG_EOF);
#endif
#undef ARCH_CCONST
#undef ARCH_CFALSE

}
