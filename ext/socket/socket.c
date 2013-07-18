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
#define EINTR  WSAEINTR
#define EAGAIN WSATRY_AGAIN
#define EWOULDBLOCK WSAEWOULDBLOCK
#define EPIPE WSAEINVAL
#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0		/* no support */
#endif
#endif

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "socket.h"

static void socket_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgSocket *socket = SG_SOCKET(self);
  const SgChar *type = (socket->type == SG_SOCKET_CLIENT)
    ? UC("client") : (socket->type == SG_SOCKET_SERVER)
    ? UC("server") : UC("unknown");
  SgObject address = (socket->address != NULL) ? socket->address: SG_FALSE;
  Sg_Printf(port, UC("#<socket %s %S>"), type, address);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SocketClass, socket_printer);


static SgString* get_address_string_rec(const struct sockaddr *addr,
					socklen_t addrlen, int port_p)
{
  int ret;
  char host[NI_MAXHOST];
  char ip[NI_MAXHOST];
  char serv[NI_MAXSERV];
  char name[NI_MAXSERV + (NI_MAXHOST<<1) + 1];
  do {
    ret = getnameinfo(addr,
		      addrlen,
		      host, sizeof(host),
		      serv, sizeof(serv), 
		      NI_NUMERICSERV);
  } while (EAI_AGAIN == ret);
  do {
    ret = getnameinfo(addr,
		      addrlen,
		      ip, sizeof(ip),
		      serv, sizeof(serv), 
		      NI_NUMERICSERV | NI_NUMERICHOST);
  } while (EAI_AGAIN == ret);
  if (port_p) {
    snprintf(name, sizeof(name), "%s(%s):%s", host, ip, serv);
  } else {
    snprintf(name, sizeof(name), "%s", host);
  }
  return SG_STRING(Sg_MakeStringC(name));
}

static SgString* get_address_string(const struct sockaddr *addr,
				    socklen_t addrlen)
{
  return get_address_string_rec(addr, addrlen, TRUE);
}

static void addrinfo_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgAddrinfo *info = SG_ADDRINFO(self);
  SgObject addr = get_address_string(info->ai->ai_addr, info->ai->ai_addrlen);
  Sg_Printf(port, UC("#<addrinfo %A>"), addr);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_AddrinfoClass, addrinfo_printer);

#define IPv4_INADDER_SIZE 0x4
#define IPv6_INADDER_SIZE 0x10
#define IPv6_INT16_SIZE   0x2

static SgObject bytevector_to_v4_string(SgObject bv)
{
  /* bv must have 4 length */
  ASSERT(SG_BVECTOR_SIZE(bv) >= IPv4_INADDER_SIZE);
  return Sg_Sprintf(UC("%d.%d.%d.%d"),
		    SG_BVECTOR_ELEMENT(bv, 0),
		    SG_BVECTOR_ELEMENT(bv, 1),
		    SG_BVECTOR_ELEMENT(bv, 2),
		    SG_BVECTOR_ELEMENT(bv, 3));
}

static SgObject bytevector_to_v6_string(SgObject bv)
{
  static const char table[] = "0123456789abcdef";
  SgObject sp = Sg_MakeStringOutputPort(39);
  int i;
  for (i = 0; i < (IPv6_INADDER_SIZE / IPv6_INT16_SIZE); i++) {
    int hi = SG_BVECTOR_ELEMENT(bv, (i<<1));
    int lo = SG_BVECTOR_ELEMENT(bv, ((i<<1)+1));
    Sg_PutcUnsafe(SG_PORT(sp), table[hi]);
    Sg_PutcUnsafe(SG_PORT(sp), table[lo]);
    if (i < (IPv6_INADDER_SIZE / IPv6_INT16_SIZE) - 1) {
      Sg_PutcUnsafe(SG_PORT(sp), ':');
    }
  }
  return Sg_GetStringFromStringPort(SG_PORT(sp));
}

static SgObject ip_to_string(SgIpAddress *ip)
{
  switch (ip->type) {
  case IPv4: return bytevector_to_v4_string(ip->ip);
  case IPv6: return bytevector_to_v6_string(ip->ip);
  default: return SG_FALSE;
  }
}

static void ip_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<ip-address %A>"), ip_to_string(SG_IP_ADDRESS(self)));
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_IpAddressClass, ip_printer);

static void socktinfo_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgSocketInfo *info = SG_SOCKET_INFO(self);
  Sg_Printf(port, UC("#<socket-info %A(%A:%d)>"),
	    info->hostname, info->ipaddress, info->port);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SocketInfoClass, socktinfo_printer);

static SgIpAddress *make_ip_address(struct sockaddr_storage *info)
{
  SgIpAddress *z = SG_NEW(SgIpAddress);
  SgObject ip;
  SG_SET_CLASS(z, SG_CLASS_IP_ADDRESS);
  if (info->ss_family == AF_INET) {
    struct sockaddr_in *s = (struct sockaddr_in *)info;
    ip = Sg_MakeByteVector(IPv4_INADDER_SIZE, 0);
    memcpy(SG_BVECTOR_ELEMENTS(ip), (void *)&s->sin_addr.s_addr,
	   IPv4_INADDER_SIZE);
    z->type = IPv4;
  } else {			/* AF_INET6 */
    struct sockaddr_in6 *s = (struct sockaddr_in6 *)info;
    ip = Sg_MakeByteVector(IPv6_INADDER_SIZE, 0);
    memcpy(SG_BVECTOR_ELEMENTS(ip), (void *)&s->sin6_addr.s6_addr,
	   IPv6_INADDER_SIZE);
    z->type = IPv6;
  }
  z->ip = ip;
  return z;
}

static SgSocketInfo* make_socket_info(struct sockaddr_storage *info)
{
  SgSocketInfo *si = SG_NEW(SgSocketInfo);
  int port, addr_len;
  SgObject ip, host;
  SG_SET_CLASS(si, SG_CLASS_SOCKET_INFO);
  if (info->ss_family == AF_INET) {
    struct sockaddr_in *s = (struct sockaddr_in *)info;
    port = ntohs(s->sin_port);
    addr_len = sizeof(struct sockaddr_in);
  } else {			/* AF_INET6 */
    struct sockaddr_in6 *s = (struct sockaddr_in6 *)info;
    port = ntohs(s->sin6_port);
    addr_len = sizeof(struct sockaddr_in6);
  }
  ip = make_ip_address(info);
  host = get_address_string_rec((struct sockaddr *)info, addr_len, FALSE);
  si->hostname = host;
  si->ipaddress = ip;
  si->port = port;
  return si;
}

static SgObject si_hostname(SgSocketInfo *si)
{
  return si->hostname;
}
static SgObject si_ip_address(SgSocketInfo *si)
{
  return si->ipaddress;
}
static SgObject si_port(SgSocketInfo *si)
{
  return SG_MAKE_INT(si->port);
}

static SgSlotAccessor si_slots[] = {
  SG_CLASS_SLOT_SPEC("hostname",   0, si_hostname, NULL),
  SG_CLASS_SLOT_SPEC("ip-address", 1, si_ip_address, NULL),
  SG_CLASS_SLOT_SPEC("port",       2, si_port, NULL),
  { { NULL } }
};

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

static SgSocket* make_socket_inner(SOCKET fd)
{
  SgSocket *s = SG_NEW(SgSocket);
  SG_SET_CLASS(s, SG_CLASS_SOCKET);
  s->socket = fd;
  Sg_RegisterFinalizer(s, socket_finalizer, NULL);
  s->type = SG_SOCKET_UNKNOWN;
  s->address = NULL;
  return s;
}

static SgSocket* make_socket(SOCKET fd, SgSocketType type, SgString *address)
{
  SgSocket *s = make_socket_inner(fd);
  s->type = type;
  s->address = address;
  s->lastError = 0;
  return s;
}

static SgAddrinfo* make_addrinfo()
{
  SgAddrinfo *info = SG_NEW(SgAddrinfo);
  SG_SET_CLASS(info, SG_CLASS_ADDRINFO);
  return info;
}

/* accessors for addressinfo */
static SgObject ai_flags(SgAddrinfo *ai)
{
  return SG_MAKE_INT(ai->ai->ai_flags);
}
static void ai_flags_set(SgAddrinfo *ai, SgObject flags)
{
  if (!SG_INTP(flags)) Sg_Error(UC("fixnum required but got %S"), flags);
  ai->ai->ai_flags = SG_INT_VALUE(flags);
}

static SgObject ai_family(SgAddrinfo *ai)
{
  return SG_MAKE_INT(ai->ai->ai_family);
}
static void ai_family_set(SgAddrinfo *ai, SgObject family)
{
  if (!SG_INTP(family)) Sg_Error(UC("fixnum required but got %S"), family);
  ai->ai->ai_family = SG_INT_VALUE(family);
}

static SgObject ai_socktype(SgAddrinfo *ai)
{
  return SG_MAKE_INT(ai->ai->ai_socktype);
}
static void ai_socktype_set(SgAddrinfo *ai, SgObject socktype)
{
  if (!SG_INTP(socktype)) Sg_Error(UC("fixnum required but got %S"), socktype);
  ai->ai->ai_socktype = SG_INT_VALUE(socktype);
}

static SgObject ai_protocol(SgAddrinfo *ai)
{
  return SG_MAKE_INT(ai->ai->ai_protocol);
}
static void ai_protocol_set(SgAddrinfo *ai, SgObject protocol)
{
  if (!SG_INTP(protocol)) Sg_Error(UC("fixnum required but got %S"), protocol);
  ai->ai->ai_protocol = SG_INT_VALUE(protocol);
}

static SgObject ai_next(SgAddrinfo *ai)
{
  if (ai->ai->ai_next) {
    SgAddrinfo *info = make_addrinfo();
    info->ai = ai->ai->ai_next;
    return info;
  }
  return SG_FALSE;
}

static SgSlotAccessor ai_slots[] = {
  SG_CLASS_SLOT_SPEC("flags",    0, ai_flags, ai_flags_set),
  SG_CLASS_SLOT_SPEC("family", 1, ai_family, ai_family_set),
  SG_CLASS_SLOT_SPEC("socktype", 2, ai_socktype, ai_socktype_set),
  SG_CLASS_SLOT_SPEC("protocol", 3, ai_protocol, ai_protocol_set),
  SG_CLASS_SLOT_SPEC("next", 4, ai_next, NULL),
  { { NULL } }
};

static void addrinfo_finalizer(SgObject self, void *data)
{
  freeaddrinfo(SG_ADDRINFO(self)->ai);
}

SgAddrinfo* Sg_MakeAddrinfo()
{
  SgAddrinfo *info = make_addrinfo();
  info->ai = SG_NEW(struct addrinfo);
  memset(info->ai, 0, sizeof(struct addrinfo));
  return info;
}

SgAddrinfo* Sg_GetAddrinfo(SgObject node, SgObject service, SgAddrinfo *hints)
{
  const char * cnode = (!SG_FALSEP(node)) ?
    Sg_Utf32sToUtf8s(SG_STRING(node)) : NULL;
  const char * csrv  = (!SG_FALSEP(service)) ?
    Sg_Utf32sToUtf8s(SG_STRING(service)) : NULL;
  int ret;
  SgAddrinfo *result = make_addrinfo();
  do {
    ret = getaddrinfo(cnode, csrv, hints->ai, &(result->ai));
  } while (EAI_AGAIN == ret);

  if (ret != 0) {
    Sg_IOError((SgIOErrorType)-1, SG_INTERN("get-addrinfo"), 
	       Sg_GetLastErrorMessageWithErrorCode(ret),
	       SG_FALSE, SG_LIST2(SG_OBJ(node), SG_OBJ(service)));
    return NULL;
  }

  Sg_RegisterFinalizer(result, addrinfo_finalizer, NULL);
  return result;
}

SgObject Sg_CreateSocket(int family, int socktype, int protocol)
{
  const SOCKET fd = socket(family, socktype, protocol);
  if (-1 == fd) {
    return SG_FALSE;
  }
  return make_socket_inner(fd);
}

SgObject Sg_SocketConnect(SgSocket *socket, SgAddrinfo* addrinfo)
{
  struct addrinfo *p = addrinfo->ai;
  if (connect(socket->socket, p->ai_addr, p->ai_addrlen) == 0) {
    socket->type = SG_SOCKET_CLIENT;
    socket->address = get_address_string(p->ai_addr, p->ai_addrlen);
    return socket;
  }
  socket->lastError = last_error;
  return SG_FALSE;
}

SgObject Sg_SocketBind(SgSocket *socket, SgAddrinfo* addrinfo)
{
  struct addrinfo *p = addrinfo->ai;
  if (bind(socket->socket, p->ai_addr, p->ai_addrlen) == 0) {
    socket->type = SG_SOCKET_SERVER;
    socket->address = get_address_string(p->ai_addr, p->ai_addrlen);
    return socket;
  }
  socket->lastError = last_error;
  return SG_FALSE;
}

SgObject Sg_SocketListen(SgSocket *socket, int backlog)
{
  if (listen(socket->socket, backlog) == 0) {
    return socket;
  }
  socket->lastError = last_error;
  return SG_FALSE;
}

#define CLOSE_SOCKET(who, socket)			\
  do {							\
    if (!Sg_SocketOpenP(socket))			\
      Sg_IOError((SgIOErrorType)-1, SG_INTERN(who),	\
		 SG_MAKE_STRING("socket is closed"),	\
		 SG_FALSE, SG_NIL);			\
  } while (0)


SgObject Sg_SocketSetopt(SgSocket *socket, int level, int name, SgObject value)
{
  int r = 0;
  CLOSE_SOCKET("socket-setsockopt!",socket);
  if (SG_BVECTORP(value)) {
    r = setsockopt(socket->socket, level, name,
		   (const char *)SG_BVECTOR_ELEMENTS(value), 
		   SG_BVECTOR_SIZE(value));
  } else if (SG_INTP(value) || SG_BIGNUMP(value)) {
    int v = Sg_GetInteger(value);
    r = setsockopt(socket->socket, level, name, (const char *)&v, sizeof(int));
  } else {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("socket-setsockopt!"),
				    SG_MAKE_STRING("bytevector or integer"),
				    value, SG_NIL);
  }
  if (r != 0) {
    socket->lastError = last_error;
    return SG_FALSE;
  }
  return SG_TRUE;
}
SgObject Sg_SocketGetopt(SgSocket *socket, int level, int name, int rsize)
{
  int r = 0;
  socklen_t rrsize = rsize;
  CLOSE_SOCKET("socket-getsockopt", socket);
  if (rsize > 0) {
    SgObject bvec = Sg_MakeByteVector(rrsize, 0);
    r = getsockopt(socket->socket, level, name, 
		   (char *)SG_BVECTOR_ELEMENTS(bvec), &rrsize);
    if (r < 0) {
      Sg_IOError((SgIOErrorType)-1, SG_INTERN("socket-getsockopt"), 
		 Sg_GetLastErrorMessageWithErrorCode(last_error),
		 SG_FALSE, SG_NIL);
    }
    SG_BVECTOR_SIZE(bvec) = rrsize;
    return SG_OBJ(bvec);
  } else {
    int val;
    rrsize = sizeof(int);
    r = getsockopt(socket->socket, level, name, (char *)&val, &rrsize);
    if (r < 0) {
      Sg_IOError((SgIOErrorType)-1, SG_INTERN("socket-getsockopt"), 
		 Sg_GetLastErrorMessageWithErrorCode(last_error),
		 SG_FALSE, SG_NIL);
    }
    return Sg_MakeInteger(val);
  }
}

int Sg_SocketReceive(SgSocket *socket, uint8_t *data, int size, int flags)
{
  /* int count = 0, osize = size; */
  CLOSE_SOCKET("socket-recv", socket);
  for (;;) {
    const int ret = recv(socket->socket, (char*)data, size,
			 /* we don't want SIGPIPE */
			 flags | MSG_NOSIGNAL);
    if (ret == -1) {
      if (errno == EINTR) {
	continue;
      } else if (errno == EPIPE) {
	if (flags & MSG_NOSIGNAL) {
	  return 0;
	} else {
	  goto err;
	}
      } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
	/* most probably non-blocking socket */
	return ret;
      } else {
      err:
	Sg_IOError((SgIOErrorType)-1, SG_INTERN("socket-recv"), 
		   Sg_GetLastErrorMessageWithErrorCode(last_error),
		   SG_FALSE, SG_NIL);
	return ret;
      }
    }
    return ret;
  }
}

int Sg_SocketSend(SgSocket *socket, uint8_t *data, int size, int flags)
{
  int rest = size;
  int sizeSent = 0;

  CLOSE_SOCKET("socket-send", socket);
  while (rest > 0) {
    const int ret = send(socket->socket, (char*)data, size, 
			 /* we don't want SIGPIPE */
			 flags | MSG_NOSIGNAL);
    if (ret == -1) {
      if (errno == EINTR) {
	continue;
      } else if (errno == EPIPE) {
	if (flags & MSG_NOSIGNAL) {
	  return 0;
	} else {
	  goto err;
	}	
      } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
	/* most probably non-blocking socket */
	continue;
      } else {
      err:
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
  SOCKET fd = -1;

  CLOSE_SOCKET("socket-accept", socket);

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
  /* FIXME socket-close should not shutdown socket but we don't have
     any way to flush socket other than shutting down write side of
     socket descriptor on Windows. */
  shutdown(socket->socket, SD_SEND);
  closesocket(socket->socket);
#else
  close(socket->socket);
#endif
  socket->socket = -1;

}

static int collect_max_fd(int max, SgObject sockets, fd_set *fds)
{
  SgObject cp;
  FD_ZERO(fds);
  SG_FOR_EACH(cp, sockets) {
    SOCKET fd;
    if (!SG_SOCKETP(SG_CAR(cp))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("socket-select"),
				      SG_MAKE_STRING("socket"),
				      SG_CAR(cp), sockets);
    }
    fd = SG_SOCKET(SG_CAR(cp))->socket;
    if (max < fd) max = fd;
    FD_SET(fd, fds);
  }
  return max;
}

static struct timeval *select_timeval(SgObject timeout, struct timeval *tm)
{
  if (SG_FALSEP(timeout)) return NULL;
  if (SG_INTP(timeout)) {
    int val = SG_INT_VALUE(timeout);
    if (val < 0) goto badtv;
    tm->tv_sec = val / 1000000;
    tm->tv_usec = val % 1000000;
    return tm;
  } else if (SG_BIGNUMP(timeout)) {
    long usec;
    SgObject sec;
    if (Sg_Sign(timeout) < 0) goto badtv;
    sec = Sg_BignumDivSI(SG_BIGNUM(timeout), 1000000, &usec);
    tm->tv_sec = Sg_GetInteger(sec);
    tm->tv_usec = usec;
    return tm;
  } else if (SG_FLONUMP(timeout)) {
    long val = Sg_GetInteger(timeout);
    if (val < 0) goto badtv;
    tm->tv_sec = val / 1000000;
    tm->tv_usec = val % 1000000;
    return tm;
  } else if (SG_PAIRP(timeout) && SG_PAIRP(SG_CDR(timeout))) {
    SgObject sec = SG_CAR(timeout);
    SgObject usec = SG_CADR(timeout);
    long isec, iusec;
    if (!Sg_IntegerP(sec) || !Sg_IntegerP(usec)) goto badtv;
    isec = Sg_GetInteger(sec);
    iusec = Sg_GetInteger(usec);
    if (isec < 0 || iusec < 0) goto badtv;
    tm->tv_sec = isec;
    tm->tv_usec = iusec;
    return tm;
  }
 badtv:
  Sg_Error(UC("timeval needs to be a real number (in microseconds) or a list"
	      " of two integers (seconds and microseconds), but got %S"),
	   timeout);
  return NULL;                /* dummy */
}

static SgObject collect_fds(SgObject sockets, fd_set *fds)
{
  SgObject h = SG_NIL, t = SG_NIL;
  SG_FOR_EACH(sockets, sockets) {
    SgSocket *socket = SG_SOCKET(SG_CAR(sockets));
    if (FD_ISSET(socket->socket, fds)) {
      SG_APPEND1(h, t, socket);
    }
  }
  return h;
}

SgObject Sg_SocketSelect(SgObject reads, SgObject writes, SgObject errors,
			 SgObject timeout)
{
  int max = 0, numfds;
  fd_set readfds, writefds, errorfds;
  struct timeval tv;
  SgObject rr, wr, er;
  max = collect_max_fd(max, reads, &readfds);
  max = collect_max_fd(max, writes, &writefds);
  max = collect_max_fd(max, errors, &errorfds);

  numfds = select(max + 1, &readfds, &writefds, &errorfds,
		  select_timeval(timeout, &tv));

  if (numfds < 0) {
    Sg_IOError((SgIOErrorType)-1, SG_INTERN("socket-select"), 
	       Sg_GetLastErrorMessageWithErrorCode(last_error),
	       SG_FALSE, SG_NIL);
  }

  rr = collect_fds(reads, &readfds);
  wr = collect_fds(writes, &writefds);
  er = collect_fds(errors, &errorfds);
  return Sg_Values3(rr, wr, er);
}

SgObject Sg_SocketPeer(SgObject socket)
{
  struct sockaddr_storage name;
  int len = sizeof(name), ret;
  ret = getpeername(SG_SOCKET(socket)->socket, (struct sockaddr *)&name, &len);
  if (ret == 0) {
    return make_socket_info(&name);
  } else {
    return SG_FALSE;
  }
}

SgObject Sg_SocketName(SgObject socket)
{
  SgObject address = SG_SOCKET(socket)->address;
  if (address) return address;
  else return SG_FALSE;
}

SgObject Sg_SocketInfo(SgObject socket)
{
  struct sockaddr_storage name;
  int len = sizeof(name), ret;
  ret = getsockname(SG_SOCKET(socket)->socket, (struct sockaddr *)&name, &len);
  if (ret == 0) {
    return make_socket_info(&name);
  } else {
    return SG_FALSE;
  }
}

SgObject Sg_IpAddressToString(SgObject ip)
{
  return ip_to_string(SG_IP_ADDRESS(ip));
}

int Sg_SocketOpenP(SgSocket *socket)
{
  return socket->socket != -1;
}

int Sg_SocketNonblocking(SgSocket *socket)
{
#if _WIN32
  unsigned long val = 1;
  if (ioctlsocket(socket->socket, FIONBIO, &val) != 0) {
    goto err;
  }
#else
  int flags = fcntl(socket->socket, F_GETFL, 0);
  flags &= ~O_SYNC;
  if (fcntl(socket->socket, F_SETFL, flags | O_NONBLOCK) != 0) {
    goto err;
  }
#endif
  return TRUE;
 err:
  Sg_IOError((SgIOErrorType)-1, SG_INTERN("socket-nonblocking!"), 
	     Sg_GetLastErrorMessageWithErrorCode(last_error),
	     SG_FALSE, socket);
  return FALSE;			/* dummy */
}
int Sg_SocketBlocking(SgSocket *socket)
{
#if _WIN32
  unsigned long val = 0;
  int err;
  if ((err = ioctlsocket(socket->socket, FIONBIO, &val)) != 0) {
    goto err;
  }
#else
  int flags = fcntl(socket->socket, F_GETFL, 0);
  flags &= ~O_NONBLOCK;
  if (fcntl(socket->socket, F_SETFL, flags | O_SYNC) != 0) {
    goto err;
  }
#endif
  return TRUE;
 err:
  Sg_IOError((SgIOErrorType)-1, SG_INTERN("socket-blocking!"), 
	     Sg_GetLastErrorMessageWithErrorCode(last_error),
	     SG_FALSE, socket);
  return FALSE;			/* dummy */  
}

SgObject Sg_SocketErrorMessage(SgSocket *socket)
{
  return Sg_GetLastErrorMessageWithErrorCode(socket->lastError);
}

static SgPort* make_port(enum SgPortDirection d, enum SgPortType t,
			 enum SgBufferMode m)
{
  SgPort *z = SG_NEW(SgPort);
  SG_INIT_PORT(z, d, t, m);
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

static int socket_close_only_port(SgObject self)
{
  if (!SG_PORT(self)->closed) {
    SG_PORT(self)->closed = TRUE;
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
      SG_BINARY_PORT(self)->position += ret;
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
  int readSize = 0;
  if (SG_PORT_HAS_U8_AHEAD(self) && size > 0) {
    buf[0] = SG_PORT_U8_AHEAD(self);
    SG_PORT_U8_AHEAD(self) = EOF;
    buf++;
    size--;
    readSize++;
  }
  for (;;) {
    int now = Sg_SocketReceive(SG_PORT_SOCKET(self), buf + readSize, size, 0);
    if (-1 == now) {
      Sg_IOReadError(SG_INTERN("read-u8"),
		     Sg_GetLastErrorMessageWithErrorCode(SG_PORT_SOCKET(self)->lastError),
		     self);
      return -1;
    }
    size -= now;
    readSize += now;
    if (now == 0) break;
    if (size == 0) break;
    /* loop */
  }
  SG_BINARY_PORT(self)->position += readSize;
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
    } else {
      Sg_WritebUnsafe(SG_PORT(buffer), read_buf, 0, read_size);
      if (1024 != read_size) {
	mark += read_size;
	break;
      } else {
	mark += read_size;
      }
    }
  }
  SG_BINARY_PORT(self)->position += mark;
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

static int socket_ready(SgObject self)
{
  SgObject socket = SG_PORT_SOCKET(self);
  struct timeval tm = {0, 0};
  fd_set fds;
  int state;
  FD_ZERO(&fds);
  FD_SET(SG_SOCKET(socket)->socket, &fds);
  state = select(SG_SOCKET(socket)->socket + 1, &fds, NULL, NULL, &tm);
  if (state < 0) {
    if (last_error == EINTR) return FALSE;
    Sg_IOError((SgIOErrorType)-1, SG_INTERN("port-ready?"), 
	       Sg_GetLastErrorMessageWithErrorCode(last_error),
	       SG_FALSE, SG_NIL);
    return FALSE;
  }
  return (state != 0);
}

static inline SgObject make_socket_port(SgSocket *socket,
					enum SgPortDirection d, 
					int closeP)
{
  SgPort *z = make_port(d, SG_BINARY_PORT_TYPE, SG_BUFMODE_NONE);
  SgBinaryPort *b = make_binary_port(SG_CUSTOM_BINARY_PORT_TYPE, socket);

  z->closed = FALSE;
  z->flush = socket_flush;
  if (closeP) {
    z->close = socket_close;
  } else {
    z->close = socket_close_only_port;
  }
  z->ready = socket_ready;
  z->impl.bport = b;

  b->open = socket_open;
  switch (d) {
  case SG_INPUT_PORT: case SG_IN_OUT_PORT:
    b->getU8 = socket_get_u8;
    b->lookAheadU8 = socket_look_ahead_u8;
    b->readU8 = socket_read_u8;
    b->readU8All = socket_read_u8_all;
    break;
  default: break;
  }
  switch (d) {
  case SG_OUTPUT_PORT: case SG_IN_OUT_PORT:
    b->putU8 = socket_put_u8;
    b->putU8Array = socket_put_u8_array;
  default: break;
  }
  b->bufferWriter = NULL;
  SG_PORT_U8_AHEAD(z) = EOF;
  return SG_OBJ(z);
}

SgObject Sg_MakeSocketPort(SgSocket *socket, int closeP)
{
  return make_socket_port(socket, SG_IN_OUT_PORT, closeP);
}

SgObject  Sg_MakeSocketInputPort(SgSocket *socket)
{
  /* I hope compiler is smart enough to remove if and switch. */
  return make_socket_port(socket, SG_INPUT_PORT, FALSE);
}
SgObject  Sg_MakeSocketOutputPort(SgSocket *socket)
{
  /* I hope compiler is smart enough to remove if and switch. */
  return make_socket_port(socket, SG_OUTPUT_PORT, FALSE);
}

void Sg_ShutdownPort(SgPort *port, int how)
{
  if (SG_BINARY_PORT(port)->type != SG_BINARY_CUSTOM_PORT_TYPE ||
      !SG_SOCKETP(SG_PORT_SOCKET(port))) {
    Sg_Error(UC("socket port required but got %S"), port);
  }
  if (!Sg_PortClosedP(port)) {
    Sg_FlushPort(port);
    Sg_SocketShutdown(SG_PORT_SOCKET(port), how);
  }
}

extern void Sg__Init_socket_stub(SgLibrary *lib);

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
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius socket)"),
				  FALSE));
  Sg__Init_socket_stub(lib);

  /* for multithreading issue, we do not add this cond-feature.
     we always support this anyway */
  /* Sg_AddCondFeature(UC("sagittarius.socket")); */
  Sg_InitStaticClassWithMeta(SG_CLASS_SOCKET, UC("<socket>"), lib, NULL,
			     SG_FALSE, NULL, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_ADDRINFO, UC("<addrinfo>"), lib, NULL,
			     SG_FALSE, ai_slots, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_IP_ADDRESS, UC("<ip-address>"), lib, NULL,
			     SG_FALSE, NULL, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_SOCKET_INFO, UC("<socket-info>"), lib,
			     NULL, SG_FALSE, si_slots, 0);
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
  /* IPPROTO_xxx for ai_protocol, i don't list up every thing but
     tcp and udp*/
#ifdef IPPROTO_IP
  ARCH_CCONST(IPPROTO_IP);
#else
  ARCH_CFALSE(IPPROTO_IP);
#endif
#ifdef IPPROTO_TCP
  ARCH_CCONST(IPPROTO_TCP);
#else
  ARCH_CFALSE(IPPROTO_TCP);
#endif
#ifdef IPPROTO_UDP
  ARCH_CCONST(IPPROTO_UDP);
#else
  ARCH_CFALSE(IPPROTO_UDP);
#endif
#ifdef IPPROTO_RAW
  ARCH_CCONST(IPPROTO_RAW);
#else
  ARCH_CFALSE(IPPROTO_RAW);
#endif
#ifdef IPPROTO_IPV6
  ARCH_CCONST(IPPROTO_IPV6);
#else
  ARCH_CFALSE(IPPROTO_IPV6);
#endif
#ifdef IPPROTO_ICMP
  ARCH_CCONST(IPPROTO_ICMP);
#else
  ARCH_CFALSE(IPPROTO_ICMP);
#endif
#ifdef IPPROTO_ICMPV6
  ARCH_CCONST(IPPROTO_ICMPV6);
#else
  ARCH_CFALSE(IPPROTO_ICMPV6);
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

#ifdef SOL_SOCKET
  ARCH_CCONST(SOL_SOCKET);
#else
  ARCH_CFALSE(SOL_SOCKET);
#endif
#ifdef SO_ACCEPTCONN
  ARCH_CCONST(SO_ACCEPTCONN);
#else
  ARCH_CFALSE(SO_ACCEPTCONN);
#endif
#ifdef SO_BINDTODEVICE
  ARCH_CCONST(SO_BINDTODEVICE);
#else
  ARCH_CFALSE(SO_BINDTODEVICE);
#endif
#ifdef SO_BROADCAST
  ARCH_CCONST(SO_BROADCAST);
#else
  ARCH_CFALSE(SO_BROADCAST);
#endif
#ifdef SO_DEBUG
  ARCH_CCONST(SO_DEBUG);
#else
  ARCH_CFALSE(SO_DEBUG);
#endif
#ifdef SO_DONTROUTE
  ARCH_CCONST(SO_DONTROUTE);
#else
  ARCH_CFALSE(SO_DONTROUTE);
#endif
#ifdef SO_ERROR
  ARCH_CCONST(SO_ERROR);
#else
  ARCH_CFALSE(SO_ERROR);
#endif
#ifdef SO_KEEPALIVE
  ARCH_CCONST(SO_KEEPALIVE);
#else
  ARCH_CFALSE(SO_KEEPALIVE);
#endif
#ifdef SO_LINGER
  ARCH_CCONST(SO_LINGER);
#else
  ARCH_CFALSE(SO_LINGER);
#endif
#ifdef SO_OOBINLINE
  ARCH_CCONST(SO_OOBINLINE);
#else
  ARCH_CFALSE(SO_OOBINLINE);
#endif
#ifdef SO_PASSCRED
  ARCH_CCONST(SO_PASSCRED);
#else
  ARCH_CFALSE(SO_PASSCRED);
#endif
#ifdef SO_PEERCRED
  ARCH_CCONST(SO_PEERCRED);
#else
  ARCH_CFALSE(SO_PEERCRED);
#endif
#ifdef SO_PRIORITY
  ARCH_CCONST(SO_PRIORITY);
#else
  ARCH_CFALSE(SO_PRIORITY);
#endif
#ifdef SO_RCVBUF
  ARCH_CCONST(SO_RCVBUF);
#else
  ARCH_CFALSE(SO_RCVBUF);
#endif
#ifdef SO_RCVLOWAT
  ARCH_CCONST(SO_RCVLOWAT);
#else
  ARCH_CFALSE(SO_RCVLOWAT);
#endif
#ifdef SO_RCVTIMEO
  ARCH_CCONST(SO_RCVTIMEO);
#else
  ARCH_CFALSE(SO_RCVTIMEO);
#endif
#ifdef SO_REUSEADDR
  ARCH_CCONST(SO_REUSEADDR);
#else
  ARCH_CFALSE(SO_REUSEADDR);
#endif
#ifdef SO_REUSEPORT
  ARCH_CCONST(SO_REUSEPORT);
#else
  ARCH_CFALSE(SO_REUSEPORT);
#endif
#ifdef SO_SNDBUF
  ARCH_CCONST(SO_SNDBUF);
#else
  ARCH_CFALSE(SO_SNDBUF);
#endif
#ifdef SO_SNDLOWAT
  ARCH_CCONST(SO_SNDLOWAT);
#else
  ARCH_CFALSE(SO_SNDLOWAT);
#endif
#ifdef SO_SNDTIMEO
  ARCH_CCONST(SO_SNDTIMEO);
#else
  ARCH_CFALSE(SO_SNDTIMEO);
#endif
#ifdef SO_TIMESTAMP
  ARCH_CCONST(SO_TIMESTAMP);
#else
  ARCH_CFALSE(SO_TIMESTAMP);
#endif
#ifdef SO_TYPE
  ARCH_CCONST(SO_TYPE);
#else
  ARCH_CFALSE(SO_TYPE);
#endif
#ifdef SOL_TCP
  ARCH_CCONST(SOL_TCP);
#else
  ARCH_CFALSE(SOL_TCP);
#endif
#ifdef TCP_NODELAY
  ARCH_CCONST(TCP_NODELAY);
#else
  ARCH_CFALSE(TCP_NODELAY);
#endif
#ifdef TCP_MAXSEG
  ARCH_CCONST(TCP_MAXSEG);
#else
  ARCH_CFALSE(TCP_MAXSEG);
#endif
#ifdef TCP_CORK
  ARCH_CCONST(TCP_CORK);
#else
  ARCH_CFALSE(TCP_CORK);
#endif
#ifdef SOL_IP
  ARCH_CCONST(SOL_IP);
#else
  ARCH_CFALSE(SOL_IP);
#endif
#ifdef IP_OPTIONS
  ARCH_CCONST(IP_OPTIONS);
#else
  ARCH_CFALSE(IP_OPTIONS);
#endif
#ifdef IP_PKTINFO
  ARCH_CCONST(IP_PKTINFO);
#else
  ARCH_CFALSE(IP_PKTINFO);
#endif
#ifdef IP_RECVTOS
  ARCH_CCONST(IP_RECVTOS);
#else
  ARCH_CFALSE(IP_RECVTOS);
#endif
#ifdef IP_RECVTTL
  ARCH_CCONST(IP_RECVTTL);
#else
  ARCH_CFALSE(IP_RECVTTL);
#endif
#ifdef IP_RECVOPTS
  ARCH_CCONST(IP_RECVOPTS);
#else
  ARCH_CFALSE(IP_RECVOPTS);
#endif
#ifdef IP_TOS
  ARCH_CCONST(IP_TOS);
#else
  ARCH_CFALSE(IP_TOS);
#endif
#ifdef IP_TTL
  ARCH_CCONST(IP_TTL);
#else
  ARCH_CFALSE(IP_TTL);
#endif
#ifdef IP_HDRINCL
  ARCH_CCONST(IP_HDRINCL);
#else
  ARCH_CFALSE(IP_HDRINCL);
#endif
#ifdef IP_RECVERR
  ARCH_CCONST(IP_RECVERR);
#else
  ARCH_CFALSE(IP_RECVERR);
#endif
#ifdef IP_MTU_DISCOVER
  ARCH_CCONST(IP_MTU_DISCOVER);
#else
  ARCH_CFALSE(IP_MTU_DISCOVER);
#endif
#ifdef IP_MTU
  ARCH_CCONST(IP_MTU);
#else
  ARCH_CFALSE(IP_MTU);
#endif
#ifdef IP_ROUTER_ALERT
  ARCH_CCONST(IP_ROUTER_ALERT);
#else
  ARCH_CFALSE(IP_ROUTER_ALERT);
#endif
#ifdef IP_MULTICAST_TTL
  ARCH_CCONST(IP_MULTICAST_TTL);
#else
  ARCH_CFALSE(IP_MULTICAST_TTL);
#endif
#ifdef IP_MULTICAST_LOOP
  ARCH_CCONST(IP_MULTICAST_LOOP);
#else
  ARCH_CFALSE(IP_MULTICAST_LOOP);
#endif
#ifdef IP_ADD_MEMBERSHIP
  ARCH_CCONST(IP_ADD_MEMBERSHIP);
#else
  ARCH_CFALSE(IP_ADD_MEMBERSHIP);
#endif
#ifdef IP_DROP_MEMBERSHIP
  ARCH_CCONST(IP_DROP_MEMBERSHIP);
#else
  ARCH_CFALSE(IP_DROP_MEMBERSHIP);
#endif
#ifdef IP_MULTICAST_IF
  ARCH_CCONST(IP_MULTICAST_IF);
#else
  ARCH_CFALSE(IP_MULTICAST_IF);
#endif

#ifndef SOMAXCONN
#define SOMAXCONN 5
#endif
  ARCH_CCONST(SOMAXCONN);

#undef ARCH_CCONST
#undef ARCH_CFALSE

}
