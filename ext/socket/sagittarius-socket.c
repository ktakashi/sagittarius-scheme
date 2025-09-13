/* sagittarius-socket.c                            -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2025  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius-socket.h"
#include "socket-selector.h"

#include <sys/types.h>
#include <string.h>
#include <signal.h>
/* we assume _WIN32 is only VC */
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
/* MSVC 2012 doesn't have this, so define it */
typedef int ssize_t;
typedef long suseconds_t;
# ifdef EINTR
#  undef EINTR
# endif
# ifdef EAGAIN
#  undef EAGAIN
# endif
# ifdef EWOULDBLOCK
#  undef EWOULDBLOCK
# endif
# ifdef EPIPE
#  undef EPIPE
# endif
# ifdef EINPROGRESS
#  undef EINPROGRESS
# endif
# ifdef ETIMEDOUT
#  undef ETIMEDOUT
# endif
# define EINTR  WSAEINTR
# define EAGAIN WSATRY_AGAIN
# define EWOULDBLOCK WSAEWOULDBLOCK
# define EPIPE WSAEINVAL
# define EINPROGRESS WSAEINPROGRESS
# define ETIMEDOUT WSAETIMEDOUT
#endif

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0		/* no support (incl. *BSD/OSX) */
#endif

/* 
   https://bugs.launchpad.net/libdrizzle/+bug/404662
   Even though it said it's fixed HOWEVER h, on FreeBSD 9.1 it still
   returns EAI_BADFLAGS! so we set this 0
 */
/* #ifdef __FreeBSD__ */
/* #undef AI_V4MAPPED */
/* #define AI_V4MAPPED 0 */
/* #endif */

static SgString* get_address_string_rec(const struct sockaddr *addr,
					socklen_t addrlen, int port_p)
{
  int ret;
  char host[NI_MAXHOST];
  char ip[NI_MAXHOST];
  char serv[NI_MAXSERV];
  char name[NI_MAXSERV + (NI_MAXHOST<<1) + 1];

  ret = getnameinfo(addr,
		    addrlen,
		    host, sizeof(host),
		    serv, sizeof(serv), 
		    NI_NUMERICSERV);
  if (ret == EAI_AGAIN) {
    /* on linux it won't fallback to numeric host so get it specifically */
    do {
      ret = getnameinfo(addr,
			addrlen,
			host, sizeof(host),
			serv, sizeof(serv), 
			NI_NUMERICSERV | NI_NUMERICHOST);
    } while (EAI_AGAIN == ret);
  }
  if (port_p) {
    do {
      ret = getnameinfo(addr,
			addrlen,
			ip, sizeof(ip),
			serv, sizeof(serv), 
			NI_NUMERICSERV | NI_NUMERICHOST);
    } while (EAI_AGAIN == ret);
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
  SgObject addr = get_address_string(info->ai->ai_addr, 
				     /* fxxk Windows! */
				     (socklen_t)info->ai->ai_addrlen);
  SgObject socktype = SG_FALSE;
  switch (info->ai->ai_socktype) {
  case SOCK_STREAM: socktype = SG_INTERN("stream"); break;
  case SOCK_DGRAM: socktype = SG_INTERN("dgram"); break;
#ifdef SOCK_RAW
  case SOCK_RAW: socktype = SG_INTERN("raw"); break;
#endif
#ifdef SOCK_RDM
  case SOCK_RDM: socktype = SG_INTERN("rdm"); break;
#endif
#ifdef SOCK_SEQPACKET
  case SOCK_SEQPACKET: socktype = SG_INTERN("seqpacket"); break;
#endif
  }

  Sg_Printf(port, UC("#<addrinfo %A %A>"), addr, socktype);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_AddrinfoClass, addrinfo_printer);

static void socket_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgSocket *socket = SG_SOCKET(self);
  const SgChar *type =
      (socket->type == SG_SOCKET_CLIENT) ? UC("client")
    : (socket->type == SG_SOCKET_SERVER) ? UC("server")
    : (socket->type == SG_SOCKET_CLOSED) ? UC("closed")
    : UC("unknown");
  SgObject address = (socket->address != NULL) 
    ? get_address_string(socket->address->addr, socket->address->addr_size)
    : SG_FALSE;
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_Printf(port, UC("#<socket %s:%d %S>"), type, socket->socket, address);
  } else {
    Sg_Printf(port, UC("#<socket %s:%d %S 0x%x>"),
	      type, socket->socket, address, self);
  }
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SocketClass, socket_printer);


static SgSockaddr * make_sockaddr(socklen_t size,
				  struct sockaddr *addr, int copyP)
{
  SgSockaddr *r = SG_NEW(SgSockaddr);
  SG_SET_CLASS(r, SG_CLASS_SOCKADDR);
  r->addr_size = size;
  if (copyP) {
    struct sockaddr *t = SG_NEW2(struct sockaddr *, size);
    memcpy(t, addr, size);
    r->addr = t;
  } else {
    r->addr = addr;
  }
  return r;
}

static void sockaddr_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgSockaddr *addr = SG_SOCKADDR(self);
  Sg_Printf(port, UC("#<sockaddr %d>"), addr->addr_size);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SockaddrClass, sockaddr_printer);

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
  SgObject r;
  SgPort *sp;
  SgStringPort tp;
  int i;
  sp = SG_PORT(Sg_InitStringOutputPort(&tp, 39));
  for (i = 0; i < (IPv6_INADDER_SIZE / IPv6_INT16_SIZE); i++) {
    int hi = SG_BVECTOR_ELEMENT(bv, (i<<1));
    int lo = SG_BVECTOR_ELEMENT(bv, ((i<<1)+1));
    Sg_PutcUnsafe(sp, table[hi]);
    Sg_PutcUnsafe(sp, table[lo]);
    if (i < (IPv6_INADDER_SIZE / IPv6_INT16_SIZE) - 1) {
      Sg_PutcUnsafe(sp, ':');
    }
  }
  r = Sg_GetStringFromStringPort(&tp);
  SG_CLEAN_STRING_PORT(&tp);
  return r;
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
#ifdef _WIN32
  s->event = CreateEvent(NULL, FALSE, FALSE, NULL);
#endif
  return s;
}

static SgSocket* make_socket(SOCKET fd, SgSocketType type, SgSockaddr *address)
{
  SgSocket *s = make_socket_inner(fd);
  s->type = type;
  s->address = address;
  s->lastError = 0;
  return s;
}

static SgAddrinfo* make_addrinfo(SgObject node, SgObject service)
{
  SgAddrinfo *info = SG_NEW(SgAddrinfo);
  SG_SET_CLASS(info, SG_CLASS_ADDRINFO);
  info->node = node;
  info->service = service;
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
  ai->ai->ai_flags = (int)SG_INT_VALUE(flags);
}

static SgObject ai_family(SgAddrinfo *ai)
{
  return SG_MAKE_INT(ai->ai->ai_family);
}
static void ai_family_set(SgAddrinfo *ai, SgObject family)
{
  if (!SG_INTP(family)) Sg_Error(UC("fixnum required but got %S"), family);
  ai->ai->ai_family = (int)SG_INT_VALUE(family);
}

static SgObject ai_socktype(SgAddrinfo *ai)
{
  return SG_MAKE_INT(ai->ai->ai_socktype);
}
static void ai_socktype_set(SgAddrinfo *ai, SgObject socktype)
{
  if (!SG_INTP(socktype)) Sg_Error(UC("fixnum required but got %S"), socktype);
  ai->ai->ai_socktype = (int)SG_INT_VALUE(socktype);
}

static SgObject ai_protocol(SgAddrinfo *ai)
{
  return SG_MAKE_INT(ai->ai->ai_protocol);
}
static void ai_protocol_set(SgAddrinfo *ai, SgObject protocol)
{
  if (!SG_INTP(protocol)) Sg_Error(UC("fixnum required but got %S"), protocol);
  ai->ai->ai_protocol = (int)SG_INT_VALUE(protocol);
}

static SgObject ai_addr(SgAddrinfo *ai)
{
  return SG_OBJ(make_sockaddr((socklen_t)ai->ai->ai_addrlen,
			      ai->ai->ai_addr, FALSE));
}

static SgObject ai_next(SgAddrinfo *ai)
{
  if (ai->ai->ai_next) {
    SgAddrinfo *info = make_addrinfo(ai->node, ai->service);
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
  SG_CLASS_SLOT_SPEC("addr", 4, ai_addr, NULL),
  SG_CLASS_SLOT_SPEC("next", 5, ai_next, NULL),
  { { NULL } }
};

SgAddrinfo* Sg_MakeAddrinfo()
{
  SgAddrinfo *info = make_addrinfo(SG_FALSE, SG_FALSE);
  info->ai = SG_NEW(struct addrinfo);
  memset(info->ai, 0, sizeof(struct addrinfo));
  return info;
}

#include "raise_incl.incl"

static void raise_io_error(SgObject who, int ret, SgObject c, SgObject irr)
{
#ifdef _WIN32
  const char *msg = gai_strerrorA(ret);
#else
  const char *msg = gai_strerror(ret);
#endif
  raise_socket_error(who, Sg_Utf8sToUtf32s(msg, (int)strlen(msg)), c, irr);
}

SgAddrinfo* Sg_GetAddrinfo(SgObject node, SgObject service, SgAddrinfo *hints)
{
  const char * cnode = (!SG_FALSEP(node)) ?
    Sg_Utf32sToUtf8s(SG_STRING(node)) : NULL;
  const char * csrv  = (!SG_FALSEP(service)) ?
    Sg_Utf32sToUtf8s(SG_STRING(service)) : NULL;
  int ret;
  SgAddrinfo *result = make_addrinfo(node, service);
  struct addrinfo *ai, *cur, *prev, *next;
  do {
    ret = getaddrinfo(cnode, csrv, hints? hints->ai: NULL, &ai);
  } while (EAI_AGAIN == ret);

  if (ret != 0) {
    raise_io_error(SG_INTERN("get-addrinfo"), ret,
		   Sg_MakeHostNotFound(node, service),
		   SG_LIST3(SG_MAKE_INT(ret), SG_OBJ(node), SG_OBJ(service)));
    return NULL;
  }
  /* copy addr info */
  result->ai = SG_NEW(struct addrinfo);
  cur = result->ai;
  next = ai;
  prev = NULL;
  while (next) {
    memcpy(cur, next, sizeof(struct addrinfo));
    /* copy sockaddr */
    cur->ai_addr = SG_NEW2(struct sockaddr *, ai->ai_addrlen);
    memcpy(cur->ai_addr, next->ai_addr, ai->ai_addrlen);
    cur->ai_canonname = NULL;	/* we don't use this */

    if (next->ai_next) {
      cur->ai_next = SG_NEW(struct addrinfo);
      if (prev) prev->ai_next = cur;
    }
    prev = cur;
    cur = cur->ai_next;
    next = next->ai_next;
  }

  freeaddrinfo(ai);
  return result;
}

SgObject Sg_CreateSocket(int family, int socktype, int protocol)
{
  const SOCKET fd = socket(family, socktype, protocol);
  if (-1 == fd) {
    return SG_FALSE;
  }

#ifdef SO_NOSIGPIPE
  const int option_value = 1;
  if (-1 == setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, 
		       (const char *)&option_value, sizeof(option_value))) {
    return SG_FALSE;
  }
#endif  
  return make_socket_inner(fd);
}
static int toggle_nagle(SOCKET fd, int value)
{
#ifdef TCP_NODELAY
  int old, stack = value;
  socklen_t len = sizeof(int);
  getsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *)&old, &len);
  /* we ignore the return value here, since this is merely performance
     optimisation */
  if (old != value) {
    setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (const char *)&stack, sizeof(int));
  }
  return old;
#endif
  return value;
}

static void flush_tcp(SOCKET fd)
{
#ifdef TCP_NODELAY
  int old = toggle_nagle(fd, 1);
  toggle_nagle(fd, old);
#endif
}

static int socket_select_int(SgFdSet *rfds, SgFdSet *wfds, SgFdSet *efds,
			     SgObject timeout);

SgObject Sg_SocketConnect(SgSocket *socket, SgAddrinfo* addrinfo,
			  SgObject timeout)
{
  struct addrinfo *p = addrinfo->ai;
  int rc;
  if (!SG_FALSEP(timeout)) {
    Sg_SocketNonblocking(socket);
  }
  rc = connect(socket->socket, p->ai_addr, (int)p->ai_addrlen);
  if (rc < 0) {
    int ec = last_error;
    if (ec == EINPROGRESS || ec == EWOULDBLOCK) {
      SgFdSet *wfd = SG_FDSET(Sg_SocketsToFdSet(SG_LIST1(socket)));
      int res = socket_select_int(NULL, wfd, NULL, timeout);
      Sg_SocketBlocking(socket);
      if (res != 1) {
	SgObject c = SG_LIST4(Sg_MakeConditionSocketConnection(socket),
			      Sg_MakeWhoCondition(SG_INTERN("socket-connect!")),
			      Sg_MakeMessageCondition(SG_MAKE_STRING("Connection timeout")),
			      Sg_MakeIrritantsCondition(timeout));
	Sg_Raise(Sg_Condition(c), FALSE);
	return SG_FALSE;	/* dummy */
      }
	  
    } else {
      goto err;
    }
  }
  toggle_nagle(socket->socket, 1);
  socket->type = SG_SOCKET_CLIENT;
  socket->address = SG_SOCKADDR(ai_addr(addrinfo));
  socket->node = addrinfo->node;
  socket->service = addrinfo->service;
  return socket;
    
 err:
  if (!SG_FALSEP(timeout)) {
    Sg_SocketBlocking(socket);
  }
  socket->lastError = last_error;
  return SG_FALSE;
}

SgObject Sg_SocketBind(SgSocket *socket, SgAddrinfo* addrinfo)
{
  struct addrinfo *p = addrinfo->ai;
  if (bind(socket->socket, p->ai_addr, (int)p->ai_addrlen) == 0) {
    struct sockaddr_storage name;
    socklen_t len = (socklen_t)p->ai_addrlen;
    int r = getsockname(socket->socket, (struct sockaddr *)&name, &len);
    if (r != 0) {
      raise_socket_error(SG_INTERN("socket-bind!"),
			 Sg_GetLastErrorMessageWithErrorCode(last_error),
			 Sg_MakeConditionSocket(socket), socket);
      return SG_FALSE;		/* dummy */
    }
    socket->type = SG_SOCKET_SERVER;
    socket->address = make_sockaddr(len, (struct sockaddr *)&name, TRUE);
    socket->node = addrinfo->node;
    socket->service = addrinfo->service;
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

#define CLOSE_SOCKET(who, socket)				\
  do {								\
    if (!Sg_SocketOpenP(socket))				\
      raise_socket_error(SG_INTERN(who),			\
			 SG_MAKE_STRING("socket is closed"),	\
			 Sg_MakeConditionSocketClosed(socket),	\
			 socket);				\
  } while (0)


SgObject Sg_SocketSetopt(SgSocket *socket, int level, int name, SgObject value)
{
  int r = 0;
  CLOSE_SOCKET("socket-setsockopt!",socket);
  if (SG_BVECTORP(value)) {
    r = setsockopt(socket->socket, level, name,
		   (const char *)SG_BVECTOR_ELEMENTS(value), 
		   (int)SG_BVECTOR_SIZE(value));
  } else if (SG_INTP(value) || SG_BIGNUMP(value)) {
    long v = Sg_GetInteger(value);
    r = setsockopt(socket->socket, level, name, (const char *)&v, sizeof(int));
  } else if (SG_TIMEP(value)) {
    if (name != SO_RCVTIMEO && name != SO_SNDTIMEO) {
      Sg_AssertionViolation(SG_INTERN("socket-setsockopt!"),
			    SG_MAKE_STRING("time object is required for SO_RCVTIMEO and SO_SNDTIMEO"),
			    value);
    }
#ifdef _WIN32
    DWORD v;
    v = (DWORD)(SG_TIME(value)->sec*1000 + SG_TIME(value)->nsec/1000000);
    r = setsockopt(socket->socket, level, name, (const char *)&v,
		   sizeof(DWORD));
#else
    struct timeval v;
    v.tv_sec = SG_TIME(value)->sec;
    v.tv_usec = SG_TIME(value)->nsec/1000;
    r = setsockopt(socket->socket, level, name, (const char *)&v,
		   sizeof(struct timeval));
#endif
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
      raise_socket_error(SG_INTERN("socket-getsockopt"), 
			 Sg_GetLastErrorMessageWithErrorCode(last_error),
			 Sg_MakeConditionSocket(socket), SG_NIL);
    }
    SG_BVECTOR_SIZE(bvec) = rrsize;
    return SG_OBJ(bvec);
  } else if (name == SO_RCVTIMEO || name == SO_SNDTIMEO) {
    SgTime *t;
#ifdef _WIN32
    DWORD v;
    rrsize = sizeof(DWORD);
    r = getsockopt(socket->socket, level, name, (char *)&v, &rrsize);
#else
    struct timeval tv;
    rrsize = sizeof(struct timeval);
    r = getsockopt(socket->socket, level, name, &tv, &rrsize);
#endif
    if (r < 0) {
      raise_socket_error(SG_INTERN("socket-getsockopt"), 
			 Sg_GetLastErrorMessageWithErrorCode(last_error),
			 Sg_MakeConditionSocket(socket), SG_NIL);
    }

    t = SG_NEW(SgTime);
    SG_SET_CLASS(t, SG_CLASS_TIME);
    t->type = SG_INTERN("time-duration");
    if (rrsize > 0) {
#ifdef _WIN32
      t->sec = v / 1000;
      t->nsec = (v % 1000) * 1000000;
#else
      t->sec = tv.tv_sec;
      t->nsec = tv.tv_usec * 1000;
#endif
      return SG_OBJ(t);
    }
    return SG_FALSE;		/*  not set */
  } else {
    int val;
    rrsize = sizeof(int);
    r = getsockopt(socket->socket, level, name, (char *)&val, &rrsize);
    if (r < 0) {
      raise_socket_error(SG_INTERN("socket-getsockopt"), 
			 Sg_GetLastErrorMessageWithErrorCode(last_error),
			 Sg_MakeConditionSocket(socket), SG_NIL);
    }
    return Sg_MakeInteger(val);
  }
}

#if EAGAIN == EWOULDBLOCK
#define NON_BLOCKING_CASE EAGAIN
#else
#define NON_BLOCKING_CASE EAGAIN: case EWOULDBLOCK
#endif

#define handleError(who, socket, r)					\
  if ((r) < 0) {							\
    int e = last_error;							\
    socket->lastError = e;						\
    switch (e) {							\
    case EINTR:								\
      continue;								\
    case EPIPE:								\
      if (flags & MSG_NOSIGNAL) {					\
	return 0;							\
      }									\
      break;								\
    case NON_BLOCKING_CASE:						\
    case ETIMEDOUT:/* Windows, maybe we should raise an error here*/	\
      /* most probably non-blocking socket */				\
      return (r);							\
    }									\
    raise_socket_error(SG_INTERN(who),					\
		       Sg_GetLastErrorMessageWithErrorCode(e),		\
		       Sg_MakeConditionSocket(socket),			\
		       SG_LIST1(SG_MAKE_INT(e)));			\
  }									\

long Sg_SocketReceive(SgSocket *socket, uint8_t *data, long size, int flags)
{
  /* int count = 0, osize = size; */
  CLOSE_SOCKET("socket-recv", socket);
  for (;;) {
    const ssize_t ret = recv(socket->socket, (char*)data, size,
			     /* we don't want SIGPIPE */
			     flags | MSG_NOSIGNAL);
    handleError("socket-recv", socket, ret);
    return ret;
  }
}

long Sg_SocketReceiveFrom(SgSocket *socket, uint8_t *data, long size,
			  int flags, SgSockaddr *addr)
{
  /* int count = 0, osize = size; */
  CLOSE_SOCKET("socket-recvfrom", socket);
  for (;;) {
    const ssize_t ret = recvfrom(socket->socket, (char*)data, size,
				 /* we don't want SIGPIPE */
				 flags | MSG_NOSIGNAL, addr->addr,
				 &addr->addr_size);
    handleError("socket-recvfrom", socket, ret);
    return ret;
  }
}

long Sg_SocketSend(SgSocket *socket, uint8_t *data, long size, int flags)
{
  long rest = size;
  long sizeSent = 0;

  CLOSE_SOCKET("socket-send", socket);
  while (rest > 0) {
    const ssize_t ret = send(socket->socket, (char*)data, size, 
			     /* we don't want SIGPIPE */
			     flags | MSG_NOSIGNAL);
    handleError("socket-send", socket, ret);
    sizeSent += ret;
    rest -= ret;
    data += ret;
    size -= ret;
  }
  /* flush_tcp(socket->socket); */
  return sizeSent;
}

long Sg_SocketSendTo(SgSocket *socket, uint8_t *data, long size, int flags,
		    SgSockaddr *addr)
{
  long rest = size;
  long sizeSent = 0;

  CLOSE_SOCKET("socket-send", socket);
  while (rest > 0) {
    const ssize_t ret = sendto(socket->socket, (char*)data, size, 
			       /* we don't want SIGPIPE */
			       flags | MSG_NOSIGNAL, addr->addr, 
			       addr->addr_size);
    handleError("socket-sendto", socket, ret);
    sizeSent += ret;
    rest -= ret;
    data += ret;
    size -= ret;
  }
  return sizeSent;
}

SgObject Sg_SocketAccept(SgSocket *socket)
{
  struct sockaddr_storage addr;
  socklen_t addrlen = sizeof(addr);
  SOCKET fd = -1;
  
#ifdef _WIN32
  SgVM *vm = Sg_VM();
  HANDLE hEvents[3];
  int r;
#endif
  
  CLOSE_SOCKET("socket-accept", socket);

#ifdef _WIN32
  ResetEvent((&vm->thread)->event);
  /* Here, we can't use FD_CLOSE, more precisely, it won't be
     provided at all. */
  hEvents[0] = CreateEvent(NULL, FALSE, FALSE, NULL);
  hEvents[1] = socket->event;
  hEvents[2] = (&vm->thread)->event;
  SG_SET_SOCKET_EVENT(socket, hEvents[0], FD_ACCEPT);
  r = WaitForMultipleObjects(3, hEvents, FALSE, INFINITE);
  SG_SET_SOCKET_EVENT(socket, hEvents[0], 0);
  CloseHandle(hEvents[0]);
  if (socket->event != INVALID_HANDLE_VALUE) {
    ResetEvent(socket->event);
  }
  if (r == WAIT_OBJECT_0 + 2) {
    WSASetLastError(EINTR);
    return SG_FALSE;		/* interrupted! */
  }
  if (r != WAIT_OBJECT_0) {
    /* socket is closed */
    raise_socket_error(SG_INTERN("socket-accept"), 
		       Sg_GetLastErrorMessageWithErrorCode(WSAECONNRESET),
		       Sg_MakeConditionSocket(socket), socket);
    return SG_UNDEF;	/* dummy */
  }
#endif
  
  for (;;) {
    if (socket->socket == INVALID_SOCKET) {
      fd = INVALID_SOCKET;
    } else {
      fd = accept(socket->socket, (struct sockaddr *)&addr, &addrlen);
    }
    if (INVALID_SOCKET == fd) {
      /* For some reason, accept may fail on Solaris without
	 last_error set. I'm not sure what this exactly means but
	 seems we can retry.
       */
      if (!last_error || last_error == EINTR) {
	SG_INTERRUPTED_THREAD() {
	  return SG_FALSE;
	} SG_INTERRUPTED_THREAD_ELSE() {
	  continue;
	} SG_INTERRUPTED_THREAD_END();
      } else if (!Sg_SocketOpenP(socket)) {
	return SG_FALSE;
      } else {
	raise_socket_error(SG_INTERN("socket-accept"), 
			   Sg_GetLastErrorMessageWithErrorCode(last_error),
			   Sg_MakeConditionSocket(socket), socket);
	return SG_UNDEF;	/* dummy */
      }
    } else {
      break;
    }
  }
  toggle_nagle(fd, 1);
  return make_socket(fd, SG_SOCKET_SERVER, 
		     make_sockaddr(addrlen, (struct sockaddr *)&addr, TRUE));
}

void Sg_SocketShutdown(SgSocket *socket, int how)
{
  if (Sg_SocketOpenP(socket)) {
    shutdown(socket->socket, how);
  }
}

void Sg_SocketClose(SgSocket *socket)
{
  SOCKET fd = socket->socket;
  if (!Sg_SocketOpenP(socket)) {
    return;
  }
  /* in case of double closing, we need to set invalid socket here. */
  socket->socket = INVALID_SOCKET;
  socket->type = SG_SOCKET_CLOSED;
  socket->address = NULL;
#ifdef _WIN32
  HANDLE event = socket->event;
  socket->event = INVALID_HANDLE_VALUE;
  /* FIXME socket-close should not shutdown socket but we don't have
     any way to flush socket other than shutting down write side of
     socket descriptor on Windows. */
  shutdown(fd, SD_SEND);
  SetEvent(event);
  closesocket(fd);
  CloseHandle(event);
#else
  close(fd);
#endif
  Sg_UnregisterFinalizer(SG_OBJ(socket));
}

/* fdset */
static void fdset_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<fdset %d %S>"), SG_FDSET(self)->maxfd,
	    SG_FDSET(self)->sockets);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_FdSetClass, fdset_printer);

static SgFdSet* make_fd_set()
{
  SgFdSet *z = SG_NEW(SgFdSet);
  SG_SET_CLASS(z, SG_CLASS_FD_SET);
  z->maxfd = -1;
  z->sockets = SG_NIL;
  FD_ZERO(&z->fdset);
  return z;
}

static SgFdSet* copy_fd_set(SgFdSet *src)
{
  if (src == NULL) {
    return NULL;
  } else {
    SgFdSet *z = SG_NEW(SgFdSet);
    SG_SET_CLASS(z, SG_CLASS_FD_SET);
    z->fdset = src->fdset;
    z->maxfd = src->maxfd;
    z->sockets = Sg_CopyList(src->sockets);
    return z;
  }
}

SgObject Sg_MakeFdSet()
{
  return SG_OBJ(make_fd_set());
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
    if (!Sg_SocketOpenP(SG_SOCKET(SG_CAR(cp)))) {
      Sg_AssertionViolation(SG_INTERN("socket-select"),
			    SG_MAKE_STRING("socket is closed"),
			    SG_LIST2(SG_CAR(cp), sockets));
    }
    fd = SG_SOCKET(SG_CAR(cp))->socket;
    /* MSDN says the first argument of select is ignored, so this is useless */
#ifndef _WIN32
    if (max < fd) max = fd;
#endif
    FD_SET(fd, fds);
  }
  return max;
}

SgObject Sg_SocketsToFdSet(SgObject sockets)
{
  SgFdSet *fdset = make_fd_set();
  fdset->maxfd = collect_max_fd(fdset->maxfd, sockets, &fdset->fdset);
  fdset->sockets = sockets;
  return SG_OBJ(fdset);
}

static struct timeval *select_timeval(SgObject timeout, struct timeval *tm)
{
  if (SG_FALSEP(timeout)) return NULL;
  if (SG_INTP(timeout)) {
    long val = SG_INT_VALUE(timeout);
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
    tm->tv_usec = (suseconds_t)usec;
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
    tm->tv_usec = (suseconds_t)iusec;
    return tm;
  } else if (SG_TIMEP(timeout)) {
    tm->tv_sec = (long)(SG_TIME(timeout)->sec);
    tm->tv_usec = SG_TIME(timeout)->nsec/1000;
    return tm;
  }
 badtv:
  Sg_Error(UC("timeval needs to be a real number (in microseconds), a list "
	      "of two integers (seconds and microseconds), or a time object "
	      "but got %S"),
	   timeout);
  return NULL;                /* dummy */
}

/* not used */
#if 0
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

SgObject Sg_CollectSockets(SgObject fdset, SgObject sockets)
{
  return collect_fds(sockets, &SG_FDSET(fdset)->fdset);
}
#endif

static SgObject skip_sockets(SgObject sockets, SgFdSet *fds)
{
  SG_FOR_EACH(sockets, sockets) {
    SgSocket *sock = SG_SOCKET(SG_CAR(sockets));
    if (FD_ISSET(sock->socket, &fds->fdset)) {
      return sockets;
    }
  }
  return SG_NIL;		/* empty huh? */
}

static SgObject remove_socket(SgFdSet *fds)
{
  SgObject ans = fds->sockets, cp, prev = SG_FALSE;

  if (SG_NULLP(ans)) return ans; /* simple */
  /* remove non set sockets first */
  cp = ans = skip_sockets(ans, fds);
  for (;!SG_NULLP(cp);) {
    SgSocket *sock = SG_SOCKET(SG_CAR(cp));
    if (!FD_ISSET(sock->socket, &fds->fdset)) {
      SgObject next = skip_sockets(cp, fds);
      SG_SET_CDR(prev, next);
      if (SG_NULLP(next)) break;
      prev = next;
      cp = SG_CDR(next);
    } else {
      prev = cp, cp = SG_CDR(cp);
    }
  }
  return ans;
}

static int socket_select_int(SgFdSet *rfds, SgFdSet *wfds, SgFdSet *efds,
			     SgObject timeout)
{
  struct timeval tv;
  int max = 0, numfds;
  
#ifdef _WIN32
  struct timeval *tv2;
  SgVM *vm = Sg_VM();
  HANDLE hEvents[2];
  hEvents[0] = CreateEvent(NULL, FALSE, FALSE, NULL);
#endif
  
  if (rfds) max = rfds->maxfd;
  if (wfds && wfds->maxfd > max) max = wfds->maxfd;
  if (efds && efds->maxfd > max) max = efds->maxfd;

  /* TODO wrap this with macro */
#ifdef _WIN32
  ResetEvent((&vm->thread)->event);
# define SET_EVENT(fdset, flags)					\
  do {									\
    if (fdset) {							\
      SgObject sockets = (fdset)->sockets;				\
      SG_FOR_EACH(sockets, sockets) {					\
	SG_SET_SOCKET_EVENT(SG_CAR(sockets), hEvents[0], flags);	\
      }									\
    }									\
  }while (0)

  SET_EVENT(rfds, FD_READ | FD_OOB);
  SET_EVENT(wfds, FD_WRITE);
  SET_EVENT(efds, FD_READ | FD_OOB);

  tv2 = select_timeval(timeout, &tv);
  DWORD millis = tv2 ? tv.tv_sec * 1000 + tv.tv_usec/1000: INFINITE;
  /* Put minimum amount of wait */
  if (tv2 && millis == 0) millis = 1;
  hEvents[1] = (&vm->thread)->event;
  int r = WaitForMultipleObjects(2, hEvents, FALSE, millis);

  if (r == WAIT_TIMEOUT) {
    /* call select without waiting */
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    r = WAIT_OBJECT_0;
  }
  /* we don't have to wait any more so close it */
  SET_EVENT(rfds, 0);
  SET_EVENT(wfds, 0);
  SET_EVENT(efds, 0);
  CloseHandle(hEvents[0]);
  
  if (r == WAIT_OBJECT_0) {
    numfds = select(max + 1, 
		    (rfds ? &rfds->fdset : NULL), 
		    (wfds ? &wfds->fdset : NULL), 
		    (efds ? &efds->fdset : NULL), 
		    tv2);
  } else {
    WSASetLastError(EINTR);
    numfds = -1;
  }
#else
 retry:
  numfds = select(max + 1, 
		  (rfds ? &rfds->fdset : NULL), 
		  (wfds ? &wfds->fdset : NULL), 
		  (efds ? &efds->fdset : NULL), 
		  select_timeval(timeout, &tv));

#endif

  if (numfds < 0) {
    if (last_error == EINTR) {
      SG_INTERRUPTED_THREAD() {
	return -1;
#ifndef _WIN32
      } SG_INTERRUPTED_THREAD_ELSE() {
	goto retry;
#endif
      } SG_INTERRUPTED_THREAD_END();
    }
    raise_socket_error(SG_INTERN("socket-select"), 
		       Sg_GetLastErrorMessageWithErrorCode(last_error),
		       /* TODO should we make different condition? */
		       Sg_MakeConditionSocket(SG_FALSE),
		       SG_LIST4(rfds? rfds: SG_FALSE,
				wfds? wfds: SG_FALSE,
				efds? efds: SG_FALSE,
				timeout));
  }
  
#define REMOVE_SOCKET(fdset_)					\
  do {								\
    if (fdset_) {						\
      (fdset_)->sockets = remove_socket(fdset_);		\
    }								\
  } while (0)
  REMOVE_SOCKET(rfds);
  REMOVE_SOCKET(wfds);
  REMOVE_SOCKET(efds);

#undef REMOVE_SOCKET
  return numfds;
}

static SgFdSet* check_fd(SgObject o)
{
  if (SG_FALSEP(o)) return NULL;
  if (!SG_FDSETP(o)) {
    Sg_Error(UC("<fdset> or #f required but got %S"), o);
  }
  return SG_FDSET(o);
}

SgObject Sg_SocketSelect(SgObject reads, SgObject writes, SgObject errors,
			 SgObject timeout)
{
  SgFdSet *r = copy_fd_set(check_fd(reads));
  SgFdSet *w = copy_fd_set(check_fd(writes));
  SgFdSet *e = copy_fd_set(check_fd(errors));
  
  int rs = socket_select_int(r, w, e, timeout);
  if (rs < 0) {
    return Sg_Values4(SG_FALSE, SG_FALSE, SG_FALSE, SG_FALSE);
  }
  return Sg_Values4(Sg_MakeInteger(rs),
		    (r ? SG_OBJ(r) : SG_FALSE),
		    (w ? SG_OBJ(w) : SG_FALSE),
		    (e ? SG_OBJ(e) : SG_FALSE));
}

SgObject Sg_SocketSelectX(SgObject reads, SgObject writes, SgObject errors,
			  SgObject timeout)
{
  SgFdSet *r = check_fd(reads);
  SgFdSet *w = check_fd(writes);
  SgFdSet *e = check_fd(errors);
  
  int rs = socket_select_int(r, w, e, timeout);
  if (rs < 0) {
    return Sg_Values4(SG_FALSE, SG_FALSE, SG_FALSE, SG_FALSE);
  }
  return Sg_Values4(Sg_MakeInteger(rs),
		    (r ? SG_OBJ(r) : SG_FALSE),
		    (w ? SG_OBJ(w) : SG_FALSE),
		    (e ? SG_OBJ(e) : SG_FALSE));
}


SgObject Sg_SocketPeer(SgObject socket)
{
  struct sockaddr_storage name;
  socklen_t len = sizeof(name);
  int ret;
  ret = getpeername(SG_SOCKET(socket)->socket, (struct sockaddr *)&name, &len);
  if (ret == 0) {
    return make_socket_info(&name);
  } else {
    return SG_FALSE;
  }
}

SgObject Sg_SocketName(SgObject socket)
{
  if (SG_SOCKET(socket)->address) {
    return get_address_string(SG_SOCKET(socket)->address->addr,
			      SG_SOCKET(socket)->address->addr_size);
  } else {
    return SG_FALSE;
  }
}

SgObject Sg_SocketInfo(SgObject socket)
{
  struct sockaddr_storage name;
  socklen_t len = sizeof(name);
  int ret;
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
  return socket->type != SG_SOCKET_CLOSED && socket->socket != INVALID_SOCKET;
}

int Sg_SocketNonblocking(SgSocket *socket)
{
#ifdef _WIN32
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
  socket->nonblocking = 1;
  return TRUE;
 err:
  raise_socket_error(SG_INTERN("socket-nonblocking!"), 
		     Sg_GetLastErrorMessageWithErrorCode(last_error),
		     Sg_MakeConditionSocket(socket), socket);
  return FALSE;			/* dummy */
}
int Sg_SocketBlocking(SgSocket *socket)
{
#ifdef _WIN32
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
  socket->nonblocking = 0;
  return TRUE;
 err:
  raise_socket_error(SG_INTERN("socket-blocking!"), 
		     Sg_GetLastErrorMessageWithErrorCode(last_error),
		     Sg_MakeConditionSocket(socket), socket);
  return FALSE;			/* dummy */  
}

SgObject Sg_SocketErrorMessage(SgSocket *socket)
{
  return Sg_GetLastErrorMessageWithErrorCode(socket->lastError);
}

/* socket port */
#define SG_PORT_SOCKET SG_SOCKET_PORT_SOCKET

static SgClass *port_cpl[] = {
  SG_CLASS_READ_ONCE_PORT,	/* marker */
  SG_CLASS_PORT,
  SG_CLASS_TOP,
  NULL
};
SG_DEFINE_BUILTIN_CLASS(Sg_SocketPortClass,
			Sg_DefaultPortPrinter, NULL, NULL, NULL,
			port_cpl);


static void socket_flush(SgObject self)
{
  SgSocket *socket = SG_PORT_SOCKET(self);
  /* let's hope it won't raise any signel if the socket is UDP or UNIX */
  flush_tcp(socket->socket);
}

static int socket_ready_int(SgObject port, SgObject socket, struct timeval *tm)
{
  fd_set fds;
  int state;

  FD_ZERO(&fds);
  FD_SET(SG_SOCKET(socket)->socket, &fds);

#ifdef _WIN32
  state = select(FD_SETSIZE, &fds, NULL, NULL, tm);
#else
  state = select(SG_SOCKET(socket)->socket + 1, &fds, NULL, NULL, tm);
#endif
  if (state < 0) {
    if (last_error == EINTR) return FALSE;
    raise_socket_error(SG_INTERN("port-ready?"), 
		       Sg_GetLastErrorMessageWithErrorCode(last_error),
		       Sg_MakeConditionSocketPort(socket, port), SG_NIL);
    return FALSE;
  }
  return FD_ISSET(SG_SOCKET(socket)->socket, &fds);
}

static int socket_open(SgObject self)
{
  return Sg_SocketOpenP(SG_PORT_SOCKET(self));
}

static int socket_close(SgObject self)
{
  if (SG_PORT(self)->closed != SG_PORT_CLOSED) {
    SG_PORT(self)->closed = SG_PORT_CLOSED;
    Sg_SocketShutdown(SG_PORT_SOCKET(self), SHUT_RDWR);
    Sg_SocketClose(SG_PORT_SOCKET(self));
  }
  return TRUE;
}

static int socket_close_only_port(SgObject self)
{
  if (SG_PORT(self)->closed != SG_PORT_CLOSED) {
    SG_PORT(self)->closed = SG_PORT_CLOSED;
  }
  return TRUE;
}

static int64_t socket_read_u8(SgObject self, uint8_t *buf, int64_t size)
{
  /* we need to read eagarly, or else something wrong happen. 
     for example, if the socket is TLS socket and encryption/decryption
     take sometime to flush socket even the data is continuous.
   */
  long readSize = 0;
  if (SG_PORT_HAS_U8_AHEAD(self) && size > 0) {
    buf[0] = SG_PORT_U8_AHEAD(self);
    SG_PORT_U8_AHEAD(self) = EOF;
    buf++;
    size--;
    readSize++;
  }
  if (size == 0) return readSize;

  do {
    /* wait a bit in case of retry (10ms?)*/
    /* struct timeval tm = {0, 10000}; */
    /* int ready; */
    long now = Sg_SocketReceive(SG_PORT_SOCKET(self), buf + readSize, 
				(long)size, 0);
    if (-1 == now) {
      int e = SG_PORT_SOCKET(self)->lastError;
      Sg_IOReadError(SG_INTERN("read-u8"),
		     Sg_GetLastErrorMessageWithErrorCode(e), self,
		     SG_NIL);
      return -1;
    }
    /* size -= now; */
    readSize += now;
    if (now == 0) break;
    if (size == 0) break;

    /* now how could we know if this socket still have some data ready
       or it's already ended. for now we use select if the socket has
       something to be read.
       FIXME: this may cause issue on TLS socket... */
#if 0
    ready = socket_ready_int(self, SG_PORT_SOCKET(self), &tm);
    if (!ready) {
      /* most likely nothing is waiting. i hope... */
      break;
    }
#endif
    /* ok something is still there, read*/
  } while (0);
  SG_PORT(self)->position += readSize;
  return readSize;
}

static int64_t socket_read_u8_all(SgObject self, uint8_t **buf)
{
  uint8_t read_buf[1024]; 
  SgPort *buffer;
  SgBytePort bp;
  int mark = 0, ready;
  struct timeval tm = {0, 10000};
  
  buffer = SG_PORT(Sg_InitByteArrayOutputPort(&bp, 1024));

  ready = socket_ready_int(self, SG_PORT_SOCKET(self), &tm);
  if (ready) {
    for (;;) {
      SgSocket *sock = SG_PORT_SOCKET(self);
      long read_size = Sg_SocketReceive(sock, read_buf, 1024, 0);
      if (-1 == read_size) {
	Sg_IOReadError(SG_INTERN("read-u8-all"),
		       Sg_GetLastErrorMessageWithErrorCode(sock->lastError),
		       self,
		       SG_NIL);
	return -1;
      } else {
	Sg_WritebUnsafe(buffer, read_buf, 0, read_size);
	if (1024 != read_size) {
	  mark += read_size;
	  break;
	} else {
	  mark += read_size;
	}
      }
    }
  }
  SG_PORT(self)->position += mark;
  *buf = Sg_GetByteArrayFromBinaryPort(&bp);
  SG_CLEAN_BYTE_PORT(&bp);
  return mark;
}


static int64_t socket_put_u8_array(SgObject self, uint8_t *v, int64_t size)
{
  int64_t written_size = Sg_SocketSend(SG_PORT_SOCKET(self), v, (int)size, 0);
  if (-1 == written_size) {
    Sg_IOWriteError(SG_INTERN("write-u8"),
		    Sg_GetLastErrorMessageWithErrorCode(SG_PORT_SOCKET(self)->lastError),
		    self,
		    SG_NIL);
    return -1;
  }
  return written_size;
}

static int socket_ready(SgObject self)
{
  SgObject socket = SG_PORT_SOCKET(self);
  struct timeval tm = {0, 0};
  return socket_ready_int(self, socket, &tm);
}

static SgPortTable socket_close_table = {
  socket_flush,
  socket_close,
  socket_ready,
  NULL,				/* lock */
  NULL,				/* unlock */
  NULL,				/* position */
  NULL,				/* set position */
  socket_open,
  socket_read_u8,
  socket_read_u8_all,
  socket_put_u8_array,
  NULL,				/* reads */
  NULL,				/* writes */
};
static SgPortTable socket_table = {
  socket_flush,
  socket_close_only_port,
  socket_ready,
  NULL,				/* lock */
  NULL,				/* unlock */
  NULL,				/* position */
  NULL,				/* set position */
  socket_open,
  socket_read_u8,
  socket_read_u8_all,
  socket_put_u8_array,
  NULL,				/* reads */
  NULL,				/* writes */
};

static inline SgObject make_socket_port(SgSocket *socket,
					SgPortDirection d, 
					int closeP)
{
  SgSocketPort *port = SG_NEW(SgSocketPort);
  SG_INIT_PORT(port, SG_CLASS_SOCKET_PORT, d,
	       (closeP)? &socket_close_table: &socket_table, SG_FALSE);
  port->socket = socket;
  return SG_OBJ(port);
}

SgObject Sg_MakeSocketPort(SgSocket *socket, int closeP)
{
  return make_socket_port(socket, SG_BIDIRECTIONAL_PORT, closeP);
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
  /* TODO should we handle transcoded port? */
  if (SG_BUFFERED_PORTP(port))
    return Sg_ShutdownPort(SG_BUFFERED_PORT_SRC(port), how);

  if (!SG_SOCKET_PORTP(port) || !SG_SOCKETP(SG_PORT_SOCKET(port))) {
    Sg_Error(UC("socket port required but got %S"), port);
  }
  if (!Sg_PortClosedP(port)) {
    Sg_FlushPort(port);
    Sg_SocketShutdown(SG_PORT_SOCKET(port), how);
  }
}

/* conditions */
static SgClass *error_cpl[] = {
  SG_CLASS_IO_ERROR,
  SG_ERROR_CONDITION_CPL,
  NULL
};

static void hnf_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A %A %A>"), SG_CLASS(Sg_ClassOf(o))->name,
	    SG_HOST_NOT_FOUND(o)->node,
	    SG_HOST_NOT_FOUND(o)->service);
}

SG_DEFINE_CONDITION_ALLOCATOR(hnf_allocate, SgHostNotFound);
SG_DEFINE_CONDITION_ACCESSOR(hnf_node, SgHostNotFound,
			     SG_HOST_NOT_FOUNDP, node);
SG_DEFINE_CONDITION_ACCESSOR(hnf_service, SgHostNotFound,
			     SG_HOST_NOT_FOUNDP, service);
static SgSlotAccessor hnf_slots[] = {
  SG_CLASS_SLOT_SPEC("node",    0, hnf_node, hnf_node_set),
  SG_CLASS_SLOT_SPEC("service", 1, hnf_service, hnf_service_set),
  { { NULL } }
};
SG_DEFINE_BASE_CLASS(Sg_HostNotFoundClass, SgHostNotFound,
		     hnf_printer, NULL, NULL, hnf_allocate, error_cpl);

static void exc_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A %A>"), SG_CLASS(Sg_ClassOf(o))->name,
	    SG_CONDITION_SOCKET(o)->socket);
}

SG_DEFINE_CONDITION_ALLOCATOR(parent_allocate, SgConditionSocket);
SG_DEFINE_CONDITION_ACCESSOR(socket_socket, SgConditionSocket,
			     SG_CONDITION_SOCKETP, socket);
static SgSlotAccessor parent_slots[] = {
  SG_CLASS_SLOT_SPEC("socket", 0, socket_socket, socket_socket_set),
  { { NULL } }
};
SG_DEFINE_BASE_CLASS(Sg_ConditionSocketClass, SgConditionSocket,
		     exc_printer, NULL, NULL, parent_allocate, error_cpl);

static SgClass *socket_exc_cpl[] = {
  SG_CLASS_CONDITION_SOCKET,
  SG_CLASS_IO_ERROR,
  SG_ERROR_CONDITION_CPL,
  NULL
};

SG_DEFINE_BASE_CLASS(Sg_ConditionSocketConnectionClass,
		     SgConditionSocketConnection,
		     exc_printer, NULL, NULL, parent_allocate,
		     socket_exc_cpl);

SG_DEFINE_BASE_CLASS(Sg_ConditionSocketClosedClass, SgConditionSocketClosed,
		     exc_printer, NULL, NULL, parent_allocate,
		     socket_exc_cpl);

SG_DEFINE_CONDITION_ALLOCATOR(cport_allocate, SgConditionSocketPort);
SG_DEFINE_CONDITION_ACCESSOR(csocket_port, SgConditionSocketPort,
			     SG_CONDITION_SOCKET_PORTP, port);
static SgSlotAccessor cport_slots[] = {
  SG_CLASS_SLOT_SPEC("port", 0, csocket_port, csocket_port_set),
  { { NULL } }
};
SG_DEFINE_BASE_CLASS(Sg_ConditionSocketPortClass, SgConditionSocketPort,
		     exc_printer, NULL, NULL, cport_allocate,
		     socket_exc_cpl);

SgObject Sg_MakeHostNotFound(SgObject node, SgObject service)
{
  SgObject c = hnf_allocate(SG_CLASS_HOST_NOT_FOUND, SG_NIL);
  SG_HOST_NOT_FOUND(c)->node = node;
  SG_HOST_NOT_FOUND(c)->service = service;
  return c;
}

SgObject Sg_MakeConditionSocket(SgObject socket)
{
  SgObject c = parent_allocate(SG_CLASS_CONDITION_SOCKET, SG_NIL);
  SG_CONDITION_SOCKET(c)->socket = socket;
  return c;
}

SgObject Sg_MakeConditionSocketConnection(SgObject socket)
{
  SgObject c = parent_allocate(SG_CLASS_CONDITION_SOCKET_CONNECTION, SG_NIL);
  SG_CONDITION_SOCKET(c)->socket = socket;
  return c;
}

SgObject Sg_MakeConditionSocketClosed(SgObject socket)
{
  SgObject c = parent_allocate(SG_CLASS_CONDITION_SOCKET_CLOSED, SG_NIL);
  SG_CONDITION_SOCKET(c)->socket = socket;
  return c;
}
SgObject Sg_MakeConditionSocketPort(SgObject socket, SgObject port)
{
  SgObject c = cport_allocate(SG_CLASS_CONDITION_SOCKET_PORT, SG_NIL);
  SG_CONDITION_SOCKET(c)->socket = socket;
  SG_CONDITION_SOCKET_PORT(c)->port = port;
  return c;
}

extern void Sg__Init_socket_stub(SgLibrary *lib);
extern void Sg__Init_selector(SgLibrary *lib);

#if defined(_WIN32)
static void finish_winsock(void *data)
{
  WSACleanup();
}
#endif

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__socket()
{
  SgLibrary *lib;
#if defined(_WIN32)
  WSADATA wsaData;
  WSAStartup(MAKEWORD(2, 2), &wsaData);
  Sg_AddCleanupHandler(finish_winsock, NULL);
#endif
  SG_INIT_EXTENSION(sagittarius__socket);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius socket)"),
				  FALSE));
  Sg__Init_socket_stub(lib);
  Sg__Init_selector(lib);

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
  Sg_InitStaticClassWithMeta(SG_CLASS_FD_SET, UC("<fdset>"), lib, NULL,
			     SG_FALSE, NULL, 0);
  /* TODO should we add socket slot? */
  Sg_InitStaticClass(SG_CLASS_SOCKET_PORT, UC("<socket-port>"), lib, NULL, 0);

  Sg_InitStaticClass(SG_CLASS_SOCKET_SELECTOR, UC("<socket-selector>"),
		     lib, NULL, 0);
  
  SG_INIT_CONDITION(SG_CLASS_HOST_NOT_FOUND, lib, 
		    "&host-not-found", hnf_slots);
  SG_INIT_CONDITION(SG_CLASS_CONDITION_SOCKET, lib, "&socket", parent_slots);
  SG_INIT_CONDITION(SG_CLASS_CONDITION_SOCKET_CONNECTION, lib, 
		    "&socket-connection", NULL);
  SG_INIT_CONDITION(SG_CLASS_CONDITION_SOCKET_CLOSED, lib, 
		    "&socket-closed", NULL);
  SG_INIT_CONDITION(SG_CLASS_CONDITION_SOCKET_PORT, lib, 
		    "&socket-port", cport_slots);

  SG_INIT_CONDITION_PRED(SG_CLASS_HOST_NOT_FOUND, lib, "host-not-found-error?");
  SG_INIT_CONDITION_CTR(SG_CLASS_HOST_NOT_FOUND, lib, 
			"make-host-not-found-error", 2);
  SG_INIT_CONDITION_ACC(hnf_node, lib, "&host-not-found-error-node");
  SG_INIT_CONDITION_ACC(hnf_service, lib, "&host-not-found-error-service");

  SG_INIT_CONDITION_PRED(SG_CLASS_CONDITION_SOCKET, lib, "socket-error?");
  SG_INIT_CONDITION_CTR(SG_CLASS_CONDITION_SOCKET, lib, "make-socket-error", 1);
  SG_INIT_CONDITION_ACC(socket_socket, lib, "&socket-error-socket");

  SG_INIT_CONDITION_PRED(SG_CLASS_CONDITION_SOCKET_CONNECTION, lib,
			 "socket-connection-error?");
  SG_INIT_CONDITION_CTR(SG_CLASS_CONDITION_SOCKET_CONNECTION, lib,
			"make-socket-connection-error", 1);

  SG_INIT_CONDITION_PRED(SG_CLASS_CONDITION_SOCKET_CLOSED, lib,
			 "socket-closed-error?");
  SG_INIT_CONDITION_CTR(SG_CLASS_CONDITION_SOCKET_CLOSED, lib,
			"make-socket-closed-error", 1);

  SG_INIT_CONDITION_PRED(SG_CLASS_CONDITION_SOCKET_PORT, lib, 
			 "socket-port-error?");
  SG_INIT_CONDITION_CTR(SG_CLASS_CONDITION_SOCKET_PORT, lib, 
			"make-socket-port-error", 1);
  SG_INIT_CONDITION_ACC(csocket_port, lib, "&socket-error-port");

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


  /* errno */
#ifdef EAGAIN
  ARCH_CCONST(EAGAIN);
#else
  ARCH_CFALSE(EAGAIN);
#endif
#ifdef EWOULDBLOCK
  ARCH_CCONST(EWOULDBLOCK);
#else
  ARCH_CFALSE(EWOULDBLOCK);  
#endif
#ifdef EPIPE
  ARCH_CCONST(EPIPE);
#else
  ARCH_CFALSE(EPIPE);
#endif
#ifdef EINTR
  ARCH_CCONST(EINTR);
#else
  ARCH_CFALSE(EINTR);
#endif
#ifdef ETIMEDOUT
  ARCH_CCONST(ETIMEDOUT);
#else
  ARCH_CFALSE(ETIMEDOUT);
#endif
#ifdef EINPROGRESS
  ARCH_CCONST(EINPROGRESS);
#else
  ARCH_CFALSE(EINPROGRESS);
#endif
#ifdef ETIMEDOUT
  ARCH_CCONST(ETIMEDOUT);
#else
  ARCH_CFALSE(ETIMEDOUT);
#endif
  
#undef ARCH_CCONST
#undef ARCH_CFALSE

}
