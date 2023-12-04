/* selector-epoll.c                                -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2023  Takashi Kato <ktakashi@ymail.com>
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
#include "socket-selector.h"
#include <string.h>
#include <sys/epoll.h>

#include "unix-socket.incl"

typedef struct epoll_context_rec
{
  int epfd;
  int stop_fd;
  struct sockaddr_un *addr;
  SgObject events;
} epoll_context_t;

static void selector_finalizer(SgObject self, void *data)
{
  Sg_CloseSocketSelector(SG_SOCKET_SELECTOR(self));
}

SgObject Sg_MakeSocketSelector()
{
  SgSocketSelector *selector = SG_NEW(SgSocketSelector);
  epoll_context_t *ctx = SG_NEW(epoll_context_t);
  struct kevent *ke = SG_NEW_ATOMIC(struct kevent);

  SG_SET_CLASS(selector, SG_CLASS_SOCKET_SELECTOR);
  if ((ctx->epfd = epoll_create1(0)) < 0) goto err;
  ctx->events = SG_NIL;

  ctx->stop_fd = unix_socket();
  if (ctx->stop_fd == -1) goto err;
  
  ctx->addr = bind_unix_socket(ctx->stop_fd);
  if (ctx->addr == NULL) {
    close(ctx->stop_fd);
    goto err;
  }
  selector->context = ctx;
  ctx->events = Sg_Cons(selector, ctx->events);

  Sg_RegisterFinalizer(selector, selector_finalizer, NULL);
  return SG_OBJ(selector);

 err: {
    int e = errno;
    char *msg = strerror(e);
    close(ctx->epfd);
    Sg_SystemError(e, UC("Setting up epoll failed: %A"),
		   Sg_Utf8sToUtf32s(msg, strlen(msg)));
  }
}

void Sg_CloseSocketSelector(SgSocketSelector *selector)
{
  epoll_context_t *ctx = (epoll_context_t *)selector->context;
  close(ctx->epfd);
  close(ctx->stop_fd);
  Sg_UnregisterFinalizer(selector);
}

SgObject Sg_SocketSelectorAdd(SgSocketSelector *selector, SgSocket *socket)
{
  epoll_context_t *ctx = (epoll_context_t *)selector->context;
  struct epoll_event ev;
  ev.events = EPOLLIN;
  ev.data.ptr = socket;
  epoll_ctl(ctx->epfd, EPOLL_CTL_ADD, socket.socket, &event);
  ctx->events = Sg_Cons(socket, ctx->events);
  return SG_OBJ(selector);
}

SgObject Sg_SocketSelectorWait(SgSocketSelector *selector, SgObject timeout)
{
  epoll_context_t *ctx = (epoll_context_t *)selector->context;
  int n = Sg_Length(ctx->events), i, c;
  long millis = -1;
  SgObject cp, r = SG_NIL;
  struct timespec spec, *sp;
  struct epoll_event *evm;
  
  sp = Sg_GetTimeSpec(timeout, &spec);
  if (sp) {
    millis = sp->tv_sec * 1000;
    millis += sp->tv_usec / 1000;
  }

  evm = SG_NEW_ATOMIC2(struct kevent *, n * sizeof(struct kevent));
  c = epoll_wait(ctx->epfd, evm, n, millis);
  if (c < 0) {
    int e = errno;
    char *msg = strerror(e);
    Sg_SystemError(e, UC("kevent failed: %A"), 
		   Sg_Utf8sToUtf32s(msg, strlen(msg)));
    return SG_UNDEF;		/* dummy */
  }
  for (i = 0; i < c; i++) {
    if (SG_SOCKETP(evm[i].data.ptr) && evm[i].events == EPOLLIN) {
      r = Sg_Cons(evm[i].data.ptr, r);
    }
  }
  return r;
}

SgObject Sg_SocketSelectorInterrupt(SgSocketSelector *selector)
{
  epoll_context_t *ctx = (epoll_context_t *)selector->context;
  const char *stop = "stop it";
  sendto_unix_socket(ctx->addr, (const uint8_t *)stop, 7);
  return selector;
}
