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

#include "unix-socket-selector.incl"

static int make_selector()
{
  return epoll_create1(0);
}


static void add_socket(unix_context_t *ctx, SgObject slot)
{
  struct epoll_event ev;
  SgSocket *socket = SG_SOCKET(SG_CAR(slot));
  ev.events = EPOLLIN;
  ev.data.ptr = slot;
  epoll_ctl(ctx->fd, EPOLL_CTL_ADD, socket->socket, &ev);
}

static void remove_socket_ctx(unix_context_t *ctx, SgSocket *socket)
{
  /* BUG on kernel < 2.6.9 */
  struct epoll_event ev;
  ev.events = EPOLLIN;
  ev.data.ptr = NULL;
  epoll_ctl(ctx->fd, EPOLL_CTL_DEL, socket->socket, &ev);
}

static void remove_socket(SgSocketSelector *selector, SgSocket *socket)
{
  unix_context_t *ctx = (unix_context_t *)selector->context;
  remove_socket_ctx(ctx, socket);
}


static SgObject wait_selector(unix_context_t *ctx, int nsock,
			      SgObject sockets, SgObject timeout)
{
  int n = nsock + 1, i, c;
  long millis = -1;
  SgObject r = SG_NIL;
  struct timespec spec, *sp;
  struct epoll_event *evm, ev;

  sp = selector_timespec(timeout, &spec);
  if (sp) {
    millis = sp->tv_sec * 1000;
    millis += sp->tv_nsec / 1000000;
  }

  ev.events = EPOLLIN;
  ev.data.ptr = SG_FALSE;
  epoll_ctl(ctx->fd, EPOLL_CTL_ADD, ctx->stop_fd, &ev);
  
  evm = SG_NEW_ATOMIC2(struct epoll_event *, n * sizeof(struct epoll_event));
  c = epoll_wait(ctx->fd, evm, n, millis);

  if (c < 0) return system_error(errno, -1);

  for (i = 0; i < c; i++) {
    if (SG_FALSEP(evm[i].data.ptr)) {
      interrupted_unix_stop(ctx);
      epoll_ctl(ctx->fd, EPOLL_CTL_DEL, ctx->stop_fd, &ev);
    } else if (SG_PAIRP(evm[i].data.ptr) && evm[i].events == EPOLLIN) {
      SgObject slot = SG_OBJ(evm[i].data.ptr);
      SgSocket *sock = SG_SOCKET(SG_CAR(slot));
      r = Sg_Cons(slot, r);
      remove_socket_ctx(ctx, sock);
    }
  }
  return r;
}
