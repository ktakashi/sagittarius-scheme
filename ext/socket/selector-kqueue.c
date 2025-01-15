/* selector-kqueue.c                                -*- mode:c; coding:utf-8; -*-
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
#include <sys/types.h>
#include <sys/event.h>

#include "unix-socket-selector.incl"

static int make_selector()
{
  return kqueue();
}

static void add_socket(SgSocketSelector *selector, SgObject slot)
{
  /* do nothing :) */
}


static void remove_socket(SgSocketSelector *selector, SgSocket *socket)
{
  /* do nothing */
}


static SgObject wait_selector(unix_context_t *ctx, int nsock,
			      SgObject sockets, struct timespec *sp)
{
  SgObject cp, r = SG_NIL;
  int i, c, n = nsock + 1;
  struct kevent *evm, ev;

  evm = SG_NEW_ATOMIC2(struct kevent *, n * sizeof(struct kevent));
  i = 0;
  SG_FOR_EACH(cp, sockets) {
    SgObject slot = SG_CAR(cp);
    SgSocket *s = SG_SOCKET(SG_CAR(slot));
    EV_SET(&evm[i++], s->socket, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, slot);
  }
  EV_SET(&evm[i++], ctx->stop_fd, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
  c = kevent(ctx->fd, evm, n, evm, n, sp);
  if (c < 0) return system_error(errno, -1);
  
  for (i = 0; i < c; i++) {
    if (evm[i].ident == ctx->stop_fd) {
      interrupted_unix_stop(ctx);
    } else if (evm[i].filter == EVFILT_READ) {
      r = Sg_Cons(evm[i].udata, r);
      EV_SET(&ev, evm[i].ident, EVFILT_READ, EV_DELETE, 0, 0, NULL);
      kevent(ctx->fd, &ev, 1, NULL, 0, 0); /* reset event of the target socket */
    }
  }
  return r;
}

