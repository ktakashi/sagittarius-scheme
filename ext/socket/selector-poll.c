/* selector-poll.c                                 -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2023-2025  Takashi Kato <ktakashi@ymail.com>
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
#include <errno.h>
#include <string.h>
#include <poll.h>

#include "unix-socket-selector.incl"

static int make_selector()
{
  /* 0 is not an error, but won't be closed :) */
  return 0;
}

/* Do nothing here */
static int register_socket_context(void *context, SgObject slot)
{
  (void)context;
  (void)slot;
  return TRUE;
}

static void unregister_socket_context(void *context, SgSocket *socket)
{
  (void)context;
  (void)socket;
}

typedef struct pollfd pollfd_t;

static SgObject wait_selector(unix_context_t *ctx, int nsock,
			      SgObject sockets, struct timespec *sp,
			      int *err)
{
  int n = nsock + 1;
  pollfd_t *pfds = SG_NEW_ATOMIC2(pollfd_t *, n * sizeof(pollfd_t));

  pfds[0].fd = ctx->stop_fd;
  pfds[0].events = POLLIN;
  pfds[0].revents = 0;

  SgObject cp;
  int i = 1;
  SG_FOR_EACH(cp, sockets) {
    SgObject slot = SG_CAR(cp);
    SgSocket *socket = SG_SOCKET(SG_CAR(slot));
    pfds[i].fd = socket->socket;
    pfds[i].events = POLLIN;
    pfds[i].revents = 0;
    i++;
  }

  int timeout = -1;
  if (sp) {
    timeout = sp->tv_sec * 1000;
    timeout += sp->tv_nsec / 1000000;
  }

  int c = poll(pfds, (nfds_t)n, timeout);

  if (c < 0) {
    if (errno == EINTR) return SG_NIL;
    *err = errno;
    return SG_FALSE;
  }
  /* check interrupt */
  if (pfds[0].revents & (POLLIN | POLLERR | POLLHUP)) {
    interrupted_unix_stop(ctx);
  }

  i = 1;
  SgObject r = SG_NIL;
  SG_FOR_EACH(cp, sockets) {
    if (pfds[i++].revents & POLLIN) {
      SgObject slot = SG_CAR(cp);
      r = Sg_Cons(slot, r);
    }
  }
  return r;
}
