/* selector-iocp.c                                 -*- mode:c; coding:utf-8; -*-
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
#include <windows.h>

#include "socket-selector.incl"

typedef struct win_context_rec
{
  HANDLE event;
  HANDLE thread;		/* waiting thread */
} win_context_t;


static void system_error(int code)
{
  Sg_SystemError(code,
		 UC("Setting up IOCP failed: %A"),
		 Sg_GetLastErrorMessageWithErrorCode(code));
}

SgObject Sg_MakeSocketSelector()
{
  SgSocketSelector *selector = SG_NEW(SgSocketSelector);
  win_context_t *ctx = SG_NEW(win_context_t);

  SG_SET_CLASS(selector, SG_CLASS_SOCKET_SELECTOR);

  ctx->event = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (ctx->event == NULL) goto err;
  
  ctx->thread = NULL;
  selector->sockets = SG_NIL;
  selector->context = ctx;

  Sg_RegisterFinalizer(selector, selector_finalizer, NULL);
  return SG_OBJ(selector);

 err:
  system_error(Sg_GetLastError());
  return SG_UNDEF;		/*  dummy */
}

void Sg_CloseSocketSelector(SgSocketSelector *selector)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  CloseHandle(ctx->event);
  Sg_UnregisterFinalizer(selector);
}

SgObject Sg_SocketSelectorAdd(SgSocketSelector *selector, SgSocket *socket)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  selector->sockets = Sg_Cons(socket, selector->sockets);
  selector_sockets(selector);
  return SG_OBJ(selector);
}

static SgObject select_socket(SOCKET fd, SgObject sockets)
{
  SgObject cp;
  SG_FOR_EACH(cp, sockets) {
    SgSocket *sock = SG_SOCKET(SG_CAR(cp));
    if (sock->socket == fd) return sock;
  }
  return SG_FALSE;
}

SgObject Sg_SocketSelectorWait(SgSocketSelector *selector, SgObject timeout)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  int n = selector_sockets(selector), millis = INFINITE, r;
  HANDLE hEvents[2];
  struct timespec spec, *sp;
  SgObject ret = SG_NIL;

  if (ctx->thread != NULL) {
    Sg_Error(UC("There's a thread already waiting for %A"), selector);
  }
  if (n == 0) return ret;
  ctx->thread = GetCurrentThread();

  hEvents[0] = CreateEvent(NULL, FALSE, FALSE, NULL);
  hEvents[1] = ctx->event;

#define SET_EVENT(sockets, event, flags)		\
  do {							\
    SgObject cp;					\
    SG_FOR_EACH(cp, sockets) {				\
      SG_SET_SOCKET_EVENT(SG_CAR(cp), event, flags);	\
    }							\
  } while(0)

  SET_EVENT(selector->sockets, hEvents[0], FD_READ | FD_OOB);
  
  sp = selector_timespec(timeout, &spec);
  if (sp) {
    millis = sp->tv_sec * 1000;
    millis += sp->tv_nsec / 1000000;
  }

  r = WaitForMultipleObjects(2, hEvents, FALSE, millis);
  if (r == WAIT_OBJECT_0) {
    /* Using WSAPoll to detect which sockets are ready to read */
    WSAPOLLFD *fds = SG_NEW_ATOMIC2(WSAPOLLFD *, n * sizeof(WSAPOLLFD));
    int i = 0;
    SgObject cp;
    SG_FOR_EACH(cp, selector->sockets) {
      SgSocket *sock = SG_SOCKET(SG_CAR(cp));
      fds[i].fd = sock->socket;
      fds[i].events = POLLRDNORM;
      i++;
    }
    r = WSAPoll(fds, n, 0); /* Some sockets must be ready at this stage */
    if (r == SOCKET_ERROR) system_error(WSAGetLastError());
    for (i = 0; i < n; i++) {
      if (fds[i].revents & POLLRDNORM) {
	/* collect sockets, should we use hashtable? */
	SgObject o = select_socket(fds[i].fd, selector->sockets);
	if (!SG_FALSEP(o)) ret = Sg_Cons(o, ret);
      }
    }
  }

  SET_EVENT(selector->sockets, hEvents[0], 0);
#undef SET_EVENT

  if (!SG_NULLP(ret)) {
    /* remove the returned sockets from the targets */
    SgObject h = SG_NIL, t = SG_NIL, cp;
    SG_FOR_EACH(cp, selector->sockets) {
      if (SG_FALSEP(Sg_Memq(SG_CAR(cp), ret))) {
	SG_APPEND1(h, t, SG_CAR(cp));
      }
    }
    selector->sockets = h;
  }
  CloseHandle(hEvents[0]);
  ctx->thread = NULL;
  return ret;
}

SgObject Sg_SocketSelectorInterrupt(SgSocketSelector *selector)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  if (ctx->thread) {
    SetEvent(ctx->event);
  }
  return selector;
}
