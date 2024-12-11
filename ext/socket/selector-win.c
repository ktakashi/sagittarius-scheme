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
  WSAEVENT event;
  HANDLE   thread;		/* waiting thread */
} win_context_t;


static void system_error(int code)
{
  Sg_SystemError(code,
		 UC("Setting up IOCP failed: %A"),
		 Sg_GetLastErrorMessageWithErrorCode(code));
}

static void remove_socket(SgSocketSelector *selector, SgSocket *socket)
{
  /* do nothing */
}

SgObject Sg_MakeSocketSelector()
{
  SgSocketSelector *selector = SG_NEW(SgSocketSelector);
  win_context_t *ctx = SG_NEW(win_context_t);

  SG_SET_CLASS(selector, SG_CLASS_SOCKET_SELECTOR);

  ctx->event = WSACreateEvent();
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
  WSACloseEvent(ctx->event);
  Sg_UnregisterFinalizer(selector);
}

SgObject Sg_SocketSelectorAdd(SgSocketSelector *selector,
			      SgSocket *socket, SgObject data)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  if (Sg_SocketOpenP(socket)) {
    selector->sockets = Sg_Cons(Sg_Cons(socket, data), selector->sockets);
    selector_sockets(selector);
  }
  return SG_OBJ(selector);
}

static SgObject select_socket(SOCKET fd, SgObject sockets)
{
  SgObject cp;
  SG_FOR_EACH(cp, sockets) {
    SgObject slot = SG_CAR(cp);
    SgSocket *sock = SG_SOCKET(SG_CAR(slot));
    if (sock->socket == fd) return slot;
  }
  return SG_FALSE;
}

SgObject Sg_SocketSelectorWait(SgSocketSelector *selector, SgObject timeout)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  int n = selector_sockets(selector), r, err = FALSE;
  const int waiting_flags = FD_READ | FD_OOB;
  struct timespec spec, *sp;
  DWORD millis = INFINITE;
  SgObject ret = SG_NIL;
  SOCKET sArray[WSA_MAXIMUM_WAIT_EVENTS];
  WSAEVENT eArray[WSA_MAXIMUM_WAIT_EVENTS] = { NULL, };

  if (ctx->thread != NULL) {
    Sg_Error(UC("There's a thread already waiting for %A"), selector);
  }
  if (n == 0) return ret;
  if (n-1 > WSA_MAXIMUM_WAIT_EVENTS) {
    Sg_Error(UC("[Windows] More than max selectable sockets are set %d > %d"),
	     n, WSA_MAXIMUM_WAIT_EVENTS);
  }
  ctx->thread = GetCurrentThread();
  eArray[n] = ctx->event;
#define SET_EVENT(sockets, flags)				\
  do {								\
    SgObject cp;						\
    int i = 0;							\
    SG_FOR_EACH(cp, sockets) {					\
      SgObject s = SG_CAAR(cp);					\
      sArray[i] = SG_SOCKET(s)->socket;				\
      eArray[i] = WSACreateEvent();				\
      if (WSAEventSelect(sArray[i], eArray[i], flags) != 0) {	\
	err = TRUE;						\
	goto cleanup;						\
      }								\
      i++;							\
    }								\
  } while(0)
  SET_EVENT(selector->sockets, waiting_flags);
  
  sp = selector_timespec(timeout, &spec);
  if (sp) {
    millis = sp->tv_sec * 1000;
    millis += sp->tv_nsec / 1000000;
  }

  r = WSAWaitForMultipleEvents(n + 1, eArray, FALSE, millis, FALSE);
  for (int i = r - WSA_WAIT_EVENT_0; i < n; i++) {
    r = WSAWaitForMultipleEvents(1, &eArray[i], TRUE, 0, FALSE);
    if (r != WSA_WAIT_FAILED && r != WSA_WAIT_TIMEOUT) {
      WSANETWORKEVENTS networkEvents;
      if (WSAEnumNetworkEvents(sArray[i], eArray[i], &networkEvents) == 0) {
	if ((networkEvents.lNetworkEvents & waiting_flags) != 0) {
	  SgObject o = select_socket(sArray[i], selector->sockets);
	  if (!SG_FALSEP(o)) ret = Sg_Cons(o, ret);
	}
      }
    }
  }

#undef SET_EVENT

  strip_sockets(selector, ret);

cleanup:
  for (int i = 0; i < n; i++) {
    if (eArray[i]) WSACloseEvent(eArray[i]);
  }
  ctx->thread = NULL;
  if (err) {
    int e = WSAGetLastError();
    Sg_Error(UC("Failed to wait selector: [%d] %S"), e,
	     Sg_GetLastErrorMessageWithErrorCode(e));
  }
  return ret;
}

int Sg_SocketSelectorWaitingP(SgSocketSelector *selector)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  return ctx->thread != NULL;
}


SgObject Sg_SocketSelectorInterrupt(SgSocketSelector *selector)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  WSASetEvent(ctx->event);
  return selector;
}
