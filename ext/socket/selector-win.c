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

#if defined(USE_BOEHM_GC)
# define GC_WIN32_THREADS
# if defined(HAVE_GC_H)
#  include <gc.h>
# elif defined(HAVE_GC_GC_H)
#  include <gc/gc.h>
# endif
#else
# error "Not supported yet"
#endif

typedef struct win_context_rec
{
  WSAEVENT event;
  SgObject results;		/* result for multiple waiting */
  HANDLE  *threads;		/* threads for multiple waiting */
  HANDLE   lock;		/* lock */
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

static void add_socket(SgSocketSelector *selector, SgObject slot)
{
  /* do nothing */
}

static void * make_selector_context()
{
  win_context_t *ctx = SG_NEW(win_context_t);

  ctx->event = WSACreateEvent();
  if (ctx->event == NULL) goto err;
  ctx->lock = CreateMutex(NULL, FALSE, NULL);

  return ctx;

 err:
  system_error(Sg_GetLastError());
  return NULL;		/*  dummy */
}

void Sg_CloseSocketSelector(SgSocketSelector *selector)
{
  if (!Sg_SocketSelectorClosedP(selector)) {
    win_context_t *ctx = (win_context_t *)selector->context;
    selector->context = NULL;
    WSACloseEvent(ctx->event);
    CloseHandle(ctx->lock);
    Sg_UnregisterFinalizer(selector);
  }
}

static SgObject win_selector_wait(win_context_t *ctx, int n,
				  SgSocketSelector *selector,
				  SgObject sockets,
				  struct timespec *sp)
{
  const int waiting_flags = FD_READ | FD_OOB;
  int r, err = FALSE;
  DWORD millis = INFINITE;
  SgObject ret = SG_NIL;

#define SET_EVENT(sockets, event, flags)					\
  do {									\
    SgObject cp;							\
    int i = 0;								\
    SG_FOR_EACH(cp, sockets) {						\
      SgObject s = SG_CAAR(cp);						\
      /* avoid unwanted sockets */					\
      if (i == n) break;						\
      if (WSAEventSelect(SG_SOCKET(s)->socket, event, flags) != 0) { \
	err = TRUE;							\
	goto cleanup;							\
      }									\
      i++;								\
    }									\
  } while(0)
  SET_EVENT(sockets, ctx->event, waiting_flags);
  
  if (sp) {
    millis = sp->tv_sec * 1000;
    millis += sp->tv_nsec / 1000000;
  }

  r = WSAWaitForMultipleEvents(1, &ctx->event, FALSE, millis, FALSE);
  /* most likely closed selector */
  if (r == WSA_WAIT_FAILED) {
    err = TRUE;
    goto cleanup;
  }
  /* unassociate event  */
  SET_EVENT(sockets, ctx->event, 0);
  WSAResetEvent(ctx->event);	/* reset event */

  if (r == WSA_WAIT_EVENT_0) {
    int batch = (n/WSA_MAXIMUM_WAIT_EVENTS) + (n%WSA_MAXIMUM_WAIT_EVENTS)>0 ? 1 : 0;
    int size = max(n, WSA_MAXIMUM_WAIT_EVENTS);
    WSAEVENT events[WSA_MAXIMUM_WAIT_EVENTS] = { NULL, };
    
    for (int i = 0; i < size; i++) events[i] = WSACreateEvent();

    SgObject cp = sockets;
    WSANETWORKEVENTS networkEvents;

    while (SG_PAIRP(cp)) {
      SgObject h = SG_NIL, t = SG_NIL;
      for (int i = 0; i < size && SG_PAIRP(cp); i++, cp = SG_CDR(cp)) {
	SgObject slot = SG_CAR(cp);
	SgSocket *so = SG_SOCKET(SG_CAR(slot));
	WSAEventSelect(so->socket, events[i], waiting_flags);
	SG_APPEND1(h, t, slot);
      }
      r = WSAWaitForMultipleEvents(size, events, FALSE, 0, FALSE);
      if (r != WSA_WAIT_TIMEOUT && r != WSA_WAIT_FAILED) {
	int i = 0;
	SgObject cp2;
	SG_FOR_EACH(cp2, h) {
	  SgObject slot = SG_CAR(cp2);
	  SgSocket *s = SG_SOCKET(SG_CAR(slot));
	  if (WSAEnumNetworkEvents(s->socket, events[i], &networkEvents) == 0) {
	    if ((networkEvents.lNetworkEvents & waiting_flags) != 0) {
	      ret = Sg_Cons(slot, ret);
	    }
	  }
	  WSAEventSelect(s->socket, NULL, 0);
	}
      }
    }
    for (int i = 0; i < size; i++) WSACloseEvent(events[i]);
  }

#undef SET_EVENT

cleanup:
  if (err) {
    int i = 0;
    SgObject cp;
    int e = WSAGetLastError();
    SG_FOR_EACH(cp, sockets) {
      if (i++ == n) break;
      SgObject s = SG_CAAR(cp);
      WSAEventSelect(SG_SOCKET(s)->socket, NULL, 0);
    }
    selector->waiting = FALSE;
    if (e == WSA_INVALID_HANDLE) {
      Sg_Error(UC("Socket selector is closed during waiting: %A"), selector);
    }
    Sg_Error(UC("Failed to wait selector: [%d] %S"), e,
	     Sg_GetLastErrorMessageWithErrorCode(e));
  }
  return ret;
}

static SgObject selector_wait(SgSocketSelector *selector, int n,
			      struct timespec *sp)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  SgObject sockets = Sg_Reverse(selector->sockets);
   return win_selector_wait(ctx, n, selector, sockets, sp);
}


SgObject Sg_SocketSelectorInterrupt(SgSocketSelector *selector)
{
  win_context_t *ctx;
  if (Sg_SocketSelectorClosedP(selector)) {
    Sg_Error(UC("Socket selector is closed: %A"), selector);
  }
  ctx = (win_context_t *)selector->context;
  WSASetEvent(ctx->event);
  return selector;
}
