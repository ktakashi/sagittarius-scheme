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
  WSAEVENT events[WSA_MAXIMUM_WAIT_EVENTS]; /* for sliding batch */
  HANDLE   lock;			    /* lock for closing... */
} win_context_t;


static void system_error(int code)
{
  Sg_SystemError(code,
		 UC("Setting up socket selector failed: %A"),
		 Sg_GetLastErrorMessageWithErrorCode(code));
}

static void * make_selector_context()
{
  win_context_t *ctx = SG_NEW(win_context_t);

  ctx->event = WSACreateEvent();
  if (ctx->event == WSA_INVALID_EVENT) goto err;
  for (int i = 0; i < WSA_MAXIMUM_WAIT_EVENTS; i++) {
    ctx->events[i] = WSACreateEvent();
    if (ctx->events[i] == WSA_INVALID_EVENT) goto err;
  }
  ctx->lock = CreateMutex(NULL, FALSE, NULL);

  return ctx;

 err:
  if (ctx->event != WSA_INVALID_EVENT) WSACloseEvent(ctx->event);
  for (int i = 0; i < WSA_MAXIMUM_WAIT_EVENTS; i++) {
    if (ctx->events[i] != WSA_INVALID_EVENT) WSACloseEvent(ctx->events[i]);
  }
  system_error(Sg_GetLastError());
  return NULL;		/*  dummy */
}

static void selector_finalizer_win(SgObject self, void *data)
{
  win_context_t *ctx = (win_context_t *)self;
  HANDLE locks = (HANDLE *)data;
  CloseHandle(ctx->lock);
  CloseHandle(locks[0]);
  CloseHandle(locks[1]);
}

void Sg_CloseSocketSelector(SgSocketSelector *selector)
{
  if (!Sg_SocketSelectorClosedP(selector)) {
    win_context_t *ctx = (win_context_t *)selector->context;
    HANDLE *locks = SG_NEW_ATOMIC2(HANDLE, sizeof(HANDLE) * 2);

    WaitForSingleObject(ctx->lock, INFINITE);

    selector->context = NULL;
    WSACloseEvent(ctx->event);
    ctx->event = INVALID_HANDLE_VALUE;
    for (int i = 0; i < WSA_MAXIMUM_WAIT_EVENTS; i++) {
      WSACloseEvent(ctx->events[i]);
      ctx->events[i] = INVALID_HANDLE_VALUE;
    }
    ReleaseMutex(ctx->lock);

    Sg_UnregisterFinalizer(selector);
    locks[0] = selector->rw_lock.read_lock.mutex;
    locks[1] = selector->rw_lock.write_lock.mutex;
    /*
      In case the selector is waiting, we need to keep the lock...
      We are even breaking the abstraction...
     */
    Sg_RegisterFinalizer(ctx, selector_finalizer_win, locks);
  }
}

static SgObject win_selector_wait(win_context_t *ctx, int n,
				  SgSocketSelector *selector,
				  SgObject sockets,
				  struct timespec *sp)
{
  const int waiting_flags = FD_READ | FD_OOB | FD_ACCEPT;
  int r, err = FALSE;
  DWORD millis = INFINITE;
  SgObject ret = SG_NIL;

#define SET_EVENT(sockets, event, flags)				\
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
    millis = (DWORD)(sp->tv_sec * 1000);
    millis += sp->tv_nsec / 1000000;
  }

  r = WSAWaitForMultipleEvents(1, &ctx->event, FALSE, millis, FALSE);
  /* most likely closed selector */
  if (r == WSA_WAIT_FAILED) {
    err = TRUE;
    goto cleanup;
  }
  /* unassociate ctx->event  */
  SET_EVENT(sockets, ctx->event, 0);
  
  if (r == WSA_WAIT_EVENT_0) {
    int off = (n%WSA_MAXIMUM_WAIT_EVENTS) > 0 ? 1 : 0;
    int batch = (n/WSA_MAXIMUM_WAIT_EVENTS) + off;
    int size = min(n, WSA_MAXIMUM_WAIT_EVENTS);
    SgObject cp = sockets;
    r = WaitForSingleObject(ctx->lock, INFINITE);
    if (r == WAIT_OBJECT_0 && ctx->event != INVALID_HANDLE_VALUE) {
      WSAResetEvent(ctx->event);	/* reset event */
      while (SG_PAIRP(cp)) {
	SgObject h = SG_NIL, t = SG_NIL;
	int count = 0;
	for (int i = 0; i < size && SG_PAIRP(cp); i++, cp = SG_CDR(cp)) {
	  SgObject slot = SG_CAR(cp);
	  SgSocket *so = SG_SOCKET(SG_CAR(slot));
	  WSAEventSelect(so->socket, ctx->events[i], waiting_flags);
	  count++;
	  SG_APPEND1(h, t, slot);
	}
	r = WSAWaitForMultipleEvents(count, ctx->events, FALSE, 0, FALSE);
	if (r != WSA_WAIT_TIMEOUT && r != WSA_WAIT_FAILED) {
	  int i = 0;
	  SgObject cp2;
	  SG_FOR_EACH(cp2, h) {
	    SgObject slot = SG_CAR(cp2);
	    SgSocket *s = SG_SOCKET(SG_CAR(slot));
	    WSANETWORKEVENTS ne;
	    if (WSAEnumNetworkEvents(s->socket, ctx->events[i], &ne) == 0) {
	      if ((ne.lNetworkEvents & waiting_flags) != 0) {
		/* a bit sloppy solution to avoid socket-accept to go
		   into inifinite waiting.

		   WSAEnumNetworkEvents clears the internal event records
		   and socket-accept on Windows waits for FD_ACCEPT event,
		   which is already cleared by the API. Now, luckily we use
		   multiple events to emulate the same behaviour as POSIX,
		   so abuse it here.
		   NOTE: socket.event member is meant to capture socket
		         close, but we can use it to avoid inifinte waiting
		*/
		if (ne.lNetworkEvents & FD_ACCEPT) WSASetEvent(s->event);
		ret = Sg_Cons(slot, ret);
	      }
	    }
	  }
	}
	SG_FOR_EACH(cp, h) {
	  SgSocket *s = SG_SOCKET(SG_CAAR(cp));
	  WSAEventSelect(s->socket, NULL, 0);
	}
      }
      ReleaseMutex(ctx->lock);
    }
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

static SgObject selector_wait(SgSocketSelector *selector, SgObject sockets,
			      void *context, int n, struct timespec *sp)
{
  win_context_t *ctx = (win_context_t *)context;
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
