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

#if 0
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

static int win_selector_wait_inner(win_context_t *ctx, int n,
				   SgSocketSelector *selector,
				   SgObject sockets,
				   struct timespec *sp,
				   SgObject *result)
{
  const int waiting_flags = FD_READ | FD_OOB;
  int r, err = FALSE;
  DWORD millis = INFINITE;
  SgObject ret = SG_NIL;
  SOCKET sArray[WSA_MAXIMUM_WAIT_EVENTS];
  WSAEVENT eArray[WSA_MAXIMUM_WAIT_EVENTS] = { NULL, };

  eArray[n] = ctx->event;
#define SET_EVENT(sockets, flags)				\
  do {								\
    SgObject cp;						\
    int i = 0;							\
    SG_FOR_EACH(cp, sockets) {					\
      SgObject s = SG_CAAR(cp);					\
      /* avoid unwanted sockets */				\
      if (i == n) break;					\
      sArray[i] = SG_SOCKET(s)->socket;				\
      eArray[i] = WSACreateEvent();				\
      if (WSAEventSelect(sArray[i], eArray[i], flags) != 0) {	\
	err = TRUE;						\
	goto cleanup;						\
      }								\
      i++;							\
    }								\
  } while(0)
  SET_EVENT(sockets, waiting_flags);
  
  if (sp) {
    millis = sp->tv_sec * 1000;
    millis += sp->tv_nsec / 1000000;
  }

  r = WSAWaitForMultipleEvents(n + 1, eArray, FALSE, millis, FALSE);
  /* most likely closed selector */
  if (r == WSA_WAIT_FAILED) {
    err = TRUE;
    goto cleanup;
  }
  /* reset interrupting event as soon as possible */
  WSAResetEvent(ctx->event);
  for (int i = r - WSA_WAIT_EVENT_0; i < n; i++) {
    r = WSAWaitForMultipleEvents(1, &eArray[i], TRUE, 0, FALSE);
    if (r != WSA_WAIT_FAILED && r != WSA_WAIT_TIMEOUT) {
      WSANETWORKEVENTS networkEvents;
      if (WSAEnumNetworkEvents(sArray[i], eArray[i], &networkEvents) == 0) {
	if ((networkEvents.lNetworkEvents & waiting_flags) != 0) {
	  SgObject o = select_socket(sArray[i], sockets);
	  if (!SG_FALSEP(o)) ret = Sg_Cons(o, ret);
	}
      }
    }
  }

#undef SET_EVENT

cleanup:
  for (int i = 0; i < n; i++) {
    if (eArray[i]) {
      WSAEventSelect(sArray[i], eArray[i], 0);
      WSACloseEvent(eArray[i]);
    }
  }
  *result = ret;
  return err;
}

static SgObject win_selector_wait(win_context_t *ctx, int n,
				  SgSocketSelector *selector,
				  SgObject sockets,
				  struct timespec *sp)
{
  SgObject r = SG_NIL;
  int err = win_selector_wait_inner(ctx, n, selector, sockets, sp, &r);
  if (err) {
    int e = WSAGetLastError();
    selector->waiting = FALSE;
    if (e == WSA_INVALID_HANDLE) {
      Sg_Error(UC("Socket selector is closed during waiting: %A"), selector);
    }
    Sg_Error(UC("Failed to wait selector: [%d] %S"), e,
	     Sg_GetLastErrorMessageWithErrorCode(e));
  }
  return r;
}

static DWORD WINAPI selector_wait_entry(void *param)
{
  void **data = (void **)param;
  SgObject sockets = SG_OBJ(data[0]);
  struct timespec *sp = (struct timespec *)data[1];
  SgSocketSelector *selector = (SgSocketSelector *)data[2];
  win_context_t *ctx = (win_context_t *)selector->context;
  SgObject r = SG_NIL, cp;
  int n = Sg_Length(sockets);
  int err = win_selector_wait_inner(ctx, n, selector, sockets, sp, &r);

  WaitForSingleObject(ctx->lock, INFINITE);
  SG_FOR_EACH(cp, r) {
    ctx->results = Sg_Cons(SG_CAR(cp), ctx->results);
  }
  ReleaseMutex(ctx->lock);
  return 0;
}

static SgObject win_selector_wait_multi(win_context_t *ctx, int n, int batch,
					SgSocketSelector *selector,
					SgObject sockets,
					struct timespec *sp)
{
  SgObject r;
  int i;
  ctx->results = SG_NIL;
  ctx->threads = SG_NEW_ARRAY(HANDLE, batch);

  for (i = 0; i < batch; i++) {
    SgObject waits = SG_NIL;
    int j;
    void **data = SG_NEW_ARRAY(void *, 3);
    for (j = 0; j < WSA_MAXIMUM_WAIT_EVENTS - 1; j++) {
      if (SG_NULLP(sockets)) break;
      waits = Sg_Cons(SG_CAR(sockets), waits);
      sockets = SG_CDR(sockets);
    }
    data[0] = waits;
    data[1] = sp;
    data[2] = selector;
    ctx->threads[i] = CreateThread(NULL, 0, selector_wait_entry,
				   data, 0, NULL);
  }
  for (i = 0; i < batch; i++) {
    WaitForSingleObject(ctx->threads[i], INFINITE);
  }
  
  r = ctx->results;
  ctx->results = SG_NIL;
  ctx->threads = NULL;
  return r;
}

static SgObject selector_wait(SgSocketSelector *selector, int n,
			      struct timespec *sp)
{
  win_context_t *ctx = (win_context_t *)selector->context;
  SgObject sockets = Sg_Reverse(selector->sockets);
  /* WSA_MAXIMUM_WAIT_EVENTS = 64... */
  if (n+1 > WSA_MAXIMUM_WAIT_EVENTS) {
    int batch = n % (WSA_MAXIMUM_WAIT_EVENTS - 1);
    return win_selector_wait_multi(ctx, n, batch, selector, sockets, sp);
  }

  return win_selector_wait(ctx, n, selector, sockets, sp);
}
#endif

static SgObject win_selector_wait(win_context_t *ctx, int n,
				  SgSocketSelector *selector,
				  SgObject sockets,
				  struct timespec *sp)
{
  const int waiting_flags = FD_READ | FD_OOB;
  int r, err = FALSE;
  DWORD millis = INFINITE;
  SgObject ret = SG_NIL;
  WSAEVENT eArray[2] = { NULL, };

  eArray[1] = ctx->event;
  eArray[0] = WSACreateEvent();
#define SET_EVENT(sockets, flags)					\
  do {									\
    SgObject cp;							\
    int i = 0;								\
    SG_FOR_EACH(cp, sockets) {						\
      SgObject s = SG_CAAR(cp);						\
      /* avoid unwanted sockets */					\
      if (i == n) break;						\
      if (WSAEventSelect(SG_SOCKET(s)->socket, eArray[0], flags) != 0) { \
	err = TRUE;							\
	goto cleanup;							\
      }									\
      i++;								\
    }									\
  } while(0)
  SET_EVENT(sockets, waiting_flags);
  
  if (sp) {
    millis = sp->tv_sec * 1000;
    millis += sp->tv_nsec / 1000000;
  }

  r = WSAWaitForMultipleEvents(2, eArray, FALSE, millis, FALSE);
  /* most likely closed selector */
  if (r == WSA_WAIT_FAILED) {
    err = TRUE;
    goto cleanup;
  }
  /* reset interrupting event as soon as possible */
  WSAResetEvent(ctx->event);
  if (r == WSA_WAIT_EVENT_0) {
    SgObject cp;
    WSANETWORKEVENTS networkEvents;
    SG_FOR_EACH(cp, sockets) {
      SgSocket *so = SG_SOCKET(SG_CAAR(cp));
      if (WSAEnumNetworkEvents(so->socket, eArray[0], &networkEvents) == 0) {
	if ((networkEvents.lNetworkEvents & waiting_flags) != 0) {
	  ret = Sg_Cons(so, ret);
	}
      }
    }
  }

#undef SET_EVENT

cleanup:
  if (eArray[0]) {
    int i = 0;
    SgObject cp;
    SG_FOR_EACH(cp, sockets) {
      SgObject s = SG_CAAR(cp);
      WSAEventSelect(SG_SOCKET(s)->socket, eArray[0], 0);
    }
    WSACloseEvent(eArray[1]);
  }

  if (err) {
    int e = WSAGetLastError();
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
