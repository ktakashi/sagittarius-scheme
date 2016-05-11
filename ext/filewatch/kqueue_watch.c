/* -*- mode: c; coding: utf-8; -*- 
 *
 * kqueue_watch.c
 *
 *   Copyright (c) 2016  Takashi Kato <ktakashi@ymail.com>
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
 *
 *  $Id: $
 */
#include <errno.h>
#include <sys/types.h>
#include <sys/event.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "filewatch.h"

/* 
   kqueue implementation limitation.

   kqueue doesn't have any way to know which file is created/modified
   when monitoring a directory. What we can do is making a manual
   snapshot of the directory. The basic idea is the followings:
    1. read all content of the directory
    2. creates event per the file (we don't do recursive monitoring)
    3. the directory is also monitored with NOTE_EXTEND
       so that we can detect file creation
    4. when NOTE_EXTEND is notified, then re-read all directory contnet
       and checks which file is created.
    5. add the created file to event list.
   However this requires load of file discriptors. (e.g. what is
   the directory contain more than 1000 files?)
   To keep it simple, we don't support directory watch on kqueue
   implementation for now.
 */

#ifndef O_EVTONLY
# define O_EVTONLY O_RDONLY
#endif

typedef struct {
  SgObject paths;		/* list of path and flags */
} kqueue_context;

SgObject Sg_MakeFileWatchContext()
{
  SgFileWatchContext *ctx = SG_NEW(SgFileWatchContext);
  kqueue_context *kc = SG_NEW(kqueue_context);
  kc->paths = SG_NIL;
  SG_SET_CLASS(ctx, SG_CLASS_FILE_WATCH_CONTEXT);
  SG_FILE_WATCH_CONTEXT_INIT(ctx, kc);
  return SG_OBJ(ctx);
}

void Sg_DestroyFileWatchContext(SgFileWatchContext *ctx)
{
  kqueue_context *kc = (kqueue_context *)ctx->context;
  kc->paths = SG_NIL;
  SG_FILE_WATCH_CONTEXT_RELESE(ctx);
}

static int symbol2flag(SgObject flag)
{
  /* we support only greatest common things. */
  /* kqueue doesn't have monitoring access */
  /* if (SG_EQ(SG_INTERN("access"), flag)) return 0; */
  if (SG_EQ(SG_MODIFY, flag)) return NOTE_WRITE;
  if (SG_EQ(SG_DELETE, flag)) return NOTE_DELETE;
  if (SG_EQ(SG_MOVE, flag))   return NOTE_RENAME;
  if (SG_EQ(SG_ATTRIBUTE, flag))   return NOTE_ATTRIB;
  /* error? */
  return 0;
}

static int get_flag(SgObject flag)
{
  int r = 0;
  if (SG_SYMBOLP(flag)) return symbol2flag(flag);
  if (SG_PAIRP(flag)) {
    SgObject cp;
    SG_FOR_EACH(cp, flag) {
      if (SG_SYMBOLP(SG_CAR(cp))) {
	r |= symbol2flag(SG_CAR(cp));
      } else {
	goto err;
      }
    }
    return r;
  }
 err:
  Sg_Error(UC("symbol or list of symbol required. %S"), flag);
  return r;			/* dummy */
}

void Sg_AddMonitoringPath(SgFileWatchContext *ctx, SgString *path,
			  SgObject flag, /* symbol or list of symbol */
			  SgObject handler)
{
  kqueue_context *kc = (kqueue_context *)ctx->context;
  SgObject f = SG_MAKE_INT(get_flag(flag));
  SgObject abp = Sg_AbsolutePath(path);

  if (SG_FALSEP(abp)) Sg_Error(UC("Path does not exist: %A"), path);
  kc->paths = Sg_Acons(abp, f, kc->paths);

  Sg_HashTableSet(ctx->handlers, abp, handler, 0);
}

SgObject Sg_RemoveMonitoringPath(SgFileWatchContext *ctx, SgString *path)
{
  kqueue_context *kc = (kqueue_context *)ctx->context;
  SgObject abp = Sg_AbsolutePath(path), cp, prev = SG_FALSE;

  if (SG_FALSEP(abp)) return SG_FALSE;

  SG_FOR_EACH(cp, kc->paths) {
    if (Sg_StringEqual(abp, SG_STRING(SG_CAAR(cp)))) {
      if (SG_FALSEP(prev)) {
	kc->paths = SG_CDR(cp);
      } else {
	SG_SET_CDR(prev, SG_CDR(cp));
      }
      break;
    }
    prev = cp;
  }
  return Sg_HashTableDelete(ctx->handlers, abp);
}


void Sg_StartMonitoring(SgFileWatchContext *ctx)
{
  kqueue_context *kc = (kqueue_context *)ctx->context;
  int n = Sg_Length(kc->paths), i;
  SgObject cp;
  /* TODO maybe we can make it only one array? */
  struct kevent *evm;		/* monitoring */
  int *fds, kq, e = 0;

  if ((kq = kqueue()) < 0) goto err;

  /* It seems allocating this much stack would cause GC issue on OSX.
     I don't know exact reason, so it might bite me later. */
#if defined(HAVE_ALLOCA) && !defined(__APPLE__)
  evm = (struct kevent *)alloca(n * sizeof(struct kevent));
  fds = (int *)alloca(n * sizeof(int));
  for (i = 0; i < n; i++) fds[i] = 0;
#else
  evm = SG_NEW_ATOMIC2(struct kevent *, n * sizeof(struct kevent));
  fds = SG_NEW_ATOMIC2(int *, n * sizeof(int));
#endif

  i = 0;
  SG_FOR_EACH(cp, kc->paths) {
    char *p = Sg_Utf32sToUtf8s(SG_CAAR(cp));
    fds[i] = open(p, O_EVTONLY);
    if (fds[i] <= 0) goto err;

    EV_SET(&evm[i], fds[i], EVFILT_VNODE, EV_ADD | EV_CLEAR,
	   SG_INT_VALUE(SG_CDAR(cp)), 0, SG_CAAR(cp));
    i++;
  }
  /* TODO check if we can call kevent twice
          first one checks if there's change, second one retrieves the event?
   */
  while (1) {
    int c;
    if (ctx->stopRequest) goto end;
    c = kevent(kq, evm, n, evm, n, NULL);
    if (c == 0) continue;
    if (c < 0) {
      if (errno == EINTR) goto end;
      goto err;
    }
    SG_FILE_WATCH_CONTEXT_LOCK(ctx);
    if (!ctx->stopRequest) {
      for (i = 0; i < c; i++) {
	SgObject e = SG_FALSE, h;
	h = Sg_HashTableRef(ctx->handlers, evm[i].udata, SG_FALSE);
	if (SG_FALSEP(h)) continue;
	if (evm[i].fflags & NOTE_WRITE)  e = SG_MODIFIED;
	if (evm[i].fflags & NOTE_DELETE) e = SG_REMOVED;
	if (evm[i].fflags & NOTE_RENAME) e = SG_RENAMED;
	if (evm[i].fflags & NOTE_ATTRIB) e = SG_ATTRIBUTE;
	Sg_Apply2(h, evm[i].udata, e);
      }
      SG_FOR_EACH(cp, kc->paths) {
	EV_SET(&evm[i], fds[i], EVFILT_VNODE, EV_ADD | EV_CLEAR,
	       SG_INT_VALUE(SG_CDAR(cp)), 0, SG_CAAR(cp));
	i++;
      }
    }
    SG_FILE_WATCH_CONTEXT_UNLOCK(ctx);
  }
  goto end;
 err:
  e = errno;
 end:
  for (i = 0; i < n; i++) {
    if (fds[i] > 0) close(fds[i]);
  }
  close(kq);
  ctx->stopRequest = FALSE;
  if (e) {
    char *msg = strerror(e);
    Sg_SystemError(e, UC("kqueue failed: %A"), 
		   Sg_Utf8sToUtf32s(msg, strlen(msg)));
  }
}
