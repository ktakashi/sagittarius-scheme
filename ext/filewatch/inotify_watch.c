/* -*- mode: c; coding: utf-8; -*- 
 *
 * inotify_watch.c
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
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/inotify.h>
#include <unistd.h>
#include <string.h>

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "filewatch.h"

typedef struct {
  SgObject paths;		/* list of path and flags */
} inotify_context;

SgObject Sg_MakeFileWatchContext()
{
  SgFileWatchContext *ctx = SG_NEW(SgFileWatchContext);
  inotify_context *ic = SG_NEW(inotify_context);
  ic->paths = SG_NIL;
  SG_SET_CLASS(ctx, SG_CLASS_FILE_WATCH_CONTEXT);
  SG_FILE_WATCH_CONTEXT_INIT(ctx, ic);
  return SG_OBJ(ctx);
}

void Sg_DestroyFileWatchContext(SgFileWatchContext *ctx)
{
  inotify_context *ic = (inotify_context *)ctx->context;
  ic->paths = SG_NIL;
  SG_FILE_WATCH_CONTEXT_RELESE(ctx);
}


static int symbol2flag(SgObject flag)
{
  /* we support only greatest common things. */
  if (SG_EQ(SG_ACCESS, 	  flag)) return IN_ACCESS;
  if (SG_EQ(SG_MODIFY, 	  flag)) return IN_MODIFY;
  if (SG_EQ(SG_DELETE, 	  flag)) return IN_DELETE;
  if (SG_EQ(SG_MOVE,   	  flag)) return IN_MOVE;
  if (SG_EQ(SG_ATTRIBUTE, flag)) return IN_ATTRIB;
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
  inotify_context *ic = (inotify_context *)ctx->context;
  SgObject abp = Sg_AbsolutePath(path);
  SgObject f = SG_MAKE_INT(get_flag(flag));

  if (SG_FALSEP(abp)) Sg_Error(UC("Path does not exist: %A"), path);
  ic->paths = Sg_Acons(abp, f, ic->paths);
  Sg_HashTableSet(ctx->handlers, abp, handler, 0);
}
SgObject Sg_RemoveMonitoringPath(SgFileWatchContext *ctx, SgString *path)
{
  inotify_context *ic = (inotify_context *)ctx->context;
  SgObject cp, abp = Sg_AbsolutePath(path), prev = SG_FALSE;

  if (SG_FALSEP(abp)) return SG_FALSE;

  SG_FOR_EACH(cp, ic->paths) {
    if (Sg_StringEqual(abp, SG_STRING(SG_CAAR(cp)))) {
      if (SG_FALSEP(prev)) {
	ic->paths = SG_CDR(cp);
      } else {
	SG_SET_CDR(prev, SG_CDR(cp));
      }
      break;
    }
    prev = cp;
  }

  return Sg_HashTableDelete(ctx->handlers, abp);
}

static int handle_events(SgFileWatchContext *ctx, int fd, SgObject wds)
{
  char buf[4096] __attribute__ ((aligned(__alignof__(struct inotify_event))));
  const struct inotify_event *event;
  ssize_t len;
  char *ptr;
  
  for (;;) {
    len = read(fd, buf, sizeof(buf));

    if (len == -1 && errno != EAGAIN) return -1;
    /* nothing to read  */
    if (len <= 0) break;

    for (ptr = buf; ptr < buf + len;
	 ptr += sizeof(struct inotify_event) + event->len) {
      SgObject handler, name = SG_FALSE, flag = SG_FALSE, cp;
      
      event = (const struct inotify_event *) ptr;
      SG_FOR_EACH(cp, wds) {
	if (SG_EQ(SG_MAKE_INT(event->wd), SG_CAAR(cp))) {
	  name = SG_CDAR(cp);
	}
      }
      if (SG_FALSEP(name)) continue;

      handler = Sg_HashTableRef(ctx->handlers, name, SG_FALSE);
      /* in what case this would happen? */
      if (SG_FALSEP(handler)) continue;

      /* added, removed, modified or renamed are the Windows compatible ones.
	 for extra, we can also add access.
       */
      if (event->mask & IN_ACCESS) flag = SG_ACCESSED;
      if (event->mask & IN_CREATE) flag = SG_ACCESSED;
      if (event->mask & IN_MODIFY) flag = SG_MODIFIED;
      if (event->mask & IN_ATTRIB) flag = SG_ATTRIBUTE;
      if (event->mask & IN_DELETE) flag = SG_REMOVED;
      /* TODO how to handle it? */
      if (event->mask & IN_MOVED_FROM) flag = SG_RENAMED;
      if (event->mask & IN_MOVED_TO)   flag = SG_RENAMED;
      /* ok, we don't know this event */
      if (SG_FALSEP(flag)) continue;

      if (event->len && Sg_DirectoryP(name)) {
	/* monitoring directory so construct file path 
	   NB: we don't support IN_OPEN or IN_CLOSE so, i believe, 
	       event->name wouldn't contain the target directory
	       itself.
	*/
	name = Sg_BuildPath(name, Sg_Utf8sToUtf32s(event->name, event->len));
      }
      /* now call handler */
      Sg_Apply2(handler, name, flag);
    }
  }
  return 0;
}

void Sg_StartMonitoring(SgFileWatchContext *ctx)
{
  inotify_context *ic = (inotify_context *)ctx->context;
  struct pollfd fds[1];
  nfds_t nfds = 1;
  int e = 0;
  SgObject wds = SG_NIL, cp;

  fds[0].fd = inotify_init1(IN_NONBLOCK);
  if (fds[0].fd < 0) goto err;
  fds[0].events = POLLIN;

  SG_FOR_EACH(cp, ic->paths) {
    char *p = Sg_Utf32sToUtf8s(SG_CAAR(cp));
    int wd = inotify_add_watch(fds[0].fd, p, SG_INT_VALUE(SG_CDAR(cp)));
    if (wd < 0) goto err;
    wds = Sg_Acons(SG_MAKE_INT(wd), SG_CAAR(cp), wds);
  }

  while (1) {
    int poll_num;

    if (ctx->stopRequest) goto end;
    poll_num = poll(fds, nfds, -1);
    if (poll_num < 0) {
      if (errno == EINTR) goto end;
      goto err;
    }
    if (poll_num > 0) {
      if (fds[0].revents & POLLIN) {
	int r = 0;
	SG_FILE_WATCH_CONTEXT_LOCK(ctx);
	if (!ctx->stopRequest) {
	  r = handle_events(ctx, fds[0].fd, wds);
	}
	SG_FILE_WATCH_CONTEXT_UNLOCK(ctx);
	if (r) goto err;
      }
    }
  }

 err:
  e = errno;
 end:
  /* this should close all watch descriptor */
  if (fds[0].fd > 0) close(fds[0].fd);

  ctx->stopRequest = FALSE;
  if (e) {
    char *msg = strerror(e);
    Sg_SystemError(e, UC("system error: %A"), 
		   Sg_Utf8sToUtf32s(msg, strlen(msg)));
  }
}
