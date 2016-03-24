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
  int fd;			/* event fd */
  SgObject wds;
} inotify_context;

SgObject Sg_MakeFileWatchContext()
{
  SgFileWatchContext *ctx;
  inotify_context *ic = SG_NEW(inotify_context);
  ic->fd = inotify_init1(IN_NONBLOCK);
  if (ic->fd < 0) {
    int e = errno;
    char *msg = strerror(e);
    Sg_SystemError(e, UC("failed to inotify_init1: %A"), 
		   Sg_Utf8sToUtf32s(msg, strlen(msg)));
  }
  ic->wds = SG_NIL;		/* TODO should we use hashtable? */
  ctx = SG_NEW(SgFileWatchContext);
  SG_SET_CLASS(ctx, SG_CLASS_FILE_WATCH_CONTEXT);
  ctx->handlers = Sg_MakeHashTableSimple(SG_HASH_STRING, 32);
  ctx->context = ic;
  ctx->stopRequest = FALSE;
  return SG_OBJ(ctx);
}

void Sg_DestroyFileWatchContext(SgFileWatchContext *ctx)
{
  inotify_context *ic = (inotify_context *)ctx->context;
  ctx->handlers = SG_NIL;
  close(ic->fd);
  ic->wds = SG_NIL;
}


static int symbol2flag(SgObject flag)
{
  /* we support only greatest common things. */
  if (SG_EQ(SG_INTERN("access"), flag)) return IN_ACCESS;
  if (SG_EQ(SG_INTERN("modify"), flag)) return IN_MODIFY;
  if (SG_EQ(SG_INTERN("delete"), flag)) return IN_DELETE;
  if (SG_EQ(SG_INTERN("move"), flag))   return IN_MOVE;
  if (SG_EQ(SG_INTERN("attribute"), flag))   return IN_ATTRIB;
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
  char *p;
  int wd;
  if (SG_FALSEP(abp)) Sg_Error(UC("Path does not exist: %A"), path);

  p = Sg_Utf32sToUtf8s(abp);
  wd = inotify_add_watch(ic->fd, p, get_flag(flag));
  if (wd < 0) {
    int e = errno;
    char *msg = strerror(e);
    Sg_SystemError(e, UC("failed to add path: %A"), 
		   Sg_Utf8sToUtf32s(msg, strlen(msg)));
  }
  ic->wds = Sg_Acons(abp, SG_MAKE_INT(wd), ic->wds);
  Sg_HashTableSet(ctx->handlers, abp, handler, 0);
}
SgObject Sg_RemoveMonitoringPath(SgFileWatchContext *ctx, SgString *path)
{
  inotify_context *ic = (inotify_context *)ctx->context;
  int wd = -1;
  SgObject cp, abp = Sg_AbsolutePath(path);

  if (SG_FALSEP(abp)) return SG_FALSE;

  SG_FOR_EACH(cp, ic->wds) {
    if (Sg_StringEqual(abp, SG_CAAR(cp))) {
      wd = SG_INT_VALUE(SG_CDAR(cp));
      break;
    }
  }
  if (wd < 0) return SG_FALSE;
  if (inotify_rm_watch(ic->fd, wd) != 0) {
    int e = errno;
    char *msg = strerror(e);
    Sg_SystemError(e, UC("failed to remove path: %A"), 
		   Sg_Utf8sToUtf32s(msg, strlen(msg)));
  }
  return Sg_HashTableDelete(ctx->handlers, abp);
}

static void handle_events(SgFileWatchContext *ctx)
{
  inotify_context *ic = (inotify_context *)ctx->context;

  char buf[4096] __attribute__ ((aligned(__alignof__(struct inotify_event))));
  const struct inotify_event *event;
  int fd = ic->fd;
  ssize_t len;
  char *ptr;
  
  for (;;) {
    len = read(fd, buf, sizeof(buf));

    if (len == -1 && errno != EAGAIN) {
      int e = errno;
      char *msg = strerror(e);
      Sg_SystemError(e, UC("failed to read from inotify_event: %A"), 
		     Sg_Utf8sToUtf32s(msg, strlen(msg)));
    }
    /* nothing to read  */
    if (len <= 0) break;

    for (ptr = buf; ptr < buf + len;
	 ptr += sizeof(struct inotify_event) + event->len) {
      SgObject handler, name = SG_FALSE, flag, cp;
      
      event = (const struct inotify_event *) ptr;
      SG_FOR_EACH(cp, ic->wds) {
	if (SG_EQ(SG_MAKE_INT(event->wd), SG_CDAR(cp))) {
	  name = SG_CAAR(cp);
	}
      }
      if (SG_FALSEP(name)) continue;

      handler = Sg_HashTableRef(ctx->handlers, name, SG_FALSE);
      /* in what case this would happen? */
      if (SG_FALSEP(handler)) continue;

      /* added, removed, modified or renamed are the Windows compatible ones.
	 for extra, we can also add access.
       */
      if (event->mask & IN_ACCESS) flag = SG_INTERN("access");
      if (event->mask & IN_MODIFY) flag = SG_INTERN("modified");
      if (event->mask & IN_ATTRIB) flag = SG_INTERN("attribute");
      if (event->mask & IN_DELETE) flag = SG_INTERN("remvoed");
      /* TODO how to handle it? */
      if (event->mask & IN_MOVED_FROM) flag = SG_INTERN("renamed");
      if (event->mask & IN_MOVED_TO)   flag = SG_INTERN("renamed");
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
}

void Sg_StartMonitoring(SgFileWatchContext *ctx)
{
  inotify_context *ic = (inotify_context *)ctx->context;
  struct pollfd fds[1];
  nfds_t nfds = 1;

  fds[0].fd = ic->fd;
  fds[0].events = POLLIN;

  while (1) {
    int poll_num;

    if (ctx->stopRequest) break;
    poll_num = poll(fds, nfds, -1);
    if (poll_num < 0) {
      int e = errno;
      char *msg;
      if (e == EINTR) break;
      msg = strerror(e);
      Sg_SystemError(e, UC("failed to poll: %A"), 
		     Sg_Utf8sToUtf32s(msg, strlen(msg)));
    }
    if (poll_num > 0) {
      if (fds[0].revents & POLLIN) {
	handle_events(ctx);
      }
    }
  }
  ctx->stopRequest = FALSE;
}
