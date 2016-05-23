/* -*- mode: c; coding: utf-8; -*- 
 *
 * fsevents_watch.c
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
 */

#include <CoreServices/CoreServices.h>
#include <string.h>

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "filewatch.h"


typedef struct {
  SgObject paths;		/* list of path and flags */
  CFRunLoopRef loop;
} fsevents_context;

SgObject Sg_MakeFileWatchContext()
{
  SgFileWatchContext *ctx = SG_NEW(SgFileWatchContext);
  fsevents_context *fc = SG_NEW(fsevents_context);
  fc->paths = SG_NIL;
  fc->loop = NULL;
  SG_SET_CLASS(ctx, SG_CLASS_FILE_WATCH_CONTEXT);
  SG_FILE_WATCH_CONTEXT_INIT(ctx, fc);
  return SG_OBJ(ctx);
}

void Sg_DestroyFileWatchContext(SgFileWatchContext *ctx)
{
  fsevents_context *fc = (fsevents_context *)ctx->context;
  fc->paths = SG_NIL;
  fc->loop = NULL;
  SG_FILE_WATCH_CONTEXT_RELESE(ctx);
}

static int symbol2flag(SgObject flag)
{
  /* we support only greatest common things. */
  /* FSEvent seems doesn't have access. */
  /* if (SG_EQ(SG_ACCESS, flag)) return 0; */
  if (SG_EQ(SG_MODIFY, flag)) return kFSEventStreamEventFlagItemModified;
  if (SG_EQ(SG_DELETE, flag)) return kFSEventStreamEventFlagItemRemoved;
  if (SG_EQ(SG_MOVE, flag))   return kFSEventStreamEventFlagItemRenamed;
  /* is this correct? */
  if (SG_EQ(SG_ATTRIBUTE, flag)) {
    return kFSEventStreamEventFlagItemFinderInfoMod;
  }
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
  fsevents_context *fc = (fsevents_context *)ctx->context;  
  SgObject f = SG_MAKE_INT(get_flag(flag));
  SgObject abp = Sg_AbsolutePath(path);

  if (SG_FALSEP(abp)) Sg_Error(UC("Path does not exist: %A"), path);
  fc->paths = Sg_Acons(abp, f, fc->paths);

  Sg_HashTableSet(ctx->handlers, abp, handler, 0);
}

SgObject Sg_RemoveMonitoringPath(SgFileWatchContext *ctx, SgString *path)
{
  fsevents_context *fc = (fsevents_context *)ctx->context;
  SgObject abp = Sg_AbsolutePath(path), cp, prev = SG_FALSE;

  if (SG_FALSEP(abp)) return SG_FALSE;

  SG_FOR_EACH(cp, fc->paths) {
    if (Sg_StringEqual(abp, SG_STRING(SG_CAAR(cp)))) {
      if (SG_FALSEP(prev)) {
	fc->paths = SG_CDR(cp);
      } else {
	SG_SET_CDR(prev, SG_CDR(cp));
      }
      break;
    }
    prev = cp;
  }
  return Sg_HashTableDelete(ctx->handlers, abp);
}

static void fsevent_callback(ConstFSEventStreamRef stream,
			     void *callbackInfo,
			     size_t numEvents,
			     void *evPaths,
			     const FSEventStreamEventFlags evFlags[],
			     const FSEventStreamEventId evIds[])
{
  SgFileWatchContext *ctx = (SgFileWatchContext *)callbackInfo;
  /* fsevents_context *fc = (fsevents_context *)ctx->context; */
  char const **paths = (char const**)evPaths;
  /* TODO filter out paths only specified events */
  /* TODO directory watch */
  int i;
  for (i = 0; i < numEvents; i++) {
    SgObject p = Sg_Utf8sToUtf32s(paths[i], strlen(paths[i]));
    SgObject h = Sg_HashTableRef(SG_HASHTABLE(ctx->handlers), p, SG_FALSE);
    if (!SG_FALSEP(h)) {
      SgObject f = SG_FALSE;

      if (evFlags[i] & kFSEventStreamEventFlagItemModified) f = SG_MODIFIED;
      if (evFlags[i] & kFSEventStreamEventFlagItemRemoved) f = SG_REMOVED;
      if (evFlags[i] & kFSEventStreamEventFlagItemRenamed) f = SG_RENAMED;
      if (evFlags[i] & kFSEventStreamEventFlagItemFinderInfoMod) {
	f = SG_ATTRIBUTE;
      }
      
      Sg_Apply2(h, p, f);
    }
  }
}

/* we do single thread mode of CFRunLoopRun */
void Sg_StartMonitoring(SgFileWatchContext *ctx)
{
  fsevents_context *fc = (fsevents_context *)ctx->context;
  int n = Sg_Length(fc->paths), i;
  SgObject cp;
  CFStringRef *arg;
  CFArrayRef p;
  FSEventStreamRef stream;
  CFAbsoluteTime latency = 0;	/* should wait? */

  arg = SG_NEW_ARRAY(CFStringRef, n);
  for (i = 0, cp = fc->paths; i < n; i++, cp = SG_CDR(cp)) {
    arg[i] = CFStringCreateWithCString(kCFAllocatorDefault,
				       Sg_Utf32sToUtf8s(SG_CAAR(cp)),
				       kCFStringEncodingUTF8);
  }
  p = CFArrayCreate(NULL, (const void **)arg, n, NULL);
  stream = FSEventStreamCreate(NULL, &fsevent_callback, ctx,
			       p, kFSEventStreamEventIdSinceNow, latency,
			       kFSEventStreamCreateFlagNone);

  fc->loop = CFRunLoopGetCurrent();
  FSEventStreamScheduleWithRunLoop(stream, fc->loop, kCFRunLoopDefaultMode);
  FSEventStreamStart(stream);

  CFRunLoopRun();

  FSEventStreamInvalidate(stream);
  FSEventStreamRelease(stream);
  fc->loop = NULL;
}

void Sg_StopRequest(SgFileWatchContext *ctx)
{
  fsevents_context *fc = (fsevents_context *)ctx->context;
  if (fc->loop) {
    SG_FILE_WATCH_CONTEXT_LOCK(ctx);
    CFRunLoopStop(fc->loop);
    SG_FILE_WATCH_CONTEXT_UNLOCK(ctx);
  }
}
