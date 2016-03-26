/* -*- mode: c; coding: utf-8; -*- 
 *
 * windows_watch.c
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
#include <windows.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "filewatch.h"

typedef struct {
  SgObject mappings;		/* list of dirs&path&flags */
#ifdef __CYGWIN__
  HANDLE event;
#endif
} windows_context;

SgObject Sg_MakeFileWatchContext()
{
  SgFileWatchContext *ctx = SG_NEW(SgFileWatchContext);
  windows_context *wc = SG_NEW(windows_context);
  wc->mappings = SG_NIL;
#ifdef __CYGWIN__
  wc->event = INVALID_HANDLE_VALUE;
#endif
  SG_SET_CLASS(ctx, SG_CLASS_FILE_WATCH_CONTEXT);

  ctx->handlers = Sg_MakeHashTableSimple(SG_HASH_STRING, 32);
  ctx->context = wc;
  ctx->stopRequest = FALSE;
  return SG_OBJ(ctx);
}

void Sg_DestroyFileWatchContext(SgFileWatchContext *ctx)
{
  windows_context *wc = (windows_context *)ctx->context;
  wc->mappings = SG_NIL;
}

static int symbol2flag(SgObject flag)
{
  /* we support only greatest common things. */
  if (SG_EQ(SG_ACCESS, flag)) return FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if (SG_EQ(SG_MODIFY, flag)) return FILE_NOTIFY_CHANGE_LAST_WRITE;
  if (SG_EQ(SG_DELETE, flag) || SG_EQ(SG_MOVE, flag)) {
    return FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME;
  }
  if (SG_EQ(SG_ATTRIBUTE, flag)) return FILE_NOTIFY_CHANGE_ATTRIBUTES;
  /* error? */
  return 0;
}
/* maybe we need to make separate include file? */
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

static SgString* get_dir(SgString *path)
{
  SgString *sab = path;
  int i;

  for (i = SG_STRING_SIZE(sab) - 1; i != 0; i--) {
    if (SG_STRING_VALUE_AT(sab, i) == '\\') break;
  }
  /* TODO should we check the position? */
  return SG_STRING(Sg_Substring(sab, 0, i));  
}

void Sg_AddMonitoringPath(SgFileWatchContext *ctx, SgString *path,
			  SgObject flag, /* symbol */
			  SgObject handler)
{
  windows_context *wc = (windows_context *)ctx->context;
  SgObject f = SG_MAKE_INT(get_flag(flag)), ab;
  SgObject cp, found = SG_FALSE;
  SgString *dir;
  ab = Sg_AbsolutePath(path);
  if (SG_FALSEP(ab)) {
    Sg_Error(UC("path not exists! %A"), path);
  }
  path = SG_STRING(ab);
  if (Sg_DirectoryP(path)) {
    dir = path;
  } else {
    dir = get_dir(path);
  }
  /* now we need to check if there's the same dir registered */
  SG_FOR_EACH(cp, wc->mappings) {
    if (Sg_StringEqual(SG_STRING(SG_CAAR(cp)), dir)) {
      found = SG_CAR(cp);
      break;
    }
  }
  if (SG_FALSEP(found)) {
    wc->mappings = Sg_Cons(SG_LIST3(dir, SG_LIST1(path), f), wc->mappings);
  } else {
    /* (dir . ((path ...) f)) */
    SgObject plst = SG_CADR(found), d = SG_CDR(found);
    int existsP = FALSE;
    SG_FOR_EACH(cp, plst) {
      if (Sg_StringEqual(SG_STRING(SG_CAR(cp)), path)) {
	existsP = TRUE;
	break;
      }
    }
    if (!existsP) {
      SG_SET_CAR(d, Sg_Cons(path, plst));
    }
  }
  Sg_HashTableSet(SG_HASHTABLE(ctx->handlers), path, handler, 0);
}

SgObject Sg_RemoveMonitoringPath(SgFileWatchContext *ctx, SgString *path)
{
  windows_context *wc = (windows_context *)ctx->context;
  SgString *dir;
  SgObject cp;
  if (Sg_DirectoryP(path)) {
    dir = path;
  } else {
    dir = get_dir(path);
  }
  SG_FOR_EACH(cp, wc->mappings) {
    if (Sg_StringEqual(dir, SG_STRING(SG_CAAR(cp)))) {
      SgObject found = SG_CAR(cp);
      SgObject d = SG_CDR(found);
      SgObject prev = SG_FALSE;
      SG_FOR_EACH(cp, found) {
	if (Sg_StringEqual(SG_STRING(SG_CAR(cp)), path)) {
	  if (SG_FALSEP(prev)) {
	    SG_SET_CAR(d, SG_CDR(cp));
	  } else {
	    SG_SET_CDR(prev, SG_CDR(cp));
	    SG_SET_CAR(d, prev);
	  }
	  break;
	}
	prev = cp;
      }
      break;
    }
  }
  return Sg_HashTableDelete(SG_HASHTABLE(ctx->handlers), path);
}

static void handle_event(SgFileWatchContext *ctx, FILE_NOTIFY_INFORMATION *fn,
			 SgObject dir)
{
  while (fn) {
    SgObject name, event = SG_FALSE;;
    DWORD len = fn->FileNameLength/2, i;
    int shortP = FALSE;
    for (i = 0; i < len; i++) {
      if (fn->FileName[i] == '~') {
	if (i+1 < len && fn->FileName[i+1] == '1') {
	  shortP = TRUE;
	  break;
	}
      }
    }
    /* fn->FileName[len] = 0; */
    /* fwprintf(stderr, L"%s\n", fn->FileName); */
    if (shortP) {
      wchar_t tmp[MAX_PATH];
      wchar_t p[MAX_PATH];
      DWORD tlen;
      for (i = 0; i < len; i++) p[i] = fn->FileName[i];
      p[i] = 0;
      tlen = GetLongPathNameW(p, tmp, MAX_PATH);
      name = Sg_WCharTsToString(tmp, tlen);
    } else {
      name = Sg_WCharTsToString(fn->FileName, len);
    }
    switch (fn->Action) {
    case FILE_ACTION_ADDED: event = SG_ACCESSED; break; /* correct? */
    case FILE_ACTION_REMOVED: event = SG_REMOVED; break;
    case FILE_ACTION_MODIFIED: event = SG_MODIFIED; break;
    case FILE_ACTION_RENAMED_OLD_NAME:
    case FILE_ACTION_RENAMED_NEW_NAME:
      event = SG_RENAMED;
      break;
    default: break;
    }
    if (!SG_FALSEP(event)) {
      SgObject h = Sg_HashTableRef(SG_HASHTABLE(ctx->handlers), dir, SG_FALSE);
      name = Sg_BuildPath(SG_STRING(dir), SG_STRING(name));
      /* we need to check for directory watch */
      if (!SG_FALSEP(h)) Sg_Apply2(h, name, event);
      h = Sg_HashTableRef(SG_HASHTABLE(ctx->handlers), name, SG_FALSE);
      if (!SG_FALSEP(h)) Sg_Apply2(h, name, event);
    }
    if (!fn->NextEntryOffset) break;
    fn = (FILE_NOTIFY_INFORMATION *)(((char *)fn) + fn->NextEntryOffset);
  }
}

#define OL_BUFFER_SIZE 1024
void Sg_StartMonitoring(SgFileWatchContext *ctx)
{
  windows_context *wc = (windows_context *)ctx->context;
  int n = Sg_Length(wc->mappings), i;
  SgObject cp, *paths = SG_NEW_ARRAY(SgObject, n);
  OVERLAPPED *ols = SG_NEW_ATOMIC2(OVERLAPPED *, n * sizeof(OVERLAPPED));
  HANDLE *events = SG_NEW_ATOMIC2(HANDLE *, (n + 1) * sizeof(HANDLE));
  HANDLE *dirs =   SG_NEW_ATOMIC2(HANDLE *, n * sizeof(HANDLE));
  FILE_NOTIFY_INFORMATION **buf =
    SG_NEW_ATOMIC2(FILE_NOTIFY_INFORMATION **, 
		   n * sizeof(FILE_NOTIFY_INFORMATION *));
  int *filters = SG_NEW_ATOMIC2(int *, n * sizeof(int)), e = 0;
  DWORD r;

  for (i = 0, cp = wc->mappings; i < n; i++, cp = SG_CDR(cp)) {
    paths[i] = SG_CAAR(cp);
    wchar_t *dir = Sg_StringToWCharTs(SG_CAAR(cp));
    BOOL rc;

    dirs[i] = CreateFileW(dir,
			  FILE_LIST_DIRECTORY,
			  FILE_SHARE_READ|FILE_SHARE_DELETE,
			  NULL,
			  OPEN_EXISTING,
			  FILE_FLAG_BACKUP_SEMANTICS|FILE_FLAG_OVERLAPPED, 
			  NULL);

    /* TODO this must be an error */
    if (dirs[i] == INVALID_HANDLE_VALUE) goto err;
    ols[i].hEvent = events[i] = CreateEvent(NULL, FALSE, FALSE, NULL);
    /* is this correct? */
    buf[i] = SG_NEW_ATOMIC2(FILE_NOTIFY_INFORMATION *, OL_BUFFER_SIZE);
    filters[i] = SG_INT_VALUE(SG_CADR(SG_CDAR(cp)));
    rc = ReadDirectoryChangesW(dirs[i], buf[i], OL_BUFFER_SIZE, FALSE,
			       filters[i], NULL, &ols[i], NULL);
    if (!rc) goto err;
  }
  /* now wait */
#ifdef __CYGWIN__
  events[n] = wc->event = CreateEvent(NULL, FALSE, FALSE, NULL);
#else
  events[n] = (&Sg_VM()->thread)->event;
#endif
  while (1) {
    if (ctx->stopRequest) goto end;
    
    r = WaitForMultipleObjects(n+1, events, FALSE, INFINITE);
    /* reset event no matter what */
    ResetEvent(events[r - WAIT_OBJECT_0]);
    if (r == WAIT_OBJECT_0 + n) {
      /* ok this is thread-interrupt! call */
      goto end;
    } else {
      int w = r - WAIT_OBJECT_0;
      DWORD bytes;
      BOOL b = GetOverlappedResult(dirs[w], &ols[w], &bytes, TRUE); 
      if (b) {
	FILE_NOTIFY_INFORMATION *fn = (FILE_NOTIFY_INFORMATION *)buf[w];
	handle_event(ctx, fn, paths[w]);
      }
      b = ReadDirectoryChangesW(dirs[w], buf[w], OL_BUFFER_SIZE, FALSE,
				filters[w], NULL, &ols[w], NULL);
      if (!b) goto err;
    }
  }

 err:
  e = GetLastError();
 end:
  for (i = 0; i < n; i++) {
    if (events[i]) CloseHandle(events[i]);
    if (dirs[i]) CloseHandle(dirs[i]);
  }
#ifdef __CYGWIN__
  CloseHandle(wc->event);
  wc->event = INVALID_HANDLE_VALUE;
#endif
  ctx->stopRequest = FALSE;
  if (e) {
    Sg_SystemError(e, UC("%A"), Sg_GetLastErrorMessageWithErrorCode(e));
  }
}

#ifdef __CYGWIN__
void Sg_InterruptMonitoring(SgFileWatchContext *ctx)
{
  windows_context *wc = (windows_context *)ctx->context;
  if (wc->event != INVALID_HANDLE_VALUE) SetEvent(wc->event);
}
#endif
