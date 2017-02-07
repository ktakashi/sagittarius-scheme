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

#ifdef __CYGWIN__
# include <sys/cygwin.h>
/* should be enough (i hope) */
# define BUF_SIZE 2048
static SgObject windows_path(SgString *path)
{
  char *p = Sg_Utf32sToUtf8s(path);
  wchar_t buf[BUF_SIZE], *tmp;
  ssize_t r, s;
  s = cygwin_conv_path(CCP_POSIX_TO_WIN_W, p, NULL, 0);
  if (s < 0) return SG_FALSE;	/* something went wrong */
  if (s > BUF_SIZE) {
    tmp = SG_NEW_ATOMIC2(wchar_t *, s);
  } else {
    tmp = buf;
  }
  r = cygwin_conv_path(CCP_POSIX_TO_WIN_W, p, tmp, s);
  if (r == 0) {
    return Sg_WCharTsToString(tmp, s >> 1);
  }
  return SG_FALSE;
}
static SgObject posix_path(SgObject path)
{
  wchar_t *p = Sg_StringToWCharTs(SG_STRING(path));
  char buf[BUF_SIZE], *tmp;
  ssize_t r, s;
  s = cygwin_conv_path(CCP_WIN_W_TO_POSIX, p, NULL, 0);
  if (s < 0) return SG_FALSE;
  if (s > BUF_SIZE) {
    tmp = SG_NEW_ATOMIC2(char *, s);
  } else {
    tmp = buf;
  }
  r = cygwin_conv_path(CCP_WIN_W_TO_POSIX, p, tmp, s);
  if (r < 0) return SG_FALSE;
  /* remove the last null */
  return Sg_Utf8sToUtf32s(tmp, s-1);
}

#else
/* dummy */
# define windows_path(x) x
# define posix_path(x)   x
#endif

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
  SG_FILE_WATCH_CONTEXT_INIT(ctx, wc);
  return SG_OBJ(ctx);
}

void Sg_DestroyFileWatchContext(SgFileWatchContext *ctx)
{
  windows_context *wc = (windows_context *)ctx->context;
  wc->mappings = SG_NIL;
  SG_FILE_WATCH_CONTEXT_RELESE(ctx);
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
  SgObject cp, found = SG_FALSE, p;
  SgString *dir;
  
  ab = Sg_AbsolutePath(path);
  /* Sg_Printf(SG_PORT(Sg_StandardErrorPort()), UC("file: %A\n"), ab); */
  if (SG_FALSEP(ab)) {
    Sg_Error(UC("path not exists! %A"), path);
  }
  p = windows_path(SG_STRING(ab));
#if __CYGWIN__
  if (SG_FALSEP(p)) {
    Sg_Error(UC("path conversion failed! %A"), path);
  }
#endif
  path = SG_STRING(p);
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
  /* use absolute path for handler */
  Sg_HashTableSet(SG_HASHTABLE(ctx->handlers), ab, handler, 0);
}

SgObject Sg_RemoveMonitoringPath(SgFileWatchContext *ctx, SgString *path)
{
  windows_context *wc = (windows_context *)ctx->context;
  SgString *dir;
  SgObject cp, ab;
  ab = Sg_AbsolutePath(path);
  if (SG_FALSEP(ab)) return SG_FALSE;

  if (Sg_DirectoryP(SG_STRING(ab))) {
    dir = SG_STRING(ab);
  } else {
    dir = get_dir(SG_STRING(path));
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
  return Sg_HashTableDelete(SG_HASHTABLE(ctx->handlers), ab);
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
    case FILE_ACTION_REMOVED: event = SG_REMOVED; break;
    case FILE_ACTION_MODIFIED: event = SG_MODIFIED; break;
    case FILE_ACTION_RENAMED_OLD_NAME:
    case FILE_ACTION_RENAMED_NEW_NAME:
      event = SG_RENAMED;
      break;
      /* correct? */
    /* case FILE_ACTION_ADDED: event = SG_ACCESSED; break; */
    default: event = SG_ACCESSED; break;
    }
    if (!SG_FALSEP(event)) {
      SgObject h = Sg_HashTableRef(SG_HASHTABLE(ctx->handlers), dir, SG_FALSE);
      SgObject p;
      name = Sg_BuildPath(SG_STRING(dir), SG_STRING(name));
      p = posix_path(name);
      /* Sg_Printf(SG_PORT(Sg_StandardErrorPort()),  */
      /* 		UC("path: %S, dir: %S, handler: %A\n"), p, dir, h); */
      /* Sg_Printf(SG_PORT(Sg_StandardErrorPort()), UC("handlers: %S\n"),  */
      /* 		Sg_HashTableKeys(SG_HASHTABLE(ctx->handlers))); */
      if (!SG_FALSEP(p)) {
	/* we need to check for directory watch */
	if (!SG_FALSEP(h)) Sg_Apply2(h, p, event);
	h = Sg_HashTableRef(SG_HASHTABLE(ctx->handlers), name, SG_FALSE);
	if (!SG_FALSEP(h)) Sg_Apply2(h, p, event);
      }
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
    /* reset thread event before wait. this avoids unexpected interruption. */
    ResetEvent(events[n]);
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
	SG_FILE_WATCH_CONTEXT_LOCK(ctx);
	if (!ctx->stopRequest) handle_event(ctx, fn, posix_path(paths[w]));
	SG_FILE_WATCH_CONTEXT_UNLOCK(ctx);
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
