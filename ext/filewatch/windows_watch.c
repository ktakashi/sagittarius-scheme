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
  SgObject ab = Sg_AbsolutePath(path);
  SgString *sab;
  int i;
  if (SG_FALSEP(ab)) {
    Sg_Error(UC("path not exists! %A"), path);
  }
  sab = SG_STRING(ab);
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
  SgObject f = SG_MAKE_INT(get_flag(flag));
  if (Sg_DirectoryP(path)) {
    /* easy */
    wc->mappings = Sg_Cons(SG_LIST3(path, SG_LIST1(path), f), wc->mappings);
  } else {
    /* now we need to get the directory from the path */
    SgObject cp, found = SG_FALSE;
    SgString *dir = get_dir(path);
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

#define OL_BUFFER_SIZE 1024
void Sg_StartMonitoring(SgFileWatchContext *ctx)
{
  windows_context *wc = (windows_context *)ctx->context;
  int n = Sg_Length(wc->mappings), i;
  SgObject cp;
  OVERLAPPED *ols = SG_NEW_ATOMIC2(OVERLAPPED *, n * sizeof(OVERLAPPED));
  HANDLE *events = SG_NEW_ATOMIC2(HANDLE *, (n + 1) * sizeof(HANDLE));
  HANDLE *dirs =   SG_NEW_ATOMIC2(HANDLE *, n * sizeof(HANDLE));
  FILE_NOTIFY_INFORMATION **buf =
    SG_NEW_ATOMIC2(FILE_NOTIFY_INFORMATION **, 
		   n * sizeof(FILE_NOTIFY_INFORMATION *));
  int *filters = SG_NEW_ATOMIC2(int *, n * sizeof(int)), e = 0;
  DWORD r;

  for (i = 0, cp = wc->mappings; i < n; i++, cp = SG_CDR(cp)) {
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
	/* TODO  */
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
