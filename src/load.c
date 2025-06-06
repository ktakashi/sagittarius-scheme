/* load.c                                          -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
#include <string.h>
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/load.h"
#include "sagittarius/private/core.h"
#include "sagittarius/private/codec.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/file.h"
#include "sagittarius/private/gloc.h"
#include "sagittarius/private/library.h"
#include "sagittarius/private/number.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/reader.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/system.h"
#include "sagittarius/private/transcoder.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/vm.h"
#include "sagittarius/private/thread.h"
#include "sagittarius/private/unicode.h"

/* need to redirect dlopen */
#include "gc-incl.inc"


static SgInternalMutex load_lock;
static SgInternalMutex dso_lock;

int Sg_Load(SgString *path)
{
  static SgObject load_stub = SG_UNDEF;
  SgObject r = SG_FALSE;
  SgVM *vm = Sg_VM();
  /* flags(#!** etc) are only per file.
     so we need to save/restore.
     TODO: do we need to lock?
   */
  int save = vm->flags;
  if (SG_UNDEFP(load_stub)) {
    SgObject gloc;
    Sg_LockMutex(&load_lock);
    gloc = Sg_FindBinding(SG_INTERN("(sagittarius)"),
			  SG_INTERN("load"),
			  SG_UNBOUND);
    if (SG_UNBOUNDP(gloc)) {
      Sg_Panic("load was not found.");
    }
    load_stub = SG_GLOC_GET(SG_GLOC(gloc));
    Sg_UnlockMutex(&load_lock);
  }
  r = Sg_Apply1(load_stub, path);
  vm->flags = save;
  return (SG_INTP(r) ? (int)SG_INT_VALUE(r) : 0);
}

int Sg_LoadFromPort(SgPort *port)
{
  static SgObject load_stub = SG_UNDEF;
  SgObject r = SG_FALSE;
  SgVM *vm = Sg_VM();
  /* flags(#!** etc) are only per file.
     so we need to save/restore.
     TODO: do we need to lock?
   */
  int save = vm->flags;
  if (SG_UNDEFP(load_stub)) {
    SgObject gloc;
    Sg_LockMutex(&load_lock);
    gloc = Sg_FindBinding(SG_INTERN("(sagittarius)"),
			  SG_INTERN("load-from-port"),
			  SG_UNBOUND);
    if (SG_UNBOUNDP(gloc)) {
      Sg_Panic("load was not found.");
    }
    load_stub = SG_GLOC_GET(SG_GLOC(gloc));
    Sg_UnlockMutex(&load_lock);
  }
  r = Sg_Apply1(load_stub, port);
  vm->flags = save;
  return (SG_INTP(r) ? (int)SG_INT_VALUE(r) : 0);
}

/*
  DynLoad

  load shared objects
 */

typedef struct dlobj_rec dlobj;
typedef struct dlobj_initfn_rec dlobj_initfn;
typedef void (*SgDynLoadInitFn)(void);

static struct
{
  SgObject dso_suffix;
  dlobj *dso_list;
} dynldinfo = { (SgObject)&dynldinfo, };

struct dlobj_initfn_rec
{
  dlobj_initfn *next;
  const char *name;
  SgDynLoadInitFn fn;
  int initialized;
};

struct dlobj_rec
{
  dlobj *next;
  SgString *path;
  int loaded;
  void *handle;
  SgVM *loader;
  dlobj_initfn *initfns;
  SgInternalMutex mutex;
  SgInternalCond  cv;
};


static dlobj* find_dlobj(SgString *path)
{
  dlobj *z = NULL;
  Sg_LockMutex(&dso_lock);
  for (z = dynldinfo.dso_list; z; z = z->next) {
    if (Sg_StringEqual(z->path, path)) break;
  }
  if (z == NULL) {
    z = SG_NEW(dlobj);
    z->path = path;
    z->loader = NULL;
    z->loaded = FALSE;
    z->initfns = NULL;
    Sg_InitMutex(&z->mutex, FALSE);
    Sg_InitCond(&z->cv);
    z->next = dynldinfo.dso_list;
    dynldinfo.dso_list = z;
  }
  Sg_UnlockMutex(&dso_lock);
  return z;
}

static void lock_dlobj(dlobj *dlo)
{
  SgVM *vm = Sg_VM();
  Sg_LockMutex(&dlo->mutex);
  while (dlo->loader != vm) {
    if (dlo->loader == NULL) break;
    Sg_Wait(&dlo->cv, &dlo->mutex);
  }
  dlo->loader = vm;
  Sg_UnlockMutex(&dlo->mutex);
}

static void unlock_dlobj(dlobj *dlo)
{
  Sg_LockMutex(&dlo->mutex);
  dlo->loader = NULL;
  Sg_NotifyAll(&dlo->cv);
  Sg_UnlockMutex(&dlo->mutex);
}

#define DYNLOAD_PREFIX "_Sg_Init_"

#ifdef _MSC_VER
#define s_strcpy(dst, src, size) strcpy_s(dst, size, src)
#else
#define s_strcpy(dst, src, size) strcpy(dst, src)
#endif


static const char* derive_dynload_initfn(const char *filename)
{
  const char *head, *tail, *s;
  char *name, *d;
  size_t size;

  head = strrchr(filename, '/');
  if (head == NULL) {
    head = strrchr(filename, '\\');
    if (head == NULL) head = filename;
	else head++;
  }
  else head++;
  tail = strchr(head, '.');
  if (tail == NULL) tail = filename + strlen(filename);

  size = sizeof(DYNLOAD_PREFIX) + tail - head;
  name = SG_NEW_ATOMIC2(char *, size);
  s_strcpy(name, DYNLOAD_PREFIX, size);
  for (s = head, d = name + sizeof(DYNLOAD_PREFIX) - 1; s < tail; s++, d++) {
    if (isalnum((int)*s)) *d = tolower((int)*s);
    else *d = '_';
  }
  *d = '\0';
  return name;
}

const char* get_initfn_name(SgObject initfn, SgString *dsopath)
{
  if (SG_STRINGP(initfn)) {
    /* WATCOM has weird export symbol name */
#ifdef __WATCOMC__
    SgObject _initfn = Sg_StringAppend2(SG_STRING(initfn),
					SG_STRING(SG_MAKE_STRING("_")));
#else
    SgObject _initfn = Sg_StringAppend2(SG_STRING(SG_MAKE_STRING("_")),
					SG_STRING(initfn));
#endif
    return Sg_Utf32sToUtf8s(SG_STRING(_initfn));
  } else {
    return derive_dynload_initfn(Sg_Utf32sToUtf8s(dsopath));
  }
}

#ifdef HAVE_DLFCN_H
# include "dl_dlopen.c"
#elif defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
# include "dl_win.c"
#else
# include "dl_dummy.c"
#endif

static void load_dlo(dlobj *dlo)
{
  SgVM *vm = Sg_VM();
  if (SG_VM_LOG_LEVEL(Sg_VM(), SG_INFO_LEVEL)) {
    Sg_Printf(vm->logPort, UC(";; Dynamically Loading %S...\n"), dlo->path);
  }
  dlo->handle = dl_open(dlo->path);
  if (dlo->handle == NULL) {
    const SgString *err = dl_error();
    if (err == NULL) {
      Sg_Error(UC("failed to link %S dynamically"), dlo->path);
    } else {
      Sg_Error(UC("failed to link %S dynamically: %S"), dlo->path, err);
    }
  }
  dlo->loaded = TRUE;
}

static dlobj_initfn* find_initfn(dlobj *dlo, const char *name)
{
  dlobj_initfn *fns = dlo->initfns;
  for (; fns != NULL; fns = fns->next) {
    if (strcmp(name, fns->name) == 0) return fns;
  }
  fns = SG_NEW(dlobj_initfn);
  fns->name = name;
  fns->fn = NULL;
  fns->initialized = FALSE;
  fns->next = dlo->initfns;
  dlo->initfns = fns;
  return fns;
}

static void call_initfn(dlobj *dlo, const char *name)
{
  dlobj_initfn *ifn = find_initfn(dlo, name);
  
  if (ifn->initialized) return;
  if (!ifn->fn) {
    ifn->fn = dl_sym(dlo->handle, name + 1);
    if (ifn->fn == NULL) {
      ifn->fn = (void(*)(void))dl_sym(dlo->handle, name);
      if (ifn->fn == NULL) {
	dl_close(dlo->handle);
	dlo->handle = NULL;
	Sg_Error(UC("dynamic linking of %S failed: "
		    "couldn't find initialization function %S"
		    "(%A)"),
		 dlo->path, Sg_MakeStringC(name),
		 dl_error());
      }
    }
  }
  ifn->fn();
  ifn->initialized = TRUE;
}

/* .dll or .so loader */
SgObject Sg_DynLoad(SgString *filename, SgObject initfn, unsigned long flags)
{
  SgVM *vm = Sg_VM();  
  SgObject spath;
  const char * volatile initname;
  dlobj * volatile dlo;

  spath = Sg_FindFile(filename, vm->dynamicLoadPath, 
		      dynldinfo.dso_suffix, TRUE);
  if (SG_FALSEP(spath)) {
    Sg_Error(UC("can't find dlopen-able library %S"), filename);
  }
  initname = get_initfn_name(initfn, SG_STRING(spath));
  dlo = find_dlobj(spath);

  lock_dlobj(dlo);
  if (!dlo->loaded) {
    SG_UNWIND_PROTECT { load_dlo(dlo); }
    SG_WHEN_ERROR{ unlock_dlobj(dlo); SG_NEXT_HANDLER; }
    SG_END_PROTECT;
  }
  ASSERT(dlo->loaded);

  SG_UNWIND_PROTECT { call_initfn(dlo, initname); }
  SG_WHEN_ERROR{ unlock_dlobj(dlo); SG_NEXT_HANDLER; }
  SG_END_PROTECT;

  unlock_dlobj(dlo);
  return SG_TRUE;
}

void* Sg_OpenSharedObject(SgString *filename)
{
  return dl_open(filename);
}

void* Sg_LookupSharedObject(void *handle, const char *symbol)
{
  /* dl_sym returns SgDynLoadInitFn so cast it*/
  return (void*)dl_sym(handle, symbol);
}

void Sg_CloseSharedObject(void *handle)
{
  dl_close(handle);
}

SgObject Sg_GetSharedError()
{
  return SG_OBJ(dl_error());
}

#ifndef __CYGWIN__
static void real_cleanup(void)
{
  dlobj *z;
  for (z = dynldinfo.dso_list; z; z = z->next) {
    if (z->handle) {
      dl_close(z->handle);
      z->handle = NULL;
    }
  }
}
#endif

/* to avoid invalid class reference during port flushing
   (it calls Sg_HasPortPosition which reads class cpa and
   if the shared object is already detached, it crashes).
   e.g. socket port, we need to delay the cleanup.
 */
static void cleanup_shared_objects(void *data)
{
  /* Not sure if this is only Cygwin issue but at least on
     Cygwin atexit is called *after* all resources are released.
     (haven't seen any official document for this but if I check
     with GDB it seems like that.) At that moment, there is no
     DLL is atached on the process and Win32 API tries to release
     it with invalid handle or something. Then it causes SEGV.*/
#ifndef __CYGWIN__
  atexit(real_cleanup);
#endif
}

void Sg__InitLoad()
{
  Sg_InitMutex(&load_lock, TRUE);
  Sg_InitMutex(&dso_lock, TRUE);
  dynldinfo.dso_suffix = SG_MAKE_STRING(SHLIB_SO_SUFFIX);
  dynldinfo.dso_list = NULL;

  Sg_AddCleanupHandler(cleanup_shared_objects, NULL);
}
