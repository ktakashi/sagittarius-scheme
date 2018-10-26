/* core.c                                          -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
#include <sagittarius/config.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#define LIBSAGITTARIUS_BODY
#include "sagittarius.h"
#include "sagittarius/core.h"
#include "sagittarius/vm.h"
#include "sagittarius/builtin-symbols.h"
#include "sagittarius/keyword.h"

#include "gc-incl.inc"

static void finalizable(void);

static void* oom_handler(size_t bytes)
{
  Sg_Panic("out of memory (%lu). aborting...", bytes);
  return NULL;			/* dummy */
}

static void init_cond_features();

extern void Sg__InitSymbol();
extern void Sg__InitNumber();
extern void Sg__InitString();
extern void Sg__InitKeyword();
extern void Sg__InitLibrary();
extern void Sg__InitFile();
extern void Sg__InitPair();
extern void Sg__InitRecord();
extern void Sg__InitConditions();
extern void Sg__InitReader();
extern void Sg__InitPort();
extern void Sg__InitLoad();
extern void Sg__InitVM();
extern void Sg__InitCache();
extern void Sg__InitCharSet();
extern void Sg__InitClos();
extern void Sg__InitIdentifier();
extern void Sg__InitWrite();
extern void Sg__InitRegex();
extern void Sg__InitUnicode();
extern void Sg__InitMacro();
extern void Sg__InitSystem();
extern void Sg__InitBaseSystem(); /* for <time>, maybe I should make time.c */
extern void Sg__InitReaderClass();

/* stub files */
extern void Sg__Init_sagittarius_compiler_procedure();
extern void Sg__Init_sagittarius_vm_debug();
extern void Sg__Init_sagittarius_vm();
extern void Sg__Init_sagittarius();
extern void Sg__Init_sagittarius_clos();
extern void Sg__Init_sagittarius_fixnums();
extern void Sg__Init_sagittarius_flonums();
extern void Sg__Init_sagittarius_treemap();
extern void Sg__Init_sagittarius_sandbox();
extern void Sg__InitInstruction();
/* compiled libraries */
extern void Sg__Init_core();
extern void Sg__Init_core_base();
extern void Sg__Init_core_macro();
extern void Sg__Init_sagittarius_compiler_util();
extern void Sg__Init_sagittarius_compiler();
/* these must be the last */
extern void Sg__Init_core_errors();
extern void Sg__Init_core_arithmetic();
extern void Sg__Init_core_program();
/* extern void Sg__Init_match_core(); */
/* extern void Sg__Init_sagittarius_interactive(); */

extern void Sg__InitExtFeatures();
extern void Sg__InitComparator();

extern void Sg__PostInitVM();
extern void Sg__PostInitCache();

#ifdef USE_BOEHM_GC
static GC_warn_proc warn_proc = NULL;
static void no_warning(char * msg, GC_word arg)
{
  /* do nothing */
}

void Sg_GCSetPrintWarning(int onP)
{
  /*  */
  if (onP) {
#if GC_VERSION_MAJOR >= 7 && GC_VERSION_MINOR >= 2
    GC_set_warn_proc(warn_proc);
#endif
  } else {
    GC_set_warn_proc(no_warning);
  }
}

#endif

void Sg_Init()
{
  SgObject nullsym, coreBase, compsym, sgsym;
#ifdef USE_BOEHM_GC
  GC_INIT();
#if GC_VERSION_MAJOR >= 7 && GC_VERSION_MINOR >= 2
  GC_set_oom_fn(oom_handler);
  GC_set_finalize_on_demand(TRUE);
  GC_set_finalizer_notifier(finalizable);
  /* GC_get_warn_proc is after 7.2 */
  warn_proc = GC_get_warn_proc();
#else
  GC_oom_fn = oom_handler;
  GC_finalize_on_demand = TRUE;
  GC_finalizer_notifier = finalizable;
#endif
#else
  /* do nothing for now*/
#endif


  /* order is important especially libraries */
  Sg__InitString();		/* string must be before symbol */
  Sg__InitSymbol();
  Sg__InitNumber();
  Sg__InitKeyword();
  /* initialize default reader macro */
  Sg__InitReader();

  Sg__InitLibrary();
  Sg__InitLoad();
  Sg__InitUnicode();

  Sg__InitVM();
  /* init clos uses findlibrary. so after VM */
  Sg__InitClos();
  Sg__InitBaseSystem();		/* <time> */
  Sg__InitReaderClass();
  Sg__InitMacro();
  /* port must be after VM to replace std ports. */
  Sg__InitPort();
  Sg__InitWrite();
  Sg__InitIdentifier();
  Sg__InitCache();

  nullsym = SG_INTERN("(core)");
  coreBase = SG_INTERN("(core base)");
  compsym = SG_INTERN("(sagittarius compiler)");
  sgsym = SG_INTERN("(sagittarius)");

  /* for (core syntax-case) we need compler library to create global id.
     so create it here
   */
  Sg_FindLibrary(compsym, TRUE);

  Sg__InitInstruction();
  Sg__Init_core();
  Sg__Init_sagittarius();
  Sg__InitPair();
  Sg__InitCharSet();
  Sg__InitFile();

  /* regex */
  Sg__InitRegex();
  /* for Windows but call it */
  Sg__InitSystem();

  Sg__InitComparator();		/* need (core) and (sagittarius) */
  Sg__Init_sagittarius_vm();
  Sg__Init_sagittarius_vm_debug();
  Sg__Init_sagittarius_clos();
  Sg__Init_sagittarius_fixnums();
  Sg__Init_sagittarius_flonums();
  Sg__Init_sagittarius_treemap();
  
  /* this is scmlib.scm */
  Sg__Init_core_base();
  /* record can be here. */
  Sg__InitRecord();
  Sg__InitConditions();

  Sg_ImportLibrary(coreBase, nullsym);

  Sg__Init_core_errors();
  Sg__Init_core_macro();
  Sg__Init_sagittarius_compiler_util();
  Sg__Init_sagittarius_compiler_procedure();
  Sg__Init_sagittarius_compiler();
  /* even these files need to be ordered */
  Sg__Init_core_arithmetic();
  Sg__Init_core_program();
  /* we need to put basic syntaxes to compiler. */
  Sg_ImportLibrary(compsym, nullsym);
  Sg_ImportLibrary(compsym, sgsym);

  /* this requires macro so after (core macro) */
  Sg__Init_sagittarius_sandbox();

  /* 
     rebind er-macro-transformer into (sagittarius)
   */
  {
    SgObject core_macro = SG_INTERN("(core macro)");
    SgLibrary *core_macro_lib = SG_LIBRARY(Sg_FindLibrary(core_macro, FALSE));
    SgLibrary *sagittarius_lib = SG_LIBRARY(Sg_FindLibrary(sgsym, FALSE));
    Sg_InsertBinding(sagittarius_lib,
		     SG_SYMBOL_ER_MACRO_TRANSFORMER,
		     Sg_FindBinding(core_macro_lib, 
				    SG_SYMBOL_ER_MACRO_TRANSFORMER,
				    SG_UNBOUND));
  }
  init_cond_features();

  /* Sg__Port* will be called after all initialisations are done. */
  Sg__PostInitVM();
  Sg__PostInitCache();

  /* this is required to even import or so on user library */
  Sg_ImportLibraryFullSpec(Sg_VM()->currentLibrary, sgsym,
			   SG_LIST1(SG_LIST4(SG_INTERN("only"),
					     SG_INTERN("import"),
					     SG_INTERN("library"),
					     SG_INTERN("define-library"))));
}

/* GC related */
void* Sg_malloc(size_t size)
{
#ifdef USE_BOEHM_GC
  return GC_MALLOC(size);
#else
  /* for now do nothing */
  return NULL;
#endif
}
void* Sg_malloc_atomic(size_t size)
{
#ifdef USE_BOEHM_GC
  return GC_MALLOC_ATOMIC(size);
#else
  /* for now do nothing */
  return NULL;
#endif
}

size_t Sg_GetHeapSize()
{
  return GC_get_heap_size();
}
size_t Sg_GetTotalBytes()
{
  return GC_get_total_bytes();
}
uintptr_t Sg_GcCount()
{
#if GC_VERSION_MAJOR >= 7 && GC_VERSION_MINOR >= 2
  return GC_get_gc_no();
#else
  return GC_gc_no;
#endif
}

/* may fail on some platform */
int Sg_GCStackBase(uintptr_t *base)
{
  /* cache for stack base (for performance) 
     It seems GC_get_stack_base is really slow on Linux. This affects
     performance of bignum caluculation (especially cryptographic
     operation such as RSA key generation). I don't think modern
     operating system can change stack base of created thread, so
     we can cache it here.
     TODO: getting too many rabbish on VM...
   */
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
  static __declspec(thread) uintptr_t cStackBase = (uintptr_t)-1;
#else
  /* we go easier way. (hope it works on portably) */
  static __thread uintptr_t cStackBase = (uintptr_t)-1;
#endif
  struct GC_stack_base b;

  if (cStackBase != (uintptr_t)-1) {
    *base = cStackBase;
    return TRUE;
  } else {
    int ok = GC_get_stack_base(&b);
    if (ok == GC_SUCCESS) {
      cStackBase = (uintptr_t)b.mem_base;
      *base = cStackBase;
      return TRUE;
    }
    *base = (uintptr_t)-1;
    return FALSE;
  }
}

/* not sure if we need to do this but just in case
   this saves some stack space to execute.
 */
#define CONTROL_AREA 0x1000
static intptr_t stack_limit()
{
  if (Sg_MainThreadP()) {
    return SG_MAIN_THREAD_STACK_SIZE_LIMIT - CONTROL_AREA;
  } else {
    return SG_CHILD_THREAD_STACK_SIZE_LIMIT - CONTROL_AREA;
  }
}
/* 
   usage:
   volatile char current_stack;
   intptr_t size = Sg_AvailableStackSize(&current_stack);
 */
intptr_t Sg_AvailableStackSize(uintptr_t csp)
{
  uintptr_t base;
  intptr_t limit = stack_limit();
  if (Sg_GCStackBase(&base)) {
    /* TODO it's assume stack grows downward */
    return limit - (base - csp);
  }
  /* sorry we just return max size. don't blame me... */
  return limit;
}

void Sg_GC()
{
#ifdef USE_BOEHM_GC
  GC_gcollect();
#else
  /* for now do nothing */
#endif
}

void Sg_RegisterFinalizer(SgObject z, SgFinalizerProc finalizer, void *data)
{
#ifdef USE_BOEHM_GC
  GC_finalization_proc ofn; void *ocd;
  GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)finalizer,
				 data, &ofn, &ocd);
#else
  /* for now do nothing */
#endif
}

void Sg_UnregisterFinalizer(SgObject z)
{
#ifdef USE_BOEHM_GC
  GC_finalization_proc ofn; void *ocd;
  GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)NULL, NULL,
				 &ofn, &ocd);
#else
  /* for now do nothing */
#endif
}

int Sg_FinalizerRegisteredP(SgObject z)
{
#ifdef USE_BOEHM_GC
  GC_finalization_proc ofn, dfn; 
  void *ocd, *dcd;
  /* it's a bit ugly to do it but we need this unfortunately... */
  GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)NULL,
				 NULL, &ofn, &ocd);
  GC_REGISTER_FINALIZER_NO_ORDER(z, ofn, ocd, &dfn, &dcd);
  return (void *)ofn != NULL;
#else
  /* for now do nothing */
#endif
}

void Sg_RegisterDisappearingLink(void **p, void *value)
{
#ifdef USE_BOEHM_GC
  GC_general_register_disappearing_link(p, value);
#else
  /* for now do nothing */
#endif
}

void Sg_UnregisterDisappearingLink(void **p)
{
#ifdef USE_BOEHM_GC
  GC_unregister_disappearing_link(p);
#else
  /* for now do nothing */
#endif
}

void* Sg_GCBase(void *value)
{
#ifdef USE_BOEHM_GC
  return GC_base(value);
#else
  /* for now do nothing */
  return NULL;
#endif
}

void finalizable()
{
  SgVM *vm = Sg_VM();
  if (vm) {
    vm->finalizerPending = TRUE;
    vm->attentionRequest = TRUE;
  }
}

SgObject Sg_VMFinalizerRun(SgVM *vm)
{
  /* for future we want to use own gc implementation */
#ifdef USE_BOEHM_GC
 retry:
  vm->finalizerPending = FALSE;
  GC_invoke_finalizers();
  /* If finalizers consumes memory and GC is invoked during
     calling finalizer, then the pending flag is set to TRUE
     and finalizers need to be called again.

     NB: not sure if we need to do this much.
  */
  if (vm->finalizerPending) goto retry;
  
  vm->finalizerPending = FALSE;
  return SG_UNDEF;
#else
  return SG_UNDEF;
#endif
}

void Sg_RegisterDL(void *data_start, void *data_end,
		   void *bss_start, void *bss_end)
{
  if (data_start < data_end) {
    Sg_AddGCRoots(data_start, data_end);
  }
  if (bss_start < bss_end) {
    Sg_AddGCRoots(bss_start, bss_end);
  }
}

void Sg_AddGCRoots(void *start, void *end)
{
#ifdef USE_BOEHM_GC
  GC_add_roots(start, end);
#else
  /* do nothing for now */
#endif
}

/* exit related */
#define EXIT_CODE(code) ((code)&0xFF)

void Sg_Exit(int code)
{
  Sg_Cleanup();
  exit(EXIT_CODE(code));
}

void Sg_EmergencyExit(int code)
{
  exit(EXIT_CODE(code));
}

struct cleanup_handler_rec
{
  void (*handler)(void *);
  void *data;
  struct cleanup_handler_rec *next;
};

static struct {
  int dirty;
  struct cleanup_handler_rec *handlers;
} cleanup = { TRUE, NULL };

void Sg_Cleanup()
{
  SgVM *vm = Sg_VM();
  SgObject hp;
  struct cleanup_handler_rec *ch;
  
  if (!cleanup.dirty) return;
  cleanup.dirty = FALSE;

  SG_FOR_EACH(hp, vm->dynamicWinders) {
    vm->dynamicWinders = SG_CDR(hp);
    Sg_Apply0(SG_CDAR(hp));
  }
  
  for (ch = cleanup.handlers; ch; ch = ch->next) {
    ch->handler(ch->data);
  }

  Sg_FlushAllPort(TRUE);
  return;
}

void* Sg_AddCleanupHandler(void (*proc)(void *data), void *data)
{
  struct cleanup_handler_rec *h = SG_NEW(struct cleanup_handler_rec);
  h->handler = proc;
  h->data = data;
  h->next = cleanup.handlers;
  cleanup.handlers = h;
  return h;
}

void Sg_DeleteCleanupHandler(void *handle)
{
  struct cleanup_handler_rec *x = NULL, *y = cleanup.handlers;
  while (y) {
    if (y == handle) {
      if (x == NULL) {
	cleanup.handlers = y->next;
      } else {
	x->next = y->next;
      }
      break;
    }
  }
}

void Sg_Panic(const char* msg, ...)
{
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  fputc('\n', stderr);
  fflush(stderr);
  _exit(EXIT_CODE(1));
}

void Sg_Abort(const char* msg)
{
  int size = (int)strlen(msg);
#ifndef _MSC_VER
  int r = write(2, msg, size);
  if (r < 0) {
    _exit(EXIT_CODE(errno));
  }
#else
  DWORD n;
  WriteConsoleA(GetStdHandle(STD_ERROR_HANDLE), msg, size, &n, NULL);
#endif
  _exit(EXIT_CODE(1));
}

static struct {
  SgObject list;
  SgInternalMutex mutex;
} cond_features = { SG_NIL };

void Sg_AddCondFeature(const SgChar *feature)
{
  if (!Sg_MainThreadP()) {
    Sg_Error(UC("child thread can not add cond-feature"));
  }
  Sg_LockMutex(&cond_features.mutex);
  cond_features.list = Sg_Cons(Sg_Intern(Sg_String(feature)),
			       cond_features.list);
  Sg_AddConstantLiteral(cond_features.list);
  Sg_UnlockMutex(&cond_features.mutex);
}

SgObject Sg_CondFeatures()
{
  return cond_features.list;
}

static void init_cond_features()
{
  Sg_InitMutex(&cond_features.mutex, FALSE);
  Sg_AddCondFeature(UC("sagittarius"));
  Sg_AddCondFeature(UC("sagittarius.os."SAGITTARIUS_PLATFORM));
  /* R7RS appendix B */
  Sg_AddCondFeature(UC("r7rs"));
  Sg_AddCondFeature(UC("ratios"));
  Sg_AddCondFeature(UC("exact-closed"));
  Sg_AddCondFeature(UC("exact-complex"));
  Sg_AddCondFeature(UC("ieee-float"));
  Sg_AddCondFeature(UC("full-unicode"));
  Sg_AddCondFeature(UC(SAGITTARIUS_PLATFORM));
  Sg_AddCondFeature(UC(SAGITTARIUS_PROCESSOR));
  /* it's useful for FFI */
#if SIZEOF_VOIDP == 8
  Sg_AddCondFeature(UC("64bit"));
#else
  Sg_AddCondFeature(UC("32bit"));
#endif

#ifdef WORDS_BIGENDIAN
  Sg_AddCondFeature(UC("big-endian"));
#else
  Sg_AddCondFeature(UC("little-endian"));
#endif
  Sg_AddCondFeature(UC("sagittarius-"SAGITTARIUS_VERSION));
  /* maybe it's useful */
  Sg_AddCondFeature(UC(SAGITTARIUS_TRIPLE));
  /* regexp (SRFI-115) 
     I don't like the name regexp but that's how it is on the SRFI.
   */
  Sg_AddCondFeature(UC("regexp-non-greedy"));
  Sg_AddCondFeature(UC("regexp-look-around"));
  Sg_AddCondFeature(UC("regexp-backrefs"));
  Sg_AddCondFeature(UC("regexp-unicode"));

  /* extlib features */
  Sg__InitExtFeatures();
}

/* Starting point of the Sagittarius engine */
void Sg_Start(SgObject fileOrPort, SgObject commandLine,
	      const char *fmt, SgObject rest)
{
  SgObject lib = Sg_FindLibrary(SG_INTERN("(core program)"), FALSE);
  SgObject args = SG_NIL;
  SgObject start = Sg_FindBinding(lib, SG_INTERN("start"), SG_UNBOUND);
  
  if (SG_UNBOUNDP(start)) Sg_Panic("`start` is not found");
  if (SG_LISTP(rest)) {
    while (*fmt) {
      if (SG_NULLP(rest)) break;
#define CASE(_c, _k)							\
      case _c:								\
	args = Sg_Acons(SG_MAKE_KEYWORD(_k), SG_CAR(rest), args);	\
	rest = SG_CDR(rest);						\
	break
      
      switch (*fmt++) {
	CASE('s', "standard");
	CASE('p', "preimports");
	CASE('e', "expressions");
	CASE('m', "main?");
	CASE('i', "interactive?");
      default: break;
      }
    }
  }
  Sg_Apply3(SG_GLOC_GET(SG_GLOC(start)), fileOrPort, commandLine, args);

}
/* somehow Visual Studio 2010 requires this to create dll.*/
#ifdef _MSC_VER
int main()
{
  return 0;
}
#endif

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
