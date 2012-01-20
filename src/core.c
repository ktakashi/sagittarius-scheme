/* -*- C -*- */
/*
 * stub.c
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius.h"
#include "sagittarius/core.h"
#include "sagittarius/vm.h"
#include "sagittarius/builtin-symbols.h"

static void finalizable();
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
extern void Sg__InitConsitions();
extern void Sg__InitReader();
extern void Sg__InitPort();
extern void Sg__InitLoad();
extern void Sg__InitVM();
extern void Sg__InitCache();
extern void Sg__InitCharSet();
extern void Sg__InitClos();

/* stub files */
extern void Sg__Init_sagittarius_compiler_procedure();
extern void Sg__Init_sagittarius_vm_debug();
extern void Sg__Init_sagittarius_vm();
extern void Sg__Init_sagittarius();
extern void Sg__Init_sagittarius_clos();
extern void Sg__InitInstruction();
/* compiled libraries */
extern void Sg__Initnull();
extern void Sg__Init_core_base();
extern void Sg__Init_core_syntax_case();
extern void Sg__Init_sagittarius_compiler_util();
extern void Sg__Init_sagittarius_compiler();
/* these must be the last */
extern void Sg__Init_core_errors();
extern void Sg__Init_core_arithmetic();
extern void Sg__Init_core_enums();
/* extern void Sg__Init_match_core(); */
extern void Sg__Init_sagittarius_interactive();
void Sg_Init()
{
  SgObject nullsym, coreBase;
#ifdef USE_BOEHM_GC
  GC_INIT();
  GC_oom_fn = oom_handler;
  GC_finalize_on_demand = TRUE;
  GC_finalizer_notifier = finalizable;
#else
  /* do nothing for now*/
#endif


  /* order is important especially libraries */
  Sg__InitString();		/* string must be before symbol */
  Sg__InitSymbol();
  Sg__InitNumber();
  Sg__InitKeyword();
  Sg__InitLibrary();
  Sg__InitLoad();

  Sg__InitVM();
  /* port must be after VM to replace std ports. */
  Sg__InitPort();
  /* initialize default reader macro */
  Sg__InitReader();
  Sg__InitCache();
  Sg__InitClos();
  
  nullsym = SG_INTERN("null");
  coreBase = SG_INTERN("(core base)");

  /* (sagittarius) library is not provided by file. so create it here */
  Sg_FindLibrary(SG_INTERN("(sagittarius)"), TRUE);
  /* for (core syntax-case) we need compler library to create global id.
     so create it here
   */
  Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), TRUE);

  Sg__InitInstruction();
  Sg__Initnull();
  /* Sg__InitFile(); */
  Sg__InitPair();
  Sg__InitCharSet();

  Sg__Init_sagittarius();
  Sg__Init_sagittarius_vm();
  Sg__Init_sagittarius_vm_debug();
  Sg__Init_sagittarius_clos();

  /* this is scmlib.scm */
  Sg__Init_core_base();

  /* generic initialization must be after the libraries initialization */
  Sg__InitRecord();
  /* each time when we put something in null, we need to add */
  /* this is even funny... orz */
  Sg_ImportLibrary(coreBase, nullsym);
  Sg__InitConsitions();
  Sg_ImportLibrary(coreBase, nullsym);

  Sg__Init_core_errors();
  Sg__Init_core_syntax_case();

  Sg__Init_sagittarius_compiler_util();
  Sg__Init_sagittarius_compiler_procedure();
  Sg__Init_sagittarius_compiler();

  /* even these files need to be ordered */
  Sg__Init_core_arithmetic();
  Sg__Init_core_enums();
  /* Sg__Init_match_core(); */
  Sg__Init_sagittarius_interactive();

  /* TODO should this be here? */
  Sg_ImportLibrary(Sg_VM()->currentLibrary, nullsym);
  Sg_ImportLibrary(Sg_VM()->currentLibrary, SG_OBJ(SG_INTERN("(sagittarius)")));

  /* we need to put basic syntaxes to compiler. */
  Sg_ImportLibrary(SG_OBJ(SG_INTERN("(sagittarius compiler)")), nullsym);
  Sg_ImportLibrary(SG_OBJ(SG_INTERN("(sagittarius compiler)")),
		   SG_OBJ(SG_INTERN("(sagittarius)")));

  /* 
     we need extra treatment for er-rename. it's defined after the 
     initialization of compiler, so we need to export it to (core base) which 
     defines er-macro-transformer.
     er-macro-transformer is defined (core base) but we want to export it with (sagittarius)
     so insert it to the library here.
   */
  {
    SgLibrary *core_base_lib = SG_LIBRARY(Sg_FindLibrary(coreBase, FALSE));
    SgLibrary *sagittarius_lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius)"), FALSE));
    Sg_InsertBinding(core_base_lib,
		     SG_INTERN("er-rename"),
		     Sg_FindBinding(nullsym, SG_INTERN("er-rename"), SG_FALSE));
    Sg_InsertBinding(sagittarius_lib,
		     SG_SYMBOL_ER_MACRO_TRANSFORMER,
		     Sg_FindBinding(core_base_lib, SG_SYMBOL_ER_MACRO_TRANSFORMER, SG_UNBOUND));
  }
  init_cond_features();
}
/* GC related */
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
  vm->finalizerPending = TRUE;
  vm->attentionRequest = TRUE;
}

SgObject Sg_VMFinalizerRun(SgVM *vm)
{
  /* for future we want to use own gc implementation */
#ifdef USE_BOEHM_GC
  GC_invoke_finalizers();
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
  write(2, msg, size);
  _exit(EXIT_CODE(1));
}

static struct {
  SgObject list;
  SgInternalMutex mutex;
} cond_features = { SG_NIL };

void  Sg_AddCondFeature(const SgChar *feature)
{
  Sg_LockMutex(&cond_features.mutex);
  cond_features.list = Sg_Cons(Sg_Intern(Sg_MakeString(feature,
						       SG_LITERAL_STRING)),
			       cond_features.list);
  Sg_UnlockMutex(&cond_features.mutex);
}

SgObject Sg_CondFeatures()
{
  return cond_features.list;
}

static void init_cond_features()
{
  Sg_AddCondFeature(UC("sagittarius"));
  Sg_AddCondFeature(UC("sagittarius.os."SAGITTARIUS_PLATFORM));
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
