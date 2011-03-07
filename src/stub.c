// -*- C -*-
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius.h"
#include "sagittarius/stub.h"

extern void Sg__InitSymbol();
extern void Sg__InitNumber();
extern void Sg__InitString();
extern void Sg__InitKeyword();
extern void Sg__InitFile();
extern void Sg__InitReader();
extern void Sg__InitVM();

/* stub files */
extern void Sg__Init_sagittarius_compiler_procedure();
extern void Sg__Init_sagittarius_vm_debug();
extern void Sg__Init_sagittarius_vm();
extern void Sg__Init_sagittarius();
extern void Sg__InitInstruction();
/* compiled libraries */
extern void Sg__Initnull();
extern void Sg__Init_core_base();
extern void Sg__Init_sagittarius_compiler_util();
extern void Sg__Init_sagittarius_compiler();

void Sg_Init()
{
  GC_INIT();
  /* order is important especially libraries */
  Sg__InitString();		/* string must be before symbol */
  Sg__InitSymbol();
  Sg__InitNumber();
  Sg__InitKeyword();

  Sg__InitVM();
  /* initialize default reader macro */
  Sg__InitReader();

  /* (sagittarius) library is not provided by file. so create it here */
  Sg_FindLibrary(SG_INTERN("(sagittarius)"), TRUE);

  Sg__InitInstruction();
  Sg__Initnull();
  Sg__Init_sagittarius();
  Sg__Init_sagittarius_vm();
  Sg__Init_sagittarius_vm_debug();

  Sg__Init_core_base();
  Sg__Init_sagittarius_compiler_util();
  Sg__Init_sagittarius_compiler_procedure();
  Sg__Init_sagittarius_compiler();

  /* generic initialization must be after the libraries initialization */
  Sg__InitFile();

  /* TODO should this be here? */
  Sg_ImportLibrary(Sg_VM()->currentLibrary, SG_OBJ(SG_INTERN("null")));
  Sg_ImportLibrary(Sg_VM()->currentLibrary, SG_OBJ(SG_INTERN("(sagittarius)")));
#if 1
  Sg_ImportLibrary(SG_OBJ(SG_INTERN("(sagittarius compiler)")),
		   SG_OBJ(SG_INTERN("null")));
  Sg_ImportLibrary(SG_OBJ(SG_INTERN("(sagittarius compiler)")),
		   SG_OBJ(SG_INTERN("(sagittarius)")));
#endif

  /* 
     we need extra treatment for er-rename. it's defined after the 
     initialization of compiler, so we need to export it to (core base) which 
     defines er-macro-transformer
   */
  {
    SgLibrary *core_base_lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(core base)"), FALSE));
    Sg_InsertBinding(core_base_lib,
		     SG_INTERN("er-rename"),
		     Sg_FindBinding(SG_INTERN("null"), SG_INTERN("er-rename")));
  }

}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
