/* subr.c                                          -*- mode:c; coding:utf-8; -*-
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/subr.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/generic.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/vm.h"

static void proc_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  if (SG_PROCEDURE_TYPE(obj) == SG_PROC_SUBR)
    Sg_Putuz(port, UC("#<subr "));
  else if (SG_PROCEDURE_TYPE(obj) == SG_PROC_CLOSURE)
    Sg_Putuz(port, UC("#<closure "));
  /* well should not be here but in case. */
  else if (SG_PROCEDURE_TYPE(obj) == SG_PROC_GENERIC)
    Sg_Putuz(port, UC("#<generic "));
  else if (SG_PROCEDURE_TYPE(obj) == SG_PROC_METHOD)
    Sg_Putuz(port, UC("#<method "));
  else if (SG_PROCEDURE_TYPE(obj) == SG_PROC_NEXT_METHOD)
    Sg_Putuz(port, UC("#<next-method "));
  Sg_Write(SG_PROCEDURE_NAME(obj), port, SG_WRITE_DISPLAY);

  Sg_Printf(port, UC(" %d:%d"),
	    SG_PROCEDURE_REQUIRED(obj), SG_PROCEDURE_OPTIONAL(obj));

  Sg_Putc(port, '>');
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ProcedureClass, proc_print);

static SgSubr* make_subr(int req, int opt, SgObject info)
{
  SgSubr *s = SG_NEW(SgSubr);
  SG_SET_CLASS(s, SG_CLASS_PROCEDURE);
  SG_PROCEDURE_INIT(s, req, opt, SG_PROC_SUBR, info);
  return s;
}

SgObject Sg_MakeSubr(SgSubrProc proc, void *data, int required, int optional,
		     SgObject info)
{
  SgSubr *s = make_subr(required, optional, info);
  s->func = proc;
  s->data = data;
  return SG_OBJ(s);
}

SgObject Sg_MakeSubrFull(SgSubrProc proc, void *data, int required,
			 int optional, SgObject info, int trans)
{
  SgSubr *s = make_subr(required, optional, info);
  s->func = proc;
  s->data = data;
  /* SG_PROCEDURE_TRANSPARENT(s) = trans; */
  return SG_OBJ(s);
}

static SgObject theNullProc = SG_NIL;
static SgObject null_proc(SgObject *args, int argc, void *data)
{
  return SG_UNDEF;
}

SgObject Sg_NullProc()
{
  if (SG_NULLP(theNullProc)) {
    theNullProc = Sg_MakeSubrFull(null_proc, NULL, 0, 1, SG_INTERN("nullproc"),
				  SG_PROC_TRANSPARENT);
  }
  return SG_OBJ(theNullProc);
}

/* for SRFI-17 */
SgObject Sg_SetterSet(SgProcedure *proc, SgProcedure *setter, int lock)
{
  if (proc->locked) {
    Sg_Error(UC("can't change the locked setter of procedure %S"), proc);
  }
  proc->setter = SG_OBJ(setter);
  proc->locked = lock;
  return SG_OBJ(proc);
}

static SgObject object_setter(SgObject *args, int argc, void *data)
{
  ASSERT(argc == 1);
  return Sg_VMApply(SG_OBJ(&Sg_GenericObjectSetter),
		    Sg_Cons(SG_OBJ(data), args[0]));
}

SgObject Sg_Setter(SgObject proc)
{
  if (SG_PROCEDUREP(proc)) {
    return SG_PROCEDURE_SETTER(proc);
  } else {
    return Sg_MakeSubr(object_setter, (void*)proc, 0, 1,
		       SG_MAKE_STRING("object-setter"));
  }
}

int Sg_HasSetter(SgObject proc)
{
  if (SG_PROCEDUREP(proc)) {
    return !SG_FALSEP(SG_PROCEDURE_SETTER(proc));
  } else {
    /* setter of object-apply is used */
    return TRUE;
  }
}
