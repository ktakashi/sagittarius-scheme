/* closure.c                                        -*- mode:c; coding:utf-8; -*-
 *   Copyright (c) 2010-2014  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/closure.h"
#include "sagittarius/code.h"
#include "sagittarius/instruction.h"
#include "sagittarius/gloc.h"
#include "sagittarius/library.h"
#include "sagittarius/identifier.h"
#include "sagittarius/pair.h"
/* for debug */
/* #include "sagittarius/vm.h" */

SgObject Sg_MakeClosure(SgObject code,
			SgObject *frees)
{
  SgClosure *cl;
  int req, opt, i, freec;
  SgObject info;
  ASSERT(SG_CODE_BUILDERP(code));

  cl = SG_NEW2(SgClosure *, sizeof(SgClosure) +
	       (sizeof(SgObject) * (SG_CODE_BUILDER_FREEC(code) - 1)));
  info = Sg_CodeBuilderFullName(SG_CODE_BUILDER(code));
  req = SG_CODE_BUILDER_ARGC(code);
  opt = SG_CODE_BUILDER_OPTIONAL(code);

  SG_SET_CLASS(cl, SG_CLASS_PROCEDURE);
  SG_PROCEDURE_INIT(cl, req, opt, SG_PROC_CLOSURE, info);

  freec = SG_CODE_BUILDER_FREEC(code);
  cl->code = code;
  for (i = 0; i < freec; i++) {
    cl->frees[i] = frees[freec - i - 1];
  }
  SG_PROCEDURE_TRANSPARENT(cl) = SG_CLOSURE_UNCHECKED;
  /* for future */
  /* cl->checked = FALSE; */
  return SG_OBJ(cl);
}

static int check_gref_call(SgObject id_or_gloc, SgObject seen, int* skippedP);

static int closure_transparent_rec(SgCodeBuilder *cb, SgObject seen)
{
  SgWord *code = cb->code;
  int size = cb->size, flag = SG_PROC_TRANSPARENT, i;
  /* for now we only checks very simple case... */
  for (i = 0; i < size; i++) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
#if 0
    if (Sg_VM()->state == RUNNING) {
      fprintf(stderr, "insn: %s\n", info->name);
    }
#endif
    switch (info->number) {
      /* constant */
    case UNDEF: case CONST: case CONSTI: case CONST_RET:
    case EQ: case EQV: case NULLP: case PAIRP: case SYMBOLP:
    case VECTORP:
      /* well error is not a side effect... */
    case ADD: case ADDI: case SUB: case SUBI: case MUL: case MULI:
    case DIV: case DIVI: case NEG: case NOT:
    case NUM_EQ: case NUM_LT: case NUM_LE: case NUM_GT: case NUM_GE:
    case CAR: case CDR: case VALUES: case VEC_LEN: case VEC_REF:
    case CAAR: case CADR: case CDAR: case CDDR:
      /* constant operations */
    case NOP: case LREF: case PUSH: case TEST: case JUMP: case SHIFTJ:
    case BNNUME: case BNLT: case BNLE: case BNGT: case BNGE:
    case BNEQ: case BNEQV: case BNNULL:
    case RECEIVE: case FRAME: case ENTER: case LEAVE: case RET:
    case LREF_PUSH: case CONST_PUSH: case CONSTI_PUSH: 
    case CAR_PUSH: case CDR_PUSH:
    case LREF_CAR: case LREF_CDR: case LREF_CAR_PUSH: case LREF_CDR_PUSH:
    case LSET:			/* it affects only local so should be fine */
      /* keep default */
      break;
      /* no-side-effect */
      /* FREF and GREF is a bit tricky but they have a chance to get
	 changed so it won't be constant */
    case FREF: case GREF: case BOX: case UNBOX: case CONS: case LIST:
    case APPEND: case FREF_PUSH: case GREF_PUSH: case CONS_PUSH:
    case FREF_CAR: case FREF_CDR: case GREF_CAR: case GREF_CDR:
    case FREF_CAR_PUSH: case FREF_CDR_PUSH:
    case GREF_CAR_PUSH: case GREF_CDR_PUSH:
      if (flag == SG_PROC_TRANSPARENT) {
	flag = SG_PROC_NO_SIDE_EFFECT;
      }
      break;
      /* conditional no-side-effect*/
    case GREF_CALL: case GREF_TAIL_CALL: {
      int skippedP = FALSE;      
      int flag2 = check_gref_call(SG_OBJ(code[i+1]), seen, &skippedP);
      if (!skippedP) flag = flag2;
      break;
    }
    case CLOSURE:
      flag = closure_transparent_rec(SG_CODE_BUILDER(SG_OBJ(code[i+1])), seen);
      break;
      /* APPLY, CALL, TAIL_CALL  */
    default: flag = SG_CLOSURE_SIDE_EFFECT; break;
    }
    /* no need to check anymore */
    if (flag == SG_CLOSURE_SIDE_EFFECT) break;
    i += info->argc;
  }
  return flag;
}

static int closure_transparent(SgObject closure, SgObject seen)
{
  /* it's already checked, so just return the previous result. */
  if (SG_PROCEDURE_TRANSPARENT(closure) != SG_CLOSURE_UNCHECKED)
    return SG_PROCEDURE_TRANSPARENT(closure);

  SG_PROCEDURE_TRANSPARENT(closure) = 
    closure_transparent_rec(SG_CODE_BUILDER(SG_CLOSURE(closure)->code), seen);
  return SG_PROCEDURE_TRANSPARENT(closure);
}

static int check_gref_call(SgObject id_or_gloc, SgObject seen, int *skippedP)
{
  SgObject proc;
  if (SG_IDENTIFIERP(id_or_gloc)) {
    id_or_gloc = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(id_or_gloc),
				SG_IDENTIFIER_NAME(id_or_gloc),
				SG_UNBOUND);
  }
  proc = SG_GLOC_GET(SG_GLOC(id_or_gloc));
  if (!SG_FALSEP(Sg_Memq(proc, seen))) {
    *skippedP = TRUE;
    return SG_CLOSURE_SIDE_EFFECT;
  }
  if (!SG_PROCEDUREP(proc)) return SG_CLOSURE_SIDE_EFFECT;
  if (SG_SUBRP(proc)) {
    int f = SG_PROCEDURE_TRANSPARENT(proc);
    if (f == SG_SUBR_SIDE_EFFECT) return SG_CLOSURE_SIDE_EFFECT;
    return f;
  } else if (SG_CLOSUREP(proc)) {
    return closure_transparent(proc, Sg_Cons(proc, seen));
  }
  /* generic function can't be determine. */
  return SG_CLOSURE_SIDE_EFFECT;

}
/* look thru the instruction and check if there is a side effect
   instruction or procedure. */
int Sg_ClosureTransparent(SgObject closure)
{
  return closure_transparent(closure, SG_NIL);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
