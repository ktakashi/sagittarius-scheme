/* -*- C -*- */
/*
 * vmcall.c
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
/* This file is included at vm.c */


#define ADJUST_ARGUMENT_FRAME(proc, argc)				\
  do {									\
    int required = SG_PROCEDURE_REQUIRED(proc);				\
    int optargs =  SG_PROCEDURE_OPTIONAL(proc);				\
    if (optargs) {							\
      int extra = argc - required;					\
      if (-1 == extra) {						\
	/* Apply call */						\
	SgObject *sp;							\
	if (SP(vm) - vm->stack >= 1) {					\
	  sp = unshift_args(SP(vm), 1);					\
	  INDEX_SET(sp, 0, SG_NIL);					\
	} else {							\
	  sp = SP(vm);							\
	  PUSH(sp, SG_NIL);						\
	}								\
	SP(vm) = sp;							\
	FP(vm) = sp - required;						\
      } else if (extra >= 0) {						\
	SgObject *sp;							\
	INDEX_SET(SP(vm), extra, stack_to_pair_args(SP(vm), extra+1));	\
	sp = SP(vm) - extra;						\
	SP(vm) = sp;							\
	FP(vm) = sp - required;						\
      } else {								\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required-1, argc, SG_UNDEF); \
      }									\
    } else if (required == argc) {					\
      FP(vm) = SP(vm) - argc;						\
    } else {								\
      SgObject args = SG_NIL;						\
      int i;								\
      for (i = 0; i < argc; i++) {					\
	args = Sg_Cons(INDEX(SP(vm), i), args);				\
      }									\
      Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					 required, argc, args);		\
    }									\
  } while (0)

#define CALL_METHOD()							\
  do {									\
    ASSERT(SG_METHODP(AC(vm)));						\
    ASSERT(!SG_FALSEP(nm));						\
    if (SG_SUBRP(SG_METHOD_PROCEDURE(AC(vm)))) {			\
      /* C-defined method */						\
      SgObject subr = SG_METHOD_PROCEDURE(AC(vm));			\
      CL(vm) = subr;							\
      PC(vm) = SG_SUBR_RETURN_CODE(subr);				\
      SG_SUBR_RETURN_CODE(subr)[0] = SG_WORD(RET);			\
      FP(vm) = SP(vm) - argc;						\
      SG_PROF_COUNT_CALL(vm, subr);					\
      AC(vm) = SG_SUBR_FUNC(subr)(FP(vm), argc, SG_SUBR_DATA(subr));	\
    } else {								\
      /* closure */							\
      SgClosure *cls = SG_CLOSURE(SG_METHOD_PROCEDURE(AC(vm)));		\
      ASSERT(SG_CODE_BUILDERP(cls->code));				\
      CL(vm) = cls;							\
      PC(vm) = SG_CODE_BUILDER(cls->code)->code;			\
      unshift_args(SP(vm), 1);						\
      INDEX_SET(SP(vm), argc, nm);					\
      argc++;								\
      CHECK_STACK(SG_CODE_BUILDER(cls->code)->maxStack, vm);		\
      ADJUST_ARGUMENT_FRAME(cls, argc);					\
      SG_PROF_COUNT_CALL(vm, cls);					\
    }									\
  } while (0)

#define CALL_FALLBACK()							\
  do {									\
    PC(vm) = PC_TO_RETURN;						\
    FP(vm) = SP(vm) - argc;						\
    SG_PROF_COUNT_CALL(vm, AC(vm));					\
    AC(vm) = SG_GENERIC(AC(vm))->fallback(FP(vm), argc, SG_GENERIC(AC(vm))); \
  } while (0)

{
  int argc;
  SgObject nm = SG_FALSE;	/* next method */
  INSN_VAL1(argc, c);

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL) && vm->state == RUNNING) {
    Sg_Printf(vm->logPort, UC("calling %S\n"), AC(vm));
    if (SG_VM_LOG_LEVEL(vm, SG_TRACE_LEVEL) && vm->state == RUNNING) {
      print_frames(vm);
    }
  }

  if (SG_SUBRP(AC(vm))) {
    CL(vm) = AC(vm);
    PC(vm) = SG_SUBR_RETURN_CODE(AC(vm));
    SG_SUBR_RETURN_CODE(AC(vm))[0] = SG_WORD(RET);
    FP(vm) = SP(vm) - argc;
    SG_PROF_COUNT_CALL(vm, AC(vm));
    AC(vm) = SG_SUBR_FUNC(AC(vm))(FP(vm), argc, SG_SUBR_DATA(AC(vm)));
  } else if (SG_CLOSUREP(AC(vm))) {
    SgClosure *cl = SG_CLOSURE(AC(vm));
    SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
    CL(vm) = AC(vm);
    CHECK_STACK(cb->maxStack, vm);
    PC(vm) = cb->code;
    ADJUST_ARGUMENT_FRAME(cl, argc);
    SG_PROF_COUNT_CALL(vm, CL(vm));
  } else if (SG_PROCEDURE_TYPE(AC(vm)) == SG_PROC_GENERIC) {
    SgObject mm;
    if (!SG_GENERICP(AC(vm))) {
      /* Scheme defined MOP */
      /* TODO */
    }

    mm = Sg_ComputeMethods(AC(vm), SP(vm)-argc, argc);
    if (!SG_NULLP(mm)) {
      /* methods are sorted by compute-methods.
	 create call-next-methods */
      nm = Sg_MakeNextMethod(SG_GENERIC(AC(vm)), SG_CDR(mm), SP(vm) - argc, 
			     argc, TRUE);
      AC(vm) = SG_CAR(mm);
      CALL_METHOD();
    } else {
      /* no applicable methods */
      CALL_FALLBACK();
    }
  } else if (SG_PROCEDURE_TYPE(AC(vm)) == SG_PROC_NEXT_METHOD) {
      SgNextMethod *n = SG_NEXT_METHOD(AC(vm));
      int use_saved_args = (argc == 0);
      if (use_saved_args) {
	CHECK_STACK(n->argc+1, vm);
	memcpy(SP(vm), n->argv, sizeof(SgObject)*n->argc);
	SP(vm) += n->argc;
	argc = n->argc;
      }
      if (SG_NULLP(n->methods)) {
	/* no applicable methods */
	AC(vm) = SG_OBJ(n->generic);
	CALL_FALLBACK();
      } else {
	nm = Sg_MakeNextMethod(n->generic, SG_CDR(n->methods),
			       SP(vm)-argc, argc, TRUE);
	AC(vm) = SG_CAR(n->methods);
	CALL_METHOD();
      }
  } else {
    Sg_AssertionViolation(SG_INTERN("apply"),
			  Sg_MakeString(UC("invalid application"), SG_LITERAL_STRING),
			  AC(vm));
  }
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
