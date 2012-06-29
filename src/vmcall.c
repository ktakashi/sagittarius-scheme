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

#undef ADJUST_ARGUMENT_FRAME
#if !defined(APPLY_CALL)
#define ADJUST_ARGUMENT_FRAME(proc, argc)				\
  do {									\
    int required = SG_PROCEDURE_REQUIRED(proc);				\
    int optargs =  SG_PROCEDURE_OPTIONAL(proc);				\
    if (optargs) {							\
      SgObject p = SG_NIL, a;						\
      if (argc < required) {						\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, argc, SG_UNDEF);	\
      }									\
      /* fold rest args */						\
      while (argc > required+optargs-1) {				\
	a = POP(SP(vm));						\
	p = Sg_Cons(a, p);						\
	argc--;								\
      }									\
      PUSH(SP(vm), p);							\
      argc++;								\
    } else {								\
      if (argc != required) {						\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, argc, SG_UNDEF);	\
      }									\
    }									\
    FP(vm) = SP(vm) - argc;						\
  } while (0)
#else  /* APPLY_CALL */
#define ADJUST_ARGUMENT_FRAME(proc, argc)				\
  do {									\
    int required = SG_PROCEDURE_REQUIRED(proc);				\
    int optargs =  SG_PROCEDURE_OPTIONAL(proc);				\
    int rargc = Sg_Length(INDEX(SP(vm), 0));				\
    SgObject p, a;							\
    if (optargs) {							\
      int __i, req_opt, oargc;						\
      if ((rargc+argc-1) < required) {					\
      	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   rargc+argc-1, argc, SG_UNDEF); \
      }									\
      req_opt = required+optargs;					\
      p = POP(SP(vm)); /* tail of arglist */				\
      oargc = argc--;							\
      if (oargc > req_opt) {						\
	/* fold rest args */						\
	p = Sg_CopyList(p);						\
	for (__i = oargc; __i > req_opt; __i--) {			\
	  a = POP(SP(vm));						\
	  argc--;							\
	  p = Sg_Cons(a, p);						\
	}								\
	PUSH(SP(vm), p);						\
	argc++;								\
	/* argc -= oargc - __i -1; */					\
      } else {								\
	/* unfold rest arg */						\
	CHECK_STACK(req_opt - oargc, vm);				\
	for (__i = oargc; SG_PAIRP(p) && __i < req_opt; __i++) {	\
	  PUSH(SP(vm), SG_CAR(p));					\
	  argc++;							\
	  p = SG_CDR(p);						\
	}								\
	p = Sg_CopyList(p);						\
	PUSH(SP(vm), p);						\
	argc++;								\
	/* argc += __i - oargc +1; */					\
      }									\
    } else {								\
      /* not optargs */							\
      if ((rargc+argc-1) != required) {					\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, rargc, SG_UNDEF);	\
      }									\
      p = POP(SP(vm));							\
      argc--;								\
      if (rargc > 0) {							\
	CHECK_STACK(rargc, vm);						\
	/* argc +=rargc; */						\
	do {								\
	  PUSH(SP(vm), SG_CAR(p));					\
	  argc++;							\
	  p = SG_CDR(p);						\
	} while (--rargc > 0);						\
      }									\
    }									\
    FP(vm) = SP(vm) - argc;						\
  } while (0)
#endif	/* APPLY_CALL */

#undef GENERIC_ENTRY
#undef APP
#undef DO_METHOD_CALL
#if !defined(APPLY_CALL)
#define GENERIC_ENTRY  generic_entry
#define DO_METHOD_CALL do_method_call
#define APP FALSE
#else
#define GENERIC_ENTRY  generic_entry_app
#define DO_METHOD_CALL do_method_call_app
#define APP TRUE
#endif

{
  int argc, proctype;
  SgObject nm = SG_FALSE;	/* next method */
  INSN_VAL1(argc, c);

  if (SG_VM_LOG_LEVEL(vm, SG_TRACE_LEVEL) && vm->state == RUNNING) {
    Sg_Printf(vm->logPort, UC(";; calling %S\n"), AC(vm));
  }
  if (!SG_PROCEDUREP(AC(vm))) {
    int i;
    CHECK_STACK(1, vm);
    for (i = 0; i < argc; i++) {
      *(SP(vm)-i) = *(SP(vm)-i-1);
    }
    *(SP(vm)-argc) = AC(vm);
    SP(vm)++; argc++;
    AC(vm) = SG_OBJ(&Sg_GenericObjectApply);
    proctype = SG_PROC_GENERIC;
    goto GENERIC_ENTRY;
  }

  proctype = SG_PROCEDURE_TYPE(AC(vm));
  if (proctype == SG_PROC_SUBR) {
    CL(vm) = AC(vm);
    PC(vm) = PC_TO_RETURN;
    /* 
       Since 0.3.4, we changed APPLY instruction behaviour not to expand
       the arguments so that it won't break the memory when it's given
       more than max stack size of arguments.
    */
    ADJUST_ARGUMENT_FRAME(AC(vm), argc);
    SG_PROF_COUNT_CALL(vm, AC(vm));
    AC(vm) = SG_SUBR_FUNC(AC(vm))(FP(vm), argc, SG_SUBR_DATA(AC(vm)));
    if (TAIL_POS(vm)) RET_INSN();
    NEXT;
  }

  if (proctype == SG_PROC_CLOSURE) {
    SgClosure * cl = SG_CLOSURE(AC(vm));
    SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
    CHECK_STACK(cb->maxStack, vm);
    ADJUST_ARGUMENT_FRAME(cl, argc);
    CL(vm) = AC(vm);
    PC(vm) = cb->code;
    AC(vm) = SG_UNDEF;		/* make default return value #<unspecified> */
    SG_PROF_COUNT_CALL(vm, CL(vm));
    NEXT;
  }

  if (proctype == SG_PROC_GENERIC) {
    SgObject mm;
    if (!SG_GENERICP(AC(vm))) {
      /* Scheme defined MOP. we modify the stack frame so that it is converted
         to an application of pure generic function apply-generic. */
      SgObject args, arg;
      int i;
#if !defined(APPLY_CALL)
      if (argc < 2) CHECK_STACK(2, vm);
      args = SG_NIL;
      for (i = 0; i < argc; i++) {
	arg = POP(SP(vm));
	args = Sg_Cons(arg, args);
      }
      argc = 2;
      PUSH(SP(vm), AC(vm));
      PUSH(SP(vm), args);
#else	/* APPLY_CALL */
      if (argc < 3) CHECK_STACK(3, vm);
      args = POP(SP(vm));
      argc--;
      for (i = 0; i < argc; i++) {
	arg = POP(SP(vm));
	args = Sg_Cons(arg, args);
      }
      argc = 2;
      PUSH(SP(vm), AC(vm));
      PUSH(SP(vm), args);
      PUSH(SP(vm), SG_NIL);
#endif	/* APPLY_CALL */
      AC(vm) = SG_OBJ(&Sg_GenericComputeApplyGeneric);
    }
  GENERIC_ENTRY:
    mm = Sg_ComputeMethods(AC(vm), SP(vm)-argc, argc, APP);
    if (!SG_NULLP(mm)) {
      /* methods are sorted by compute-methods.
	 create call-next-methods */
#if defined(APPLY_CALL)
      if (argc-1 < SG_GENERIC_MAX_REQARGS(AC(vm))) {
	SgObject args = POP(SP(vm));
	CHECK_STACK(SG_GENERIC_MAX_REQARGS(AC(vm)) - argc, vm);
	while (argc <= SG_GENERIC_MAX_REQARGS(AC(vm)) && SG_PAIRP(args)) {
	  PUSH(SP(vm), SG_CAR(args));
	  args = SG_CDR(args);
	  argc++;
	}
	PUSH(SP(vm), args);
      }
#endif
      nm = Sg_MakeNextMethod(SG_GENERIC(AC(vm)), SG_CDR(mm), SP(vm) - argc, 
			     argc, TRUE);
      AC(vm) = SG_CAR(mm);
      proctype = SG_PROC_METHOD;
    }
  } else if (proctype == SG_PROC_NEXT_METHOD) {
    SgNextMethod *n = SG_NEXT_METHOD(AC(vm));
    int use_saved_args = FALSE;
#if !defined(APPLY_CALL)
    use_saved_args = (argc == 0);
#else
    use_saved_args = (argc == 1 && SG_NULLP(INDEX(SP(vm), 0)));
#endif
    if (use_saved_args) {
      CHECK_STACK(n->argc+1, vm);
      memcpy(SP(vm), n->argv, sizeof(SgObject)*n->argc);
      SP(vm) += n->argc;
      argc = n->argc;
    }
    if (SG_NULLP(n->methods)) {
      /* no applicable methods */
      AC(vm) = SG_OBJ(n->generic);
      proctype = SG_PROC_GENERIC;
    } else {
      nm = Sg_MakeNextMethod(n->generic, SG_CDR(n->methods),
			     SP(vm)-argc, argc, TRUE);
      AC(vm) = SG_CAR(n->methods);
      proctype = SG_PROC_METHOD;
    }
    if (use_saved_args) {
      goto DO_METHOD_CALL;
    }
  } else {
    Sg_Panic("something's wrong");
  }
 DO_METHOD_CALL:
  if (proctype == SG_PROC_GENERIC) {
    /* we have no applicable methods */
#if defined(APPLY_CALL)
    SgObject args = POP(SP(vm));
    argc--;
    while (SG_PAIRP(args)) {
      PUSH(SP(vm), SG_CAR(args));
      args = SG_CDR(args);
      argc++;
    }
#endif
    PC(vm) = PC_TO_RETURN;
    FP(vm) = SP(vm) - argc;
    SG_PROF_COUNT_CALL(vm, AC(vm));
    AC(vm) = SG_GENERIC(AC(vm))->fallback(FP(vm), argc, SG_GENERIC(AC(vm)));
    if (TAIL_POS(vm)) RET_INSN();
    NEXT;
  }

  ASSERT(proctype = SG_PROC_METHOD);
  ASSERT(!SG_FALSEP(nm));
  if (SG_SUBRP(SG_METHOD_PROCEDURE(AC(vm)))) {
    /* C-defined method */
    SgObject subr = SG_METHOD_PROCEDURE(AC(vm));
    ADJUST_ARGUMENT_FRAME(AC(vm), argc);
    CL(vm) = subr;
    PC(vm) = PC_TO_RETURN;
    SG_PROF_COUNT_CALL(vm, subr);
    AC(vm) = SG_SUBR_FUNC(subr)(FP(vm), argc, SG_SUBR_DATA(subr));
    if (TAIL_POS(vm)) RET_INSN();
  } else {
    /* closure */
    SgClosure *cls = SG_CLOSURE(SG_METHOD_PROCEDURE(AC(vm)));
    ASSERT(SG_CODE_BUILDERP(cls->code));
    /* shift one for call-next-method */
    SP(vm) = shift_one_args(SP(vm), argc);
    INDEX_SET(SP(vm), argc, nm);
    argc++;
    CHECK_STACK(SG_CODE_BUILDER(cls->code)->maxStack, vm);
    ADJUST_ARGUMENT_FRAME(cls, argc);
    CL(vm) = cls;
    PC(vm) = SG_CODE_BUILDER(cls->code)->code;
    SG_PROF_COUNT_CALL(vm, cls);
  }
  NEXT;
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
