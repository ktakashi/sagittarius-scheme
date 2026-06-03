/* vmcall.c                                        -*- mode:c; coding:utf-8; -*-
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
/* This file is included at vm.c */

#define MAKE_CALL_IRRITANTS()			\
  Sg_Cons(SG_PROCEDURE_NAME(AC(vm)), Sg_ArrayToList(SP(vm)-argc, argc))

#undef ADJUST_ARGUMENT_FRAME
#if !defined(APPLY_CALL)
#define ADJUST_ARGUMENT_FRAME(proc, argc)				\
  do {									\
    int required = SG_PROCEDURE_REQUIRED(proc);				\
    int optargs =  SG_PROCEDURE_OPTIONAL(proc);				\
    if (optargs) {							\
      SgObject p = SG_NIL, a;						\
      if (argc < required) {						\
	SgObject irr = MAKE_CALL_IRRITANTS();				\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, argc, irr);	\
      }									\
      /* fold rest args */						\
      while (argc > required+optargs-1) {				\
	a = POP(SP(vm));						\
	p = Sg_Cons(a, p);						\
	argc--;								\
      }									\
      PUSH(SP(vm), p);							\
      argc++;								\
    } else if (argc != required) {					\
      SgObject irr = MAKE_CALL_IRRITANTS();				\
      Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					 required, argc, irr);		\
    }									\
    FP(vm) = SP(vm) - argc;						\
  } while (0)
#else  /* APPLY_CALL */
#define ADJUST_ARGUMENT_FRAME(proc, argc)				\
  do {									\
    int required = SG_PROCEDURE_REQUIRED(proc);				\
    int optargs =  SG_PROCEDURE_OPTIONAL(proc);				\
    int rargc = (int)Sg_Length(INDEX(SP(vm), 0));			\
    SgObject p, a;							\
    if (optargs) {							\
      int __i, req_opt, oargc;						\
      if ((rargc+argc-1) < required) {					\
	SgObject irr = MAKE_CALL_IRRITANTS();				\
      	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, rargc+argc-1, irr); \
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
	SgObject irr = MAKE_CALL_IRRITANTS();				\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, rargc+argc-1, irr); \
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
  vm->valuesCount = 1;		/* default */
#ifdef SHOW_CALL_TRACE
  if (MOSTLY_FALSE(SG_VM_LOG_LEVEL(vm,SG_TRACE_LEVEL)&&vm->state == RUNNING)) {
    Sg_Printf(vm->logPort, UC(";; calling %S\n"), AC(vm));
  }
#endif
  if (MOSTLY_FALSE(!SG_PROCEDUREP(AC(vm)))) {
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
  switch (proctype) {
  case SG_PROC_SUBR: {
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
    CHECK_ATTENTION;
    NEXT;
  } break;
    
  case SG_PROC_CLOSURE: {
    SgClosure * cl = SG_CLOSURE(AC(vm));
    SgCodeBuilder *cb = SG_CODE_BUILDER(cl->code);
    CHECK_STACK(cb->maxStack, vm);
    ADJUST_ARGUMENT_FRAME(cl, argc);

#ifdef HAVE_JIT
    /* JIT execution path */
    if (Sg_JitVerbose()) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("VM: Calling closure %A, cb=%p jitFlags=%d, jitCode=%p\n"),
		SG_CODE_BUILDER_NAME(cb), cb, cb->jitFlags, cb->jitCode);
    }
    if (Sg_JitEnabled() && cb->jitFlags == SG_JIT_FLAG_COMPILED && cb->jitCode != NULL) {
      /* Execute JIT-compiled code */
      SgJitCompiledCode jitFunc = (SgJitCompiledCode)cb->jitCode;
      SgObject jitResult;
      CL(vm) = AC(vm);
      SG_PROF_COUNT_CALL(vm, CL(vm));
      if (Sg_JitVerbose()) {
	Sg_Printf(Sg_StandardErrorPort(),
		  UC("VM: Executing JIT code for %A FP=%p FP[0]=%A\n"),
		  SG_CODE_BUILDER_NAME(cb), FP(vm), FP(vm)[0]);
      }
      jitResult = jitFunc(vm, cl);
      /* For YIELD_PRESERVE_AC, the helper already set vm->ac correctly.
       * Don't overwrite it. For other cases, store the result to AC. */
      if (!SG_JIT_YIELD_PRESERVE_AC_P(jitResult)) {
        AC(vm) = jitResult;
      }
      if (Sg_JitVerbose()) {
	int isYield = SG_JIT_YIELD_P(jitResult);
	if (!isYield) {
	  Sg_Printf(Sg_StandardErrorPort(),
		    UC("VM: JIT code returned AC=%A (yield=0)\n"),
		    AC(vm));
	} else {
	  Sg_Printf(Sg_StandardErrorPort(),
		    UC("VM: JIT code returned (yield=%d, preserveAC=%d)\n"),
		    isYield, SG_JIT_YIELD_PRESERVE_AC_P(jitResult));
	}
      }
      /* Check for yield marker - JIT wants interpreter to continue */
      if (SG_JIT_YIELD_P(jitResult)) {
        if (Sg_JitVerbose()) {
          if (SG_JIT_YIELD_PRESERVE_AC_P(jitResult)) {
            Sg_Printf(Sg_StandardErrorPort(),
                      UC("VM: JIT yielded to interpreter for %A (PRESERVE_AC, AC=%A)\n"),
                      CL(vm), AC(vm));
          } else {
            Sg_Printf(Sg_StandardErrorPort(),
                      UC("VM: JIT yielded to interpreter for %A\n"),
                      CL(vm));
          }
        }
        /* VM state is already set up by JIT helper, just continue.
         * For normal yield, clear AC. For YIELD_PRESERVE_AC, AC is already set. */
        if (!SG_JIT_YIELD_PRESERVE_AC_P(jitResult)) {
          AC(vm) = SG_UNDEF;
        }
        NEXT;
      }
      /* JIT function completed entire closure - return to caller */
      RET_INSN();
      CHECK_ATTENTION;
      NEXT;
    } else if (Sg_JitEnabled() && cb != NULL && cb->jitFlags == 0 &&
               vm->state != COMPILING && vm->state != IMPORTING &&
               !SG_JIT_CONTEXT_ACTIVE(vm)) {
      /* Track call count for hot code detection.
       * Skip JIT when in COMPILING/IMPORTING state to prevent compiler
       * code from being JIT compiled.
       * Also skip when inside JIT execution to prevent JIT-during-JIT. */
      cb->callCount++;
      uint32_t count = cb->callCount;
      int threshold = Sg_GetJitThreshold();
      if ((int)count >= threshold) {
        /* Mark as "compiling" to prevent double compilation */
	if (cb->jitFlags == 0) {
	  cb->jitFlags = SG_JIT_FLAG_COMPILING;
	  if (Sg_JitVerbose()) {
	    Sg_Printf(Sg_StandardErrorPort(),
		      UC("VM: AUTO-JIT compiling %A\n"),
		      SG_CODE_BUILDER_NAME(cb));
	  }
	  cb->jitCode = Sg_JitCompile(cb);
	  if (cb->jitCode != NULL) {
	    cb->jitFlags = SG_JIT_FLAG_COMPILED;
	    /* Skip JIT execution on first compile - fall through to interpreter.
	     * This allows the JIT code to be used on subsequent calls. */
	  } else {
	    cb->jitFlags = SG_JIT_FLAG_FAILED;
	  }
	}
      }
    }
    /* If jitFlags has unexpected value, skip JIT and fall through to interpreter */
#endif /* HAVE_JIT */
    /* Interpreter fallback */
    CL(vm) = AC(vm);
    PC(vm) = cb->code;
    AC(vm) = SG_UNDEF;		/* make default return value #<unspecified> */
    SG_PROF_COUNT_CALL(vm, CL(vm));
    NEXT;
  } break;

  case SG_PROC_GENERIC: {
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
      if (SG_METHOD_LEAF_P(SG_CAR(mm))) {
	nm = SG_TRUE;		/* dummy */
      } else {
	nm = Sg_MakeNextMethod(SG_GENERIC(AC(vm)), SG_CDR(mm), SP(vm) - argc, 
			       argc, TRUE);
      }
      AC(vm) = SG_CAR(mm);
      proctype = SG_PROC_METHOD;
    }
  } break;

  case SG_PROC_NEXT_METHOD: {
    SgNextMethod *n = SG_NEXT_METHOD(AC(vm));
    int use_saved_args = FALSE;
#if !defined(APPLY_CALL)
    use_saved_args = (argc == 0);
#else
    SgObject last = INDEX(SP(vm), 0);
    /* (apply call-next-methods '()) */
    use_saved_args = (argc == 1 && SG_NULLP(last));
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
      if (SG_METHOD_LEAF_P(SG_CAR(n->methods))) {
	nm = SG_TRUE;		/* dummy */
      } else {
	if (use_saved_args) {
	  nm = Sg_MakeNextMethod(n->generic, SG_CDR(n->methods),
				 n->argv, n->argc, FALSE);
	} else {
#if !defined(APPLY_CALL)
	  nm = Sg_MakeNextMethod(n->generic, SG_CDR(n->methods),
				 SP(vm)-argc, argc, TRUE);
#else
	  /* e.g. (apply call-next-methods (list 'a))
	     The top of the stack contains a list, if we save it
	     as it is, then the saved argv will be ((a)).
	     So, we need to precompute the frame
	   */
	  int l = Sg_Length(last), m = argc - 1, i;
	  SgObject *argv = SG_NEW_ARRAY(SgObject, m + l), tmp;
	  if (argc - 1 != 0) {
	    memcpy(SP(vm), n->argv, sizeof(SgObject)*(m));
	  }
	  for (tmp = last, i = 0; i < l; i++, tmp = SG_CDR(tmp)) {
	    argv[m + i] = SG_CAR(tmp);
	  }
	  nm = Sg_MakeNextMethod(n->generic, SG_CDR(n->methods),
				 argv, m + l, FALSE);
#endif
	}
      }
      AC(vm) = SG_CAR(n->methods);
      proctype = SG_PROC_METHOD;
    }
    /* if (use_saved_args) { */
    /*   goto DO_METHOD_CALL; */
    /* } */
  } break;
    /* MOP can return method directly so do not panic yet */
  case SG_PROC_METHOD: {
    Sg_Error(UC("%S appeared. Incorrect MOP is defined!"), AC(vm));
  } break;

  default: Sg_Panic("something's wrong");
  }

  /* not used... */
 /* DO_METHOD_CALL: */
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
    CHECK_ATTENTION;
    NEXT;
  }

  /* ASSERT(proctype = SG_PROC_METHOD); */
  /* ASSERT(!SG_FALSEP(nm)); */
  if (SG_SUBRP(SG_METHOD_PROCEDURE(AC(vm)))) {
    /* C-defined method */
    SgObject subr = SG_METHOD_PROCEDURE(AC(vm));
    ADJUST_ARGUMENT_FRAME(AC(vm), argc);
    CL(vm) = subr;
    PC(vm) = PC_TO_RETURN;
    SG_PROF_COUNT_CALL(vm, subr);
    AC(vm) = SG_SUBR_FUNC(subr)(FP(vm), argc, SG_SUBR_DATA(subr));
    if (TAIL_POS(vm)) RET_INSN();
    CHECK_ATTENTION;
  } else {
    /* closure */
    SgClosure *cls = SG_CLOSURE(SG_METHOD_PROCEDURE(AC(vm)));
    /* ASSERT(SG_CODE_BUILDERP(cls->code)); */
    /* shift one for call-next-method */
    SP(vm) = shift_one_args(SP(vm), argc);
    INDEX_SET(SP(vm), argc, nm);
    argc++;
    CHECK_STACK(SG_CODE_BUILDER(cls->code)->maxStack, vm);
    ADJUST_ARGUMENT_FRAME(cls, argc);
    CL(vm) = cls;
    PC(vm) = SG_CODE_BUILDER(cls->code)->code;
    AC(vm) = SG_UNDEF;		/* default undef */
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
