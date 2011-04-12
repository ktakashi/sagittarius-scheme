;; -*- scheme -*-

;; definitions of vm instruction.
;; NB: c, val1, val2 are defined in vm.c
;;     most of macros are also defined in vm.c
#!compatible

(define-cgen-stmt assertion-violation
    ((_ who msg)
     (dispatch
      `(Sg_AssertionViolation ,who (Sg_MakeString ,msg SG_LITERAL_STRING) '())))
    ((_ who msg irritants)
     (dispatch
      `(Sg_AssertionViolation ,who (Sg_MakeString ,msg SG_LITERAL_STRING) ,irritants))))

(define-cgen-stmt wrong-type-of-argument-violation
    ((_ who msg got)
     (dispatch
      `(Sg_WrongTypeOfArgumentViolation ,who (Sg_MakeString ,msg SG_LITERAL_STRING) ,got '())))
    ((_ who msg got irritants)
     (dispatch
      `(Sg_WrongTypeOfArgumentViolation ,who (Sg_MakeString ,msg SG_LITERAL_STRING) ,got ,irritants))))

;; utility statement.
(define-cgen-stmt for
  ((_ init cond incl . more)
   ((renderer) "for (")
   (renderer-no-indent #t)
   (unless (null? init)
     (dispatch init))
   (renderer-no-indent #t)
   ((renderer) ";")
   (unless (null? cond)
     (dispatch cond))
   (renderer-no-indent #t)
   ((renderer) ";")
   (unless (null? incl)
     (dispatch incl))
   (renderer-no-indent #t)
   ((renderer) (format ") {~%"))
   (renderer-no-indent #f)
   (renderer-indent-incl!)
   (dispatch `(begin ,@more))
   (renderer-indent-decl!)
   ((renderer) "}")))

#|
CASE(NOP) {
 NEXT;
}
|#
(define-inst NOP (0 0 #f) (:value 0)
  ;; do nothing
  )

#|
CASE(HALT) {
  return AC(vm);
}
|#
(define-inst HALT (0 0 #f) :return)

#|
CASE(UNDEF) {
  AC(vm) = SG_UNDEF;
  NEXT;
}
|#
(define-inst UNDEF (0 0 #f)
  (set! (AC vm) SG_UNDEF))

#|
CASE(CONST) {
  CONST_INSN(vm);
  NEXT;
}
|#
(define-inst CONST (0 1 #f)
  (CONST_INSN vm))

#|
CASE(CONSTI) {
  INSN_VAL1(val1 c);
  AC(vm) = SG_MAKE_INT(val1);
  NEXT;
}
|#
(define-inst CONSTI (1 0 #f)
  (INSN_VAL1 val1 c)
  (set! (AC vm) (SG_MAKE_INT val1)))

#|
CASE(LREF) {
  LREF_INSN(vm, c);
  NEXT;
}
|#
(define-inst LREF (1 0 #t)
  (LREF_INSN vm c))

#|
CASE(LSET) {
  INSN_VAL1(val1, c);
  SG_BOX(REFER_LOCAL(vm, val1))->value = AC(vm);
  NEXT;
}
|#
(define-inst LSET (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (-> (SG_BOX (REFER_LOCAL vm val1)) value) (AC vm)))

#|
      CASE(FREF) {
	FREF_INSN(vm, c);
	NEXT;
      }
|#
(define-inst FREF (1 0 #t)
  (FREF_INSN vm c))

#|
      CASE(FSET) {
	INSN_VAL1(val1, c);
	SG_BOX(INDEX_CLOSURE(vm, val1))->value = AC(vm);
	NEXT;
      }
|#
(define-inst FSET (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (-> (SG_BOX (INDEX_CLOSURE vm val1)) value) (AC vm)))

#|
      CASE(GREF) {
	GREF_INSN(vm);
	NEXT;
      }
|#
(define-inst GREF (0 1 #t)
  (GREF_INSN vm))

#|
      CASE(GSET) {
	SgObject var = FETCH_OPERAND(PC(vm));
	ASSERT(SG_IDENTIFIERP(var));
	Sg_InsertBinding(SG_IDENTIFIER_LIBRARY(var), SG_IDENTIFIER_NAME(var), AC(vm));
	NEXT;
      }
|#
(define-inst GSET (0 1 #t)
  (let ((var (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_IDENTIFIERP var))
    (Sg_InsertBinding (SG_IDENTIFIER_LIBRARY var)
		      (SG_IDENTIFIER_NAME var)
		      (AC vm))))
#|
      CASE(PUSH) {
	PUSH_INSN(vm);
	NEXT;
      }
|#
(define-inst PUSH (0 0 #f)
  (PUSH_INSN vm))

#|
      CASE(BOX) {
	INSN_VAL1(val1, c);
	INDEX_SET(SP(vm), val1, make_box(INDEX(SP(vm), val1)));
	NEXT;
      }
|#
(define-inst BOX (1 0 #f)
  (INSN_VAL1 val1 c)
  (INDEX_SET (SP vm) val1 (make_box (INDEX (SP vm) val1))))

#|
      CASE(UNBOX) {
	ASSERT(SG_BOXP(AC(vm)));
	AC(vm) = SG_BOX(AC(vm))->value;
	NEXT;
      }
|#
(define-inst UNBOX (0 0 #f)
  (ASSERT (SG_BOXP (AC vm)))
  (set! (AC vm) (-> (SG_BOX (AC vm)) value)))

#|
      CASE(ADD) {
	BUILTIN_TWO_ARGS(vm, Sg_Add);
	NEXT;
      }
|#
(define-inst ADD (0 0 #t)
  (BUILTIN_TWO_ARGS vm Sg_Add))

#|
      CASE(ADDI) {
	BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Add, c);
	NEXT;
      }
|#
(define-inst ADDI (1 0 #t)
  (BUILTIN_ONE_ARG_WITH_INSN_VALUE vm Sg_Add c))

#|
      CASE(SUB) {
	BUILTIN_TWO_ARGS(vm, Sg_Sub);
	NEXT;
      }
|#
(define-inst SUB (0 0 #t)
  (BUILTIN_TWO_ARGS vm Sg_Sub))

#|
      CASE(SUBI) {
	BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Sub, c);
	NEXT;
      }
|#
(define-inst SUBI (1 0 #t)
  (BUILTIN_ONE_ARG_WITH_INSN_VALUE vm Sg_Sub c))

#|
      CASE(MUL) {
	BUILTIN_TWO_ARGS(vm, Sg_Mul);
	NEXT;
      }
|#
(define-inst MUL (0 0 #t)
  (BUILTIN_TWO_ARGS vm Sg_Mul))

#|
      CASE(MULI) {
	BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Mul, c);
	NEXT;
      }
|#
(define-inst MULI (1 0 #t)
  (BUILTIN_ONE_ARG_WITH_INSN_VALUE vm Sg_Mul c))

#|
      CASE(DIV) {
	BUILTIN_TWO_ARGS(vm, Sg_Div);
	NEXT;
      }
|#
(define-inst DIV (0 0 #t)
  (BUILTIN_TWO_ARGS vm Sg_Div))

#|
      CASE(DIVI) {
	BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Div, c);
	NEXT;
      }
|#
(define-inst DIVI (1 0 #t)
  (BUILTIN_ONE_ARG_WITH_INSN_VALUE vm Sg_Div c))

#|
      CASE(NEG) {
	BUILTIN_ONE_ARG(vm, Sg_Negate);
	NEXT;
      }
|#
(define-inst NEG (0 0 #t)
  (BUILTIN_ONE_ARG vm Sg_Negate))

#|
      CASE(TEST) {
	SgObject n = FETCH_OPERAND(PC(vm));
	ASSERT(SG_INTP(n));
	if (SG_FALSEP(AC(vm))) {
	  PC(vm) += SG_INT_VALUE(n) - 1;
	}
	NEXT;
      }
|#
(define-inst TEST (0 1 #t) :label
  (let ((n (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_INTP n))
    (if (SG_FALSEP (AC vm))
	(set! (PC vm) (+ (PC vm) (- (SG_INT_VALUE n) 1))))))

#|
      CASE(JUMP) {
	SgObject n = FETCH_OPERAND(PC(vm));
	ASSERT(SG_INTP(n));
	PC(vm) += SG_INT_VALUE(n) - 1;
	NEXT;
      }
|#
(define-inst JUMP (0 1 #t) :label
  (let ((n (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_INTP n))
    (set! (PC vm) (+ (PC vm) (- (SG_INT_VALUE n) 1)))))

#|
      CASE(SHIFTJ) {
	int i;

	INSN_VAL2(val1, val2, c);
	for (i = val2; ; i--) {
	  if (i <= 0 && SG_CLOSURE(DC(vm))->mark) break;
	  DC(vm) = SG_CLOSURE(DC(vm))->prev;
	}
	ASSERT(SG_CLOSUREP(DC(vm)));
	FP(vm) = SG_CLOSURE(DC(vm))->mark;
	SP(vm) = shift_args(FP(vm), val1, SP(vm));
	NEXT;
      }
|#
(define-inst SHIFTJ (2 0 #f)
  (INSN_VAL2 val1 val2 c)
  (let ((i::int val2))
    (for () () (set! i (- i 1))
     (when (and (<= i 0)
		(-> (SG_CLOSURE (DC vm)) mark))
       (break))
     (set! (DC vm) (-> (SG_CLOSURE (DC vm)) prev)))
    (ASSERT (SG_CLOSUREP (DC vm)))
    (set! (FP vm) (-> (SG_CLOSURE (DC vm)) mark))
    (set! (SP vm) (shift_args (FP vm) val1 (SP vm)))))

#|
      CASE(MARK) {
	SG_CLOSURE(DC(vm))->mark = FP(vm);
	NEXT;
      }
|#
(define-inst MARK (0 0 #f)
  (set! (-> (SG_CLOSURE (DC vm)) mark) (FP vm)))

#|
      CASE(BNNUME) {
	BRANCH_TEST2(Sg_NumEq);
	NEXT;
      }
|#
(define-inst BNNUME (0 1 #t) :label
  (BRANCH_TEST2 Sg_NumEq))

#|
      CASE(BNLT) {
	BRANCH_TEST2(Sg_NumLt);
	NEXT;
      }
|#
(define-inst BNLT (0 1 #t) :label
  (BRANCH_TEST2 Sg_NumLt))

#|
      CASE(BNLE) {
	BRANCH_TEST2(Sg_NumLe);
	NEXT;
      }
|#
(define-inst BNLE (0 1 #t) :label
  (BRANCH_TEST2 Sg_NumLe))

#|
      CASE(BNGT) {
	BRANCH_TEST2(Sg_NumGt);
	NEXT;
      }
|#
(define-inst BNGT (0 1 #t) :label
  (BRANCH_TEST2 Sg_NumGt))

#|
      CASE(BNGE) {
	BRANCH_TEST2(Sg_NumGe);
	NEXT;
      }
|#
(define-inst BNGE (0 1 #t) :label
  (BRANCH_TEST2 Sg_NumGe))

#|
      CASE(BNEQ) {
	BRANCH_TEST2(SG_EQ);
	NEXT;
      }
|#
(define-inst BNEQ (0 1 #t) :label
  (BRANCH_TEST2 SG_EQ))

#|
      CASE(BNEQV) {
	BRANCH_TEST2(Sg_EqvP);
	NEXT;
      }
|#
(define-inst BNEQV (0 1 #t) :label
  (BRANCH_TEST2 Sg_EqvP))

#|
      CASE(BNNULL) {
	BRANCH_TEST1(SG_NULLP);
	NEXT;
      }
|#
(define-inst BNNULL (0 1 #t) :label
  (BRANCH_TEST1 SG_NULLP))

#|
      CASE(NOT) {

      }
|#
(define-inst NOT (0 0 #f)
  (if (SG_FALSEP (AC vm))
      (set! (AC vm) SG_TRUE)
      (set! (AC vm) SG_FALSE)))

#|
      CASE(NUM_EQ) {
	BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumEq);
	NEXT;
      }
|#
(define-inst NUM_EQ (0 0 #t)
  (BUILTIN_TWO_ARGS_COMPARE vm Sg_NumEq))

#|
      CASE(NUM_LT) {
	BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumLt);
	NEXT;
      }
|#
(define-inst NUM_LT (0 0 #t)
  (BUILTIN_TWO_ARGS_COMPARE vm Sg_NumLt))

#|
      CASE(NUM_LE) {
	BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumLe);
	NEXT;
      }
|#
(define-inst NUM_LE (0 0 #t)
  (BUILTIN_TWO_ARGS_COMPARE vm Sg_NumLe))

#|
      CASE(NUM_GT) {
	BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumGt);
	NEXT;
      }
|#
(define-inst NUM_GT (0 0 #t)
  (BUILTIN_TWO_ARGS_COMPARE vm Sg_NumGt))

#|
      CASE(NUM_GE) {
	BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumGe);
	NEXT;
      }
|#
(define-inst NUM_GE (0 0 #t)
  (BUILTIN_TWO_ARGS_COMPARE vm Sg_NumGe))

#|
      CASE(RECEIVE) {
	int i;
	int numValues = 0;
	INSN_VAL2(val1, val2, c);
	if (!SG_VALUESP(AC(vm))) {
	  numValues = 1;
	} else {
	  numValues = SG_VALUES(AC(vm))->size;
	}
	if (numValues < val1) {
	  Sg_Error(UC("received fewer values than expected"));
	}
	if ((val2 == 0) && (numValues > val1)) {
	  Sg_Error(UC("received more values than expected"));
	}
	if (val2 == 0) {
	  /* (receive (a b c) ...) */
	  if (val1 == 1) {
	    /* (values 'a) creates non values object */
	    PUSH(SP(vm), AC(vm));
	  } else if (val1 > 0) {
	    for (i = 0; i < val1; i++) {
	      PUSH(SP(vm), SG_VALUES_ELEMENT(AC(vm), i));
	    }
	  }
	} else if (val1 == 0) {
	  /* (receive a ...) */
	  SgObject h = SG_NIL, t = SG_NIL;
	  if (numValues == 1) {
	    /* (values 'a) creates non values object */
	    SG_APPEND1(h, t, AC(vm));
	  } else {
	    for (i = 0; i < numValues; i++) {
	      SG_APPEND1(h, t, SG_VALUES_ELEMENT(AC(vm), i));
	    }
	  }
	  PUSH(SP(vm), h);
	} else {
	  /* (receive (a b . c) ...) */
	  SgObject h = SG_NIL, t = SG_NIL;
	  for (i = 0; ; i++) {
	    if (i < val1) {
	      PUSH(SP(vm), SG_VALUES_ELEMENT(AC(vm), i));
	    } else if (i < SG_VALUES(AC(vm))->size) {
	      SG_APPEND1(h, t, SG_VALUES_ELEMENT(AC(vm), i));
	    } else {
	      PUSH(SP(vm), h);
	      break;
	    }
	  }
	}
	NEXT;
      }
|#
(define-inst RECEIVE (2 0 #t)
  (INSN_VAL2 val1 val2 c)
  (let ((i::int 0)
	(numValues::int 0))
    (if (not (SG_VALUESP (AC vm)))
	(set! numValues 1)
	(set! numValues (SG_VALUES_SIZE (AC vm))))
    (if (< numValues val1)
	(assertion-violation 'receive
			     "recieved fewer values than expected"))
    (if (and (== val2 0)
	     (> numValues val1))
	(assertion-violation 'receive
			     "recieved more values than expected"))
    (cond ((== val2 0)
	   ;; (receive (a b c) ...)
	   (cond ((== val1 1)
		  ;; (values 'a) creates non values object
		  (PUSH (SP vm) (AC vm)))
		 ((> val1 0)
		  (for (set! i 0) (< i val1) (set! i (+ i 1))
		   (PUSH (SP vm) (SG_VALUES_ELEMENT (AC vm) i))))))
	  ((== val1 0)
	   ;; (receive a ...)
	   (let ((h '())
		 (t '()))
	     (if (== numValues 1)
		 (SG_APPEND1 h t (AC vm))
		 (for (set! i 0) (< i numValues) (set! i (+ i 1))
		      (SG_APPEND1 h t (SG_VALUES_ELEMENT (AC vm) i))))
	     (PUSH (SP vm) h)))
	  (else
	   ;; (receive (a b . c) ...)
	   (let ((h '())
		 (t '()))
	     (for (set! i 0) () (set! i (+ i 1))
		  (cond ((< i val1)
			 (PUSH (SP vm) (SG_VALUES_ELEMENT (AC vm) i)))
			((< i (SG_VALUES_SIZE (AC vm)))
			 (SG_APPEND1 h t (SG_VALUES_ELEMENT (AC vm) i)))
			(else
			 (PUSH (SP vm) h)
			 (break)))))))))

#|
      CASE(CLOSURE) {
	SgObject cb = FETCH_OPERAND(PC(vm));
	if (!SG_CODE_BUILDERP(cb)) {
	  Sg_Error(UC("code-builder required, but got %S"), cb);
	}
	AC(vm) = Sg_MakeClosure(cb, SP(vm) - SG_CODE_BUILDER(cb)->freec);
	SP(vm) -= SG_CODE_BUILDER(cb)->freec;
	NEXT;
      }
|#
(define-inst CLOSURE (0 1 #f)
  (let ((cb (FETCH_OPERAND (PC vm))))
    (if (not (SG_CODE_BUILDERP cb))
	(wrong-type-of-argument-violation 'closure
					  "code-builder"
					  cb))
    (set! (AC vm) (Sg_MakeClosure cb (- (SP vm) (SG_CODE_BUILDER_FREEC cb))))
    (set! (SP vm) (- (SP vm) (SG_CODE_BUILDER_FREEC cb)))))

#|
      CASE(APPLY) {
	SgObject args = POP(SP(vm));
	if (SG_NULLP(args)) {
	  vm->callCode[0] = MERGE_INSN_VALUE1(CALL, 0);
	  PC(vm) = vm->callCode;
	} else {
	  int length, shiftLen;
	  SgObject *sp;
	  if (!SG_PAIRP(args)) {
	    Sg_AssertionViolation(SG_INTERN("apply"), SG_INTERN("bug?"), AC(vm));
	    NEXT;
	  }
	  length = Sg_Length(args);
	  shiftLen = length > 1 ? length - 1: 0;
	  sp = SP(vm) + shiftLen + 1;
	  pair_args_to_stack(SP(vm), 0, args);
	  vm->callCode[0] = MERGE_INSN_VALUE1(CALL, length);
	  PC(vm) = vm->callCode;
	  SP(vm) = sp;
	}
	NEXT;
      }
|#
(define-inst APPLY (0 0 #f)
  (let ((args (POP (SP vm))))
    (cond ((SG_NULLP args)
	   (set! (-> vm (arrayref callCode 0)) (MERGE_INSN_VALUE1 CALL 0))
	   (set! (PC vm) (-> vm callCode)))
	  (else
	   (let ((length::int 0)
		 (shiftLen::int 0)
		 (sp::SgObject* NULL))
	     (if (not (SG_PAIRP args))
		 (Sg_AssertionViolation (Sg_Intern (Sg_MakeString "apply" SG_LITERAL_STRING))
					(Sg_Intern (Sg_MakeString "bug?" SG_LITERAL_STRING))
					(AC vm)))
	     (set! length (Sg_Length args))
	     (if (> length 1)
		 (set! shiftLen (- length 1)))
	     (set! sp (+ (SP vm) shiftLen 1))
	     (pair_args_to_stack (SP vm) 0 args)
	     (set! (-> vm (arrayref callCode 0)) (MERGE_INSN_VALUE1 CALL length))
	     (set! (PC vm) (-> vm callCode))
	     (set! (SP vm) sp))))))

#|
      CASE(CALL) {
	#include "vmcall.c"
	NEXT;
      }
|#
(define-inst CALL (1 0 #t)
  (decl-code
   (.include "vmcall.c")))

#|
      CASE(LOCAL_CALL) {
	LOCAL_CALL_INSN(vm, c);
	NEXT;
      }
|#
(define-inst LOCAL_CALL (1 0 #t)
  (LOCAL_CALL_INSN vm c))

#|
      CASE(TAIL_CALL) {
	TAIL_CALL_INSN(vm, c);
	#include "vmcall.c"
	NEXT;
      }
|#
(define-inst TAIL_CALL (1 0 #t)
  (TAIL_CALL_INSN vm c)
  (decl-code
   (.include "vmcall.c")))

#|
      CASE(LOCAL_TAIL_CALL) {
	TAIL_CALL_INSN(vm, c);
	LOCAL_CALL_INSN(vm, c);
	NEXT;
      }
|#
(define-inst LOCAL_TAIL_CALL (1 0 #t)
  (TAIL_CALL_INSN vm c)
  (LOCAL_CALL_INSN vm c))

#|

      CASE(RET) {
	SgObject *sp = FP(vm);
	PC(vm) = INDEX(sp, 3);
	DC(vm) = INDEX(sp, 2);
	CL(vm) = INDEX(sp, 1);
	FP(vm) = INDEX(sp, 0);
	SP(vm) = sp - SG_FRAME_SIZE;
	NEXT;
      }
|#
(define-inst RET (0 0 #f)
  (RET_INSN)
  #;(let ((sp::SgObject* (FP vm)))
    (set! (PC vm) (INDEX sp 3))
    (set! (DC vm) (INDEX sp 2))
    (set! (CL vm) (INDEX sp 1))
    (set! (FP vm) (INDEX sp 0))
    (set! (SP vm) (- sp SG_FRAME_SIZE))))

#|
      CASE(FRAME) {
	/* Sould it be word? */
	SgObject n = FETCH_OPERAND(PC(vm));
	int skipSize;
	ASSERT(SG_INTP(n));
	skipSize = SG_INT_VALUE(n);
	make_call_frame(vm, PC(vm) + skipSize - 1);
	NEXT;
      }
|#
(define-inst FRAME (0 1 #f) :label
  (let ((n (FETCH_OPERAND (PC vm)))
	(skipSize::int 0))
    (ASSERT (SG_INTP n))
    (set! skipSize (SG_INT_VALUE n))
    (make_call_frame vm (+ (PC vm) (- skipSize 1)))))

#|
      CASE(LET_FRAME) {
	/* TODO expand stack */
	INSN_VAL1(val1, c);
	PUSH(SP(vm), DC(vm));
	PUSH(SP(vm), FP(vm));
	NEXT;
      }
|#
(define-inst LET_FRAME (1 0 #t)
  (INSN_VAL1 val1 c)
  (CHECK_STACK val1 vm)
  (PUSH (SP vm) (DC vm))
  (PUSH (SP vm) (FP vm)))

#|
      CASE(POP_LET_FRAME) {
	INSN_VAL1(val1, c);
	SP(vm) = discard_let_frame(vm, val1);
	NEXT;
      }
|#
(define-inst POP_LET_FRAME (1 0 #f)
  (INSN_VAL1 val1 c)
  (set! (SP vm) (discard_let_frame vm val1)))

#|
      CASE(DISPLAY) {
	SgObject new_c;
	INSN_VAL1(val1, c);
	new_c = make_display(val1, SP(vm));
	SG_CLOSURE(new_c)->prev = DC(vm);
	DC(vm) = new_c;
	SP(vm) = SP(vm) - val1;
	NEXT;
      }
|#
(define-inst DISPLAY (1 0 #f)
  (let ((new_c SG_UNDEF))
    (INSN_VAL1 val1 c)
    (set! new_c (make_display val1 (SP vm)))
    (set! (-> (SG_CLOSURE new_c) prev) (DC vm))
    (set! (DC vm) new_c)
    (set! (SP vm) (- (SP vm) val1))))

#|
      CASE(ENTER) {
	INSN_VAL1(val1, c);
	FP(vm) = SP(vm) - val1;
	NEXT;
      }
|#
(define-inst ENTER (1 0 #f)
  (INSN_VAL1 val1 c)
  (set! (FP vm) (- (SP vm) val1)))

#|
      CASE(LEAVE) {
	SgObject *sp = FP(vm);
	FP(vm) = (SgObject*)INDEX(sp, 0);
	DC(vm) = INDEX(sp, 1);
	SP(vm) = sp - SG_LET_FRAME_SIZE;
	NEXT;
      }
|#
(define-inst LEAVE (0 0 #f)
  (let ((sp::SgObject* (FP vm)))
    (set! (FP vm) (cast SgObject* (INDEX sp 0)))
    (set! (DC vm) (INDEX sp 1))
    (set! (SP vm) (- sp SG_LET_FRAME_SIZE))))

#|
      CASE(DEFINE) {
	SgObject var = FETCH_OPERAND(PC(vm));
	ASSERT(SG_IDENTIFIERP(var));
	Sg_InsertBinding(SG_IDENTIFIER(var)->library, SG_IDENTIFIER(var)->name, AC(vm));
	AC(vm) = SG_UNDEF;
	NEXT;
      }
|#
(define-inst DEFINE (1 1 #t)
  (let ((var (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_IDENTIFIERP var))
    (Sg_InsertBinding (SG_IDENTIFIER_LIBRARY var)
		      (SG_IDENTIFIER_NAME var)
		      (AC vm))
    (set! (AC vm) SG_UNDEF)))

#|
      CASE(CAR) {
	if (!SG_PAIRP(AC(vm))) {
	  Sg_Error(UC("car: pair required, but got %S"), AC(vm));
	}
	BUILTIN_ONE_ARG(vm, SG_CAR);
	NEXT;
      }
|#
(define-inst CAR (0 0 #t)
  (if (not (SG_PAIRP (AC vm)))
      (wrong-type-of-argument-violation 'car "pair" (AC vm)))
  (BUILTIN_ONE_ARG vm SG_CAR))

#|
      CASE(CDR) {
	if (!SG_PAIRP(AC(vm))) {
	  Sg_Error(UC("cdr: pair required, but got %S"), AC(vm));
	}
	BUILTIN_ONE_ARG(vm, SG_CDR);
	NEXT;
      }
|#
(define-inst CDR (0 0 #t)
  (if (not (SG_PAIRP (AC vm)))
      (wrong-type-of-argument-violation 'cdr "pair" (AC vm)))
  (BUILTIN_ONE_ARG vm SG_CDR))

#|
      CASE(CONS) {
	BUILTIN_TWO_ARGS(vm, Sg_Cons);
	NEXT;
      }
|#
(define-inst CONS (0 0 #t)
  (BUILTIN_TWO_ARGS vm Sg_Cons))

#|
      CASE(LIST) {
	int i;
	SgObject ret = SG_NIL;
	INSN_VAL1(val1, c);
	if (val1 > 0) {
	  ret = Sg_Cons(AC(vm), ret);
	  for (i = 0; i < val1 - 1; i++) {
	    ret = Sg_Cons(INDEX(SP(vm), i), ret);
	  }
	  SP(vm) -= val1 - 1;
	}
	AC(vm) = ret;
	NEXT;
      }
|#
(define-inst LIST (1 0 #t)
  (INSN_VAL1 val1 c)
  (let ((i::int 0)
	(n::int (- val1 1))
	(ret '()))
    (when (> val1 0)
      (set! ret (Sg_Cons (AC vm) ret))
      (for (set! i 0) (< i n) (set! i (+ i 1))
	   (set! ret (Sg_Cons (INDEX (SP vm) i) ret)))
      (set! (SP vm) (- (SP vm) n)))
    (set! (AC vm) ret)))

#|
      CASE(VALUES) {
	SgObject v;
	INSN_VAL1(val1, c);
	v = Sg_MakeValues(val1);
	if (val1 > 0) {
	  int i, n;
	  n = val1 - 1;
	  SG_VALUES_ELEMENT(v, n) = AC(vm);
	  for (i = 0; i < n; i++) {
	    SG_VALUES_ELEMENT(v, n - i - 1) = INDEX(SP(vm), i);
	  }
	  SP(vm) -= n;
	}
	AC(vm) = v;
	NEXT;
      }
|#
(define-inst VALUES (1 0 #t)
  (let ((v (AC vm)))
    (INSN_VAL1 val1 c)
    (when (> val1 1)
      (set! v (Sg_MakeValues val1))
      (let ((i::int 0)
	    (n::int (- val1 1)))
	(set! (SG_VALUES_ELEMENT v n) (AC vm))
	(for (set! i 0) (< i n) (set! i (+ i 1))
	     (set! (SG_VALUES_ELEMENT v (- n i 1))
		   (INDEX (SP vm) i)))
	(set! (SP vm) (- (SP vm) n))))
    (set! (AC vm) v)))

#|
      CASE(EQ) {
	BUILTIN_TWO_ARGS_COMPARE(vm, SG_EQ);
	NEXT;
      }
|#
(define-inst EQ (0 0 #t)
  (BUILTIN_TWO_ARGS_COMPARE vm SG_EQ))

#|
      CASE(EQV) {
	BUILTIN_TWO_ARGS_COMPARE(vm, Sg_EqvP);
	NEXT;
      }
|#
(define-inst EQV (0 0 #t)
  (BUILTIN_TWO_ARGS_COMPARE vm Sg_EqvP))

#|
      CASE(NULLP) {
	AC(vm) = SG_MAKE_BOOL(SG_NULLP(AC(vm)));
	NEXT;
      }
|#
(define-inst NULLP (0 0 #t)
  (set! (AC vm) (SG_MAKE_BOOL (SG_NULLP (AC vm)))))

(define-inst PAIRP (0 0 #t)
  (set! (AC vm) (SG_MAKE_BOOL (SG_PAIRP (AC vm)))))

(define-inst SYMBOLP (0 0 #t)
  (set! (AC vm) (SG_MAKE_BOOL (SG_SYMBOLP (AC vm)))))

#|
      CASE(VECTOR) {
	SgObject v;
	INSN_VAL1(val1, c);
	v = Sg_MakeVector(val1, SG_UNDEF);
	if (val1 > 0) {
	  int i, n;
	  n = val1 - 1;
	  SG_VECTOR_ELEMENT(v, n) = AC(vm);
	  for (i = 0; i < n; i++) {
	    SG_VECTOR_ELEMENT(v, n - i - 1) = INDEX(SP(vm), i);
	  }
	  SP(vm) -= n;
	}
	AC(vm) = v;
	NEXT;
      }
|#
(define-inst VECTOR (1 0 #t)
  (let ((v SG_UNDEF))
    (INSN_VAL1 val1 c)
    (set! v (Sg_MakeVector val1 SG_UNDEF))
    (if (> val1 0)
	(let ((i::int 0)
	      (n::int (- val1 1)))
	  (set! (SG_VECTOR_ELEMENT v n) (AC vm))
	  (for (set! i 0) (< i n) (set! i (+ i 1))
	       (set! (SG_VECTOR_ELEMENT v (- n i 1))
		     (INDEX (SP vm) i)))
	  (set! (SP vm) (- (SP vm) n))))
    (set! (AC vm) v)))

#|
      CASE(VECTORP) {
	AC(vm) = SG_MAKE_BOOL(SG_VECTORP(AC(vm)));
	NEXT;
      }
|#
(define-inst VECTORP (0 0 #t)
  (set! (AC vm) (SG_MAKE_BOOL (SG_VECTORP (AC vm)))))

#|
      CASE(VEC_LEN) {
	if (!SG_VECTORP(AC(vm))) {
	  Sg_Error(UC("vector-length: vector required, but got %S"), AC(vm));
	}
	AC(vm) = SG_MAKE_INT(SG_VECTOR_SIZE(AC(vm)));
	NEXT;
      }
|#
(define-inst VEC_LEN (0 0 #t)
  (if (not (SG_VECTORP (AC vm)))
      (wrong-type-of-argument-violation 'vector-length
					"vector" (AC vm)))
  (set! (AC vm) (SG_MAKE_INT (SG_VECTOR_SIZE (AC vm)))))

#|
      CASE(VEC_REF) {
	if (!SG_VECTORP(INDEX(SP(vm), 0))) {
	  Sg_Error(UC("vector-ref: vector required, but got %S"), INDEX(SP(vm), 0));
	}
	if (!SG_INTP(AC(vm))) {
	  Sg_Error(UC("vector-ref: fixnum required, but got %S"), AC(vm));
	}
	AC(vm) = SG_VECTOR_ELEMENT(INDEX(SP(vm), 0), SG_INT_VALUE(AC(vm)));
	SP(vm) -= 1;
	NEXT;
      }
|#
(define-inst VEC_REF (0 0 #t)
  (if (not (SG_VECTORP (INDEX (SP vm) 0)))
      (wrong-type-of-argument-violation 'vector-ref "vector" (INDEX (SP vm) 0)))
  (if (not (SG_INTP (AC vm)))
      (wrong-type-of-argument-violation 'vector-ref "fixnum" (AC vm)))
  (set! (AC vm) (SG_VECTOR_ELEMENT (INDEX (SP vm) 0) (SG_INT_VALUE (AC vm))))
  (set! (SP vm) (- (SP vm) 1)))

#|
      CASE(VEC_SET) {
	if (!SG_VECTORP(INDEX(SP(vm), 1))) {
	  Sg_Error(UC("vector-set!: vector required, but got %S"), INDEX(SP(vm), 1));
	}
	if (!SG_INTP(INDEX(SP(vm), 0))) {
	  Sg_Error(UC("vector-set!: fixnum required, but got %S"), INDEX(SP(vm), 0));
	}
	SG_VECTOR_ELEMENT(INDEX(SP(vm), 1), SG_INT_VALUE(INDEX(SP(vm), 0))) = AC(vm);
	AC(vm) = SG_UNDEF;
	SP(vm) -= 2;
	NEXT;
      }
|#
(define-inst VEC_SET (0 0 #t)
  (if (not (SG_VECTORP (INDEX (SP vm) 1)))
      (wrong-type-of-argument-violation 'vector-set! "vector" (INDEX (SP vm) 1)))
  (if (not (SG_INTP (INDEX (SP vm) 0)))
      (wrong-type-of-argument-violation 'vector-set! "fixnum" (INDEX (SP vm) 0)))
  (set! (SG_VECTOR_ELEMENT (INDEX (SP vm) 1)
			   (SG_INT_VALUE (INDEX (SP vm) 0)))
	(AC vm))
  (set! (AC vm) SG_UNDEF)
  (set! (SP vm) (- (SP vm) 2)))

;; combined instructions
(define-inst LREF_PUSH (1 0 #t) :combined
  (LREF PUSH))

(define-inst FREF_PUSH (1 0 #t) :combined
  (FREF PUSH))

(define-inst GREF_PUSH (0 1 #t) :combined
  (GREF PUSH))

(define-inst CONST_PUSH (0 1 #f) :combined
  (CONST PUSH))

(define-inst CONSTI_PUSH (1 0 #f) :combined
  (CONSTI PUSH))

(define-inst GREF_CALL (1 1 #t) :combined
  (GREF CALL))

(define-inst GREF_TAIL_CALL (1 1 #t) :combined
  (GREF TAIL_CALL))

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
