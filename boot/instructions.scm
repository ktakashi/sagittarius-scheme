;; -*- scheme -*-

;; definitions of vm instruction.
;; NB: c, val1, val2 are defined in vm.c
;;     most of macros are also defined in vm.c
#!compatible

(define-cgen-stmt assertion-violation
    ((_ who msg)
     (dispatch
      `(begin
	 (Sg_AssertionViolation ,who (Sg_MakeString ,msg SG_LITERAL_STRING) '())
	 (return SG_UNDEF))))
    ((_ who msg irritants)
     (dispatch
      `(begin
	 (Sg_AssertionViolation ,who (Sg_MakeString ,msg SG_LITERAL_STRING) ,irritants)
	 (return SG_UNDEF)))))
      

(define-cgen-stmt wrong-type-of-argument-violation
    ((_ who msg got)
     (dispatch
      `(begin
	 (Sg_WrongTypeOfArgumentViolation ,who (Sg_MakeString ,msg SG_LITERAL_STRING) ,got '())
	 (return SG_UNDEF))))
    ((_ who msg got irritants)
     (dispatch
      `(begin
	 (Sg_WrongTypeOfArgumentViolation ,who (Sg_MakeString ,msg SG_LITERAL_STRING) ,got ,irritants)
	 (return SG_UNDEF)))))

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

(define-inst NOP (0 0 #f) (:value 0)
  ;; do nothing
  )

(define-inst HALT (0 0 #f) :return)

(define-inst UNDEF (0 0 #f)
  (set! (AC vm) SG_UNDEF))

(define-inst CONST (0 1 #f)
  (set! (AC vm) (FETCH_OPERAND (PC vm)))
  #;(CONST_INSN vm))

(define-inst CONSTI (1 0 #f)
  (INSN_VAL1 val1 c)
  (set! (AC vm) (SG_MAKE_INT val1)))

(define-inst LREF (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (AC vm) (REFER_LOCAL vm val1))
  #;(LREF_INSN vm c))

(define-inst LSET (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (-> (SG_BOX (REFER_LOCAL vm val1)) value) (AC vm)))

(define-inst FREF (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (AC vm) (INDEX_CLOSURE vm val1))
  #;(FREF_INSN vm c))

(define-inst FSET (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (-> (SG_BOX (INDEX_CLOSURE vm val1)) value) (AC vm)))

(define-inst GREF (0 1 #t)
  (GREF_INSN vm))

(define-inst GSET (0 1 #t)
  (let ((var (FETCH_OPERAND (PC vm))))
    (ASSERT (or (SG_IDENTIFIERP var)
		(SG_GLOCP var)))
    (if (SG_GLOCP var)
	(SG_GLOC_SET (SG_GLOC var) (AC vm))
	(let ((oldval (Sg_FindBinding (SG_IDENTIFIER_LIBRARY var)
				      (SG_IDENTIFIER_NAME var)
				      SG_UNBOUND)))
	  (when (SG_UNBOUNDP oldval)
	    (assertion-violation 'set!
				 "unbound variable"
				 (SG_IDENTIFIER_NAME var)))
	  ;;(SG_GLOC_SET (SG_GLOC oldval) (AC vm))
	  ;;(set! (pointer (- (PC vm) 1)) (SG_WORD oldval))
	  (let ((g (Sg_MakeBinding (SG_IDENTIFIER_LIBRARY var)
				   (SG_IDENTIFIER_NAME var)
				   (AC vm)
				   0)))
	    (set! (pointer (- (PC vm) 1)) (SG_WORD g)))))
    (set! (AC vm) SG_UNDEF)))

(define-inst PUSH (0 0 #f)
  (PUSH (SP vm) (AC vm))
  #;(PUSH_INSN vm))

(define-inst BOX (1 0 #f)
  (INSN_VAL1 val1 c)
  (INDEX_SET (SP vm) val1 (make_box (INDEX (SP vm) val1))))

(define-inst UNBOX (0 0 #f)
  (ASSERT (SG_BOXP (AC vm)))
  (set! (AC vm) (-> (SG_BOX (AC vm)) value)))

(define-inst ADD (0 0 #t)
  (cond ((and (SG_INTP (AC vm))
	      (SG_INTP (INDEX (SP vm) 0)))
	 (let ((n::long (+ (SG_INT_VALUE (INDEX (SP vm) 0))
			   (SG_INT_VALUE (AC vm)))))
	   (post-- (SP vm))
	   (if (and (<= SG_INT_MIN n)
		    (>= SG_INT_MAX n))
	       (set! (AC vm) (SG_MAKE_INT n))
	       (set! (AC vm) (Sg_MakeBignumFromSI n)))))
	(else
	 (BUILTIN_TWO_ARGS vm Sg_Add))))

(define-inst ADDI (1 0 #t)
  (INSN_VAL1 val1 c)
  (cond ((SG_INTP (AC vm))
	 (let ((n::long (+ val1 (SG_INT_VALUE (AC vm)))))
	   (if (and (<= SG_INT_MIN n)
		    (>= SG_INT_MAX n))
	       (set! (AC vm) (SG_MAKE_INT n))
	       (set! (AC vm) (Sg_MakeBignumFromSI n)))))
	(else
	 (BUILTIN_ONE_ARG_WITH_INSN_VALUE vm Sg_Add c))))

(define-inst SUB (0 0 #t)
  (cond ((and (SG_INTP (AC vm))
	      (SG_INTP (INDEX (SP vm) 0)))
	 (let ((n::long (- (SG_INT_VALUE (INDEX (SP vm) 0))
			   (SG_INT_VALUE (AC vm)))))
	   (post-- (SP vm))
	   (if (and (<= SG_INT_MIN n)
		    (>= SG_INT_MAX n))
	       (set! (AC vm) (SG_MAKE_INT n))
	       (set! (AC vm) (Sg_MakeBignumFromSI n)))))
	(else
	 (BUILTIN_TWO_ARGS vm Sg_Sub))))

(define-inst SUBI (1 0 #t)
  (INSN_VAL1 val1 c)
  (cond ((SG_INTP (AC vm))
	 (let ((n::long (- val1 (SG_INT_VALUE (AC vm)))))
	   (if (and (<= SG_INT_MIN n)
		    (>= SG_INT_MAX n))
	       (set! (AC vm) (SG_MAKE_INT n))
	       (set! (AC vm) (Sg_MakeBignumFromSI n)))))
	(else
	 (BUILTIN_ONE_ARG_WITH_INSN_VALUE vm Sg_Sub c))))

(define-inst MUL (0 0 #t)
  (BUILTIN_TWO_ARGS vm Sg_Mul))

(define-inst MULI (1 0 #t)
  (INSN_VAL1 val1 c)
  (BUILTIN_ONE_ARG_WITH_INSN_VALUE vm Sg_Mul c))

;;
;; R6RS requires &assertion exception when divisor was 0.
;; however on Sagittarius scheme we try to calculate if arguments are known,
;; such as (/ 0 0) case. In this case and if #!r6rs was set, it'll cause
;; uncatchable exception. If I can find a nice way to handle compile time
;; exception, this might be fixed.
(define-inst DIV (0 0 #t)
  (let ((exact::int (and (Sg_ExactP (INDEX (SP vm) 0))
			 (Sg_ExactP (AC vm)))))
  (when (and exact
	     (SG_VM_IS_SET_FLAG vm SG_R6RS_MODE)
	     (Sg_ZeroP (AC vm)))
    (assertion-violation '/
			 "undefined for 0"
			 (SG_LIST2 (INDEX (SP vm) 0) (AC vm))))
  (BUILTIN_TWO_ARGS vm Sg_Div)))

(define-inst DIVI (1 0 #t)
  (INSN_VAL1 val1 c)
  (BUILTIN_ONE_ARG_WITH_INSN_VALUE vm Sg_Div c))

(define-inst NEG (0 0 #t)
  (BUILTIN_ONE_ARG vm Sg_Negate))

(define-inst TEST (0 1 #t) :label
  (cond ((SG_FALSEP (AC vm))
	 (let ((n (FETCH_OPERAND (PC vm))))
	   (ASSERT (SG_INTP n))
	   (set! (PC vm) (+ (PC vm) (- (SG_INT_VALUE n) 1)))))
	(else
	 (post++ (PC vm)))))

(define-inst JUMP (0 1 #t) :label
  (let ((n (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_INTP n))
    (set! (PC vm) (+ (PC vm) (- (SG_INT_VALUE n) 1)))))

(define-inst SHIFTJ (2 0 #f)
  (INSN_VAL2 val1 val2 c)
  #;(shiftj_process vm val2 val1)
  (set! (SP vm) (shift_args (+ (FP vm) val2) val1 (SP vm))))

(define-cgen-stmt branch-number-test
  ((_ op func)
   (dispatch
    `(let ((n (FETCH_OPERAND (PC vm)))
	   (s (INDEX (SP vm) 0))
	   (t::int FALSE))
       (cond ((and (SG_INTP (AC vm))
		   (SG_INTP s))
	      (set! t (,op (cast intptr_t s) (cast intptr_t (AC vm)))))
	     (else
	      (set! t (,func s (AC vm)))))
       (cond (t
	      (set! (AC vm) SG_TRUE))
	     (else
	      (set! (AC vm) SG_FALSE)
	      (set! (PC vm) (+ (PC vm) (- (SG_INT_VALUE n) 1)))))
       (post-- (SP vm))))))

(define-inst BNNUME (0 1 #t) :label
  ;;(BRANCH_TEST2 Sg_NumEq))
  (branch-number-test == Sg_NumEq))

(define-inst BNLT (0 1 #t) :label
  ;;(BRANCH_TEST2 Sg_NumLt))
  (branch-number-test < Sg_NumLt))

(define-inst BNLE (0 1 #t) :label
  ;;(BRANCH_TEST2 Sg_NumLe))
  (branch-number-test <= Sg_NumLe))

(define-inst BNGT (0 1 #t) :label
  ;;(BRANCH_TEST2 Sg_NumGt))
  (branch-number-test > Sg_NumGt))

(define-inst BNGE (0 1 #t) :label
  ;;(BRANCH_TEST2 Sg_NumGe))
  (branch-number-test >= Sg_NumGe))

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

(define-cgen-stmt builtin-number-compare
  ((_ op func)
   (dispatch
    `(let ((s (INDEX (SP vm) 0)))
       (cond ((and (SG_INTP (AC vm))
		   (SG_INTP s))
	      (set! (AC vm) (SG_MAKE_BOOL (,op (cast intptr_t s)
					       (cast intptr_t (AC vm))))))
	     (else
	      (set! (AC vm) (SG_MAKE_BOOL (,func s (AC vm))))))
       (post-- (SP vm))))))

(define-inst NUM_EQ (0 0 #t)
  ;;(BUILTIN_TWO_ARGS_COMPARE vm Sg_NumEq))
  (builtin-number-compare == Sg_NumEq))

(define-inst NUM_LT (0 0 #t)
  ;;(BUILTIN_TWO_ARGS_COMPARE vm Sg_NumLt))
  (builtin-number-compare < Sg_NumLt))

(define-inst NUM_LE (0 0 #t)
  ;;(BUILTIN_TWO_ARGS_COMPARE vm Sg_NumLe))
  (builtin-number-compare <= Sg_NumLe))

(define-inst NUM_GT (0 0 #t)
  ;;(BUILTIN_TWO_ARGS_COMPARE vm Sg_NumGt))
  (builtin-number-compare > Sg_NumGt))

(define-inst NUM_GE (0 0 #t)
  ;;(BUILTIN_TWO_ARGS_COMPARE vm Sg_NumGe))
  (builtin-number-compare >= Sg_NumGe))

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
			     "recieved fewer values than expected"
			     (AC vm)))
    (if (and (== val2 0)
	     (> numValues val1))
	(assertion-violation 'receive
			     "recieved more values than expected"
			     (AC vm)))
    (cond ((== val2 0)
	   ;; (receive (a b c) ...)
	   (cond ((== val1 1)
		  ;; (values 'a) creates non values object
		  (PUSH (SP vm) (AC vm)))
		 ((> val1 0)
		  (for (set! i 0) (< i val1) (post++ i)
		   (PUSH (SP vm) (SG_VALUES_ELEMENT (AC vm) i))))))
	  ((== val1 0)
	   ;; (receive a ...)
	   (let ((h '())
		 (t '()))
	     (if (== numValues 1)
		 (SG_APPEND1 h t (AC vm))
		 (for (set! i 0) (< i numValues) (post++ i)
		      (SG_APPEND1 h t (SG_VALUES_ELEMENT (AC vm) i))))
	     (PUSH (SP vm) h)))
	  (else
	   ;; (receive (a b . c) ...)
	   (let ((h '())
		 (t '()))
	     (for (set! i 0) () (post++ i)
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

;; apply stack frame
;; sp >|      |
;;     | argN |
;;     |   :  |
;;     | arg0 |
;; fp >| proc | ac = rest
;; this instruction convert stack layout like this
;; sp >|      |
;;     | rest |
;;     | argN |
;;     |   :  |
;; fp >| arg0 | ac = proc
;; instruction:
;;   apply argc tail?
;; if tail? is 1, then we need to shift args. like tail_call
(define-inst APPLY (2 0 #f)
  (INSN_VAL2 val1 val2 c)
  (let ((rargc::int (Sg_Length (AC vm)))
	(nargc::int (- val1 2))
	(proc (INDEX (SP vm) nargc))
	(fp::SgObject* (- (SP vm) (- val1 1))))
    (when (< rargc 0)
      (assertion-violation 'apply "improper list not allowed" (AC vm)))
    (shift_args fp nargc (SP vm))
    (cond ((== rargc 0)
	   (set! (SP vm) (- (SP vm) 1))
	   (when val2
	     (set! (SP vm) (shift_args (FP vm) nargc (SP vm))))
	   (set! (-> vm (arrayref callCode 0)) (MERGE_INSN_VALUE1 CALL nargc))
	   (set! (PC vm) (-> vm callCode)))
	  (else
	   (INDEX_SET (SP vm) 0 (SG_CAR (AC vm)))
	   (dolist (v (SG_CDR (AC vm)))
	     (PUSH (SP vm) v))
	   (when val2
	     (set! (SP vm) (shift_args (FP vm) (+ nargc rargc) (SP vm))))
	   (set! (-> vm (arrayref callCode 0))
		 (MERGE_INSN_VALUE1 CALL (+ nargc rargc)))
	   (set! (PC vm) (-> vm callCode))))
    (set! (AC vm) proc))
    
  #;(let ((args (POP (SP vm))))
    (cond ((SG_NULLP args)
	   (set! (-> vm (arrayref callCode 0)) (MERGE_INSN_VALUE1 CALL 0))
	   (set! (PC vm) (-> vm callCode)))
	  (else
	   (let ((length::int 0)
		 (shiftLen::int 0)
		 (sp::SgObject* NULL))
	     (if (not (SG_PAIRP args))
		 (assertion-violation (Sg_Intern (Sg_MakeString "apply" SG_LITERAL_STRING))
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
  (CHECK_STACK (SG_CLOSURE_MAX_STACK (AC vm)) vm)
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
  (CHECK_STACK (SG_CLOSURE_MAX_STACK (AC vm)) vm)
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
  (let ((n (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_INTP n))
    (PUSH_CONT vm (+ (PC vm) (- (SG_INT_VALUE n) 1)))))


#|
      CASE(ENTER) {
	INSN_VAL1(val1, c);
	FP(vm) = SP(vm) - val1;
	NEXT;
      }
|#
(define-inst ENTER (1 0 #f)
  (INSN_VAL1 val1 c)
  (set! (FP vm) (- (SP vm) val1))
  #;(set! (-> vm fpOffset) (CALC_OFFSET vm val1)))

(define-inst LEAVE (1 0 #f)
  (INSN_VAL1 val1 c)
  #;(leave_process vm val1)
  (set! (SP vm) (- (SP vm) val1)))


(define-inst DEFINE (1 1 #t)
  (INSN_VAL1 val1 c)
  (let ((var (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_IDENTIFIERP var))
    (Sg_MakeBinding (SG_IDENTIFIER_LIBRARY var)
		    (SG_IDENTIFIER_NAME var)
		    (AC vm)
		    val1)
    (set! (AC vm) SG_UNDEF)))

;; This instruction is just mark for compiled cache.
;; So it doesn't do any thing.
(define-inst LIBRARY (0 1 #f)
  ;; discards library and move to next.
  (let ((lib (Sg_FindLibrary (FETCH_OPERAND (PC vm)) FALSE)))
    (set! (-> vm currentLibrary) (cast SgLibrary* lib))))

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
      (for (set! i 0) (< i n) (post++ i)
	   (set! ret (Sg_Cons (INDEX (SP vm) i) ret)))
      (set! (SP vm) (- (SP vm) n)))
    (set! (AC vm) ret)))

(define-inst APPEND (1 0 #t)
  (INSN_VAL1 val1 c)
  (let ((nargs::int (- val1 1))
	(i::int 0)
	(ret '()))
    (when (> nargs 0)
      (set! ret (AC vm))
      (for () (< i nargs) (post++ i)
	   (when (< (Sg_Length (INDEX (SP vm) i)) 0)
	     (wrong-type-of-argument-violation 'append
					       "list" (INDEX (SP vm) i)))
	   (set! ret (Sg_Append2 (INDEX (SP vm) i) ret)))
      (set! (SP vm) (- (SP vm) nargs)))
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
  (INSN_VAL1 val1 c)
  (if (== val1 0)
      (set! (AC vm) (Sg_MakeValues 0))
      (let ((v (AC vm)))
	(when (> val1 1)
	  (set! v (Sg_MakeValues val1))
	  (let ((i::int 0)
		(n::int (- val1 1)))
	    (set! (SG_VALUES_ELEMENT v n) (AC vm))
	    (for (set! i 0) (< i n) (post++ i)
		 (set! (SG_VALUES_ELEMENT v (- n i 1))
		       (INDEX (SP vm) i)))
	    (set! (SP vm) (- (SP vm) n))))
	(set! (AC vm) v))))

(define-inst EQ (0 0 #t)
  (BUILTIN_TWO_ARGS_COMPARE vm SG_EQ))

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
	  (for (set! i 0) (< i n) (post++ i)
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

(define-cgen-stmt check-vector-range
  ((_ name v index)
   (dispatch 
    `(when (or (>= ,index (SG_VECTOR_SIZE ,v))
	       (< ,index 0))
       (assertion-violation ',name "index out of range" (SG_MAKE_INT ,index))))))

(define-inst VEC_REF (0 0 #t)
  (if (not (SG_VECTORP (INDEX (SP vm) 0)))
      (wrong-type-of-argument-violation 'vector-ref "vector" (INDEX (SP vm) 0)))
  (if (not (SG_INTP (AC vm)))
      (wrong-type-of-argument-violation 'vector-ref "fixnum" (AC vm)))
  (check-vector-range vector-ref (INDEX (SP vm) 0) (SG_INT_VALUE (AC vm)))
  (set! (AC vm) (SG_VECTOR_ELEMENT (INDEX (SP vm) 0) (SG_INT_VALUE (AC vm))))
  (post-- (SP vm))
  #;(set! (SP vm) (- (SP vm) 1)))

(define-inst VEC_SET (0 0 #t)
  (if (not (SG_VECTORP (INDEX (SP vm) 1)))
      (wrong-type-of-argument-violation 'vector-set! "vector" (INDEX (SP vm) 1)))
  (if (not (SG_INTP (INDEX (SP vm) 0)))
      (wrong-type-of-argument-violation 'vector-set! "fixnum" (INDEX (SP vm) 0)))
  (check-vector-range vector-set! (INDEX (SP vm) 1) (SG_INT_VALUE (INDEX (SP vm) 0)))
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

(define-inst SET_CAR (0 0 #t)
  (if (not (SG_PAIRP (INDEX (SP vm) 0)))
      (wrong-type-of-argument-violation 'set-car! "pair" (INDEX (SP vm) 0)))
  (SG_SET_CAR (INDEX (SP vm) 0) (AC vm))
  (post-- (SP vm))
  (set! (AC vm) SG_UNDEF))

(define-inst SET_CDR (0 0 #t)
  (if (not (SG_PAIRP (INDEX (SP vm) 0)))
      (wrong-type-of-argument-violation 'set-cdr! "pair" (INDEX (SP vm) 0)))
  (SG_SET_CDR (INDEX (SP vm) 0) (AC vm))
  (post-- (SP vm))
  (set! (AC vm) SG_UNDEF))

(define-inst CAAR (0 0 #t) :combined
  (CAR CAR))

(define-inst CADR (0 0 #t) :combined
  (CDR CAR))

(define-inst CDAR (0 0 #t) :combined
  (CAR CDR))

(define-inst CDDR (0 0 #t) :combined
  (CDR CDR))

(define-inst CAR_PUSH (0 0 #t) :combined
  (CAR PUSH))

(define-inst CDR_PUSH (0 0 #t) :combined
  (CDR PUSH))

(define-inst CONS_PUSH (0 0 #t) :combined
  (CONS PUSH))

(define-inst LREF_CAR (1 0 #t) :combined
  (LREF CAR))

(define-inst LREF_CDR (1 0 #t) :combined
  (LREF CDR))

(define-inst FREF_CAR (1 0 #t) :combined
  (FREF CAR))

(define-inst FREF_CDR (1 0 #t) :combined
  (FREF CDR))

(define-inst GREF_CAR (0 1 #t) :combined
  (GREF CAR))

(define-inst GREF_CDR (0 1 #t) :combined
  (GREF CDR))

(define-inst LREF_CAR_PUSH (1 0 #t) :combined
  (LREF CAR PUSH))

(define-inst LREF_CDR_PUSH (1 0 #t) :combined
  (LREF CDR PUSH))

(define-inst FREF_CAR_PUSH (1 0 #t) :combined
  (FREF CAR PUSH))

(define-inst FREF_CDR_PUSH (1 0 #t) :combined
  (FREF CDR PUSH))

(define-inst GREF_CAR_PUSH (0 1 #t) :combined
  (GREF CAR PUSH))

(define-inst GREF_CDR_PUSH (0 1 #t) :combined
  (GREF CDR PUSH))

(define-inst CONST_RET (0 1 #f) :combined
  (CONST RET))

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
