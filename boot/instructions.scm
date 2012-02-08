;; -*- scheme -*-

;; definitions of vm instruction.
;; NB: c, val1, val2 are defined in vm.c
;;     most of macros are also defined in vm.c
#!compatible

(define-cise-stmt assertion-violation
  ((_ who msg)
   `(begin
      (Sg_AssertionViolation (SG_INTERN ,who) (SG_MAKE_STRING ,msg) '())))
  ((_ who msg irritants)
   `(begin
      (Sg_AssertionViolation (SG_INTERN ,who) (SG_MAKE_STRING ,msg) ,irritants)
      )))

(define-cise-stmt wrong-type-of-argument-violation
  ((_ who msg got)
   `(begin
      (Sg_WrongTypeOfArgumentViolation (SG_INTERN ,who)
				       (SG_MAKE_STRING ,msg) ,got '())))
  ((_ who msg got irritants)
   `(begin
      (Sg_WrongTypeOfArgumentViolation (SG_INTERN ,who)
				       (SG_MAKE_STRING ,msg) ,got ,irritants))))

(define-inst NOP (0 0 #f) )

(define-inst HALT (0 0 #f) :return)

(define-inst UNDEF (0 0 #f)
  (set! (AC vm) SG_UNDEF))

(define-inst CONST (0 1 #f)
  (set! (AC vm) (FETCH_OPERAND (PC vm))))

(define-inst CONSTI (1 0 #f)
  (INSN_VAL1 val1 c)
  (set! (AC vm) (SG_MAKE_INT val1)))

(define-inst LREF (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (AC vm) (REFER_LOCAL vm val1)))

(define-inst LSET (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (-> (SG_BOX (REFER_LOCAL vm val1)) value) (AC vm)
	(AC vm) SG_UNDEF))

(define-inst FREF (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (AC vm) (INDEX_CLOSURE vm val1)))

(define-inst FSET (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (-> (SG_BOX (INDEX_CLOSURE vm val1)) value) (AC vm)
	(AC vm) SG_UNDEF))

(define-inst GREF (0 1 #t)
  (let ((var (FETCH_OPERAND (PC vm))))
    (cond ((SG_GLOCP var)
	   (set! (AC vm) (SG_GLOC_GET (SG_GLOC var))))
	  ((SG_IDENTIFIERP var)
	   (let ((value (Sg_FindBinding (SG_IDENTIFIER_LIBRARY var)
					(SG_IDENTIFIER_NAME var)
					SG_UNBOUND)))
	     (cond ((SG_GLOCP value)
		    (set! (AC vm) (SG_GLOC_GET (SG_GLOC value))
			  (pointer (- (PC vm) 1)) (SG_WORD value)))
		   ((SG_UNBOUNDP value)
		    (assertion-violation "vm"
					 "unbound variable" var))
		   (else (ASSERT FALSE)))))
	  (else (ASSERT FALSE)))))

(define-inst GSET (0 1 #t)
  (let ((var (FETCH_OPERAND (PC vm))))
    (cond ((SG_GLOCP var)
	   (SG_GLOC_SET (SG_GLOC var) (AC vm)))
	  ((SG_IDENTIFIERP var)
	   (let ((oldval (Sg_FindBinding (SG_IDENTIFIER_LIBRARY var)
					 (SG_IDENTIFIER_NAME var)
					 SG_UNBOUND)))
	     (if (SG_UNBOUNDP oldval)
		 (assertion-violation "set!"
				      "unbound variable"
				      (SG_IDENTIFIER_NAME var))
		 (let ((g (Sg_MakeBinding (SG_IDENTIFIER_LIBRARY var)
					  (SG_IDENTIFIER_NAME var)
					  (AC vm)
					  0)))
		   (set! (pointer (- (PC vm) 1)) (SG_WORD g))))))
	  (else (ASSERT FALSE))))
  (set! (AC vm) SG_UNDEF))

(define-inst PUSH (0 0 #f)
  (PUSH (SP vm) (AC vm)))

(define-inst BOX (1 0 #f)
  (INSN_VAL1 val1 c)
  (INDEX_SET (SP vm) val1 (make_box (INDEX (SP vm) val1))))

(define-inst UNBOX (0 0 #f)
  (ASSERT (SG_BOXP (AC vm)))
  (set! (AC vm) (-> (SG_BOX (AC vm)) value)))

(define-cise-stmt call-two-args-proc
  ((_ obj proc)
   `(begin
      (set! (AC vm) (,proc ,obj (AC vm)))
      (post-- (SP vm)))))

(define-inst ADD (0 0 #t)
  (let ((obj (INDEX (SP vm) 0)))
    (cond ((and (SG_INTP (AC vm)) (SG_INTP obj))
	   (let ((n::long (+ (SG_INT_VALUE obj) (SG_INT_VALUE (AC vm)))))
	     (post-- (SP vm))
	     (if (and (<= SG_INT_MIN n)
		      (>= SG_INT_MAX n))
		 (set! (AC vm) (SG_MAKE_INT n))
		 (set! (AC vm) (Sg_MakeBignumFromSI n)))))
	  (else
	   (call-two-args-proc obj Sg_Add)))))

(define-cise-stmt call-one-arg-with-insn-value
  ((_ proc code)
   `(set! (AC vm) (,proc (SG_MAKE_INT val1) (AC vm)))))

(define-inst ADDI (1 0 #t)
  (INSN_VAL1 val1 c)
  (cond ((SG_INTP (AC vm))
	 (let ((n::long (+ val1 (SG_INT_VALUE (AC vm)))))
	   (if (and (<= SG_INT_MIN n)
		    (>= SG_INT_MAX n))
	       (set! (AC vm) (SG_MAKE_INT n))
	       (set! (AC vm) (Sg_MakeBignumFromSI n)))))
	(else
	 (call-one-arg-with-insn-value Sg_Add c))))

(define-inst SUB (0 0 #t)
  (let ((obj (INDEX (SP vm) 0)))
    (cond ((and (SG_INTP (AC vm)) (SG_INTP obj))
	   (let ((n::long (- (SG_INT_VALUE obj) (SG_INT_VALUE (AC vm)))))
	     (post-- (SP vm))
	     (if (and (<= SG_INT_MIN n)
		      (>= SG_INT_MAX n))
		 (set! (AC vm) (SG_MAKE_INT n))
		 (set! (AC vm) (Sg_MakeBignumFromSI n)))))
	  (else
	   (call-two-args-proc obj Sg_Sub)))))

(define-inst SUBI (1 0 #t)
  (INSN_VAL1 val1 c)
  (cond ((SG_INTP (AC vm))
	 (let ((n::long (- val1 (SG_INT_VALUE (AC vm)))))
	   (if (and (<= SG_INT_MIN n)
		    (>= SG_INT_MAX n))
	       (set! (AC vm) (SG_MAKE_INT n))
	       (set! (AC vm) (Sg_MakeBignumFromSI n)))))
	(else
	 (call-one-arg-with-insn-value Sg_Sub c))))

(define-inst MUL (0 0 #t)
  (call-two-args-proc (INDEX (SP vm) 0) Sg_Mul))

(define-inst MULI (1 0 #t)
  (INSN_VAL1 val1 c)
  (call-one-arg-with-insn-value Sg_Mul c))

;;
;; R6RS requires &assertion exception when divisor was 0.
;; however on Sagittarius scheme we try to calculate if arguments are known,
;; such as (/ 0 0) case. In this case and if #!r6rs was set, it'll cause
;; uncatchable exception. If I can find a nice way to handle compile time
;; exception, this might be fixed.
(define-inst DIV (0 0 #t)
  (let* ((obj (INDEX (SP vm) 0))
	 (exact::int (and (Sg_ExactP obj) (Sg_ExactP (AC vm)))))
    (if (and exact
	     (SG_VM_IS_SET_FLAG vm SG_R6RS_MODE)
	     (Sg_ZeroP (AC vm)))
	(assertion-violation "/" "undefined for 0"
			     (SG_LIST2 obj (AC vm)))
	(call-two-args-proc obj Sg_Div))))

(define-inst DIVI (1 0 #t)
  (INSN_VAL1 val1 c)
  (call-one-arg-with-insn-value Sg_Div c))

(define-cise-stmt call-one-arg
  ((_ proc)
   `(set! (AC vm) (,proc (AC vm)))))

(define-inst NEG (0 0 #t)
  (call-one-arg Sg_Negate))

(define-inst TEST (0 1 #t) :label
  (cond ((SG_FALSEP (AC vm))
	 (let ((n (PEEK_OPERAND (PC vm))))
	   (ASSERT (SG_INTP n))
	   (set! (PC vm) (+ (PC vm) (SG_INT_VALUE n)))))
	(else
	 (post++ (PC vm)))))

(define-inst JUMP (0 1 #t) :label
  (let ((n (PEEK_OPERAND (PC vm))))
    (ASSERT (SG_INTP n))
    (set! (PC vm) (+ (PC vm) (SG_INT_VALUE n)))))

(define-inst SHIFTJ (2 0 #f)
  (INSN_VAL2 val1 val2 c)
  (set! (SP vm) (shift_args (+ (FP vm) val2) val1 (SP vm))))

(define-cise-expr branch-number-test-helper
  ((_ n)
   `(set! (AC vm) SG_FALSE
	  (PC vm) (+ (PC vm) (SG_INT_VALUE ,n))))
  ((_)
   `(begin
      (set! (AC vm) SG_TRUE)
      (post++ (PC vm)))))
(define-cise-stmt branch-number-test
  ((_ op func)
   `(let ((n (PEEK_OPERAND (PC vm)))
	  (s (INDEX (SP vm) 0))
	  (t::int FALSE))
      (if (and (SG_INTP (AC vm)) (SG_INTP s))
	  (if (,op (cast intptr_t s) (cast intptr_t (AC vm)))
	      (branch-number-test-helper)
	      (branch-number-test-helper n))
	  (if (,func s (AC vm))
	      (branch-number-test-helper)
	      (branch-number-test-helper n)))
      (post-- (SP vm)))))

(define-inst BNNUME (0 1 #t) :label
  (branch-number-test == Sg_NumEq))

(define-inst BNLT (0 1 #t) :label
  (branch-number-test < Sg_NumLt))

(define-inst BNLE (0 1 #t) :label
  (branch-number-test <= Sg_NumLe))

(define-inst BNGT (0 1 #t) :label
  (branch-number-test > Sg_NumGt))

(define-inst BNGE (0 1 #t) :label
  (branch-number-test >= Sg_NumGe))

(define-cise-stmt branch-test2
  ((_ proc)
   `(let ((n (PEEK_OPERAND (PC vm))))
      (if (,proc (INDEX (SP vm) 0) (AC vm))
	  (begin
	    (set! (AC vm) SG_TRUE)
	    (post++ (PC vm)))
	  (begin
	    (+= (PC vm) (SG_INT_VALUE n))
	    (set! (AC vm) SG_FALSE)))
      (post-- (SP vm)))))

(define-inst BNEQ (0 1 #t) :label
  (branch-test2 SG_EQ))

(define-inst BNEQV (0 1 #t) :label
  (branch-test2 Sg_EqvP))

(define-cise-stmt branch-test1
  ((_ proc)
   `(let ((n (PEEK_OPERAND (PC vm))))
      (if (,proc (AC vm))
	  (begin
	    (set! (AC vm) SG_TRUE)
	    (post++ (PC vm)))
	  (begin
	    (+= (PC vm) (SG_INT_VALUE n))
	    (set! (AC vm) SG_FALSE))))))

(define-inst BNNULL (0 1 #t) :label
  (branch-test1 SG_NULLP))

(define-inst NOT (0 0 #f)
  (if (SG_FALSEP (AC vm))
      (set! (AC vm) SG_TRUE)
      (set! (AC vm) SG_FALSE)))

(define-cise-stmt builtin-number-compare
  ((_ op func)
   `(let ((s (INDEX (SP vm) 0)))
      (if (and (SG_INTP (AC vm)) (SG_INTP s))
	  (set! (AC vm) (SG_MAKE_BOOL (,op (cast intptr_t s)
					   (cast intptr_t (AC vm)))))
	  (set! (AC vm) (SG_MAKE_BOOL (,func s (AC vm)))))
      (post-- (SP vm)))))

(define-inst NUM_EQ (0 0 #t)
  (builtin-number-compare == Sg_NumEq))

(define-inst NUM_LT (0 0 #t)
  (builtin-number-compare < Sg_NumLt))

(define-inst NUM_LE (0 0 #t)
  (builtin-number-compare <= Sg_NumLe))

(define-inst NUM_GT (0 0 #t)
  (builtin-number-compare > Sg_NumGt))

(define-inst NUM_GE (0 0 #t)
  (builtin-number-compare >= Sg_NumGe))

(define-inst RECEIVE (2 0 #t)
  (INSN_VAL2 val1 val2 c)
  (let ((numValues::int 0))
    (if (SG_VALUESP (AC vm))
	(set! numValues (SG_VALUES_SIZE (AC vm)))
	(set! numValues 1))
    (if (< numValues val1)
	(assertion-violation "receive"
			     "recieved fewer values than expected"
			     (AC vm)))
    (if (and (== val2 0)
	     (> numValues val1))
	(assertion-violation "receive"
			     "recieved more values than expected"
			     (AC vm)))
    (cond ((== val2 0)
	   ;; (receive (a b c) ...)
	   (cond ((== val1 1)
		  ;; (values 'a) creates non values object
		  (PUSH (SP vm) (AC vm)))
		 ((> val1 0)
		  (dotimes (i val1)
		    (PUSH (SP vm) (SG_VALUES_ELEMENT (AC vm) i))))))
	  ((== val1 0)
	   ;; (receive a ...)
	   (let ((h '())
		 (t '()))
	     (if (== numValues 1)
		 (SG_APPEND1 h t (AC vm))
		 (dotimes (i numValues)
		   (SG_APPEND1 h t (SG_VALUES_ELEMENT (AC vm) i))))
	     (PUSH (SP vm) h)))
	  (else
	   ;; (receive (a b . c) ...)
	   (let ((h '())
		 (t '())
		 (i::int 0))
	     (for (() () (post++ i))
		  (cond ((< i val1)
			 (PUSH (SP vm) (SG_VALUES_ELEMENT (AC vm) i)))
			((< i (SG_VALUES_SIZE (AC vm)))
			 (SG_APPEND1 h t (SG_VALUES_ELEMENT (AC vm) i)))
			(else
			 (PUSH (SP vm) h)
			 (break)))))))))

(define-inst CLOSURE (0 1 #f)
  (let ((cb (FETCH_OPERAND (PC vm))))
    (if (not (SG_CODE_BUILDERP cb))
	(wrong-type-of-argument-violation "closure"
					  "code-builder"
					  cb))
    (set! (AC vm) (Sg_MakeClosure cb (- (SP vm) (SG_CODE_BUILDER_FREEC cb)))
	  (SP vm) (- (SP vm) (SG_CODE_BUILDER_FREEC cb)))))

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
      (assertion-violation "apply" "improper list not allowed" (AC vm)))
    (shift_args fp nargc (SP vm))
    (cond ((== rargc 0)
	   (set! (SP vm) (- (SP vm) 1))
	   (when val2
	     (set! (SP vm) (shift_args (FP vm) nargc (SP vm))))
	   (set! (arrayref (ref (pointer vm) callCode) 0) 
		 (MERGE_INSN_VALUE1 CALL nargc)
		 (PC vm) (-> vm callCode)))
	  (else
	   (INDEX_SET (SP vm) 0 (SG_CAR (AC vm)))
	   (dolist (v (SG_CDR (AC vm)))
	     (PUSH (SP vm) v))
	   (when val2
	     (set! (SP vm) (shift_args (FP vm) (+ nargc rargc) (SP vm))))
	   (set! (arrayref (ref (pointer vm) callCode) 0)
		 (MERGE_INSN_VALUE1 CALL (+ nargc rargc))
		 (PC vm) (-> vm callCode))))
    (set! (AC vm) proc)))

(define-inst CALL (1 0 #t)
  (.include "vmcall.c"))


(define-cise-stmt local-call-process
  ((_ c)
   `(begin
      (INSN_VAL1 val1 ,c)
      (cond ((SG_CLOSUREP (AC vm))
	     (when (and (SG_VM_LOG_LEVEL vm SG_DEBUG_LEVEL)
			(== (-> vm state) RUNNING))
	       (Sg_Printf (-> vm logPort) (UC "calling %S\n") (AC vm))
	       (when (and (SG_VM_LOG_LEVEL vm SG_TRACE_LEVEL)
			  (== (-> vm state) RUNNING))
		 (print_frames vm)))
	     (let ((cb::SgCodeBuilder* (-> (SG_CLOSURE (AC vm)) code)))
	       (set! (CL vm) (AC vm)
		     (PC vm) (-> cb code)
		     (FP vm) (- (SP vm) val1))))
	    (else (ASSERT FALSE))))))

(define-inst LOCAL_CALL (1 0 #t)
  (CHECK_STACK (SG_CLOSURE_MAX_STACK (AC vm)) vm)
  (local-call-process c))

(define-cise-stmt tail-call-process
  ((_ code)
   `(begin
      (INSN_VAL1 val1 ,code)
      (set! (SP vm) (shift_args (FP vm) val1 (SP vm))))))

(define-inst TAIL_CALL (1 0 #t)
  (tail-call-process c)
  (.include "vmcall.c"))

(define-inst LOCAL_TAIL_CALL (1 0 #t)
  (CHECK_STACK (SG_CLOSURE_MAX_STACK (AC vm)) vm)
  (tail-call-process c)
  (local-call-process c))

(define-inst RET (0 0 #f)
  (RET_INSN))

(define-inst FRAME (0 1 #f) :label
  (let ((n (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_INTP n))
    (PUSH_CONT vm (+ (PC vm) (- (SG_INT_VALUE n) 1)))))


(define-inst ENTER (1 0 #f)
  (INSN_VAL1 val1 c)
  (set! (FP vm) (- (SP vm) val1)))

(define-inst LEAVE (1 0 #f)
  (INSN_VAL1 val1 c)
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

(define-inst CAR (0 0 #t)
  (if (not (SG_PAIRP (AC vm)))
      (wrong-type-of-argument-violation "car" "pair" (AC vm)))
  (call-one-arg SG_CAR))

(define-inst CDR (0 0 #t)
  (if (not (SG_PAIRP (AC vm)))
      (wrong-type-of-argument-violation "cdr" "pair" (AC vm)))
  (call-one-arg SG_CDR))

(define-inst CONS (0 0 #t)
  (call-two-args-proc (INDEX (SP vm) 0) Sg_Cons))

(define-inst LIST (1 0 #t)
  (INSN_VAL1 val1 c)
  (let ((n::int (- val1 1))
	(ret '()))
    (when (> val1 0)
      (set! ret (Sg_Cons (AC vm) ret))
      (dotimes (i n)
	(set! ret (Sg_Cons (INDEX (SP vm) i) ret)))
      (set! (SP vm) (- (SP vm) n)))
    (set! (AC vm) ret)))

(define-inst APPEND (1 0 #t)
  (INSN_VAL1 val1 c)
  (let ((nargs::int (- val1 1))
	(ret '()))
    (when (> nargs 0)
      (set! ret (AC vm))
      (dotimes (i nargs)
	(let ((obj (INDEX (SP vm) i)))
	  (when (< (Sg_Length obj) 0)
	    (wrong-type-of-argument-violation "append"
					      "list" obj))
	  (set! ret (Sg_Append2 obj ret))))
      (set! (SP vm) (- (SP vm) nargs)))
    (set! (AC vm) ret)))

(define-inst VALUES (1 0 #t)
  (INSN_VAL1 val1 c)
  (if (== val1 0)
      (set! (AC vm) (Sg_MakeValues 0))
      (let ((v (AC vm)))
	(when (> val1 1)
	  (set! v (Sg_MakeValues val1))
	  (let ((n::int (- val1 1)))
	    (set! (SG_VALUES_ELEMENT v n) (AC vm))
	    (dotimes (i n)
	      (set! (SG_VALUES_ELEMENT v (- n i 1))
		    (INDEX (SP vm) i)))
	    (set! (SP vm) (- (SP vm) n))))
	(set! (AC vm) v))))

(define-cise-stmt call-two-args-compare
  ((_ obj proc)
   `(begin
      (set! (AC vm) (SG_MAKE_BOOL (,proc ,obj (AC vm))))
      (post-- (SP vm)))))

(define-inst EQ (0 0 #t)
  (call-two-args-compare (INDEX (SP vm) 0) SG_EQ))

(define-inst EQV (0 0 #t)
  (call-two-args-compare (INDEX (SP vm) 0) Sg_EqvP))

(define-inst NULLP (0 0 #t)
  (set! (AC vm) (SG_MAKE_BOOL (SG_NULLP (AC vm)))))

(define-inst PAIRP (0 0 #t)
  (set! (AC vm) (SG_MAKE_BOOL (SG_PAIRP (AC vm)))))

(define-inst SYMBOLP (0 0 #t)
  (set! (AC vm) (SG_MAKE_BOOL (SG_SYMBOLP (AC vm)))))

(define-inst VECTOR (1 0 #t)
  (let ((v SG_UNDEF))
    (INSN_VAL1 val1 c)
    (set! v (Sg_MakeVector val1 SG_UNDEF))
    (if (> val1 0)
	(let ((i::int 0)
	      (n::int (- val1 1)))
	  (set! (SG_VECTOR_ELEMENT v n) (AC vm))
	  (for ((set! i 0) (< i n) (post++ i))
	       (set! (SG_VECTOR_ELEMENT v (- n i 1))
		     (INDEX (SP vm) i)))
	  (set! (SP vm) (- (SP vm) n))))
    (set! (AC vm) v)))

(define-inst VECTORP (0 0 #t)
  (set! (AC vm) (SG_MAKE_BOOL (SG_VECTORP (AC vm)))))

(define-inst VEC_LEN (0 0 #t)
  (if (not (SG_VECTORP (AC vm)))
      (wrong-type-of-argument-violation "vector-length"
					"vector" (AC vm)))
  (set! (AC vm) (SG_MAKE_INT (SG_VECTOR_SIZE (AC vm)))))

(define-inst VEC_REF (0 0 #t)
  (let ((obj (INDEX (SP vm) 0)))
    (if (SG_VECTORP obj)
	(if (SG_INTP (AC vm))
	    (let ((index::int (SG_INT_VALUE (AC vm))))
	      (if (or (>= index (SG_VECTOR_SIZE obj))
		      (< index 0))
		  (assertion-violation "vector-ref" "index out of range"
				       (SG_LIST2 obj (AC vm)))
		  (begin
		    (set! (AC vm) (SG_VECTOR_ELEMENT obj index))
		    (post-- (SP vm)))))
	    (wrong-type-of-argument-violation "vector-ref" "fixnum" (AC vm)))
	(wrong-type-of-argument-violation "vector-ref" "vector" obj))))

(define-inst VEC_SET (0 0 #t)
  (let ((obj (INDEX (SP vm) 1))
	(index (INDEX (SP vm) 0)))
    (if (SG_VECTORP obj)
	(if (SG_LITERAL_VECTORP obj)
	    (assertion-violation "vector-set!"
				 "attempt to modify immutable vector"
				 (SG_LIST1 obj))
	    (if (SG_INTP index)
		(let ((i::int (SG_INT_VALUE index)))
		  (if (or (>= i (SG_VECTOR_SIZE obj))
			  (< i 0))
		      (assertion-violation "vector-set!" "index out of range"
					   (SG_LIST2 obj index))
		      (set! (SG_VECTOR_ELEMENT obj i) (AC vm)
			    (AC vm) SG_UNDEF
			    (SP vm) (- (SP vm) 2))))
		(wrong-type-of-argument-violation "vector-set!"
						  "fixnum" index)))
	(wrong-type-of-argument-violation "vector-set!" "vector" obj))))

;; combined instructions
;; CONST_PUSH, LREF_PUSH and FREF_PUSH are treated specially for heavy head call
(define-inst LREF_PUSH (1 0 #t)
  (INSN_VAL1 val1 c)
  (PUSH (SP vm) (REFER_LOCAL vm val1)))

(define-inst FREF_PUSH (1 0 #t)
  (INSN_VAL1 val1 c)
  (PUSH (SP vm) (INDEX_CLOSURE vm val1)))

(define-inst GREF_PUSH (0 1 #t) :combined
  (GREF PUSH))

(define-inst CONST_PUSH (0 1 #f)
  (PUSH (SP vm) (FETCH_OPERAND (PC vm))))

(define-inst CONSTI_PUSH (1 0 #f) :combined
  (CONSTI PUSH))

(define-inst GREF_CALL (1 1 #t) :combined
  (GREF CALL))

(define-inst GREF_TAIL_CALL (1 1 #t) :combined
  (GREF TAIL_CALL))

(define-inst SET_CAR (0 0 #t)
  (let ((obj (INDEX (SP vm) 0)))
    (if (SG_PAIRP obj)
	(if (Sg_ConstantLiteralP obj)
	    (assertion-violation "set-car!" "attempt to modify constant literal"
				 obj)
	    (begin
	      (SG_SET_CAR obj (AC vm))
	      (post-- (SP vm))
	      (set! (AC vm) SG_UNDEF)))
	(wrong-type-of-argument-violation "set-car!" "pair" obj))))


(define-inst SET_CDR (0 0 #t)
  (let ((obj (INDEX (SP vm) 0)))
    (if (SG_PAIRP obj)
	(if (Sg_ConstantLiteralP obj)
	    (assertion-violation "set-cdr!" "attempt to modify constant literal"
				 obj)
	    (begin
	      (SG_SET_CDR obj (AC vm))
	      (post-- (SP vm))
	      (set! (AC vm) SG_UNDEF)))
	(wrong-type-of-argument-violation "set-cdr!" "pair" obj))))

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
