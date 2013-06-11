;; -*- scheme -*-

;; definitions of vm instruction.
;; NB: c, val1, val2 are defined in vm.c
;;     most of macros are also defined in vm.c
#!compatible

(define-cise-stmt assertion-violation
  ((_ who msg)
   `(begin
      (Sg_AssertionViolation (SG_INTERN ,who) (SG_MAKE_STRING ,msg) '())
      (return SG_UNDEF)))
  ((_ who msg irritants)
   `(begin
      (Sg_AssertionViolation (SG_INTERN ,who) (SG_MAKE_STRING ,msg) ,irritants)
      (return SG_UNDEF))))

(define-cise-stmt wrong-type-of-argument-violation
  ((_ who msg got)
   `(begin
      (Sg_WrongTypeOfArgumentViolation (SG_INTERN ,who)
				       (SG_MAKE_STRING ,msg) ,got '())
      (return SG_UNDEF)))
  ((_ who msg got irritants)
   `(begin
      (Sg_WrongTypeOfArgumentViolation (SG_INTERN ,who)
				       (SG_MAKE_STRING ,msg) ,got ,irritants)
      (return SG_UNDEF))))

(define-cise-stmt $goto-insn
  ((_ insn)
   `(,(format "goto label_~a;" insn))))

(define-cise-stmt $result
  ((_ expr)
   `(begin
      ,@(case (result-type)
	  ((reg) `((set! (AC vm) ,expr) NEXT1))
	  ((push) `((PUSH (SP vm) ,expr) NEXT1))
	  ((call comb) `((set! (AC vm) ,expr)))
	  ((ret) `((set! (AC vm) ,expr)
		   (RET_INSN)
		   NEXT))))))

(define-cise-stmt $result:n
  ((_ expr)
   (let ((r (gensym "cise__")))
     `(let ((,r :: long ,expr))
	(if (and (<= SG_INT_MIN ,r) (>= SG_INT_MAX ,r))
	    ($result (SG_MAKE_INT ,r))
	    ($result (Sg_MakeBignumFromSI ,r)))))))

(define-cise-stmt $result:f
  ((_ expr)
   (let ((r (gensym "cise__")))
     `(let ((,r :: double ,expr))
	($result (Sg_MakeFlonum ,r))))))

;; coercion
(define-cise-stmt $result:b
  ((_ expr) `($result (SG_MAKE_BOOL ,expr))))
(define-cise-stmt $result:i
  ((_ expr) (let ((r (gensym "cise__")))
	      `(let ((,r :: long ,expr)) ($result (SG_MAKE_INT ,r))))))

(define-inst NOP (0 0 #f) NEXT)

;; this should not happen but just in case
(define-inst HALT (0 0 #f) (return (AC vm)))

(define-inst UNDEF (0 0 #f) ($result SG_UNDEF))
(define-inst CONST (0 1 #f)
  (let ((val (FETCH_OPERAND (PC vm))))
    ($result val)))

(define-inst CONSTI (1 0 #f)
  ;;(INSN_VAL1 val1 c)
  ($result:i (INSN_VALUE1 c)))

(define-inst LREF (1 0 #t)
  (INSN_VAL1 val1 c)
  ($result (REFER_LOCAL vm val1)))

(define-inst LSET (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (-> (SG_BOX (REFER_LOCAL vm val1)) value) (AC vm)
	(AC vm) SG_UNDEF)
  NEXT1)

(define-inst FREF (1 0 #t)
  (INSN_VAL1 val1 c)
  ($result (INDEX_CLOSURE vm val1)))

(define-inst FSET (1 0 #t)
  (INSN_VAL1 val1 c)
  (set! (-> (SG_BOX (INDEX_CLOSURE vm val1)) value) (AC vm)
	(AC vm) SG_UNDEF)
  NEXT)

(define-inst GREF (0 1 #t)
  (let ((v ))
    (REFER_GLOBAL vm v)
    ($result v)))

(define-inst GSET (0 1 #t)
  (let ((var (FETCH_OPERAND (PC vm))))
    (if (SG_GLOCP var)
	(SG_GLOC_SET (SG_GLOC var) (AC vm))
	(let ((oldval ))
	  (FIND_GLOBAL vm var oldval)
	  (let ((g (Sg_MakeBinding (SG_IDENTIFIER_LIBRARY var)
				   (SG_IDENTIFIER_NAME var)
				   (AC vm)
				   0)))
	    (set! (pointer (- (PC vm) 1)) (SG_WORD g))))))
  (set! (AC vm) SG_UNDEF)
  NEXT1)

(define-inst PUSH (0 0 #f)
  (PUSH (SP vm) (AC vm))
  NEXT)

(define-inst BOX (1 0 #f)
  (INSN_VAL1 val1 c)
  (INDEX_SET (SP vm) val1 (make_box (INDEX (SP vm) val1)))
  NEXT)

(define-inst UNBOX (0 0 #f)
  (set! (AC vm) (-> (SG_BOX (AC vm)) value))
  NEXT)

(define-cise-stmt call-two-args-proc
  ((_ obj proc)
   `(let ((v ,obj))
      ($result (,proc v (AC vm))))))

(define-inst ADD (0 0 #t)
  (let ((obj (POP (SP vm))))
    (cond ((and (SG_INTP (AC vm)) (SG_INTP obj))
	   ($result:n (+ (SG_INT_VALUE obj) (SG_INT_VALUE (AC vm)))))
	  ((and (SG_FLONUMP (AC vm)) (SG_FLONUMP obj))
	   ($result (Sg_MakeFlonum (+ (SG_FLONUM_VALUE obj)
				      (SG_FLONUM_VALUE (AC vm))))))
	  (else
	   (call-two-args-proc obj Sg_Add)))))

(define-cise-stmt call-one-arg-with-insn-value
  ((_ proc code)
   `($result (,proc (SG_MAKE_INT val1) (AC vm)))))

(define-inst ADDI (1 0 #t)
  (INSN_VAL1 val1 c)
  (cond ((SG_INTP (AC vm))
	 ($result:n (+ val1 (SG_INT_VALUE (AC vm)))))
	((SG_FLONUMP (AC vm))
	 ($result:f (+ (cast double val1) (SG_FLONUM_VALUE (AC vm)))))
	(else
	 (call-one-arg-with-insn-value Sg_Add c))))

(define-inst SUB (0 0 #t)
  (let ((obj (POP (SP vm))))
    (cond ((and (SG_INTP (AC vm)) (SG_INTP obj))
	   ($result:n (- (SG_INT_VALUE obj) (SG_INT_VALUE (AC vm)))))
	  ((and (SG_FLONUMP (AC vm)) (SG_FLONUMP obj))
	   ($result:f (- (SG_FLONUM_VALUE obj) (SG_FLONUM_VALUE (AC vm)))))
	  (else
	   (call-two-args-proc obj Sg_Sub)))))

(define-inst SUBI (1 0 #t)
  (INSN_VAL1 val1 c)
  (cond ((SG_INTP (AC vm))
	 ($result:n (- val1 (SG_INT_VALUE (AC vm)))))
	((SG_FLONUMP (AC vm))
	 ($result:f (- (cast double val1) (SG_FLONUM_VALUE (AC vm)))))
	(else
	 (call-one-arg-with-insn-value Sg_Sub c))))

(define-inst MUL (0 0 #t)
  (call-two-args-proc (POP (SP vm)) Sg_Mul))

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
  (let* ((obj (POP (SP vm)))
	 (exact::int (and (Sg_ExactP obj) (Sg_ExactP (AC vm)))))
    (if (and exact (Sg_ZeroP (AC vm)))
	(assertion-violation "/" "undefined for 0" (SG_LIST2 obj (AC vm)))
	(call-two-args-proc obj Sg_Div))))

(define-inst DIVI (1 0 #t)
  (INSN_VAL1 val1 c)
  (call-one-arg-with-insn-value Sg_Div c))

(define-cise-stmt call-one-arg
  ((_ proc)
   `($result (,proc (AC vm)))))

(define-inst NEG (0 0 #t) (call-one-arg Sg_Negate))

(define-inst TEST (0 1 #t) :label
  (cond ((SG_FALSEP (AC vm))
	 (let ((n (PEEK_OPERAND (PC vm))))
	   (+= (PC vm) (cast intptr_t n))))
	(else
	 (post++ (PC vm))))
  NEXT)

(define-inst JUMP (0 1 #t) :label
  (let ((n (PEEK_OPERAND (PC vm))))
    (+= (PC vm) (cast intptr_t n)))
  NEXT)

(define-inst SHIFTJ (2 0 #f)
  (INSN_VAL2 val1 val2 c)
  (set! (SP vm) (shift_args (+ (FP vm) val2) val1 (SP vm)))
  NEXT)

(define-cise-expr branch-number-test-helper
  ((_ p)
   (let ((n (gensym "cise__")))
     `(let ((,n ,p))
	(set! (AC vm) SG_FALSE)
	(+= (PC vm) (cast intptr_t ,n)))))
  ((_)
   `(begin
      (set! (AC vm) SG_TRUE)
      (post++ (PC vm)))))
(define-cise-stmt branch-number-test
  ((_ op func)
   `(let ((s (POP (SP vm))))
      (cond ((logand (SG_INTP (AC vm)) (SG_INTP s))
	     (if (,op (cast intptr_t s) (cast intptr_t (AC vm)))
		 (branch-number-test-helper)
		 (branch-number-test-helper (PEEK_OPERAND (PC vm)))))
	    ((logand (SG_FLONUMP (AC vm)) (SG_FLONUMP s))
	     (if (,op (SG_FLONUM_VALUE s) (SG_FLONUM_VALUE (AC vm)))
		 (branch-number-test-helper)
		 (branch-number-test-helper (PEEK_OPERAND (PC vm)))))
	    (else
	     (if (,func s (AC vm))
		 (branch-number-test-helper)
		 (branch-number-test-helper (PEEK_OPERAND (PC vm))))))
      NEXT)))

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
      (if (,proc (POP (SP vm)) (AC vm))
	  (begin
	    (set! (AC vm) SG_TRUE)
	    (post++ (PC vm)))
	  (begin
	    (+= (PC vm) (cast intptr_t n))
	    (set! (AC vm) SG_FALSE)))
      NEXT)))

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
	    (set! (AC vm) SG_FALSE)
	    (+= (PC vm) (cast intptr_t n))))
      NEXT)))

(define-inst BNNULL (0 1 #t) :label
  (branch-test1 SG_NULLP))

(define-inst NOT (0 0 #f)
  ($result:b (SG_FALSEP (AC vm))))

(define-cise-stmt builtin-number-compare
  ((_ op func)
   `(let ((s (POP (SP vm))))
      (if (and (SG_INTP (AC vm)) (SG_INTP s))
	  ($result:b (,op (cast intptr_t s) (cast intptr_t (AC vm))))
	  ($result:b (,func s (AC vm)))))))

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
  (let ((numValues::int (-> vm valuesCount)))
    (when (< numValues val1)
      (assertion-violation "receive"
			   "recieved fewer values than expected"
			   (AC vm)))
    (when (and (== val2 0) (> numValues val1))
      (assertion-violation "receive"
			   "recieved more values than expected"
			   (AC vm)))
    (cond ((== val2 0)
	   ;; (receive (a b c) ...)
	   (when (> val1 0) (PUSH (SP vm) (AC vm)))
	   (dotimes (i (- val1 1))
	     (PUSH (SP vm) (SG_VALUES_REF vm i))))
	  ((== val1 0)
	   ;; (receive a ...)
	   (let ((h '()) (t '()))
	     (when (> numValues 0) (SG_APPEND1 h t (AC vm)))
	     (when (> numValues 1)
	       (dotimes (i (- numValues 1))
		 (SG_APPEND1 h t (SG_VALUES_REF vm i))))
	     (PUSH (SP vm) h)))
	  (else
	   ;; (receive (a b . c) ...)
	   (let ((h '()) (t '()) (i::int 0))
	     (PUSH (SP vm) (AC vm))
	     (for (() (< i (- numValues 1)) (post++ i))
		  (if (< i (- val1 1))
		      (PUSH (SP vm) (SG_VALUES_REF vm i))
		      (SG_APPEND1 h t (SG_VALUES_REF vm i))))
	     (PUSH (SP vm) h)))))
  NEXT1)

(define-inst CLOSURE (0 1 #f)
  (let ((cb (FETCH_OPERAND (PC vm))))
    ;; If this happend this must be panic.
    ;; (when (SG_CODE_BUILDERP cb)
    ;;   (wrong-type-of-argument-violation "closure" "code-builder" cb))
    (-= (SP vm) (SG_CODE_BUILDER_FREEC cb))
    ($result (Sg_MakeClosure cb (SP vm)))))

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
	   (post-- (SP vm))
	   (when val2
	     (set! (SP vm) (shift_args (FP vm) nargc (SP vm))))
	   (set! (AC vm) proc)
	   ;; c is definec in vm.c and contains current INSN
	   ;; we need to decieve the as if this call is CALL
	   (set! c (MERGE_INSN_VALUE1 CALL nargc))
	   ($goto-insn CALL))
	  (else
	   (INDEX_SET (SP vm) 0 (AC vm))
	   (when val2
	     (set! (SP vm) (shift_args (FP vm) (+ nargc 1) (SP vm))))
	   (set! c (MERGE_INSN_VALUE1 CALL (+ nargc 1)))
	   (set! (AC vm) proc)
	   (goto tail_apply_entry)))))

(define-inst CALL (1 0 #t)
  (label call_entry)
  (.undef APPLY_CALL)
  (.include "vmcall.c")
  (label tail_apply_entry)
  (.define APPLY_CALL)
  (.include "vmcall.c")
  )

(define-cise-stmt local-call-process
  ((_ c)
   `(begin
      (INSN_VAL1 val1 ,c)
      (.if "defined(SHOW_CALL_TRACE)"
	      (when (and (SG_VM_LOG_LEVEL vm SG_TRACE_LEVEL)
			 (== (-> vm state) RUNNING))
		(Sg_Printf (-> vm logPort) (UC ";; calling %S\n") (AC vm))))
      (let ((cb::SgCodeBuilder* (-> (SG_CLOSURE (AC vm)) code)))
	(set! (CL vm) (AC vm)
	      (PC vm) (-> cb code)
	      (FP vm) (- (SP vm) val1))))))

(define-inst LOCAL_CALL (1 0 #t)
  (CHECK_STACK (SG_CLOSURE_MAX_STACK (AC vm)) vm)
  (local-call-process c)
  NEXT)

(define-cise-stmt tail-call-process
  ((_ code)
   `(begin
      (INSN_VAL1 val1 ,code)
      (set! (SP vm) (shift_args (FP vm) val1 (SP vm))))))

(define-inst TAIL_CALL (1 0 #t)
  (tail-call-process c)
  ($goto-insn CALL))

(define-inst LOCAL_TAIL_CALL (1 0 #t)
  (CHECK_STACK (SG_CLOSURE_MAX_STACK (AC vm)) vm)
  (tail-call-process c)
  (local-call-process c)
  NEXT)

(define-inst RET (0 0 #f)
  (RET_INSN)
  NEXT)

(define-inst FRAME (0 1 #f) :label
  (let ((n (FETCH_OPERAND (PC vm))))
    (PUSH_CONT vm (+ (PC vm) (- (cast intptr_t n) 1))))
  NEXT)

;; TODO remove this instruction from compiler.
;; this was only for sanity and now it doesn't do anything.
(define-inst ENTER (1 0 #f)
  ;;(INSN_VAL1 val1 c)
  ;;(set! (FP vm) (- (SP vm) val1))
  NEXT)

(define-inst LEAVE (1 0 #f)
  (INSN_VAL1 val1 c)
  (-= (SP vm) val1)
  NEXT)

(define-inst DEFINE (1 1 #t)
  (INSN_VAL1 val1 c)
  (let ((var (FETCH_OPERAND (PC vm))))
    (ASSERT (SG_IDENTIFIERP var))
    (Sg_MakeBinding (SG_IDENTIFIER_LIBRARY var)
		    (SG_IDENTIFIER_NAME var)
		    (AC vm)
		    val1)
    (set! (AC vm) SG_UNDEF))
  NEXT)

;; This instruction is just mark for compiled cache.
;; So it doesn't do any thing.
(define-inst LIBRARY (0 1 #f)
  ;; discards library and move to next.
  (let ((lib (Sg_FindLibrary (FETCH_OPERAND (PC vm)) FALSE)))
    (set! (-> vm currentLibrary) (cast SgLibrary* lib)))
  NEXT)

(define-inst CAR (0 0 #t)
  (if (SG_PAIRP (AC vm))
      (call-one-arg SG_CAR)
      (wrong-type-of-argument-violation "car" "pair" (AC vm))))

(define-inst CDR (0 0 #t)
  (if (SG_PAIRP (AC vm))
      (call-one-arg SG_CDR)
      (wrong-type-of-argument-violation "cdr" "pair" (AC vm))))

(define-inst CONS (0 0 #t)
  (call-two-args-proc (POP (SP vm)) Sg_Cons))

(define-inst LIST (1 0 #t)
  (INSN_VAL1 val1 c)
  (let ((n::int (- val1 1))
	(ret '()))
    (when (> val1 0)
      (set! ret (Sg_Cons (AC vm) ret))
      (dotimes (i n)
	(set! ret (Sg_Cons (INDEX (SP vm) i) ret)))
      (-= (SP vm) n))
    ($result ret)))

(define-inst APPEND (1 0 #t)
  (INSN_VAL1 val1 c)
  (let ((nargs::int (- val1 1))
	(ret '()))
    (when (> val1 0)
      (set! ret (AC vm))
      (dotimes (i nargs)
	(let ((obj (INDEX (SP vm) i)))
	  (when (< (Sg_Length obj) 0)
	    (wrong-type-of-argument-violation "append" "list" obj))
	  (set! ret (Sg_Append2 obj ret))))
      (-= (SP vm) nargs))
    ($result ret)))

(define-inst VALUES (1 0 #t)
  (INSN_VAL1 val1 c)
  (let ((v (AC vm)) (n::int (- val1 1)))
    (set! (-> vm valuesCount) val1)
    (when (> n DEFAULT_VALUES_SIZE)
      (SG_ALLOC_VALUES_BUFFER vm (- n DEFAULT_VALUES_SIZE)))
    (for (() (> n 0) (post-- n))
	 (SG_VALUES_SET vm (- n 1) v)
	 (set! v (POP (SP vm))))
    (set! (AC vm) v))
  NEXT)

(define-cise-stmt call-two-args-compare
  ((_ obj proc)
   `(let ((v ,obj))
      ($result:b (,proc v (AC vm))))))

(define-inst EQ (0 0 #t)
  (call-two-args-compare (POP (SP vm)) SG_EQ))

(define-inst EQV (0 0 #t)
  (call-two-args-compare (POP (SP vm)) Sg_EqvP))

(define-inst NULLP (0 0 #t)
  ($result:b (SG_NULLP (AC vm))))

(define-inst PAIRP (0 0 #t)
  ($result:b (SG_PAIRP (AC vm))))

(define-inst SYMBOLP (0 0 #t)
  ($result:b (SG_SYMBOLP (AC vm))))

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
	  (-= (SP vm) n)))
    ($result v)))

(define-inst VECTORP (0 0 #t)
  ($result:b (SG_VECTORP (AC vm))))

(define-inst VEC_LEN (0 0 #t)
  (if (SG_VECTORP (AC vm))
      ($result:i (SG_VECTOR_SIZE (AC vm)))
      (wrong-type-of-argument-violation "vector-length" "vector" (AC vm))))

(define-inst VEC_REF (0 0 #t)
  (let ((obj (POP (SP vm))))
    (unless (SG_VECTORP obj)
      (wrong-type-of-argument-violation "vector-ref" "vector" obj))
    (unless (SG_INTP (AC vm))
      (wrong-type-of-argument-violation "vector-ref" "fixnum" (AC vm)))
    (let ((index::int (SG_INT_VALUE (AC vm))))
      (when (or (>= index (SG_VECTOR_SIZE obj)) (< index 0))
	(assertion-violation "vector-ref" "index out of range" 
			     (SG_LIST2 obj (AC vm))))
      ($result (SG_VECTOR_ELEMENT obj index)))))

(define-inst VEC_SET (0 0 #t)
  (let ((index (POP (SP vm)))
	(obj (POP (SP vm))))
    (unless (SG_VECTORP obj)
      (wrong-type-of-argument-violation "vector-set!" "vector" obj))
    (when (SG_LITERAL_VECTORP obj)
      (assertion-violation "vector-set!"
			   "attempt to modify immutable vector"
			   (SG_LIST1 obj)))
    (unless (SG_INTP index)
      (wrong-type-of-argument-violation "vector-set!" "fixnum" index))
    (let ((i::int (SG_INT_VALUE index)))
      (when (or (>= i (SG_VECTOR_SIZE obj)) (< i 0))
	(assertion-violation "vector-set!" "index out of range" 
			     (SG_LIST2 obj index)))
      (set! (SG_VECTOR_ELEMENT obj i) (AC vm))
      ($result SG_UNDEF))))

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
  (let ((obj (POP (SP vm))))
    (unless (SG_PAIRP obj)
      (wrong-type-of-argument-violation "set-car!" "pair" obj))
    (when (Sg_ConstantLiteralP obj)
      (assertion-violation "set-car!" "attempt to modify constant literal" obj))
    (SG_SET_CAR obj (AC vm))
    ($result SG_UNDEF)))


(define-inst SET_CDR (0 0 #t)
  (let ((obj (POP (SP vm))))
    (unless (SG_PAIRP obj)
      (wrong-type-of-argument-violation "set-cdr!" "pair" obj))
    (when (Sg_ConstantLiteralP obj)
      (assertion-violation "set-cdr!" "attempt to modify constant literal" obj))
    (SG_SET_CDR obj (AC vm))
    ($result SG_UNDEF)))

(define-cise-stmt $cxxr
  ((_ name a b)
   `(let ((obj (AC vm)))
      (if (SG_PAIRP obj)
	  (let ((obj2 (,b obj)))
	    (if (SG_PAIRP obj2)
		($result (,a obj2))
		(wrong-type-of-argument-violation ,name "pair" obj2 obj)))
	  (wrong-type-of-argument-violation ,name "pair" obj)))))

(define-inst CAAR (0 0 #t) ($cxxr "caar" SG_CAR SG_CAR))
(define-inst CADR (0 0 #t) ($cxxr "cadr" SG_CAR SG_CDR))
(define-inst CDAR (0 0 #t) ($cxxr "cdar" SG_CDR SG_CAR))
(define-inst CDDR (0 0 #t) ($cxxr "cddr" SG_CDR SG_CDR))

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

;; for Sg_Apply(n) related
;; try to use pre-allocated values buffer. if the given argument is more than
;; max then it must be stored in the rest.
(define-inst APPLY_VALUES (1 1 #f)
  (let ((rest (FETCH_OPERAND (PC vm)))
	(i::int))
    (INSN_VAL1 val1 c)
    (CHECK_STACK val1 vm)
    (for ((set! i 0) (< i val1) (post++ i))
	 (when (== i DEFAULT_VALUES_SIZE) (break))
	 (PUSH (SP vm) (aref (-> vm values) i)))
    (dolist (v rest)
      (PUSH (SP vm) v))
    ($goto-insn TAIL_CALL)))


;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
