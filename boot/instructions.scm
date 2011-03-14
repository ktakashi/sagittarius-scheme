;; (define-inst name insn-param-count arg-count src? value)
;; (define-comment comment)
(define-inst NOP 0 0 #f 0)

(define-inst HALT 0 0 #f)

(define-inst UNDEF 0 0 #f)
(define-inst CONST 0 1 #f)

(define-inst CONSTI 1 0 #f)

(define-inst LREF 1 0 #t)

(define-inst LSET 1 0 #t)

(define-inst FREF 1 0 #t)

(define-inst FSET 1 0 #t)

(define-inst GREF 0 1 #t)

(define-inst GSET 0 1 #t)

(define-inst PUSH 0 0 #f)

(define-inst BOX 1 0 #f)
(define-inst UNBOX 0 0 #f)
(define-inst ADD 0 0 #t)

(define-inst ADDI 1 0 #t)

(define-inst SUB 0 0 #t)

(define-inst SUBI 1 0 #t)

(define-inst MUL 0 0 #t)

(define-inst MULI 1 0 #t)

(define-inst DIV 0 0 #t)

(define-inst DIVI 1 0 #t)
(define-inst NEG 0 0 #t) ;; negative

(define-inst TEST 0 1 :label #t)
(define-inst JUMP 0 1 :label #f) ;"///< jump. jump to label"
(define-inst SHIFTJ 2 0 #f) ; for jump-call
(define-inst MARK 0 0 #f) ; mark frame pointer on current closure (for shiftj)
(define-comment "// stack[0] = register")
(define-inst BNNUME 0 1 :label #t)
(define-inst BNLT 0 1 :label #t)
(define-inst BNLE 0 1 :label #t)
(define-inst BNGT 0 1 :label #t)
(define-inst BNGE 0 1 :label #t)
(define-inst BNEQ 0 1 :label #t)
(define-inst BNEQV 0 1 :label #t)
(define-inst BNNULL 0 1 :label #t)

(define-inst NOT 0 0 #f)

(define-inst NUM_EQ 0 0 #t)
(define-inst NUM_LT 0 0 #t)
(define-inst NUM_LE 0 0 #t)
(define-inst NUM_GT 0 0 #t)
(define-inst NUM_GE 0 0 #t)

(define-inst RECEIVE 2 0 #t)
(define-inst CLOSURE 0 1 #f)
(define-inst APPLY 0 0 #f)
(define-inst CALL 1 0 #t)
(define-inst LOCAL_CALL 1 0 #t)
(define-inst TAIL_CALL 1 0 #t)
(define-inst LOCAL_TAIL_CALL 1 0 #t)
(define-inst RET 0 0 #f)
(define-inst FRAME 0 1 :label #f)
(define-inst LET_FRAME 1 0 #t)
(define-insn POP_LET_FRAME 1 0 #f)
(define-insn DISPLAY 1 0 #f)
(define-insn ENTER 1 0 #f)
(define-insn LEAVE 0 0 #f)

(define-inst DEFINE 1 1 #t) ;"///< define variable")
(define-inst CAR 0 0 #t) ;"///< car")
(define-inst CDR 0 0 #t) ;"///< cdr")
(define-inst CONS 0 0 #t)
(define-inst LIST 1 0 #t)
(define-inst VALUES 1 0 #t)
(define-inst EQ 0 0 #t) ;"///< eq?")
(define-inst EQV 0 0 #t) ;"///< eqv?")
(define-inst NULLP 0 0 #t)

(define-inst VECTOR 1 0 #t)
(define-inst VECTORP 0 0 #t)
(define-inst VEC_LEN 0 0 #t)
(define-inst VEC_REF 0 0 #t)
(define-inst VEC_SET 0 0 #t)

(define-inst LREF_PUSH 1 0 #t)
(define-inst FREF_PUSH 1 0 #t)
(define-inst GREF_PUSH 0 1 #t)
(define-inst CONST_PUSH 0 1 #f) ;"///< const + push")
(define-inst CONSTI_PUSH 1 0 #f) ;"///< consti + push")
(define-inst GREF_CALL 1 1 #t)
(define-inst GREF_TAIL_CALL 1 1 #t)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
