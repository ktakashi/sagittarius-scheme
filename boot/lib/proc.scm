;; -*- Scheme -*-
;; procedure
;; procedure might be inline assembler such as CAR, CDR etc.
;; this structure must have both procedure definition and 
;; insn.
;; proporties:
;;   name      -  procedure name. mostly for debug i guess...
;;   type      -  if it has insn it has (inline) or #f
;;   insn      -  insn
;;   optional  -  if this procedure takes optional arguments
;;   body      -  procedure body.

(define (make-procedure name inliner reqargs optional body)
  (vector '.procedure name inliner reqargs optional body))

(define (procedure-name p)
  (vector-ref p 1))
(define (procedure-inliner p)
  (vector-ref p 2))
(define (procedure-inliner-set! p inliner)
  (vector-set! p 2 inliner))
(define (procedure-reqargs p)
  (vector-ref p 3))
(define (procedure-optional p)
  (vector-ref p 4))
(define (procedure-body p)
  (vector-ref p 5))

(define (procedure? p)
  (and (vector? p)
       (eq? (vector-ref p 0) '.procedure)))

(define (inline? p)
  (and (procedure? p)
       ;; TODO add no-inline flag here
       (procedure-inliner p)))

(define (find-procedure name lib)
  (find-binding lib name #f))

(cond-expand
 (gauche
  (define-macro (declare-procedure name args type library proc)
    (let ((lib (ensure-library-name library)))
      (receive (reqargs opt?) (parse-args args)
	(let ((inline (parse-type type)))
	  (let ((proc `(make-procedure ',name ,inline ,reqargs ,opt? ,proc)))
	    `(%insert-binding ',lib ',name
			      ,proc)))))))
 (sagittarius
  (define-syntax declare-procedure
    (er-macro-transformer
     (lambda (form rename compare)
       (smatch form
	 ((- name args type library proc)
	  (let ((lib (ensure-library-name library)))
	    (receive (reqargs opt?) (parse-args args)
	      (let ((inline (parse-type type)))
		(let ((proc `(make-procedure ',name ,inline ,reqargs ,opt? ,proc)))
		  `(%insert-binding ',lib ',name
				    ,proc))))))))))))

(declare-procedure + objs (:inline ADD) :null
  +)
(declare-procedure +. objs (:inline ADDI) :null
  +.)

(declare-procedure - (obj1 . rest) (:inline SUB) :null
  -)
(declare-procedure -. (obj1 . rest) (:inline SUBI) :null
  -.)

(declare-procedure * objs (:inline MUL) :null
  *)
(declare-procedure *. objs (:inline MULI) :null
  *.)

(declare-procedure / (obj1 . rest) (:inline DIV) :null
  /)
(declare-procedure /. (obj1 . rest) (:inline DIVI) :null
  /.)

(declare-procedure list objs (:inline LIST) :null
  (lambda a a))
(declare-procedure append a (:inline APPEND) :null append)

(declare-procedure eq?   (a b) (:inline EQ) :null eq?)
(declare-procedure eqv?  (a b) (:inline EQV) :null eqv?)

(declare-procedure =  (a b . rest) (:inline NUM_EQ) :null =)
(declare-procedure <  (a b . rest) (:inline NUM_LT) :null <)
(declare-procedure <= (a b . rest) (:inline NUM_LE) :null <=)
(declare-procedure >  (a b . rest) (:inline NUM_GT) :null >)
(declare-procedure >= (a b . rest) (:inline NUM_GE) :null >=)
(declare-procedure zero? (n) (:inline -1) :null zero?)

;; list procedure
(declare-procedure car (l) (:inline CAR) :null car)
(declare-procedure cdr (l) (:inline CDR) :null cdr)
(declare-procedure caar (l) (:inline CAAR) :null car)
(declare-procedure cadr (l) (:inline CADR) :null cadr)
(declare-procedure cdar (l) (:inline CDAR) :null cdar)
(declare-procedure cddr (l) (:inline CDDR) :null cddr)
(declare-procedure cons (a b) (:inline CONS) :null cons)
(declare-procedure acons (a b c) (:inline -1) :sagittarius acons)
(declare-procedure set-car! (a b) (:inline SET_CAR) :null set-car!)
(declare-procedure set-cdr! (a b) (:inline SET_CDR) :null set-cdr!)

;; conditions
(declare-procedure null? (x) (:inline NULLP) :null null?)
(declare-procedure pair? (x) (:inline PAIRP) :null pair?)
(declare-procedure symbol? (x) (:inline SYMBOLP) :null symbol?)
(declare-procedure not (x) (:inline NOT) :null not)

;; vector
(declare-procedure vector rest (:inline VECTOR) :null vector)
(declare-procedure vector? (x) (:inline VECTORP) :null vector?)
(declare-procedure vector-length (x) (:inline VEC_LEN) :null vector-length)
(declare-procedure vector-ref (v i) (:inline -1) :null vector-ref)
(declare-procedure vector-set! (vec i v) (:inline -1) :null vector-set!)
;; values
(declare-procedure values rest (:inline VALUES) :null values)

(declare-procedure apply (a . b) (:inline APPLY) :null apply)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
