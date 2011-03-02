;; -*- Scheme -*-

;; from Gauche
;; pattern language compiler
;;   - convert literals into identifiers
;;   - recognize repeartable subpatterns and replace it to syntax-pattern node
;;   - convert free symbols in the template into identifiers
;;   - convert pattern variables into LREF object
(define-simple-struct pattern-context '.pattern-context #f
  name     ;; name of this macro (for error msg)
  form     ;; form being compiled (for error msg)
  literals ;; list of literal identifiers
  pvars    ;; list of (pvar . pvref)
  pvcount  ;; counter of pattern variables
  maxlevel ;; maximum level
  tvars    ;; list of identifiers inserted in template
  library  ;; libaray where this macro is defined
  env      ;; eompiler env of this macro definition
)

(define make-pattern-context
  (lambda (name literals library env)
    (vector '.pattern-context
	    name
	    #f      ;; form
	    literals
	    '()     ;; pvars
	    0       ;; pvcount
	    0       ;; maxlevel
	    '()     ;; tvars
	    library
	    env)))

(define pvcount++
  (lambda (ctx)
    (pattern-context-pvcount-set! ctx (+ (pattern-context-pvcount ctx) 1))))
(define maxlevel++
  (lambda (ctx)
    (pattern-context-maxlevel-set! ctx (+ (pattern-context-maxlevel ctx) 1))))

(define-simple-struct syntax-pattern '.syntax-pattern #f
  pattern  ;; subpattern
  vars     ;; pattern variables in this subpattern
  level    ;; level of this subpattern
  repeat   ;; does this subpattern repeat?
)
(define make-syntax-pattern
  (lambda (level repeat)
    (vector '.syntax-pattern '() '() level repeat)))
(define syntax-pattern?
  (lambda (o)
    (and (vector? o)
	 (eq? (vector-ref o 0) '.syntax-pattern))))


(define-simple-struct syntax-rule-branch '.syntax-rule-branch make-syntax-rule-branch
  pattern  ;; pattern to match
  template ;; template to be expanded
  numpvars ;; # of pattern variables
  maxlevel ;; maximum # of nested subpatterns
)

(define-simple-struct syntax-rules '.syntax-rules #f
  name         ;; name of the macro (for debug)
  numrules     ;; # of rules
  maxnumpvars  ;; max # of pattern variables
  rules        ;; variable length
)

(define make-syntax-rules
  (lambda (name nr maxnumpvars)
    (vector '.sytnax-rules name nr maxnumpvars '())))
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
