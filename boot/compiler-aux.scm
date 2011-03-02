;; -*- Scheme -*-

;; not so smart solution...
(define ensure-library-name
  (lambda (tag)
    (case tag
      ((:null) 'null)
      ((:sagittarius) '(sagittarius))
      (else
       (error "invalid library tag:" tag)))))

;; for declare-procedure
(define (parse-type type)
  (if (and (pair? type)
	   (= (length type) 2))
      (cadr type)
      #f))

(define (parse-args vars)
  (let loop ((vars vars)
	     (n 0))
    (cond ((null? vars) (values n #f))
	  ((pair? vars) (loop (cdr vars) (+ n 1)))
	  (else (values n #t)))))

(cond-expand
 (gauche
  (use util.match)
   ;; from Gauche
   (define-macro (define-simple-struct name tag constructor . slot-defs)
     (define (take l n) ; we can't use srfi-1 take, so here it is.
       (if (zero? n)
	   '()
	   (cons (car l) (take (cdr l) (- n 1)))))
     (define (make-constructor)
       (let ((args (gensym))
	     (num-slots  (length slot-defs))
	     (slot-names (map (lambda (s) (if (symbol? s) s (car s))) slot-defs))
	     (init-vals  (map (lambda (s) (if (symbol? s) #f (cadr s))) slot-defs)))
	 `(define-macro (,constructor . ,args)
	    (match ,args
	      ,@(let loop ((n 0)
			   (r '()))
		  (if (> n num-slots)
		      r
		      (let ((carg (take slot-names n)))
			(loop (+ n 1)
			      (cons
			       `(,carg
				 (list 'vector
				       ,@(if tag `(',tag) '())
				       ,@carg
				       ,@(map (lambda (x) (list 'quote x))
					      (list-tail init-vals n))))
			       r)))
		      ))))
	 ))
     `(begin
	,@(if constructor
	      `(,(make-constructor))
	      '())
	,@(let loop ((s slot-defs) (i (if tag 1 0)) (r '()))
	    (if (null? s)
		(reverse r)
		(let* ((slot-name (if (pair? (car s)) (caar s) (car s)))
		       (acc (string->symbol (string-append (symbol->string name) "-" (symbol->string slot-name))))
		       (mod (string->symbol (string-append (symbol->string name) "-" (symbol->string slot-name) "-set!"))))
		  (loop (cdr s)
			(+ i 1)
			(cons
			 `(define-macro (,acc obj)
			    `(vector-ref ,obj ,,i))
			 (cons
			  `(define-macro (,mod obj val)
			     `(vector-set! ,obj ,,i ,val))
			  r)))))))
     )
   ;; For now I use define-simple-struct for macro
					;(load "macro/expander.scm")

   ;; define-enum <name> [<vals> ...]
   ;; create enum. the value starts from 0.
   ;; the result must be like this.
   ;; (defime-enum tag NAME1 NAME2)
   ;; -> 
   ;; (begin
   ;;   (define tag '((NAME1 . 0) (NAME2 . 1)))
   ;;   (define NAME1 0)
   ;;   (define NAME2 1))
   (define-macro (define-enum name . vals)
     (define make-tag-list
       (lambda (tags)
	 `(define ,name ',tags)))
     (define make-enum
       (lambda (vals)
	 (let ((len (length vals)))
	   (let loop ((i 0)
		      (vals vals)
		      (r '())
		      (tags '()))
	     (if (= i len)
		 (cons (make-tag-list (reverse tags)) (reverse r))
		 (begin
		   (loop (+ i 1)
			 (cdr vals)
			 (cons `(define ,(car vals) ,i) r)
			 (cons (cons (car vals) i) tags))))))))
     `(begin
	,@(make-enum vals)))

   (define-macro (case/unquote obj . clauses)
     (let1 tmp (gensym)
       (define (expand-clause clause)
	 (smatch clause
	   (((item) . body)
	    `((eqv? ,tmp ,item) ,@body))
	   (((item ___) . body)
	    (let1 ilist (list 'quasiquote
			      (map (cut list 'unquote <>) item))
	      `((memv ,tmp ,ilist) ,@body)))
	   (((? (lambda (x) (eq? 'else x)) -) . body)
	    `(else ,@body))))
       `(let ((,tmp ,obj))
	  (cond ,@(map expand-clause clauses)))))
   )
 ;; for sagittarius
  (sagittarius
   (define-syntax define-simple-struct
     (er-macro-transformer
      (lambda (form rename compare)
	(define (take l n)
	  (if (zero? n)
	      '()
	      (cons (car l) (take (cdr l) (- n 1)))))
	(define (make-constructor name tag constructor slot-defs)
	  (let ((args (gensym))
		(num-slots  (length slot-defs))
		(slot-names (map (lambda (s) (if (symbol? s) s (car s))) slot-defs))
		(init-vals  (map (lambda (s) (if (symbol? s) #f (cadr s))) slot-defs)))
	    `(define-syntax ,constructor
	       (syntax-rules ()
		 ,@(let loop ((n 0)
			      (r '()))
		     (if (> n num-slots)
			 r
			 (let ((carg (take slot-names n)))
			   (loop (+ n 1)
				 (cons `((_ ,@carg)
					 (vector
					  ,@(if tag `(,tag) '())
					  ,@carg
					  ,@(map (lambda (x) x)
						 (list-tail init-vals n))))
				       r)))
			 ))))
	    ))
	(smatch form
	  ((_ name tag constructor . slot-defs)
	   `(begin
	      ,@(if constructor
		    `(,(make-constructor name tag constructor slot-defs))
		    '())
	      ,@(let loop ((s slot-defs)
			   (i (if tag 1 0))
			   (r '()))
		  (if (null? s)
		      (reverse r)
		      (let* ((slot-name (if (pair? (car s)) (caar s) (car s)))
			     (acc (string->symbol (string-append (symbol->string name) "-" (symbol->string slot-name))))
			     (mod (string->symbol (string-append (symbol->string name) "-" (symbol->string slot-name) "-set!"))))
			(loop (cdr s)
			      (+ i 1)
			      (cons
			       `(define-syntax ,acc
				  (syntax-rules ()
				    ((_ obj)
				     (vector-ref obj ,i))))
			       (cons
				`(define-syntax ,mod
				   (syntax-rules ()
				     ((_ obj val)
				      (vector-set! obj ,i val))))
				r))))))))))))
   (define-syntax define-enum
     (er-macro-transformer
      (lambda (form rename compare)
	(define make-tag-list
	  (lambda (name tags)
	    `(define-constant ,name ',tags)))
	(define make-enum
	  (lambda (name vals)
	    (let ((len (length vals)))
	      (let loop ((i 0)
			 (vals vals)
			 (r '())
			 (tags '()))
		(if (= i len)
		    (cons (make-tag-list name (reverse tags)) (reverse r))
		    (begin
		      (loop (+ i 1)
			    (cdr vals)
			    (cons `(define-constant ,(car vals) ,i) r)
			    (cons (cons (car vals) i) tags))))))))
	(smatch form
	  ((_ name . vals)
	   `(begin
	      ,@(make-enum name vals)))))))

   (define-syntax case/unquote
     (er-macro-transformer
      (lambda (form rename compare)
	(smatch form
	  ((- obj . clauses)
	   (let ((tmp (gensym)))
	     (define (expand-clause clause)
	       (define else?
		 (lambda (x)
		   (eq? x 'else)))
	       (smatch clause
		 (((item) . body)
		  `((eqv? ,tmp ,item) ,@body))
		 (((item1 . more) . body)
		  (let ((ilist (list 'quasiquote
				     (append (list (list 'unquote item1))
					     (map (lambda (x) (list 'unquote x)) more)))))
		    `((memv ,tmp ,ilist) ,@body)))
		 ((else . body)
		  (or (else? else)
		      (error "invalid symbol test clause:" else))
		  `(else ,@body))))
	     `(let ((,tmp ,obj))
    		(cond ,@(map expand-clause clauses)))))))))

   ) ; sagittarius
)
;;;;
;; IForm
(define-enum .intermediate-tags.
  $UNDEF
  $DEFINE
  $LREF
  $LSET
  $GREF
  $GSET
  $CONST
  $IF
  $LET
  $LAMBDA
  $RECEIVE
  $LABEL
  $SEQ
  $CALL
  $ASM
  $IT
  $LIST)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
