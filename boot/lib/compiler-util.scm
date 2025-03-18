;; -*- Scheme -*-
;;#!nounbound
(library (sagittarius compiler util)
    (export ensure-library-name
	    *history* history history!
	    imap imap2 ifor-each ifor-each2 ifold ifilter-map
	    iany $append-map1 $vm-warn

	    case/unquote define-simple-struct
	    
	    generate-dispatch-table

	    argcount-ok? constant-folding-warning

	    id->bound-gloc guard add-backtrace format-source-info
	    truncate-program else =>

	    make-label-dic
	    copy-label-dic
	    label-seen?
	    label-push!
	    label-dic-info
	    label-dic-info-set!
	    label-dic-info-push!)
    (import (core)
	    (core base)
	    (core errors)
	    (sagittarius)
	    (for (compat r7rs) expand)
	    (for (rename (match) (match smatch)) expand)
	    (sagittarius vm)
	    (sagittarius vm debug))

(define ensure-library-name
  (lambda (tag)
    (case tag
      ((:null) '(core))
      ((:sagittarius) '(sagittarius))
      ((:base) '(core base))
      ;; only 'program' to run R6RS script
      ((:r6rs-script) '(r6rs-script))
      (else
       (error 'ensure-library-name "invalid library tag:" tag)))))

(define *history* (make-core-parameter '()))
(define (history o) (assq o (*history*)))
(define (history! n o)
  (let ((o (cond ((assq o (*history*)) => cdr) (else o))))
    (*history* (acons n o (*history*)))
    n))

;; utility macros
(define-syntax imap
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis (caddr form))
	   (loop (gensym)))
       `(let ,loop ((r '()) (p ,lis))
	  (if (null? p)
	      (reverse! r)
	      (,loop (cons (,proc (car p)) r) (cdr p))))))))

(define-syntax imap2
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis1 (caddr form))
	   (lis2 (cadddr form))
	   (loop (gensym)))
       `(let ,loop ((r '()) (p1 ,lis1) (p2 ,lis2))
	  (if (or (null? p1) (null? p2))
	      (reverse! r)
	      (,loop (cons (,proc (car p1) (car p2)) r)
		    (cdr p1) (cdr p2))))))))

(define-syntax ifor-each
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis (caddr form))
	   (loop (gensym)))
       `(let ,loop ((p ,lis))
	  (unless (null? p)
	    (,proc (car p))
	    (,loop (cdr p))))))))

(define-syntax ifor-each2
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis1 (caddr form))
	   (lis2 (cadddr form))
	   (loop (gensym)))
       `(let ,loop ((p1 ,lis1) (p2 ,lis2))
	  (unless (or (null? p1) (null? p2))
	    (,proc (car p1) (car p2))
	    (,loop (cdr p1) (cdr p2))))))))

(define-syntax ifold
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (seed (caddr form))
	   (lis  (cadddr form))
	   (loop (gensym)))
       `(let ((pr ,proc))
	  (let ,loop ((p1 ,lis) (knil ,seed))
	    (if (null? p1)
		knil
		(,loop (cdr p1) (pr (car p1) knil)))))))))

(define-syntax ifilter-map
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis  (caddr form))
	   (loop (gensym)))
       `(let ((p ,proc))
	  (let ,loop ((l ,lis) (r '()))
	    (if (null? l)
		(reverse! r)
		(cond ((p (car l)) => (lambda (x) (,loop (cdr l) (cons x r))))
		      (else (,loop (cdr l) r))))))))))

(define-syntax iany
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((pred (cadr form))
	   (lis  (caddr form))
	   (loop (gensym)))
       `(let ((p ,pred))
	  (let ,loop ((l ,lis))
	    (cond ((null? l) #f)
		  ((p (car l)))
		  (else (,loop (cdr l))))))))))

(define-syntax $append-map1
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((f (cadr form))
	   (l (caddr form)))
       `(apply append (imap ,f ,l))))))

(define-syntax $vm-warn
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((fmt (cadr form))
	   (args (cddr form)))
     `(when (vm-log-level 'warn)
       (let ((msg (format/ss ,fmt ,@args)))
	 (vm-warn msg)))))))

(define-syntax case/unquote
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((_ obj . clauses)
	(let ((tmp (gensym)))
	  (define (expand-clause clause)
	    (define else?
	      (lambda (x)
		(let ((name (if (identifier? x) (id-name x) x)))
		  (eq? name 'else))))
	    (let ()
	      (smatch clause
		(((item) . body)
		 `((eqv? ,tmp ,item) ,@body))
		(((item1 . more) . body)
		 (let ((ilist (list
			       'quasiquote
			       (append (list (list 'unquote item1))
				       (imap (lambda (x) (list 'unquote x)) 
					     more)))))
		   `((memv ,tmp ,ilist) ,@body)))
		((else . body)
		 (or (else? else)
		     (syntax-error "invalid symbol test clause"
				   (unwrap-syntax clause)))
		 `(else ,@body)))))
	  `(let ((,tmp ,obj))
	     (cond ,@(imap expand-clause clauses)))))))))

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
	     (slot-names (imap (lambda (s) (if (symbol? s) s (car s)))
			       slot-defs))
	     (init-vals  (imap (lambda (s) (if (symbol? s) #f (cadr s)))
			       slot-defs)))
	 `(define-syntax ,constructor
	    (syntax-rules ()
	      ,@(let loop ((n 0) (r '()))
		  (if (> n num-slots)
		      r
		      (let ((carg (take slot-names n)))
			(loop (+ n 1)
			      (cons `((_ ,@carg)
				      (vector
				       ,@(if tag `(,tag) '())
				       ,@carg
				       ,@(imap (lambda (x) x)
					       (list-tail init-vals n))))
				    r)))))))))
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
		   (reverse! r)
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

(define-syntax generate-dispatch-table
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((_ prefix)
	`(vector ,@(imap (lambda (p)
			   (string->symbol (string-append
					    (symbol->string prefix) "/"
					    (symbol->string (car p)))))
			 .intermediate-tags.)))))))

;; used both pass1 and pass2
(define (id->bound-gloc id)
  (let ((gloc (find-binding (id-library id) (id-name id) #f)))
    (and gloc (gloc-bound? gloc) gloc)))

;; to avoid unneccessary stack trace, we use guard.
;; this is not the same as the one in exceptions.scm
;; this does not use call/cc
(define-syntax guard
  (syntax-rules ()
    ((_ (var . clauses) . body)
     (with-error-handler
       (lambda (e)
	 (let ((var e))
	   (%guard-rec var e . clauses)))
       (lambda () . body) #t))))

(define-syntax %guard-rec
  (syntax-rules (else =>)
    ((%guard-rec var exc)
     (raise exc))
    ((%guard-rec var exc (else . exprs))
     (begin . exprs))
    ((%guard-rec var exc (test => proc) . more)
     (let ((tmp test))
       (if tmp
	   (proc tmp)
	   (%guard-rec var exc . more))))
    ((%guard-rec var exc (test . exprs) . more)
     (if test
	 (begin . exprs)
	 (%guard-rec var exc . more)))
    ((%guard-rec var exc other . more)
     (syntax-error "malformed guard clause" other))))

(define (add-backtrace c src) (make-trace-condition src))

(define (format-source-info info)
  (if info
      (format "~s:~d" (car info) (cdr info))
      #f))

(define (truncate-program program)
  (if (circular-list? program)
      program
      (unwrap-syntax program)))

;; used both pass1 and pass2
(define (argcount-ok? args reqargs optarg?)
  (let ((nargs (length args)))
    (or (and (not optarg?) (= nargs reqargs))
	(and optarg? (>= nargs reqargs)))))

;; args must be a list of $const node.
;; used both pass2 and pass3
(define (constant-folding-warning src who args)
  ($vm-warn "~s: gave up constant folding with given argument(s)."
	    ;; should be fine right?
	    (if (circular-list? src) src (unwrap-syntax src))
	    #;`(,(if (symbol? who)
		   who
		   (string->symbol (format "~a" who)))
	      ,@(imap $const-value args)))
  ;; for convenience
  #f)

;; label dictionary
(define (make-label-dic init)  (list init))
(define (copy-label-dic label) (cons (car label) (cdr label)))
(define (label-seen? label-dic label-node)
  (memq label-node (cdr label-dic)))
(define (label-push! label-dic label-node)
  (set-cdr! label-dic (cons label-node (cdr label-dic))))
(define (label-dic-info label-dic) (car label-dic))
(define (label-dic-info-set! label-dic val) (set-car! label-dic val))
(define (label-dic-info-push! label-dic val)
  (set-car! label-dic (cons val (car label-dic))))
)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
