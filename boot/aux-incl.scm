;; Workaround file to be merged into compiler-aux.scm in the future release

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
       ((- obj . clauses)
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

(define (add-backtrace c src) (make-trace-condition (truncate-program src)))

(define (format-source-info info)
  (if info
      (format "~s:~d" (car info) (cdr info))
      #f))

(define (truncate-program program)
  (if (circular-list? program)
      program
      (unwrap-syntax program)))

;; IFORM must be a $LAMBDA node. This expands the application of IFORM
;; on IARGS (list of IForm) into a mere $LET node.
;; used both pass1 and pass2
(define (expand-inlined-procedure src iform iargs)
  (let ((lvars ($lambda-lvars iform))
	(args (adjust-arglist src
			      ($lambda-args iform)
			      ($lambda-option iform)
			      iargs ($lambda-name iform))))
    (ifor-each2 (lambda (lv a) (lvar-initval-set! lv a)) lvars args)
    ($let src 'let lvars args ($lambda-body iform))))

;; Adjust argmuent list according to reqargs and optarg count.
;; Used in procedure inlining and local call optimization.
;; used both pass1 and pass2
(define (adjust-arglist src reqargs optarg iargs name)
  (unless (argcount-ok? iargs reqargs (> optarg 0))
    (raise (condition (make-compile-error
		       (format-source-info (source-info src))
		       (truncate-program src))
		      (make-who-condition name)
		      (make-message-condition 
		       (format 
			"wrong number of arguments: ~s requires ~a, but got ~a"
			name reqargs (length iargs))))))
  (if (zero? optarg)
      iargs
      (receive (reqs opts) (split-at iargs reqargs)
	(append! reqs (list ($list #f opts))))))

;; used both pass1 and pass2
(define (argcount-ok? args reqargs optarg?)
  (let ((nargs (length args)))
    (or (and (not optarg?) (= nargs reqargs))
	(and optarg? (>= nargs reqargs)))))

;; see if the given iform is referentially transparent. That is the iform is
;; side effect free, and alto the value of iform won't change even if we move
;; iform to a differnet place in the subtree.
(define (everyc proc lis c)             ;avoid closure allocation
  (or (null? lis)
      (let loop ((lis lis))
        (smatch lis
          ((x) (proc x c))
          ((x . xs) (and (proc x c) (loop xs)))))))
;; used both pass2 and pass3
(define (transparent? iform) (transparent?/rec iform (make-label-dic #f)))
(define (transparent?/rec iform labels)
  (case/unquote (iform-tag iform)
   (($LREF   ) (zero? (lvar-set-count ($lref-lvar iform))))
   (($GREF   ) (inlinable-binding? ($gref-id iform) #t))
   (($CONST $LAMBDA $IT $UNDEF) #t)
   (($IF     ) (and (transparent?/rec ($if-test iform) labels)
		    (transparent?/rec ($if-then iform) labels)
		    (transparent?/rec ($if-else iform) labels)))
   (($LET    ) (and (everyc transparent?/rec ($let-inits iform) labels)
		    (transparent?/rec ($let-body iform) labels)))
   (($LABEL  ) (or (label-seen? labels iform)
		   (begin (label-push! labels iform)
			  (transparent?/rec ($label-body iform) labels))))
   (($SEQ    ) (everyc transparent?/rec ($seq-body iform) labels))
   (($CALL   ) (and (no-side-effect-call? ($call-proc iform) ($call-args iform))
		    (everyc transparent?/rec ($call-args iform) labels)))
   (($ASM    ) (and (no-side-effect-insn? ($asm-insn iform) ($asm-args iform))
		    (everyc transparent?/rec ($asm-args iform) labels)))
   (($LIST   ) (everyc transparent?/rec ($list-args iform) labels))
   (($RECEIVE) (and (transparent?/rec ($receive-expr iform) labels)
		    (transparent?/rec ($receive-body iform) labels)))
   (else #f)))

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

