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
