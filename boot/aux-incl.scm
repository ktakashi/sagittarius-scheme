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
	   (lis (caddr form)))
       `(let ((pr ,proc))
	  (let loop ((r '()) (p ,lis))
	    (if (null? p)
		(reverse! r)
		(loop (cons (pr (car p)) r) (cdr p)))))))))

(define-syntax imap2
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis1 (caddr form))
	   (lis2 (cadddr form)))
       `(let ((pr ,proc))
	  (let loop ((r '()) (p1 ,lis1) (p2 ,lis2))
	    (if (or (null? p1) (null? p2))
		(reverse! r)
		(loop (cons (pr (car p1) (car p2)) r)
		      (cdr p1) (cdr p2)))))))))

(define-syntax ifor-each
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis (caddr form)))
       `(let ((pr ,proc))
	  (let loop ((p ,lis))
	    (unless (null? p)
	      (pr (car p))
	      (loop (cdr p)))))))))

(define-syntax ifor-each2
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis1 (caddr form))
	   (lis2 (cadddr form)))
       `(let ((pr ,proc))
	  (let loop ((p1 ,lis1) (p2 ,lis2))
	    (unless (or (null? p1) (null? p2))
	      (pr (car p1) (car p2))
	      (loop (cdr p1) (cdr p2)))))))))

(define-syntax ifold
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (seed (caddr form))
	   (lis (cadddr form)))
       `(let ((pr ,proc))
	  (let loop ((p1 ,lis) (knil ,seed))
	    (if (null? p1)
		knil
		(loop (cdr p1) (pr (car p1) knil)))))))))

(define-syntax ifilter-map
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((proc (cadr form))
	   (lis (caddr form)))
       `(let ((p ,proc))
	  (let loop ((l ,lis) (r '()))
	    (if (null? l)
		(reverse! r)
		(cond ((p (car l)) => (lambda (x) (loop (cdr l) (cons x r))))
		      (else (loop (cdr l) r))))))))))

(define-syntax iany
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((pred (cadr form))
	   (lis (caddr form)))
       `(let ((p ,pred))
	  (let loop ((l ,lis))
	    (cond ((null? l) #f)
		  ((p (car l)))
		  (else (loop (cdr l))))))))))

(define-syntax $vm-warn
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((fmt (cadr form))
	   (args (cddr form)))
     `(when (vm-log-level 'warn)
       (let ((msg (format/ss ,fmt ,@args)))
	 (vm-warn msg)))))))
