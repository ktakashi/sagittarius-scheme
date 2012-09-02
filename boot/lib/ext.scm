;; need to move to C++
#!core
(cond-expand
 (sagittarius.scheme.vm
  (define (constant-literal? o) #f)
  )
 (gauche
  (define (constant-literal? o) #f)
  )
 (else #t))
(define (cond-features) '(sagittarius sagittarius.scheme.vm))

;; for dependency problem
(define identifier?
  (lambda (id)
    (and (vector? id)
	 (> (vector-length id) 1)
	 (eq? (vector-ref id 0) '.identifier))))

(define identifier->symbol
  (lambda (s)
    (if (symbol? s)
	s
	(id-name s))))

(define (id-name id)
  (vector-ref id 1))
(define (id-envs id)
  (vector-ref id 2))

(define (syntax-object=? a b) #f)

(define (unrename-symbol sym)
  (let loop ((lst (string->list (symbol->string sym)))
	     (r '()))
    (cond ((null? lst)
	   (string->symbol (list->string (reverse r))))
	  ((char=? (car lst) #\`)
	   (string->symbol (list->string (reverse r))))
	  (else
	   (loop (cdr lst) (cons (car lst) r))))))
    
(define (free-identifier=? id1 id2)
  (and (eq? (id-name id1) (id-name id2))
       (eq? (id-envs id1) (id-envs id2))))

;; duplicated
(define get-binding-frame
  (lambda (var env)
    (let loop ((frame env))
      (if (pair? frame)
	  (if (pair? (car frame))
	      (let loop2 ((fp (cdar frame)))
		(if (pair? fp)
		    (if (eq? (caar fp) var)
			frame
			(loop2 (cdr fp)))
		    (loop (cdr frame))))
	      (loop (cdr frame)))
	  '()))))

;; i don't think it's smart but ...
;;(define *usage-env* '())
;;(define *macro-env* '())

;; from chibi-scheme
(define identifier=?
  (lambda (e1 id1 e2 id2)
    (define (id-name id)
      (vector-ref id 1))
    (define (id-envs id)
      (vector-ref id 2))
    (or (eq? id1 id2) ;; shortcut
	;; strip p1env to frames
	(let ()
	  (set! e1 (if (vector? e1) (vector-ref e1 1) e1))
	  (set! e2 (if (vector? e2) (vector-ref e2 1) e2))
	  (let ((lam1 #f) (lam2 #f))
	    (when (identifier? id1)
	      (set! e1 (id-envs id1))   ;; this is only frames
	      (set! id1 (id-name id1))) ;; symbol name
	    (when (identifier? id2)
	      (set! e2 (id-envs id2))   ;; this is only frames
	      (set! id2 (id-name id2))) ;; symbol name
	    (cond ((get-binding-frame id1 e1)
		   => (lambda (cell)
			(unless (null? cell)
			  (set! lam1 cell)))))
	    (cond ((get-binding-frame id2 e2)
		   => (lambda (cell)
			(unless (null? cell)
			  (set! lam2 cell)))))
	    (and (eq? id1 id2)
		 (eq? lam1 lam2)))))))

(define variable?
  (lambda (o)
    (or (symbol? o)
	(identifier? o))))

(define (id-memq id lst)
    (if (identifier? id)
	(memq (id-name id) lst)
	(memq id lst)))

;; generics for stub
(define (make-generic name prn ctr . fields)
  (vector '.generic name prn ctr fields))

(define (register-generic name g lib)
  #f)

(define (create-instance g)
  (vector '.instance g (make-eq-hashtable)))

(define (generic-ref ins name)
  (hashtable-ref (vector-ref ins 2) name #f))

(define (generic-set! ins name value)
  (hashtable-set! (vector-ref ins 2) name value))

(define (retrieve-generic name)
  #f)

;; dummy dynamic-wind
(define (dynamic-wind b t a)
  (b)
  (let ((r (t)))
    (a)
    r))
;; simple list-transpose*
(define (list-transpose* . rest)
  (let ((len (length (car rest))))
    (let loop ((i 0)
	       (rest rest)
	       (r '()))
      (if (= i len)
	  (reverse r)
	  (loop (+ i 1)
		(let loop ((r '())
			   (p rest))
		  (if (null? p)
		      (reverse r)
		      (loop (cons (cdr (car p)) r) (cdr p))))
		(cons 
		 (let loop ((r '())
			   (p rest))
		  (if (null? p)
		      (reverse r)
		      (loop (cons (car (car p)) r) (cdr p))))
		 r))))))
;; for boot code, we don't have any different length list
(define list-transpose+ list-transpose*)

;; stub
(define (condition . components)
  components)
(define (make-assertion-violation)
  #(.assertion-violation))
(define (make-who-condition who)
  `#(.who-condition ,who))
(define (make-message-condition msg)
  `#(.message-condition ,msg))
(define (make-irritants-condition . irr)
  `#(.irritants-condition ,@irr))
(define (syntax-violation who msg . irr)
  (error who msg irr))

(define (raise e) 
  (if (pair? e)
      (let loop ((e e))
	(unless (null? e)
	  (display (car e))(newline)
	  (loop (cdr e))))
      (print e))
  (error 'raise "error has been raised"))

;; dummies
(define (make-record-type-descriptor . _) #t)
(define (make-record-constructor-descriptor . _) #t)
(define (make-record-type . _) #t)
(define (record-type-rcd . _) #t)
(define (record-type-rtd . _) #t)
(define (record-constructor . _) #t)
(define (record-predicate . _) #t)
(define (record-accessor . _) #t)