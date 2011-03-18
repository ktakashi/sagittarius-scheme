(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

;;;;;
;; arithmetic
(define gcd
  (lambda args
    (define recn
      (lambda (arg args)
	(if (null? args)
	    arg
	    (recn (%gcd arg (car args)) (cdr args)))))
    (let ((args (map (lambda (arg)
		       (unless (integer? arg)
			 (error 'gcd "integer required, but got" arg))
		       (abs arg))
		     args)))
      (cond ((null? args) 0)
	    ((null? (cdr args)) (car args))
	    (else (recn (car args) (cdr args)))))))

(define lcm
  (lambda args
    (define lcm2
      (lambda (u v)
	(let ((g (%gcd u v)))
	  (if (zero? u) 0 (* (quotient u g) v)))))
    (define recn
      (lambda (arg args)
	(if (null? args)
	    arg
	    (recn (lcm2 arg (car args)) (cdr args)))))
    (let ((args (map (lambda (arg)
		       (unless (integer? arg)
			 (error 'lcm "integer required, but got" arg))
		       (abs arg))
		     args)))
      (cond ((null? args) 1)
	    ((null? (cdr args)) (car args))
	    (else (recn (car args) (cdr args)))))))

(define (div-and-mod x y)
  (let ((d (div x y))
	(m (mod x y)))
    (values d m)))

(define (div0-and-mod0 x y)
  (let ((d0 (div0 x y))
	(m0 (mod0 x y)))
    (values d0 m0)))

(define (list-tail ls k)
  (if (eq? k 0)
      ls
      (list-tail (cdr ls) (- k 1))))

(define (hashtable-for-each proc ht)
  (for-each proc (hashtable-keys ht) (hashtable-values ht)))

(define (any pred ls)
  (if (pair? ls) (if (pred (car ls)) (car ls) (any pred (cdr ls))) #f))

(define (call-with-values producer consumer)
  (receive vals (producer) (apply consumer vals)))

;; er-macro-transformer
(define er-macro-transformer
  (lambda (f)
    (lambda (expr)
      (let ((dict (make-eq-hashtable)))
	(define (rename s) (er-rename s (cdr expr) dict))
	(define (compare a b)
	  (or (eq? a b)
	      (cond ((and (symbol? a)
			  (identifier? b))
		     (eq? (rename a) b))
		    ((and (identifier? a)
			  (symbol? b))
		     (eq? a (rename b)))
		    (else #f))))
	(f (car expr) rename compare)))))

;; from chibi scheme
(define (map-onto proc ls init)
  (let lp ((ls ls) (res init))
    (if (null? ls) res (lp (cdr ls) (cons (proc (car ls)) res)))))

(define (fold kons knil ls . lists)
  (if (null? lists)
      (let lp ((ls ls) (acc knil))
        (if (pair? ls) (lp (cdr ls) (kons (car ls) acc)) acc))
      (let lp ((lists (cons ls lists)) (acc knil))
        (if (every pair? lists)
            (lp (map cdr lists) (apply kons (map-onto car lists (list acc))))
            acc))))

;; from Ypsilon
(define wrong-type-argument-message
  (lambda (expect got . nth)
    (if (null? nth)
        (format "expected ~a, but got ~a" expect got)
        (format "expected ~a, but got ~a, as argument ~a" expect got (car nth)))))

(define map
  (lambda (proc lst1 . lst2)
    (define map-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
               (cons (proc (car lst))
                     (map-1 proc (cdr lst)))))))
    (define map-n
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
               (cons (apply proc (car lst))
                     (map-n proc (cdr lst)))))))
    (if (null? lst2)
        (if (list? lst1)
            (map-1 proc lst1)
            (assertion-violation 'map (wrong-type-argument-message "proper list" lst1 2) (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (map-n proc lst)))
              (else
               (assertion-violation 'map "expected same length proper lists" (cons* proc lst1 lst2)))))))

(define for-each
  (lambda (proc lst1 . lst2)
    (define for-each-1
      (lambda (proc lst)
	(if (null? lst)
	    (undefined)
	    (begin
	      (proc (car lst))
	      (for-each-1 proc (cdr lst))))))
    (define for-each-n
      (lambda (proc lst)
	(cond ((null? lst) (undefined))
	      (else
	       (apply proc (car lst))
	       (for-each-n proc (cdr lst))))))
    (if (null? lst2)
        (if (list? lst1)
            (for-each-1 proc lst1)
            (assertion-violation 'for-each (wrong-type-argument-message "proper list" lst1 2) (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (for-each-n proc lst)))
              (else
               (assertion-violation 'for-each "expected same length proper lists" (cons* proc lst1 lst2)))))))

(define fold-left
  (lambda (proc seed lst1 . lst2)
    ;; shift down
    (define fold-left-1
      (lambda (proc seed lst)
	(cond ((null? lst) seed)
	      (else
	       (fold-left-1 proc (proc seed (car lst)) (cdr lst))))))
    
    (define fold-left-n
      (lambda (proc seed lst)
	(cond ((null? lst) seed)
	      (else
	       (fold-left-n proc (apply proc (append (list seed) (car lst))) (cdr lst))))))

    (if (null? lst2)
        (if (list? lst1)
            (fold-left-1 proc seed lst1)
            (assertion-violation 'fold-left (format "expected proper list, but got ~r, as argument 3" lst1) (cons* proc seed lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (fold-left-n proc seed lst)))
              (else
               (assertion-violation 'fold-left "expected same length proper lists" (cons* proc seed lst1 lst2)))))))

(define fold-right
  (lambda (proc seed lst1 . lst2)
    ;; shift down
    (define fold-right-1
      (lambda (proc seed lst)
	(cond ((null? lst) seed)
	      (else
	       (proc (car lst) (fold-right-1 proc seed (cdr lst)))))))
    
    (define fold-right-n
      (lambda (proc seed lst)
	(cond ((null? lst) seed)
	      (else
	       (apply proc (append (car lst) (list (fold-right-n proc seed (cdr lst)))))))))

    (if (null? lst2)
        (if (list? lst1)
            (fold-right-1 proc seed lst1)
            (assertion-violation 'fold-right (format "expected proper list, but got ~r, as argument 3" lst1) (cons* proc seed lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (fold-right-n proc seed lst)))
              (else
               (assertion-violation 'fold-right "expected same length proper lists" (cons* proc seed lst1 lst2)))))))

(define (vector-map proc vec1 . vec2)
  (list->vector
   (apply map proc (vector->list vec1)
	  (map vector->list vec2))))

(define (vector-for-each proc vec1 . vec2)
  (apply for-each proc (vector->list vec1)
	 (map vector->list vec2)))

(define (string-for-each proc str1 . str2)
  (apply for-each proc (string->list str1)
	 (map string->list str2)))

;;;;
;; ports

;; string ports
(define open-string-output-port
  (lambda ()
    (let* ((port (open-output-string))
	   (proc (lambda () (let ((s (get-output-string port)))
			      (set-port-position! port 0)
			      s))))
      (values port proc))))

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
