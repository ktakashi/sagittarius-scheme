;; -*- scheme -*-

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
