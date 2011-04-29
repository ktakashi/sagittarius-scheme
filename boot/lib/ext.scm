;; need to move to C++
;; for dependency problem
(define identifier?
  (lambda (id)
    (and (vector? id)
	 (eq? (vector-ref id 0) '.identifier))))

(define identifier->symbol
  (lambda (s)
    (if (symbol? s)
	s
	(id-name s))))

;; from chibi-scheme
(define identifier=?
  (lambda (e1 id1 e2 id2)
    (define (id-name id)
      (vector-ref id 1))
    (define (id-envs id)
      (vector-ref id 2))
    ;; strip p1env to frames
    (set! e1 (vector-ref e1 1))
    (set! e2 (vector-ref e2 1))
    (let ((lam1 #f) (lam2 #f))
      (when (identifier? id1)
	(set! e1 (id-envs id1))   ;; this is only frames
	(set! id1 (id-name id1))) ;; symbol name
      (when (identifier? id2)
	(set! e2 (id-envs id2))   ;; this is only frames
	(set! id2 (id-name id2))) ;; symbol name
      (cond ((assq id1 e1)
	     => (lambda (cell)
		  (set! lam1 (cdr cell)))))
      (cond ((assq id2 e2)
	     => (lambda (cell)
		  (set! lam2 (cdr cell)))))
      (and (eq? id1 id2)
	   (eq? lam1 lam2)))))

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
