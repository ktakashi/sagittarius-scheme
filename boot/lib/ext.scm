;; need to move to C++
;; for dependency problem
(define identifier?
  (lambda (id)
    (and (vector? id)
	 (eq? (vector-ref id 0) '.identifier))))

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
