(import (sagittarius vm))
(define (print . args)
  (for-each (lambda (arg)
	      (display arg))
	    args)
  (newline))

(define record-printer
  (lambda (inst . port)
    (let ((p (if (null? port)
		 (current-output-port)
		 (car port)))
	  (rtd (generic-ref inst 'rtd)))
      (format p "#<record ~s ~a~a~a>"
	      (record-type-name rtd)
	      (if (record-type-opaque? rtd) "opaque " "")
	      (if (record-type-sealed? rtd) "sealed " "")
	      rtd))))

#|
(define &condition
  (let* ((rtd (make-record-type-descriptor '&condition #f #f #f #f '#()))
         (rcd (make-record-constructor-descriptor rtd #f #f)))
    (make-record-type '&condition rtd rcd)))

(define &warning
  (let ((rtd (make-record-type-descriptor
	      '&warning
	      (record-type-rtd &condition) #f #f #f '#((mutable x)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&warning rtd rcd))))

(define compound-condition?
  (lambda (obj)
    #f))

(define simple-condition?
  (lambda (obj)
    (and (record? obj)
         (rtd-ancestor? (record-type-rtd &condition) (record-rtd obj)))))


(define condition-predicate
  (lambda (rtd)
    (or (rtd-ancestor? (record-type-rtd &condition) rtd)
        (assertion-violation 'condition-predicate (format "expected record-type-descriptor of a subtype of &condition, but got ~r" rtd)))
    (lambda (obj)
      (cond ((simple-condition? obj)
             (rtd-ancestor? rtd (record-rtd obj)))
            ((compound-condition? obj)
             (any1 (lambda (component) (rtd-ancestor? rtd (record-rtd component)))
                   (compound-condition-component obj)))
            (else #f)))))


(define make-warning (record-constructor (record-type-rcd &warning)))
(define warning? (condition-predicate (record-type-rtd &warning)))
|#
;(display make-warning)
;(define warning (make-warning 1))
;(print warning)
;(print (warning? warning))
;(print (warning? 'a))
;(display &warning)(newline)
;(make-warning)
;(display (make-warning))

(let* ([&point (make-record-type-descriptor 'point #f #f #t #f
                                            '#((mutable x) (mutable y)))]
       [&point-cd (make-record-constructor-descriptor &point #f #f)]
       [make-point (record-constructor &point-cd)]
       [point? (record-predicate &point)]
       [point-x (record-accessor &point 0)]
       [point-y (record-accessor &point 1)]
       [point-x-set! (record-mutator &point 0)]
       [point-y-set! (record-mutator &point 1)]
       [p1 (make-point 1 2)])
  (print (and (point? p1)
	      (= (point-x p1) 1)
	      (= (point-y p1) 2)
	      (point-x-set! p1 5)
	      (= (point-x p1) 5))))

(let* ([&point (make-record-type-descriptor 'point #f #f #f #f
                                            '#((mutable x) (mutable y)))]
       [&point-cd/abs (make-record-constructor-descriptor
                       &point #f
                       (lambda (new)
                         (lambda (x y)
                           (new (abs x) (abs y)))))]
       [point-x (record-accessor &point 0)]
       [point-y (record-accessor &point 1)]
       [make-point/abs
        (record-constructor &point-cd/abs)])
  (print (and
	  (= (point-x (make-point/abs -1 -2)) 1)
	  (= (point-y (make-point/abs -1 -2)) 2))))

(let* ([&point (make-record-type-descriptor 'point #f #f #f #f
                                            '#((mutable x) (mutable y)))]
       [&point2 (make-record-type-descriptor 'point2 &point #f #f #f
                                             '#((mutable x) (mutable y)))]
       [make-point2 (record-constructor (make-record-constructor-descriptor &point2 #f #f))]
       [point? (record-predicate &point)]
       [point-x (record-accessor &point 0)]
       [point-y (record-accessor &point 1)]
       [point-x-set! (record-mutator &point 0)]
       [point-y-set! (record-mutator &point 1)]
       [point2? (record-predicate &point2)]
       [point2-xx (record-accessor &point2 0)]
       [point2-yy (record-accessor &point2 1)]
       [point2-xx-set! (record-mutator &point2 0)]
       [point2-yy-set! (record-mutator &point2 1)]
       [p2 (make-point2 1 2 3 4)])
(print
  (and  (point? p2)
	(point2? p2)
	(= (point-x p2) 1)
	(= (point-y p2) 2)
	(= (point2-xx p2) 3)
	(= (point2-yy p2) 4)
	(point-x-set! p2 5)
	(= (point-x p2) 5)
	(point-y-set! p2 6)
	(= (point-y p2) 6)
	(point2-xx-set! p2 7)
	(= (point2-xx p2) 7)
	(point2-yy-set! p2 8)
	(= (point2-yy p2) 8))
       )
)