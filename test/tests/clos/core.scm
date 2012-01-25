(library (tests clos core)
    (export run-clos-core-tests)
    (import (rnrs)
	    (clos core)
	    (srfi :64 testing))

  ;; from tiny CLOS tutorial
  (define <person> (make <class> 
		     :direct-supers (list <class>)
		     :direct-slots '(name age)
		     :definition-name '<person>))

  (add-method initialize
	      (make-method (list <person> <list>)
		   (lambda (person initargs call-next-method)
		     (slot-set! person 'name (car initargs))
		     (unless (null? (cdr initargs))
		       (slot-set! person 'age (cadr initargs))))))

  (define <student> (make <person>
		      :direct-supers (list <person>)
		      :direct-slots (list 'credits 'cours-list)
		      :definition-name '<student>))
  (define <student> (make-class 
		     (list <person>)
		     (list 'credits 'cours-list)))
  
  (define sam (make <person> "Sam" 38))

  (define (run-clos-core-tests)
    (test-equal 38 (slot-ref sam 'age))
    (test-equal 39 (begin (slot-set! sam 'age (+ (slot-ref sam 'age) 1))
			  (slot-ref sam 'age)))

    )
)