(import (sagittarius vm))
(define (print . args)
  (for-each (lambda (arg)
	      (display arg))
	    args)
  (newline))

(define make-nested-conser
  (lambda (desc rtd argc)
    (let ((generic (apply make-generic
			  (generic-ref rtd 'name)
			  (lambda (inst . port)
			    (let ((p (if (null? port)
					 (current-output-port)
					 (car port))))
			      (display "#<record " p)
			      (display (generic-ref rtd 'name))
			      (display ">")))
			  #f
			  (map cdr (generic-ref rtd 'fields)))))
      ((rcd-protocol desc)
       ((let loop ((desc desc))
	  (cond ((rcd-parent desc)
		 => (lambda (parent)
		      (lambda extra-field-values
			(lambda protocol-args
			  (lambda this-field-values
			    (apply ((rcd-protocol parent)
				    (apply (loop parent)
					   (append this-field-values extra-field-values)))
				   protocol-args))))))
		(else
		 (lambda extra-field-values
		   (lambda this-field-values
		     (let ((field-values (append this-field-values extra-field-values)))
		       (if (= (length field-values) argc)
			   (let ((fields (generic-ref rtd 'fields))
				 (record (create-instance generic)))
			     (for-each (lambda (n v)
					 (generic-set! record (cdr n) v))
				       fields field-values)
			     record)
			   (assertion-violation "record constructor" "wrong number of arguments" field-values)))))))))))))

(%insert-binding (find-library (string->symbol "(core base)") #f) 'make-nested-conser make-nested-conser)

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


(define make-warning (record-constructor (record-type-rcd &warning)))
;(define warning? (condition-predicate (record-type-rtd &warning)))

;(display make-warning)
(define warning (make-warning 1))
(print warning)
(print (generic-ref warning 'x))
;(display &warning)(newline)
;(make-warning)
;(display (make-warning))
