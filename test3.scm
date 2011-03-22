(import (sagittarius vm))
(define make-nested-conser
  (lambda (desc rtd argc)
    ((rcd-protocol desc)
     ((let loop ((desc desc))
        (cond ((rcd-parent desc)
               => (lambda (parent)
                    (lambda extra-field-values
		      (display 'hoge)
		      #t
                      #;(lambda protocol-args
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
                         (apply tuple rtd field-values)
                         (assertion-violation "record constructor" "wrong number of arguments" field-values))))))))))))

(%insert-binding (find-library (string->symbol "(core base)") #f) 'make-nested-conser make-nested-conser)

(define &condition
  (let* ((rtd (make-record-type-descriptor '&condition #f #f #f #f '#()))
         (rcd (make-record-constructor-descriptor rtd #f #f)))
    (make-record-type '&condition rtd rcd)))

(define &warning
  (let ((rtd (make-record-type-descriptor '&warning (record-type-rtd &condition) #f #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&warning rtd rcd))))

#;(define record-constructor
  (lambda (desc)
    (or (record-constructor-descriptor? desc)
        (assertion-violation 'record-constructor (wrong-type-argument-message "record-constructor-descriptor" desc)))
    (let ((rtd (rcd-rtd desc)))
      (if (rcd-parent desc)
          (make-nested-conser desc rtd (rtd-total-field-count rtd))
          (make-simple-conser desc rtd (length (rtd-fields rtd)))))))


(define make-warning (record-constructor (record-type-rcd &warning)))
;(define warning? (condition-predicate (record-type-rtd &warning)))

;(display make-warning)
(make-warning)
;(display &warning)(newline)
;(make-warning)
;(display (make-warning))
