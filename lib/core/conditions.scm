;; -*- scheme -*-
#!core
(library (core conditions)

  (export define-condition-type
          condition simple-conditions condition?
          condition-predicate condition-accessor
          &condition
          &message make-message-condition message-condition? condition-message
          &warning make-warning warning?
          &serious make-serious-condition serious-condition?
          &error make-error error?
          &violation make-violation violation?
          &assertion make-assertion-violation assertion-violation?
          &irritants make-irritants-condition irritants-condition? condition-irritants
          &who make-who-condition who-condition? condition-who
          &non-continuable make-non-continuable-violation non-continuable-violation?
          &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
          &lexical make-lexical-violation lexical-violation?
          &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
          &undefined make-undefined-violation undefined-violation?
	  ;; &i/o
	  &i/o make-i/o-error i/o-error?
	  &i/o-read make-i/o-read-error i/o-read-error?
	  &i/o-write make-i/o-write-error i/o-write-error?
	  &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
	  &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
	  &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
	  &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
	  &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
	  &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
	  &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port
	  &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
	  &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char
	  )
  (import (core)
	  (core base)
          (core syntax)
	  (core record)
	  (core record procedural)
	  (core errors)
	  (clos core)
	  (sagittarius))

  ;; ok first initialise the conditions' meta class
  (define-syntax initialize-builtin-condition
    (syntax-rules ()
      ((_ class parent fields)
       (define dummy
	 (begin
	   (let* ((rtd (make <record-type-descriptor>
			 :name (class-name class) 
			 :parent (and parent (record-type-rtd parent))
			 :uid #f
			 :sealed? #f :opaque? #f
			 :fields fields :class class))
		  ;; we can use rcd :)
		  (rcd (make-record-constructor-descriptor rtd #f #f)))
	     (slot-set! class 'rtd rtd)
	     (slot-set! class 'rcd rcd)))))))

  (initialize-builtin-condition &condition #f #())
  (initialize-builtin-condition &message &condition #((immutable message)))
  (initialize-builtin-condition &who     &condition #((immutable who)))
  (initialize-builtin-condition &irritants &condition #((immutable irritants)))
  (initialize-builtin-condition &warning &condition #())
  (initialize-builtin-condition &serious &condition #())
  (initialize-builtin-condition &error   &serious #())
  (initialize-builtin-condition &violation &serious #())
  (initialize-builtin-condition &assertion &violation #())
  (initialize-builtin-condition &non-continuable &violation #())
  (initialize-builtin-condition &implementation-restriction &violation #())
  (initialize-builtin-condition &lexical &violation #())
  (initialize-builtin-condition &syntax  &violation #((immutable form) (immutable subform)))
  (initialize-builtin-condition &undefined &violation #())

  (initialize-builtin-condition &i/o &error #())
  (initialize-builtin-condition &i/o-read &i/o #())
  (initialize-builtin-condition &i/o-write &i/o #())
  (initialize-builtin-condition &i/o-invalid-position &i/o #((immutable position)))
  (initialize-builtin-condition &i/o-filename &i/o #((immutable filename)))
  (initialize-builtin-condition &i/o-file-protection &i/o-filename #())
  (initialize-builtin-condition &i/o-file-is-read-only &i/o-file-protection #())
  (initialize-builtin-condition &i/o-file-already-exists &i/o-filename #())
  (initialize-builtin-condition &i/o-file-does-not-exist &i/o-filename #())
  (initialize-builtin-condition &i/o-port &i/o #((immutable port)))
  (initialize-builtin-condition &i/o-encoding &i/o-port #((immutable char)))
  (initialize-builtin-condition &i/o-decoding &i/o-port #())

  (define (condition-predicate rtd)
    (let ((class (slot-ref rtd 'class)))
      (lambda (o)
	(cond ((simple-condition? o) (is-a? o class))
	      ((compound-condition? o)
	       (let loop ((cp (&compound-condition-components o)))
		 (cond ((null? cp) #f)
		       ((is-a? (car cp) class))
		       (else (loop (cdr cp))))))
	      (else #f)))))

  (define (condition-accessor rtd proc)
    (let ((class (slot-ref rtd 'class)))
      (lambda (o)
	(define (err)
	  (assertion-violation 
	   'condition-accessor
	   (format "expected condition of a subtype ~s" (class-name class))
	   o class))
	(cond ((and (simple-condition? o) (is-a? o class))
	       (proc o))
	      ((compound-condition? o)
	       (let loop ((cp (&compound-condition-components o)))
		 (cond ((null? cp) (err))
		       ((is-a? (car cp) class) (proc (car cp)))
		       (else (loop (cdr cp))))))
	      (else (err))))))

  (define condition-message 
    (condition-accessor (record-type-rtd &message) &message-message))
  (define condition-who 
    (condition-accessor (record-type-rtd &who) &who-who))
  (define condition-irritants
    (condition-accessor (record-type-rtd &irritants) &irritants-irritants))
  (define syntax-violation-form 
    (condition-accessor (record-type-rtd &syntax) &syntax-violation-form))
  (define syntax-violation-subform
    (condition-accessor (record-type-rtd &syntax) &syntax-violation-subform))
  (define i/o-error-position
    (condition-accessor (record-type-rtd &i/o-invalid-position)
			&i/o-invalid-position-position))
  (define i/o-error-filename
    (condition-accessor (record-type-rtd &i/o-filename) &i/o-filename-filename))
  (define i/o-error-port
    (condition-accessor (record-type-rtd &i/o-port) &i/o-port-port))
  (define i/o-encoding-error-char
    (condition-accessor (record-type-rtd &i/o-encoding) &i/o-encoding-char))

  (define-syntax define-condition-type
    (lambda (x)
      (syntax-case x ()
        ((_ condition-type supertype
            constructor predicate
            (cond-fields cond-accessors) ...)
         (with-syntax (((rec-accessors ...) (generate-temporaries (syntax (cond-fields ...)))))
           (syntax (begin
                     (define-record-type (condition-type constructor temp)
                       (parent supertype)
                       (fields (immutable cond-fields rec-accessors) ...))
                     (define predicate
                       (condition-predicate (record-type-descriptor condition-type)))
                     (define cond-accessors
                       (condition-accessor (record-type-descriptor condition-type) rec-accessors)) ...)))))))

  ) ;[end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
