;; -*- scheme -*-
#!core
(library (core record)
    (export define-record-type
	    record-type-descriptor
	    record-constructor-descriptor

	    fields mutable immutable parent protocol sealed
	    opaque nongenerative parent-rtd)
    (import (core)
	    (core base)
	    (core syntax)
	    (sagittarius))

  (define-syntax mutable
    (lambda (x)
      (syntax-error 'mutable "invalid expression" x)))
  (define-syntax immutable
    (lambda (x)
      (syntax-error 'immutable "invalid expression" x)))
  (define-syntax parent
    (lambda (x)
      (syntax-error 'parent "invalid expression" x)))
  (define-syntax protocol
    (lambda (x)
      (syntax-error 'protocol "invalid expression" x)))
  (define-syntax parent-rtd
    (lambda (x)
      (syntax-error 'parent-rtd "invalid expression" x)))
  (define-syntax sealed
    (lambda (x)
      (syntax-error 'sealed "invalid expression" x)))
  (define-syntax opaque
    (lambda (x)
      (syntax-error 'opaque "invalid expression" x)))
  (define-syntax nongenerative
    (lambda (x)
      (syntax-error 'nongenerative "invalid expression" x)))
  (define-syntax fields
    (lambda (x)
      (syntax-error 'fields "invalid expression" x)))

  (define-syntax define-record-type
    (lambda (x)
      (let ((stash (make-eq-hashtable)))

        (define stash-set!
          (lambda (key value)
            (and (hashtable-ref stash key #f)
                 (syntax-violation 'define-record-type (format "duplicate ~a clause" key) x))
            (hashtable-set! stash key (list value))))

        (define stash-ref
          (lambda (key default)
            (cond ((hashtable-ref stash key #f) => car)
                  (else default))))

        (define parse-record-clauses
          (lambda (first-name record-clauses)
	    (define method-name
              (lambda (spec second-name)
                (datum->syntax 
		 second-name
		 (string->symbol (format spec first-name
					 (syntax->datum second-name))))))

            (for-each
             (lambda (c)
               (syntax-case c (parent protocol parent-rtd sealed opaque nongenerative fields)
                 ((parent e1)
		  (identifier? #'e1)
                  (stash-set! 'parent #'e1))
                 ((protocol e1)
                  (stash-set! 'protocol #'e1))
                 ((parent-rtd e1 e2)
                  (stash-set! 'parent-rtd (cons #'e1 #'e2)))
                 ((sealed e1)
                  (boolean? (syntax->datum #'e1))
                  (stash-set! 'sealed #'e1))
                 ((opaque e1)
                  (boolean? (syntax->datum #'e1))
                  (stash-set! 'opaque #'e1))
                 ((nongenerative e1)
                  (identifier? #'e1)
                  (stash-set! 'nongenerative #'e1))
                 ((nongenerative)
                  (stash-set! 'nongenerative #`,(string->symbol (format "non-generative-record-type ~a" (gensym)))))
                 ((fields specs ...)
                  (stash-set!
                   'fields
                   (map (lambda (spec)
			  (syntax-case spec (immutable mutable)
                            ((immutable name accessor)
                             (and (identifier? #'name) (identifier? #'accessor))
                             #'((immutable name) accessor #f))
                            ((mutable name accessor mutator)
                             (and (identifier? #'name) (identifier? #'accessor) 
				  (identifier? #'mutator))
                             #'((mutable name) accessor mutator))
                            ((immutable name)
                             (identifier? #'name)
                             (with-syntax ((proc (method-name "~a-~a" #'name)))
                               #'((immutable name) proc #f)))
                            ((mutable name)
                             (identifier? #'name)
                             (with-syntax
                                 ((proc1 (method-name "~a-~a" #'name))
                                  (proc2 (method-name "~a-~a-set!" #'name)))
                               #'((mutable name) proc1 proc2)))
                            (name
                             (identifier? #'name)
                             (with-syntax ((proc (method-name "~a-~a" #'name)))
                               #'((immutable name) proc #f)))
                            (_
                             (syntax-violation 'define-record-type
					       "malformed field spec" x spec))))
			#'(specs ...))))
                 (_ (syntax-violation 'define-record-type 
				      "malformed record clauses"
				      (syntax->datum x) 
				      (syntax->datum c)))))
             record-clauses)))

        (syntax-case x ()
          ((k (record-name constructor-name predicate-name) record-clauses ...)
	   (and (identifier? #'record-name) (identifier? #'constructor-name) (identifier? #'predicate-name))
           (begin
             (parse-record-clauses (syntax->datum #'record-name) #'(record-clauses ...))
             (and (stash-ref 'parent-rtd #f)
                  (stash-ref 'parent #f)
                  (syntax-violation 'define-record-type "definition have both parent and parent-rtd clause" x))
             (with-syntax
                 ((record-type
                   (with-syntax ((parent (stash-ref 'parent (syntax #f))))
                     (with-syntax ((rtd-parent (cond ((stash-ref 'parent-rtd #f) => car)
                                                     ((stash-ref 'parent #f) (syntax (record-type-rtd parent)))
                                                     (else (syntax #f))))
                                   (rcd-parent (cond ((stash-ref 'parent-rtd #f) => cdr)
                                                     ((stash-ref 'parent #f) (syntax (record-type-rcd parent)))
                                                     (else (syntax #f))))
                                   (uid (stash-ref 'nongenerative (syntax #f)))
                                   (sealed (stash-ref 'sealed (syntax #f)))
                                   (opaque (stash-ref 'opaque (syntax #f)))
                                   (protocol (stash-ref 'protocol (syntax #f)))
                                   (((fields _ _) ...) (datum->syntax #'k (stash-ref 'fields '()))))
                       (syntax (define record-name
                                 (let* ((rtd (make-record-type-descriptor 'record-name rtd-parent 'uid sealed opaque '#(fields ...)))
                                        (rcd (make-record-constructor-descriptor rtd rcd-parent protocol)))
                                   (make-record-type 'record-name rtd rcd)))))))
                  (constructor
                   (syntax (define constructor-name (record-constructor (record-type-rcd record-name)))))
                  (predicate
                   (syntax (define predicate-name (record-predicate (record-type-rtd record-name)))))
                  ((accessors ...)
                   (let ((index -1))
                     (filter values
                             (map (lambda (spec)
                                    (set! index (+ index 1))
                                    (and (cadr spec)
                                         (with-syntax ((name (datum->syntax #'record-name (cadr spec))) (n (datum->syntax #'k index)))
                                           (syntax (define name (record-accessor (record-type-rtd record-name) n))))))
                                  (stash-ref 'fields '())))))
                  ((mutators ...)
                   (let ((index -1))
                     (filter values
                             (map (lambda (spec)
                                    (set! index (+ index 1))
				    (and (caddr spec)
					 (with-syntax ((name (datum->syntax #'record-name (caddr spec))) (n (datum->syntax #'k index)))
					   (syntax (define name (record-mutator (record-type-rtd record-name) n))))))
                                  (stash-ref 'fields '()))))))
               (syntax (begin record-type constructor predicate accessors ... mutators ...)))))

          ((_ record-name record-clauses ...)
           (identifier? #'record-name)
           (let ((base-name (symbol->string (syntax->datum (syntax record-name)))))
             (with-syntax ((constructor-name (datum->syntax #'record-name (string->symbol (string-append "make-" base-name))))
                           (predicate-name (datum->syntax #'record-name (string->symbol (string-append base-name "?")))))
               (syntax (define-record-type (record-name constructor-name predicate-name) record-clauses ...)))))))))

  (define-syntax record-type-descriptor
    (lambda (x)
      (syntax-case x ()
        ((_ name) (identifier? #'name) (syntax (record-type-rtd name))))))

  (define-syntax record-constructor-descriptor
    (lambda (x)
      (syntax-case x ()
        ((_ name) (identifier? #'name) (syntax (record-type-rcd name))))))
  
)
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
