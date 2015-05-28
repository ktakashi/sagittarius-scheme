;; -*- scheme -*-
(import (rnrs)
	(rename (rnrs eval) (eval r6rs:eval))
	(rnrs mutable-pairs)
	(core errors)
	(srfi :64 testing))


(test-begin "R6RS+ functionality tests")
;; map
(test-equal "map with different length"
	    '((a . d) (b . e) (c . f))
	    (map cons '(a b c d e f) '(d e f)))
;; for-each
(test-equal "for-each with different length"
	    '((g . j) (h . k) (i . l))
	    (let* ((r `((a . d) (b . e) (c . f)))
		   (c r))
	      (guard (e (else (describe-condition e)))
		(for-each (lambda (a b)
			    (let ((t (car c)))
			      (set! c (cdr c))
			      (set-car! t a)
			      (set-cdr! t b)))
			  '(g h i j k l) '(j k l))
		r)))

(test-assert "string-ref fallback" (boolean? (string-ref "abc" 3 #f)))
(test-equal "string-copy" 
	    "bcdef"
	    (string-copy "abcdef" 1))
(test-equal "string-copy" 
	    "bcd"
	    (string-copy "abcdef" 1 4))

(define v '#(1 2 3 4 5 6))
(define l '(1 2 3 4 5 6))

;; fallback
(test-assert "vector fallback" (boolean? (vector-ref v 6 #f)))
(test-equal "vector->list with start" 
	    '(2 3 4 5 6)
	    (vector->list v 1))
(test-equal "vector->list with start and end" 
	    '(2 3)
	    (vector->list v 1 3))
(test-equal "list->vector with start"
	    '#(2 3 4 5 6)
	    (list->vector l 1))
(test-equal "list->vector with start and end"
	    '#(2 3)
	    (list->vector l 1 3))

(test-equal "vector-fill!"
	    '(#(1 1 1 #f #f #f #f #f #f #f)
	      #(#f #f #f 2 2 2 #f #f #f #f)
	      #(3 3 3 3 3 3 3 3 3 3))
	    (list (let ((v (make-vector 10 #f)))
		    (vector-fill! v 1 0 3)
		    v)
		  (let ((v (make-vector 10 #f)))
		    (vector-fill! v 2 3 6)
		    v)
		  (let ((v (make-vector 10 #f)))
		    (vector-fill! v 3)
		    v)
		  ))

(test-equal "bytevector-copy"
	    #vu8(2 3 4 5 6)
	    (bytevector-copy #vu8(1 2 3 4 5 6) 1))

(test-equal "bytevector-copy"
	    #vu8(2 3 4 5)
	    (bytevector-copy #vu8(1 2 3 4 5 6) 1 5))

;; macro defininion comes *after* the usage
;; this actually not a valid R6RS code
;; eval can't have define but we allow it
(test-equal "simple wrap" '(123)
	    (r6rs:eval '(begin
			  (define (fun) (mac))
			  (fun)
			  (let-syntax ((print (syntax-rules ()
						((_ o ...) (list o ...)))))
			    (define-syntax mac
			      (syntax-rules ()
				((_) (print 123))))))
		       (environment '(rnrs))))
(test-equal "let-syntax define" '(hoge)
	    (r6rs:eval '(begin
			  (define (fun) (foo))
			  (let-syntax ((print (syntax-rules ()
						((_ o ...) (list o ...)))))
			    (define-syntax mac
			      (syntax-rules ()
				((_)
				 (begin
				   (print 123)
				   (flush-output-port (current-output-port))))))
			    (define (foo)
			      (print 'hoge))
			    )
			  (fun))
		       (environment '(rnrs))))
(test-equal "simple wrap (letrec-syntax)" '(123)
	    (r6rs:eval '(begin
			  (define (fun) (mac))
			  (fun)
			  (letrec-syntax ((print (syntax-rules ()
						   ((_ o ...) (list o ...)))))
			    (define-syntax mac
			      (syntax-rules ()
				((_) (print 123))))))
		       (environment '(rnrs))))
(test-equal "let-syntax define (letrec-syntax)" '(hoge)
	    (r6rs:eval '(begin
			  (define (fun) (foo))
			  (letrec-syntax ((print (syntax-rules ()
						   ((_ o ...) (list o ...)))))
			    (define-syntax mac
			      (syntax-rules ()
				((_)
				 (begin
				   (print 123)
				   (flush-output-port (current-output-port))))))
			    (define (foo)
			      (print 'hoge))
			    )
			  (fun))
		       (environment '(rnrs))))

;; call #107
(let ()
  (define-record-type <foo>
    (fields a b))
  
  (define foo-rcd (make-record-constructor-descriptor 
		   (record-type-descriptor <foo>) #f
		   (lambda (p) (lambda () (p 1 2)))))
  
  (define (foo-fields foo) (list (<foo>-a foo) (<foo>-b foo)))
  (test-equal "multiple record constructor descriptor (1)"
	      '(a b)
	      (foo-fields
	       ((record-constructor (record-constructor-descriptor <foo>))
		'a 'b)))
  (test-equal "multiple record constructor descriptor (2)"
	      '(1 2)
	      (foo-fields ((record-constructor foo-rcd))))
  )

;; call #112
;; FIXME the test case should use approximate value
;; for now disabled
;;(test-equal "(cos 0+i)"  1.5430806348152437-0.0i  (cos 0+i))
;;(test-equal "(tan 0+i)"  0.0+0.761594155955765i   (tan 0+i))
;;(test-equal "(sin 0+i)"  0.0+1.1752011936438014i  (sin 0+i))
;;(test-equal "(sin 0+i)"  0.0+1.1752011936438014i  (sin 0+i))
;;(test-equal "(asin 0+i)" -0.0+0.8813735870195429i (asin 0+i))
;;(test-equal "(acos 0+i)"  1.5707963267948966-0.8813735870195429i (acos 0+i))
;;(test-equal "(atan 0+i)"  0.0+inf.0i                            (atan 0+i))
(test-error "(atan 0+i 0+i)" condition? (atan 0+i 0+i))
(test-error "(atan 0 0+i)" condition? (atan 0 0+i))
(test-error "(atan 0+i 0)" condition? (atan 0+i 0))

;; call #115
(test-error "u8-list->bytevector (non u8 list)" assertion-violation?
	    (u8-list->bytevector '(a)))
(test-error "u8-list->bytevector (error)" assertion-violation?
	    (u8-list->bytevector '(1 2 . 3)))

;; BOM for UTF16
;; it can only be done by using (utf-16-codec)
(test-equal "BOM" 
	    ;; internally, it will always big endign
	    #vu8(#xFE #xFF)
	    (string->bytevector "" (make-transcoder (utf-16-codec))))

(test-error "parent has custom protocol"
	    condition?
	    (eval
	     '(let ()
		(define-record-type this-parent
		  (fields count elements)
		  (protocol
		   (lambda (p)
		     (lambda (size)
		       (p size (make-vector size))))))
		;; error
		(define-record-type child
		  (fields attr)
		  (parent this-parent)))
	     (environment '(rnrs))))

;; From Larceny
(let ()
  (define (string~? s1 s2)
    (define (replacement? c)
      (char=? c #\xfffd))
    (define (canonicalized s)
      (let loop ((rchars (reverse (string->list s)))
		 (cchars '()))
	(cond ((or (null? rchars) (null? (cdr rchars)))
	       (list->string cchars))
	      ((and (replacement? (car rchars))
		    (replacement? (cadr rchars)))
	       (loop (cdr rchars) cchars))
	      (else
	       (loop (cdr rchars) (cons (car rchars) cchars))))))
    (string=? (canonicalized s1) (canonicalized s2)))
  (test-assert "utf-8, errors 1"
       (string~? (utf8->string '#vu8(#x61                             ; a
				     #xc0 #x62                        ; ?b
				     #xc1 #x63                        ; ?c
				     #xc2 #x64                        ; ?d
				     #x80 #x65                        ; ?e
				     #xc0 #xc0 #x66                   ; ??f
				     #xe0 #x67                        ; ?g
				     ))
		 "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e\xfffd;\xfffd;f\xfffd;g"))
  
  (test-assert "utf-8, errors 2"
       (string~? (utf8->string '#vu8(#xe0 #x80 #x80 #x68              ; ???h
				     #xe0 #xc0 #x80 #x69              ; ???i
				     #xf0 #x6a                        ; ?j
				     ))
		 "\xfffd;\xfffd;\xfffd;h\xfffd;\xfffd;\xfffd;i\xfffd;j"))

  (test-assert "utf-8, errors 3"
       (string~? (utf8->string '#vu8(#x61                             ; a
				     #xf0 #x80 #x80 #x80 #x62         ; ????b
				     #xf0 #x90 #x80 #x80 #x63         ; .c
				     ))
		 "a\xfffd;\xfffd;\xfffd;\xfffd;b\x10000;c"))

  (test-assert "utf-8, errors 4"
       (string~? (utf8->string '#vu8(#x61                             ; a
				     #xf0 #xbf #xbf #xbf #x64         ; .d
				     #xf0 #xbf #xbf #x65              ; ?e
				     #xf0 #xbf #x66                   ; ?f
				     ))
			 "a\x3ffff;d\xfffd;e\xfffd;f"))

  (test-assert "utf-8, errors 5"
       (string~? (utf8->string '#vu8(#x61                             ; a
				     #xf4 #x8f #xbf #xbf #x62         ; .b
				     #xf4 #x90 #x80 #x80 #x63         ; ????c
				     ))

			 "a\x10ffff;b\xfffd;\xfffd;\xfffd;\xfffd;c"))

  (test-assert "utf-8, errors 6"
       (string~? (utf8->string '#vu8(#x61                             ; a
				     #xf5 #x80 #x80 #x80 #x64         ; ????d
				     ))
		 "a\xfffd;\xfffd;\xfffd;\xfffd;d")))

(test-end)
