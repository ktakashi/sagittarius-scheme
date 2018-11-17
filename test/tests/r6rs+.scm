;; -*- scheme -*-
(import (rnrs)
	(rnrs eval)
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
	    (eval '(begin
		     (define (fun) (mac))
		     (fun)
		     (let-syntax ((print (syntax-rules ()
					   ((_ o ...) (list o ...)))))
		       (define-syntax mac
			 (syntax-rules ()
			   ((_) (print 123))))))
		  (environment '(rnrs))))
(test-equal "let-syntax define" '(hoge)
	    (eval '(begin
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
	    (eval '(begin
		     (define (fun) (mac))
		     (fun)
		     (letrec-syntax ((print (syntax-rules ()
					      ((_ o ...) (list o ...)))))
		       (define-syntax mac
			 (syntax-rules ()
			   ((_) (print 123))))))
		  (environment '(rnrs))))
(test-equal "let-syntax define (letrec-syntax)" '(hoge)
	    (eval '(begin
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

(let ()
  (define buf-size 10)
  (define bv (make-bytevector buf-size (char->integer #\a)))
  (define (bytevector-append . bvs)
    (let* ((len (fold-left (lambda (sum bv) 
			     (+ (bytevector-length bv) sum)) 0 bvs))
	   (r (make-bytevector len)))
      (fold-left (lambda (off bv)
		   (let ((len (bytevector-length bv)))
		     (bytevector-copy! bv 0 r off len)
		     (+ off len)))
		 0 bvs)
      r))

  (let ((bv2 (bytevector-append bv #vu8(#xe0 #x67 #x0a))))
    (call-with-port (transcoded-port 
		     (open-bytevector-input-port bv2) 
		     (make-transcoder (utf-8-codec)
				      (eol-style lf)
				      (error-handling-mode replace)))
       (lambda (in)
	 (get-string-n in (+ 1 buf-size))
	 (test-equal "read string after error code" "g\n"
		     (get-string-all in))))
    (call-with-port (transcoded-port 
		     (open-bytevector-input-port #vu8(#xe0 #x67 #x0a))
		     (make-transcoder (utf-8-codec)
				      (eol-style lf)
				      (error-handling-mode replace)))
       (lambda (in)
	 (get-char in)
	 (test-equal "read char after error code" #\g (get-char in))))))

;; call #127
;; in $lref
(test-error "letrec (1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar bar))
		       bar))
		  (environment '(rnrs))))
;; in $asm
(test-error "letrec (2)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (cons 'a bar)))
		       bar))
		  (environment '(rnrs))))
;; in $call
(test-error "letrec (3)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (for-all (lambda (a) a) bar)))
		       bar))
		  (environment '(rnrs))))

;; in $let
(test-error "letrec (4)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (let ((bar2 bar)) bar2)))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec (4.1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (let ((bar2 'a)) bar)))
		       bar))
		  (environment '(rnrs))))
;; in $let ok
(test-assert "letrec (5)"
	     (eval '(lambda (bar)
		      (letrec ((bar (let ((bar 'foo)) bar)))
			bar))
		   (environment '(rnrs))))

;; in $receive
(test-error "letrec (6)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (receive (bar) (values bar) bar)))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec (6.1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (receive (buz) (values a) bar)))
		       bar))
		  (environment '(rnrs))))
;; in $receive ok
(test-error "letrec (7)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (receive (bar) (values 'a) bar)))
		       bar))
		  (environment '(rnrs))))

;; in $seq
(test-error "letrec (8)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (begin bar #t)))
		       bar))
		  (environment '(rnrs))))
;; in $list
(test-error "letrec (9)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (list bar #t)))
		       bar))
		  (environment '(rnrs))))

;; in $if
(test-error "letrec (10)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (if bar #t #f)))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec (10.1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (if #t bar #f)))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec (10.2)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar (if #f #f bar)))
		       bar))
		  (environment '(rnrs))))

;; referred above
(test-error "letrec (11)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((bar buz)
			      (buz #t))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec (11.1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec ((buz #t)
			      (bar buz))
		       bar))
		  (environment '(rnrs))))
;; ok
(test-assert "letrec (12)"
	     (eval '(lambda (bar)
		      (letrec ((bar (lambda () bar)))
			bar))
		   (environment '(rnrs))))

;; letrec*
;; in $lref
(test-error "letrec* (1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar bar))
		       bar))
		  (environment '(rnrs))))
;; in $asm
(test-error "letrec* (2)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (cons 'a bar)))
		       bar))
		  (environment '(rnrs))))
;; in $call
(test-error "letrec* (3)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (for-all (lambda (a) a) bar)))
		       bar))
		  (environment '(rnrs))))

;; in $let
(test-error "letrec* (4)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (let ((bar2 bar)) bar2)))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec* (4.1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (let ((bar2 'a)) bar)))
		       bar))
		  (environment '(rnrs))))
;; in $let ok
(test-assert "letrec* (5)"
	     (eval '(lambda (bar)
		      (letrec* ((bar (let ((bar 'foo)) bar)))
			bar))
		   (environment '(rnrs))))

;; in $receive
(test-error "letrec* (6)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (receive (bar) (values bar) bar)))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec* (6.1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (receive (buz) (values a) bar)))
		       bar))
		  (environment '(rnrs))))
;; in $receive ok
(test-error "letrec* (7)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (receive (bar) (values 'a) bar)))
		       bar))
		  (environment '(rnrs))))

;; in $seq
(test-error "letrec* (8)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (begin bar #t)))
		       bar))
		  (environment '(rnrs))))
;; in $list
(test-error "letrec* (9)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (list bar #t)))
		       bar))
		  (environment '(rnrs))))

;; in $if
(test-error "letrec* (10)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (if bar #t #f)))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec* (10.1)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (if #t bar #f)))
		       bar))
		  (environment '(rnrs))))
(test-error "letrec* (10.2)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar (if #f #f bar)))
		       bar))
		  (environment '(rnrs))))

;; referred above
(test-error "letrec* (11)"
	    syntax-violation?
	    (eval '(lambda (bar)
		     (letrec* ((bar buz)
			      (buz #t))
		       bar))
		  (environment '(rnrs))))
(test-assert "letrec* (11.1)"
	    (eval '(lambda (bar)
		     (letrec* ((buz #t)
			       (bar buz))
		       bar))
		  (environment '(rnrs))))
;; ok
(test-assert "letrec* (12)"
	     (eval '(lambda (bar)
		      (letrec* ((bar (lambda () bar)))
			bar))
		   (environment '(rnrs))))

;; #e1@1
(test-error "#e1@1" implementation-restriction-violation?
	    (read (open-string-input-port "#e1@1")))

;; peek buffer issue
(define (test-port in peek read expected)
  (test-equal (format "port position of ~a ~a" in expected)
	      expected
	      (let* ((c0 (peek in))
		     (pos (port-position in))
		     (c1 (begin
			   (set-port-position! in 1)
			   (read in))))
		(list c0 pos c1)))
  (close-port in))

(test-port (open-string-input-port "ab") lookahead-char get-char '(#\a 0 #\b))
(test-port (open-bytevector-input-port #vu8(1 2)) lookahead-u8 get-u8 
	   '(1 0 2))
(let ((file "peek.tmp"))
  (when (file-exists? file) (delete-file file))
  (call-with-output-file file
    (lambda (out) (put-string out "abcdefg\n")))
  (test-port (open-file-input-port file) lookahead-u8 get-u8 
	     '(97 0 98))
  (test-port (open-file-input-port file 
				   (file-options no-fail)
				   (buffer-mode block)
				   (native-transcoder))
	     lookahead-char get-char
	     '(#\a 0 #\b))
  (delete-file file))

;; invalid identifer
(test-error "read identifier(quote)" lexical-violation?
	    (read (open-string-input-port "#!r6rs foo'bar")))
(test-error "read identifier(unquote)" lexical-violation?
	    (read (open-string-input-port "#!r6rs foo,bar")))
(test-error "read identifier(quasiquote)" lexical-violation?
	    (read (open-string-input-port "#!r6rs foo`bar")))

;; negative result of gcd
(test-equal "gcd returns non-negative integer"
	    2182600451
	    (gcd -165543184715050652143983385447792 15946333291432216432322993695213691))

(test-equal 'b
	    (eval '(let ()
		     (let-syntax ()
		       (define foo 'b))
		     foo)
		  (environment '(rnrs))))

(test-equal 'b
	    (eval '(let ()
		     (letrec-syntax ()
		       (define foo 'b))
		     foo)
		  (environment '(rnrs))))

(test-end)
