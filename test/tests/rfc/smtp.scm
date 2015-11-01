;; -*- mode:scheme; coding:utf-8; -*-
(import (rnrs)
	(rnrs mutable-strings)
	(rnrs eval)
	(rfc smtp commands)
	(rfc smtp extensions)
	(rfc smtp conditions)
	(rfc smtp client)
	(rfc base64)
	(rfc mime)
	(rfc :5322)
	(srfi :1) ;; for alist-delete
	(srfi :64))

(test-begin "SMTP")

;; command test
(define-syntax test-command 
  (syntax-rules ()
    ((_ expected expr args ...)
     (test-equal 'expr expected
		 (call-with-string-output-port
		  (lambda (out) (expr out args ...)))))))

;; write commands
(test-command "HELO example.com\r\n" smtp-helo "example.com")
(test-command "EHLO example.com\r\n" smtp-ehlo "example.com")

(test-command "MAIL FROM: <foo@example.com>\r\n" smtp-mail "<foo@example.com>")
(test-command "RCPT TO: <foo@example.com>\r\n" smtp-rcpt "<foo@example.com>")

(test-command "DATA\r\n" smtp-data)

(test-command "RSET\r\n" smtp-rset)
(test-command "VRFY String\r\n" smtp-vrfy "String")

(test-command "EXPN String\r\n" smtp-expn "String")

(test-command "HELP\r\n" smtp-help)
(test-command "HELP String\r\n" smtp-help "String")
(test-command "NOOP\r\n" smtp-noop)
(test-command "NOOP String\r\n" smtp-noop "String")

(test-command "QUIT\r\n" smtp-quit)

(define expected-credential (base64-encode-string "foo\x0;foo\x0;bar"))

(define (make-emulative-port str)
  (define (string-copy! src spos dst dpos size)
    (do ((i 0 (+ i 1)) (spos spos (+ spos 1)) (dpos dpos (+ dpos 1)))
	((= i size) size)
      (string-set! dst dpos (string-ref src spos))))
  ;; (define str "235")
  (define pos 0)
  (define str-len (string-length str))
  (let-values (((out extract) (open-string-output-port)))
    (define (write! src start size) (put-string out src start size) size)
    (define (read! out start size)
      (if (>= pos str-len)
	  0
	  (let ((size (min size (- str-len pos))))
	    (string-copy! str pos out start size)
	    (set! pos (+ size pos))
	    size)))
    (define (close) #f)
    (values 
     (make-custom-textual-input/output-port "emutative-port" 
					    read! write! #f #f close)
     extract)))
  
(define (test-auth expected thunk)
  (let*-values (((out extract) (make-emulative-port "235"))
		((status content) (smtp-auth out thunk)))
      (test-equal "auth status" 235 status)
      (test-equal "auth value" expected (extract))))
(test-auth (format "AUTH PLAIN ~a\r\n" expected-credential)
	   (smtp-plain-authentication "foo" "bar"))
(test-auth  "AUTH SOMETHING\r\n"
	    (lambda () (values "SOMETHING" #f)))

(let-values (((out extract) (make-emulative-port "500")))
  (test-error "auth error" smtp-authentication-failure?
	      (smtp-auth out (smtp-plain-authentication "foo" "bar"))))

;; read response
;; From SASM project
;; maybe we want to put this somewhere...
(define-syntax test-values
  (syntax-rules (or ? ~ list)
    ((_ "tmp" name (e e* ...) (expected ...) (var ...) (var2 ... ) expr)
     (test-values "tmp" name (e* ...) (expected ... e) 
		  (var ... t) (var2 ... t2)
		  expr))
    ((_ "tmp" name () (expected ...) (var ...) (var2 ...) expr)
     (let ((var #f) ...)
       (test-assert 'expr
		    (let-values (((var2 ...) expr))
		      (set! var var2) ...
		      #t))
       (test-values "equal" name (expected ...) (var ...))))
    ;; compare
    ((_ "equal" name () ()) (values))
    ;; de-construct list
    ((_ "equal" name ((list e ...) e* ...) (v1 v* ...))
     (begin
       (test-values "list" name (e ...) v1)
       (test-values "equal" name (e* ...) (v* ...))))
    ;; record inspection
    ((_ "equal" name ((~ (slot e) ...) e* ...) (v1 v* ...))
     (begin
       (if (record? v1) ;; record must be opaque
	   (let ((rtd (record-rtd v1)))
	     (let ((acc (find-accessor rtd 'slot)))
	       (test-equal '(name (~ slot e)) e (acc v1)))
	     ...)
	   (test-assert '(name (~ (slot e) ...)) #f))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name ((? pred) e* ...) (v1 v* ...))
     (begin
       (test-assert '(name (? pred)) (pred v1))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name ((or e ...) e* ...) (v1 v* ...))
     (begin
       (test-assert '(name (or e ...)) (member v1 '(e ...)))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name (e e* ...) (v1 v* ...))
     (begin
       (test-equal '(name e) e v1)
       (test-values "equal" name (e* ...) (v* ...))))
    ;; comparing list elements
    ((_ "list" name (e e* ...) v1)
     (begin 
       (test-values "equal" name (e) ((car v1)))
       (test-values "list"  name (e* ...) (cdr v1))))
    ((_ "list" name () v1) (values))
    ((_ (expected ...) expr)
     (test-values expr (expected ...) expr))
    ((_ name (expected ...) expr)
     (test-values "tmp" name (expected ...) () () () expr))))

(define response-1 "200 OK\r\n")
(define response-2 "250-First line\r\n\
                    250-Second line\r\n\
                    250-234 Text beginning with numbers\r\n\
                    250 The last line\r\n")
(define response-3 "200\r\n")
(define invalid-response "200-OK\r\n\
                          250 OK\r\n")
(define (read-response r)
  (smtp-recv (open-string-input-port r)))

(test-values "response-1" (200 "OK") (read-response response-1))
(test-values "response-2" (250 "First line\nSecond line\n234 Text beginning with numbers\nThe last line")
	     (read-response response-2))
(test-values "response-3" (200 "") (read-response response-3))

(test-error "invalid-response" smtp-invalid-response-error? 
	    (read-response invalid-response))

;; Client APIs
(define-syntax test-address
  (lambda (x)
    (define (make-ctr/pred k type)
      (let* ((s (symbol->string (syntax->datum type)))
	     (c (string->symbol (string-append "make-smtp-" s))))
	(datum->syntax k c)))
    (syntax-case x ()
      ((k type addr)
       (with-syntax ((ctr (make-ctr/pred #'k #'type)))
	 #'(test-assert '(ctr addr) (ctr addr))))
      ((k type name addr)
       (with-syntax ((ctr (make-ctr/pred #'k #'type)))
	 #'(test-assert '(ctr name addr) (ctr name addr)))))))

(test-address from "ktakashi@ymail.com")
(test-address to "ktakashi@ymail.com")
(test-address cc "ktakashi@ymail.com")
(test-address bcc "ktakashi@ymail.com")
(test-address from "Takashi Kato" "ktakashi@ymail.com")
(test-address to  "Takashi Kato" "ktakashi@ymail.com")
(test-address cc  "Takashi Kato" "ktakashi@ymail.com")
(test-address bcc "Takashi Kato" "ktakashi@ymail.com")

(define-syntax test-address-pred
  (lambda (x)
    (define (make-ctr/pred k type)
      (let* ((s (symbol->string (syntax->datum type)))
	     (c (string->symbol (string-append "make-smtp-" s)))
	     (p (string->symbol (string-append "smtp-" s "?"))))
	(datum->syntax k (list c p))))
    (syntax-case x ()
      ((k type addr)
       (with-syntax (((ctr pred) (make-ctr/pred #'k #'type)))
	 #'(test-assert 'pred (pred (ctr addr))))))))
    
(test-address-pred from "ktakashi@ymail.com")
(test-address-pred to "ktakashi@ymail.com")
(test-address-pred cc "ktakashi@ymail.com")
(test-address-pred bcc "ktakashi@ymail.com")

(test-assert "smtp-mail?"
	     (smtp-mail? (make-smtp-mail (make-smtp-from "ktakashi@ymail.com")
					 "Subject" "Message")))
(test-error "not from object" assertion-violation?
	    (make-smtp-mail "ktakashi@ymail.com" "Subject" "Message"))

;; make sure the file has eol style 'lf
(define test-mail
  "From: <ktakashi@ymail.com>\r
To: <ktakashi@ymail.com>\r
Subject: Subject\r
\r
Message")
(test-equal "smtp-mail->string (1)"
	    test-mail
	    (smtp-mail->string
	     (let ((mail (make-smtp-mail (make-smtp-from "ktakashi@ymail.com")
					 "Subject" "Message")))
	       (smtp-mail-add-recipent! mail
					(make-smtp-to "ktakashi@ymail.com")))))

;; High level
(test-assert "smtp:mail" (smtp-mail?
			  (smtp:mail (smtp:from "ktakashi@ymail.com"))))
;; need at least one recipient
(test-error "smtp-mail->string" assertion-violation?
	    (smtp-mail->string 
	     (smtp:mail (smtp:from "ktakashi@ymail.com"))))
(test-equal "smtp:mail (default)"
	    "From: <ktakashi@ymail.com>\r
To: <ktakashi@ymail.com>\r
Subject: no subject\r
\r
"
	    (smtp-mail->string 
	     (smtp:mail (smtp:from "ktakashi@ymail.com")
			(smtp:to "ktakashi@ymail.com"))))
(test-equal "smtp-mail->string (1)"
	    test-mail
	    (smtp-mail->string
	     (smtp:mail
	      (smtp:from "ktakashi@ymail.com")
	      (smtp:subject "Subject")
	      "Message"
	      (smtp:to "ktakashi@ymail.com"))))

(test-equal "smtp-mail->string (2)"
  "From: <ktakashi@ymail.com>\r
To: <ktakashi@ymail.com>\r
Cc: <ktakashi@ymail.com>\r
Subject: Subject\r
\r
Message"
	    (smtp-mail->string
	     (smtp:mail
	      (smtp:from "ktakashi@ymail.com")
	      (smtp:subject "Subject")
	      "Message"
	      (smtp:to "ktakashi@ymail.com")
	      (smtp:cc "ktakashi@ymail.com"))))

(test-equal "smtp-mail->string (3)"
  "From: <ktakashi@ymail.com>\r
To: <ktakashi@ymail.com>\r
Cc: <ktakashi@ymail.com>\r
Subject: Subject\r
\r
Message\r
Second line"
	    (smtp-mail->string
	     (smtp:mail
	      (smtp:from "ktakashi@ymail.com")
	      (smtp:subject "Subject")
	      "Message"
	      "Second line"
	      (smtp:to "ktakashi@ymail.com")
	      (smtp:cc "ktakashi@ymail.com"))))

;; order doesn't matte except from
(test-equal "smtp-mail->string (4)"
  "From: <ktakashi@ymail.com>\r
To: <ktakashi@ymail.com>\r
Cc: <ktakashi@ymail.com>\r
Subject: Subject\r
\r
Message\r
Second line"
	    (smtp-mail->string
	     (smtp:mail
	      (smtp:from "ktakashi@ymail.com")
	      (smtp:subject "Subject")
	      "Message"
	      (smtp:to "ktakashi@ymail.com")
	      "Second line"
	      (smtp:cc "ktakashi@ymail.com"))))

(test-equal "smtp-mail->string (5)"
  "From: <ktakashi@ymail.com>\r
To: <ktakashi@ymail.com>\r
Cc: <ktakashi@ymail.com>\r
X-Powered-By: Sagittarius Scheme\r
Subject: Subject\r
\r
Message\r
Second line"
	    (smtp-mail->string
	     (smtp:mail
	      (smtp:from "ktakashi@ymail.com")
	      (smtp:subject "Subject")
	      "Message"
	      (smtp:to "ktakashi@ymail.com")
	      "Second line"
	      (smtp:cc "ktakashi@ymail.com")
	      (smtp:header "X-Powered-By" "Sagittarius Scheme"))))
(test-equal "smtp-mail->string (6)"
  "From: <ktakashi@ymail.com>\r
To: <ktakashi@ymail.com>\r
Cc: <ktakashi@ymail.com>\r
X-Powered-By: Sagittarius Scheme 0.6.9\r
Subject: Subject\r
\r
Message\r
Second line"
	    (smtp-mail->string
	     (smtp:mail
	      (smtp:from "ktakashi@ymail.com")
	      (smtp:subject "Subject")
	      "Message"
	      (smtp:to "ktakashi@ymail.com")
	      "Second line"
	      (smtp:cc "ktakashi@ymail.com")
	      (smtp:header "X-Powered-By" "Sagittarius Scheme")
	      (smtp:header "X-Powered-By" "Sagittarius Scheme 0.6.9"))))

(test-equal "smtp-mail->string (3)"
  "From: <ktakashi@ymail.com>\r
To: <ktakashi@ymail.com>\r
Cc: <ktakashi@ymail.com>\r
X-Bogus: =?utf-8?B?44GC44GE44GG44GI44GK?=\r
Subject: =?utf-8?B?44GC44GE44GG44GI44GK?=\r
\r
Message"
	    (smtp-mail->string
	     (smtp:mail
	      (smtp:from "ktakashi@ymail.com")
	      (smtp:subject "あいうえお")
	      "Message"
	      (smtp:to "ktakashi@ymail.com")
	      (smtp:cc "ktakashi@ymail.com")
	      (smtp:header "X-Bogus" "あいうえお"))))

(test-equal "attachment"
	    '(#t "multipart" "mixed"
		 (("from" "<ktakashi@ymail.com>")
		  ("to" "<ktakashi@ymail.com>")
		  ("subject" "Subject")
		  ("mime-version" "1.0"))
		 #t)
	    (let* ((mail-content (smtp-mail->string
				  (smtp:mail
				   (smtp:from "ktakashi@ymail.com")
				   (smtp:to "ktakashi@ymail.com")
				   (smtp:subject "Subject")
				   "Message"
				   (smtp:attachment "text" "plain"
						    "Attach" "foo.txt"))))
		   (utf8-multi-part (string->utf8 mail-content))
		   (in (open-bytevector-input-port utf8-multi-part))
		   (headers (rfc5322-read-headers in))
		   (r '()))
	      (if (mime-parse-version (rfc5322-header-ref headers
							  "mime-version"))
		  (let ((packet (mime-parse-message in headers 
				    (lambda (packet port)
				      (get-bytevector-all port)))))
		    (set! r (cons (mime-part? packet) r))
		    (set! r (cons (mime-part-type packet) r))
		    (set! r (cons (mime-part-subtype packet) r))
		    (set! r (cons (alist-delete "content-type"
						(mime-part-headers packet)
						equal?) r))
		    (set! r (cons (for-all mime-part? 
					   (mime-part-content packet)) r))
		    (reverse r))
		  #f)))

;; TODO proper test...
(test-assert "alternative (1)"
	    (smtp-mail->string
	     (smtp:mail
	      (smtp:from "ktakashi@ymail.com")
	      (smtp:to "ktakashi@ymail.com")
	      (smtp:subject "Subject")
	      (smtp:alternative 
	       ("text" "plain" "Plain text")
	       ("text" "html" "HTML message")))))
(test-error "alternative (2)" syntax-violation?
	    (eval
	     '(smtp-mail->string
	       (smtp:mail
		(smtp:from "ktakashi@ymail.com")
		(smtp:to "ktakashi@ymail.com")
		(smtp:subject "Subject")
		(smtp:alternative 
		 ("text" "plain" "Plain text")
		 ("text" "html" "HTML message"))
		(smtp:alternative 
		 ("text" "plain" "alternative can't be twice"))))
	     (environment '(rfc smtp client))))
(test-error "alternative (3)" syntax-violation?
	    (eval
	     '(smtp-mail->string
	       (smtp:mail
		(smtp:from "ktakashi@ymail.com")
		(smtp:to "ktakashi@ymail.com")
		(smtp:subject "Subject")
		(smtp:alternative 
		 ("text" "plain" "Must have more than 1"))))
	     (environment '(rfc smtp client))))

;; TODO make fake server and test client

(test-end)
