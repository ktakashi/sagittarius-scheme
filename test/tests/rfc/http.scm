;; -*- scheme -*-
#!read-macro=sagittarius/regex
(import (rnrs)
	(rfc http)
	(rfc mime)
	(rfc :5322)
	(rfc cookie)
	(sagittarius)
	(sagittarius io)
	(sagittarius regex)
	(sagittarius control)
	(sagittarius socket)
	(match)
	(shorten)
	(clos user)
	(util port)
	(only (binary io) open-chunked-binary-input/output-port)
	(util concurrent)
	(srfi :18 multithreading)
	(srfi :26 cut)
	(srfi :64 testing))

(define (read-from-string s)
  (call-with-input-string s
    (cut read <>)))

(define (assoc-ref param name)
  (and-let* ((slot (assoc name param)))
    (cdr slot)))
(test-begin "RFC HTTP tests")

(test-equal "http-user-agent"
	    (format "sagittarius.http/~a" (sagittarius-version))
	    (*http-user-agent*))
(test-equal "http-compose-query" "/search?q=foo%20bar&n=20"
	    (http-compose-query "/search" '((q "foo bar") (n 20))))

;; from Gauche
(define *http-port* "6726")

(define (alist-equal? alis1 alis2)
  (define (%sort alis)
    (list-sort (lambda (a b) (string<? (car a) (car b))) alis))
  (and (list? alis1)
       (list? alis2)
       (equal? (%sort alis1) (%sort alis2))))
(define %predefined-contents
  (let1 ht (make-string-hashtable)
    (hashtable-set! ht "/redirect01"
		     `("HTTP/1.x 302 Moved Temporarily\n"
		       ,(format "Location: http://localhost:~a/redirect02\n\n"
				*http-port*)))
    (hashtable-set! ht "/redirect11"
		     '("HTTP/1.x 302 Moved Temporarily\n"
		       "Location: /redirect12\n\n"))
    (hashtable-set! ht "/loop1"
		     '("HTTP/1.x 302 Moved Temporarily\n"
		       "Location: /loop2\n\n"))
    (hashtable-set! ht "/loop2"
		     '("HTTP/1.x 302 Moved Temporarily\n"
		       "Location: /loop1\n\n"))
    (hashtable-set! ht "/chunked"
		     '("HTTP/1.x 200 OK\nTransfer-Encoding: chunked\n\n"
		       "2\r\nOK\n0\r\n\r\n"))
    (hashtable-set! ht "/void"
		     '("HTTP/1.x 404 Not found\nContent-type: text/plain\n"
		       "Content-length: 9\n\nNot found"))
    (hashtable-set! ht "/cookie/set-cookie"
		     '("HTTP/1.x 200 Not found\n\
                        Content-type: text/plain\n\
                        Content-Length: 2\n\
                        Set-Cookie: c1=v1\n\
                        Set-Cookie: c2=v2; Secure\n\
                        Set-Cookie: c3=v3; Path=/cookie\n\
                        Set-Cookie: c3=v3; Path=/idontcare\n\n\
                        OK"))
    ht))


(define server-done? #f)
(define (http-server socket)
  (let loop ()
    (let* ([client  (socket-accept socket)]
	   [in/out  (transcoded-port (buffered-port (socket-port client #f)
						    (buffer-mode block))
				     (make-transcoder (utf-8-codec) 'lf))]
	   [request-line (get-line in/out)])
      (define (finish)
	(close-port in/out)
	(socket-shutdown client SHUT_RDWR)
	(socket-close client))
      (cond ((#/^(\S+) (\S+) HTTP\/1\.1$/ request-line)
	     => (lambda (m)
		  (let* ([method (m 1)]
			 [request-uri (m 2)]
			 [headers (rfc5322-read-headers in/out)]
			 [bodylen
			  (cond [(and-let* ((slot (assoc "content-length" headers)))
				   (cdr slot))
				 => (lambda (e) (string->number (car e)))]
				[else 0])]
			 [body (get-bytevector-n in/out bodylen #t)])
		    (cond
		     [(equal? request-uri "/exit")
		      (display "HTTP/1.x 200 OK\nContent-Type: text/plain\n\n" in/out)
		      (display "exit" in/out)
		      (set! server-done? #t)
		      (finish)]
		     [(hashtable-ref %predefined-contents request-uri #f)
		      => (lambda (x)
			   (for-each (cut display <> in/out) x)
			   (close-port in/out)
			   (socket-shutdown client SHUT_RDWR)
			   (socket-close client)
			   (loop))]
		     [(equal? request-uri "/cookie/check-cookie")
		      ;; we must have only 2 cookies
		      (display "HTTP/1.x 200 OK\nContent-Type: text/plain\n\n" in/out)
		      (cond ((rfc5322-header-ref headers "cookie") =>
			     (lambda (c)
			       (let ((cookies (parse-cookies-string c)))
				 (if (= (length cookies) 2)
				     (display "OK" in/out)
				     (display "NG" in/out)))))
			    (else (display "NG" in/out)))
		      (finish)
		      (loop)]
		     [else
		      (display "HTTP/1.x 200 OK\nContent-Type: text/plain\n\n" in/out)
		      ;; to avoid SIGPIPE
		      (unless (string=? method "HEAD")
			(write `(("method" ,method)
				 ("request-uri" ,request-uri)
				 ("request-body" ,(utf8->string body))
				 ,@headers)
			       in/out))
		      (finish)
		      (loop)]))))
	    (else
	     (error 'http-server "malformed request line:" request-line))))))

(define server-up? (make-shared-queue))
(define server-thread
  (make-thread (lambda ()
		 (guard (e (else (shared-queue-put! server-up? #f) (raise e)))
		   (let1 socket (make-server-socket *http-port*)
		     (shared-queue-put! server-up? #t)
		     (let loop ()
		       ;; somehow socket connection is lost and
		       ;; failed to send packet. why?
		       ;; NB: on Windows or Cygwin. So anti virus?
		       (guard (e (else (report-error e))) (http-server socket))
		       ;; /exit is not called yet, retry
		       (unless server-done? (loop)))
		     (socket-shutdown socket SHUT_RDWR)
		     (socket-close socket))))))
(thread-start! server-thread)
;; since 0.3.8 client/server socket creation are really slow on linux.
;; needs to be improved.
(unless (shared-queue-get! server-up?) (thread-join! server-thread))

(let ([expected `(("method" "GET")
                  ("request-uri" "/get")
                  ("request-body" "")
                  ("host" ,(format "localhost:~a" *http-port*))
                  ("user-agent" ,(format "sagittarius.http/~a"
					 (sagittarius-version)))
                  ("my-header" "foo"))]
      [host (format "localhost:~a" *http-port*)])
  (define (req-body . args)
    (receive (s h b) (apply http-request args) b))
  (define (read-result retr)
    (define in/out (open-chunked-binary-input/output-port))
    (let loop ()
      (receive (port size) (retr)
	(cond ((and size (= size 0))
	       (set-port-position! in/out 0)
	       (let ((in (transcoded-port in/out (native-transcoder))))
		 (let loop2  ((r '()))
		   (let ((e (read in)))
		     (if (eof-object? e)
			 r
			 (loop2 (append r e)))))))
	      (size
	       (put-bytevector in/out (get-bytevector-n port size))
	       (loop))
	      (else
	       (put-bytevector in/out (get-bytevector-all port size))
	       (loop))))))
  (test-assert "http-get, default string receiver"
	       (alist-equal? 
		expected
		(receive (code headers body)
		    (http-request 'GET host "/get" :my-header "foo")
		  (and (equal? code "200")
		       (equal? headers '(("content-type" "text/plain")))
		       (read-from-string body))))
	       )

  (test-assert "http-get, custom receiver"
	       (alist-equal?
		expected
		(req-body 'GET host "/get"
			  :receiver 
			  (lambda (code hdrs total retr) (read-result retr))
			  :my-header "foo"))
	       )

  (if (file-exists? "test.o") (delete-file "test.o"))
  (test-assert "http-get, file receiver" 
	       (alist-equal?
		expected
		(let1 f (req-body 'GET host "/get"
				  :receiver (http-file-receiver "test.o")
				  :my-header :foo)
		  (and (equal? f "test.o")
		       (with-input-from-file "test.o" read)))))
  (if (file-exists? "test.o") (delete-file "test.o"))

  (test-assert "http-get, file receiver (tmp)" 
	       (alist-equal?
		expected
		(let1 f (req-body 'GET host "/get"
				  :receiver (http-file-receiver "test.o"
								:temporary? #t)
				  :my-header :foo)
		  (and (not (equal? f "test.o"))
		       (begin0 (with-input-from-file f read)
			       (delete-file f))))))

  (let ()
    (define cond-receiver
      (http-cond-receiver
       ["404" (lambda (code hdrs total retr)
                (let1 sink (open-file-output-port
                            (cond-expand
                             [windows "NUL"]
                             [else "/dev/null"]) (file-options no-fail))
                  (let loop ()
                    (receive (port size) (retr)
                      (cond
                       [(and size (= size 0)) (close-output-port sink) 404]
                       [else (copy-binary-port sink port :size size)
			     (loop)])))))]
       ["200" (lambda (code hdrs total retr) (read-result retr))]
       ))

    (test-equal "http-get, cond-receiver" expected
           (req-body 'GET host "/get"
                     :receiver cond-receiver
                     :my-header :foo))

    (test-equal "http-get, cond-receiver" 404
           (req-body 'GET host "/void"
                     :receiver cond-receiver
                     :my-header :foo)))
  )

(test-equal "http-get (redirect)" "/redirect02"
       (receive (code headers body)
           (http-request 'GET (format "localhost:~a" *http-port*) "/redirect01")
         (cond ((assoc-ref (read-from-string body) "request-uri")
                => car))))

(test-equal "http-get (redirect)" "/redirect12"
       (receive (code headers body)
           (http-request 'GET (format "localhost:~a" *http-port*) "/redirect11")
         (cond ((assoc-ref (read-from-string body) "request-uri")
                => car))))

(test-equal "http-get (no redirect)" "/redirect12"
       (receive (code headers body)
           (http-request 'GET (format "localhost:~a" *http-port*) "/redirect11"
                         :redirect-handler #f)
         (rfc5322-header-ref headers "location")))

(test-equal "http-get (custom redirect)" "/foofoo"
       (receive (code headers body)
           (http-request 'GET (format "localhost:~a" *http-port*) "/redirect11"
                         :redirect-handler (^[meth code hdrs body]
                                             `(GET . "/foofoo")))
         (cond ((assoc-ref (read-from-string body) "request-uri")
                => car))))

(test-equal "http-get (custom redirect to HEAD)" #f
       (receive (code headers body)
           (http-request 'GET (format "localhost:~a" *http-port*) "/redirect11"
                         :redirect-handler (^[meth code hdrs body]
                                             `(HEAD . "/foofoo")))
         body))

(test-error "http-get (loop)" &http-error
	    (http-request 'GET (format "localhost:~a" *http-port*) "/loop1"))

(test-equal "http-get (chunked body)" "OK"
       (receive (code headers body)
           (http-request 'GET (format "localhost:~a" *http-port*) "/chunked")
         body))

(test-equal "http-head" #t
       (receive (code headers body)
           (http-request 'HEAD (format "localhost:~a" *http-port*) "/")
         (and (equal? code "200")
              (equal? headers '(("content-type" "text/plain")))
              (not body))))

(let1 expected `(("method" "POST")
                 ("request-uri" "/post")
                 ("content-length" "4")
                 ("host" ,(format "localhost:~a" *http-port*))
                 ("user-agent" ,(format "sagittarius.http/~a"
					 (sagittarius-version)))
                 ("request-body" "data"))
  (define (tester msg thunk)
    (test-assert (format "http-post ~a" msg)
		 (alist-equal?
		  expected
		  (receive (code headers body) (thunk)
		    (and (equal? code "200")
			 (equal? headers '(("content-type" "text/plain")))
			 (read-from-string body))))
		 ))

  (tester "(new API)"
          (lambda ()
            (http-request 'POST (format "localhost:~a" *http-port*) "/post"
                          :sender (http-string-sender "data"))))

  (tester "(old API)"
          (lambda ()
            (http-post (format "localhost:~a" *http-port*) "/post" "data")))
  )

(let1 expected '(("a" "b") ("c" "d"))
  (define (tester msg thunk)
    (test-equal 
     (format "http-post (multipart/form-data) ~a" msg) expected
     (receive (code headers body) (thunk)
       (and-let* ([ (equal? code "200") ]
		  [ (equal? headers '(("content-type" "text/plain"))) ]
		  [r (read-from-string body)]
		  [body (assoc "request-body" r)]
		  [part (mime-parse-message-string (cadr body)
						   r mime-body->string)]
		  [ (is-a? part <mime-part>) ]
		  [ (list? (mime-part-content part)) ])
	 (map (lambda (p)
		(match (mime-parse-content-disposition
			(rfc5322-header-ref (mime-part-headers p)
					   "content-disposition"))
		  [("form-data" ("name" . name))
		   (list name (mime-part-content p))]
		  [else
		   (list (mime-part-headers p) (mime-part-content p))]))
	      (mime-part-content part))))))

  (tester "(new API)"
          (lambda ()
            (http-request 'POST (format "localhost:~a" *http-port*) "/post"
                          :sender
                          (http-multipart-sender '(("a" "b") ("c" "d"))))))
  (tester "(old API)"
          (lambda ()
            (http-post (format "localhost:~a" *http-port*) "/post"
                       '(("a" "b") ("c" "d")))))
  )

(let ((cookie-jar (make-cookie-jar))
      (server (format "localhost:~a" *http-port*)))
  (let-values (((s h b) (http-get server "/cookie/set-cookie"
				  :cookie-jar cookie-jar)))
    (test-equal "200" s)
    (test-equal "OK" b)
    (test-equal 4 (cookie-jar-size cookie-jar)))
  (let-values (((s h b) (http-get server "/cookie/check-cookie"
				  :cookie-jar cookie-jar)))
    (test-equal "OK" b)))

;; stop
(test-equal "exit" "exit"
	    (receive (s h b) 
		(http-request 'GET (format "localhost:~a" *http-port*) "/exit")
	      b))
(test-assert (thread-join! server-thread 10))

(test-end)
