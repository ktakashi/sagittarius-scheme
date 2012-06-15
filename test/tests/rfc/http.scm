;; -*- scheme -*-
#< (sagittarius regex) >
(import (rnrs)
	(rfc http)
	(rfc mime)
	(rfc :5322)
	(sagittarius io)
	(sagittarius regex)
	(sagittarius control)
	(sagittarius socket)
	(match)
	(shorten)
	(clos user)
	(util port)
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
	    (http-user-agent))
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
    ht))

(define (http-server socket)
  (let loop ()
    (let* ([client  (socket-accept socket)]
	   [in/out  (transcoded-port (socket-port client)
				     (make-transcoder (utf-8-codec) 'lf))]
	   [request-line (get-line in/out)])
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
		      (socket-close client)]
		     [(hashtable-ref %predefined-contents request-uri #f)
		      => (lambda (x)
			   (for-each (cut display <> in/out) x)
			   (socket-close client)
			   (loop))]
		     [else
		      (display "HTTP/1.x 200 OK\nContent-Type: text/plain\n\n" in/out)
		      ;; to avoid SIGPIPE
		      (unless (string=? method "HEAD")
			(write `(("method" ,method)
				 ("request-uri" ,request-uri)
				 ("request-body" ,(utf8->string body))
				 ,@headers)
			       in/out))
		      (socket-close client)
		      (loop)]))))
	    (else
	     (error 'http-server "malformed request line:" request-line))))))

(define server-thread
  (make-thread (lambda ()
		 (let1 socket (make-server-socket *http-port*)
		   (http-server socket)))))
(thread-start! server-thread)

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
			  :receiver (lambda (code hdrs total retr)
				      (let loop ((result '()))
					(receive (port size) (retr)
					  (if (and size (= size 0))
					      result
					      (loop (append result (read port)))))))
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
       ["200" (lambda (code hdrs total retr)
                (let loop ((result '()))
                  (receive (port size) (retr)
                    (if (and size (= size 0))
                      result
                      (loop (append result (read port)))))))]
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
;; stop
(test-equal "exit" "exit"
	    (receive (s h b) 
		(http-request 'GET (format "localhost:~a" *http-port*) "/exit")
	      b))
(test-assert (thread-join! server-thread 10))

(test-end)
