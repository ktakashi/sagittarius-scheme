;; -*- scheme -*-
#!read-macro=sagittarius/regex
(import (rnrs)
	(rfc http)
	(rfc mime)
	(rfc :5322)
	(sagittarius)
	(sagittarius io)
	(sagittarius regex)
	(sagittarius control)
	(sagittarius socket)
	(match)
	(shorten)
	(clos user)
	(util port)
	(srfi :18 multithreading)
	(srfi :19 time)
	(srfi :26 cut)
	(srfi :64 testing)
	(crypto)
	(rfc tls)
	(rfc x.509)
	(clos user))

;; the same as HTTP test but using HTTPS
(define keypair (generate-key-pair RSA :size 1024))

(define cert (make-x509-basic-certificate keypair 1
                      (make-x509-issuer '((C . "NL")))
                      (make-validity (current-date)
                             (current-date))
                      (make-x509-issuer '((C . "NL")))))

(define (read-from-string s)
  (call-with-input-string s
    (cut read <>)))

(define (assoc-ref param name)
  (and-let* ((slot (assoc name param)))
    (cdr slot)))
(test-begin "HTTPS tests")

(define *http-port* "6727")

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

(define server-up? #f)
(define server-done? #f)
(define (http-server socket)
  (let loop ()
    (and-let* ([client (tls-socket-accept socket)])
      (guard (e (else (report-error e)
		      (socket-shutdown client SHUT_RDWR)
		      (socket-close client)))
	(let* ([in/out (transcoded-port (buffered-port (tls-socket-port client)
						       (buffer-mode block))
					(make-transcoder (utf-8-codec) 'lf))]
	       [request-line (get-line in/out)])

	  (cond ((eof-object? request-line)
		 (error 'http-server "unexpected EOF" client in/out))
		((#/^(\S+) (\S+) HTTP\/1\.1$/ request-line)
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
			  (flush-output-port in/out)
			  (close-port in/out)]
			 [(hashtable-ref %predefined-contents request-uri #f)
			  => (lambda (x)
			       (for-each (cut display <> in/out) x)
			       (close-port in/out))]
			 [else
			  (display "HTTP/1.x 200 OK\nContent-Type: text/plain\n\n" in/out)
			  ;; to avoid SIGPIPE
			  (unless (string=? method "HEAD")
			    (write `(("method" ,method)
				     ("request-uri" ,request-uri)
				     ("request-body" ,(utf8->string body))
				     ,@headers)
				   in/out))
			  (flush-output-port in/out)
			  (close-port in/out)]))))
		(else
		 (error 'http-server "malformed request line"
			request-line))))))
    (unless server-done? (loop))))

(define server-thread
  (make-thread (lambda ()
		 (let1 socket (make-server-tls-socket *http-port* (list cert))
		   (set! server-up? #t)
		   (http-server socket)
		   (socket-shutdown socket SHUT_RDWR)
		   (socket-close socket)))))

(thread-start! server-thread)

(let loop ()
  (unless server-up?
    (thread-yield!)
    (loop)))

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
		    (http-request 'GET host "/get" :secure #t :my-header "foo")
		  (and (equal? code "200")
		       (equal? headers '(("content-type" "text/plain")))
		       (read-from-string body))))
	       )

  (test-assert "http-get, custom receiver"
	       (alist-equal?
		expected
		(req-body 'GET host "/get"
			  :secure #t
			  :receiver 
			  (lambda (code hdrs total retr)
			    (let loop ((result '()))
			      (receive (port size) (retr)
				(if (and size (= size 0))
				    result
				    (loop (append result (read (transcoded-port port (native-transcoder)))))))))
			  :my-header "foo"))
	       )

  (if (file-exists? "test.o") (delete-file "test.o"))
  (test-assert "http-get, file receiver" 
	       (alist-equal?
		expected
		(let1 f (req-body 'GET host "/get"
				  :secure #t
				  :receiver (http-file-receiver "test.o")
				  :my-header :foo)
		  (and (equal? f "test.o")
		       (with-input-from-file "test.o" read)))))
  (if (file-exists? "test.o") (delete-file "test.o"))

  (test-assert "http-get, file receiver (tmp)" 
	       (alist-equal?
		expected
		(let1 f (req-body 'GET host "/get"
				  :secure #t
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
                      (loop (append result 
				    (read (transcoded-port port (native-transcoder)))))))))]
       ))

    (test-equal "http-get, cond-receiver" expected
           (req-body 'GET host "/get"
                     :receiver cond-receiver
		     :secure #t
                     :my-header :foo))

    (test-equal "http-get, cond-receiver" 404
           (req-body 'GET host "/void"
                     :receiver cond-receiver
		     :secure #t
                     :my-header :foo)))
  )

(test-equal "exit" "exit"
	    (receive (s h b) 
		(http-request 'GET (format "localhost:~a" *http-port*) "/exit"
			      :secure #t)
	      b))
(set! server-done? #t)
(guard (e (else #t)) (thread-interrupt! server-thread))
(test-assert (thread-join! server-thread 10))


(test-end)
