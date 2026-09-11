#!read-macro=sagittarius/bv-string
(import (rnrs)
	(net http-client framing)
	(net http-client conditions)
	(srfi :64))

(test-begin "net/http-client framing")

(define (framing headers :optional (status "200") (method 'GET) (version "1.1"))
  (call-with-values
      (lambda () (http:response-framing status method headers version))
    list))

(define (raises? pred thunk)
  (guard (e ((pred e) #t)
	    (else #f))
    (thunk)
    #f))

(define (collect-chunked-body payload)
  (define in (open-bytevector-input-port payload))
  (define reader (http:make-chunk-reader))
  (define chunks '())
  (define ended? #f)
  (define (sink data end?)
    (unless (zero? (bytevector-length data))
      (set! chunks (cons data chunks)))
    (when end? (set! ended? #t)))
  (let loop ()
    (case (reader in sink)
      ((continue) (loop))
      ((done)
       (values (if (null? chunks) #vu8() (apply bytevector-append (reverse chunks)))
	       ended?
	       in))
      (else (assertion-violation 'collect-chunked-body "Unknown reader state")))))

(test-equal "header tokens"
	    '("keep-alive" "close")
	    (http:header-tokens "keep-alive, close"))
(test-assert "header token exact match"
	     (http:header-has-token? "keep-alive, close" "close"))
(test-assert "header token is not substring"
	     (not (http:header-has-token? "x-close-me" "close")))
(test-equal "last transfer coding"
	    "chunked"
	    (http:last-transfer-coding "gzip, chunked"))

(test-equal "HEAD no body"
	    '(none #f)
	    (framing '(("content-length" "10")) "200" 'HEAD "1.1"))
(test-equal "204 no body"
	    '(none #f)
	    (framing '(("content-length" "10")) "204" 'GET "1.1"))
(test-equal "304 no body"
	    '(none #f)
	    (framing '(("transfer-encoding" "chunked")) "304" 'GET "1.1"))

(test-equal "transfer encoding wins over content length"
	    '(chunked #f)
	    (framing '(("transfer-encoding" "gzip, chunked")
		       ("content-length" "5"))))
(test-assert "chunked must be final coding"
	     (raises? http-protocol-error?
		      (lambda ()
			(framing '(("transfer-encoding" "chunked, gzip"))))))

(test-equal "content length valid"
	    '(length 5)
	    (framing '(("content-length" "5"))))
(test-equal "multiple equal content length values"
	    '(length 5)
	    (framing '(("content-length" "5")
		       ("content-length" "5"))))
(test-assert "mismatched content length"
	     (raises? http-protocol-error?
		      (lambda ()
			(framing '(("content-length" "5, 6"))))))
(test-assert "scientific content length rejected"
	     (raises? http-protocol-error?
		      (lambda ()
			(framing '(("content-length" "1e5"))))))
(test-assert "negative content length rejected"
	     (raises? http-protocol-error?
		      (lambda ()
			(framing '(("content-length" "-1"))))))

(test-equal "http/1.1 defaults to reusable"
	    #t
	    (http:connection-reusable-after? '() "1.1"))
(test-equal "http/1.1 close is not reusable"
	    #f
	    (http:connection-reusable-after? '(("connection" "close")) "1.1"))
(test-equal "http/1.0 defaults to not reusable"
	    #f
	    (http:connection-reusable-after? '() "1.0"))
(test-equal "http/1.0 keep-alive is reusable"
	    #t
	    (http:connection-reusable-after? '(("connection" "keep-alive")) "1.0"))

(let-values (((body ended? in)
	      (collect-chunked-body #*"5\r\nhello\r\n0\r\n\r\n")))
  (test-equal "chunked body happy path" #*"hello" body)
  (test-assert "chunked body end signalled" ended?)
  (test-assert "chunked payload fully consumed" (eof-object? (lookahead-u8 in))))

(let-values (((body ended? in)
	      (collect-chunked-body #*"5;a=b\r\nhello\r\n0\r\nX-T: v\r\n\r\n")))
  (test-equal "chunked extension" #*"hello" body)
  (test-assert "chunked extension end" ended?)
  (test-assert "chunked trailers consumed" (eof-object? (lookahead-u8 in))))

(test-assert "chunked missing CRLF"
	     (raises? http-protocol-error?
		      (lambda ()
			(collect-chunked-body #*"5\r\nhelloXX0\r\n\r\n"))))
(test-assert "chunked invalid size line"
	     (raises? http-protocol-error?
		      (lambda ()
			(collect-chunked-body #*"zz\r\n"))))
(test-assert "chunked truncated data"
	     (raises? http-connection-error?
		      (lambda ()
			(collect-chunked-body #*"5\r\nhel"))))

(test-end)
