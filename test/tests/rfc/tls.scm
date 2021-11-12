(import (rnrs) 
	(sagittarius threads) 
	(sagittarius object) 
	(sagittarius socket) 
	(rfc tls)
	(rfc base64)
	(rfc x.509) 
	(crypto)
	(srfi :18)
	(srfi :19)
	(srfi :64))


(test-begin "RFC TLS")

(define (shutdown&close s)
  (tls-socket-shutdown s SHUT_RDWR)
  (tls-socket-close s))

(define keypair (generate-key-pair RSA :size 1024))
(define 1year (make-time time-duration 0 (* 1 60 60 24 365)))
;; NB timezone must be set (probably with Z), so specifying zone offset 0.
(define cert (make-x509-basic-certificate keypair 1
              (make-x509-issuer '((C . "NL")))
              (make-validity (current-date 0)
			     (time-utc->date
			      (add-duration! (current-time) 1year) 0))
              (make-x509-issuer '((C . "NL")))))

(define client-keypair (generate-key-pair RSA :size 1024))
(define client-cert (make-x509-basic-certificate client-keypair 2
		     (make-x509-issuer '((C . "NL")))
		     (make-validity (current-date 0)
				    (time-utc->date
				     (add-duration! (current-time) 1year) 0))
		     (make-x509-issuer '((C . "NL")))))

(define server-socket (make-server-tls-socket "10001" (list cert)
					      :private-key (keypair-private keypair)
					      :authorities (list client-cert)
					      :client-certificate-required? #f
					      ;; self signed certificate
					      ;; will be rejected if
					      ;; we use default verifier
					      :certificate-verifier #f))

(define (server-run)
  (define end? #f)
  (let loop ()
    (guard (e (else (report-error e) (loop)))
    (let ((addr (tls-socket-accept server-socket)))
      (guard (e (else (report-error e) (unless end? (loop))))
	(call-with-tls-socket addr
	  (lambda (sock)
	    (let ((p (transcoded-port (tls-socket-port sock #f) 
				      (native-transcoder))))
	      (call-with-port p
	        (lambda (p)
		  (let lp2 ((r (get-line p)))
		    (cond ((or (not (string? r)) (string=? r "test-end"))
			   (set! end? #t))
			  ((or (not (string? r)) (string=? r "end")) (loop))
			  ((or (not (string? r)) (string=? r "certificate"))
			   (let* ((cert (tls-socket-peer-certificate sock))
				  (b64 (if cert
					   (utf8->string
					    (base64-encode (x509-certificate->bytevector cert)))
					    "")))
			     (put-string p (number->string (string-length b64)))
			     (put-string p "\r\n")
			     (put-string p b64)
			     (lp2 (get-line p))))
			  (else
			   (let ((res (string->utf8 (string-append r "\r\n"))))
			     (when (string=? r "wait")
			       ;; wait one sec
			       (thread-sleep! 1))
			     (put-bytevector p res 0 (bytevector-length res) #t)
			     (lp2 (get-line p)))))))))))))))
  )
(define server-thread (thread-start! (make-thread server-run)))
(thread-sleep! 2)

(let ((client-socket (make-client-tls-socket "localhost" "10001")))
  (test-assert "tls-socket?"(tls-socket? client-socket))
  (test-equal "raw socket-send"
	      (+ (string-length "hello") 2) ;; for \r\n
	      (tls-socket-send client-socket (string->utf8 "hello\r\n") 0))
  (test-equal "raw socket-recv"
	      (string->utf8 "hello\r\n")
	      (tls-socket-recv client-socket (+ (string-length "hello") 2) 0))

  (test-equal "raw socket-send (2)"
	      (+ (string-length "hello") 2) ;; for \r\n
	      (tls-socket-send client-socket (string->utf8 "hello\r\n") 0))
  (test-equal "raw socket-recv!"
	      (+ (string-length "hello") 2)
	      (let ((bv (make-bytevector (+ (string-length "hello") 2))))
		(tls-socket-recv! client-socket bv 0 
				  (+ (string-length "hello") 2) 0)))

  ;; make port
  (let ((port (tls-socket-port client-socket)))
    (test-assert "port?" (port? port))
    (test-assert "binary-port?" (binary-port? port))
    (test-assert "input-port?" (input-port? port))
    (test-assert "output-port?" (output-port? port))

    (test-assert (put-bytevector port (string->utf8 "put from port\r\n")))
    (thread-sleep! 2) ;; I hope it's enough
    (test-assert "port-ready?" (port-ready? port))
    (test-equal "get-bytevector-n"
		(string->utf8 "put from port\r\n")
		(get-bytevector-n port
				  (string-length "put from port\r\n")))
    ;; textual
    (let ((text-port (transcoded-port port
				      (make-transcoder (utf-8-codec)
						       'crlf))))
      (test-assert (put-string text-port "put from text port\r\n"))
      (test-equal "get-line" "put from text port" (get-line text-port))
      ;; end test
      (test-assert (put-string text-port "end\r\n"))
      ;; the test server is a bit too naive to handle this...
      ;; (close-port text-port)
      ))
  (test-assert "peer certificate"
	       (x509-certificate? (tls-socket-peer-certificate client-socket)))
  (shutdown&close client-socket))

;; send certificate
(let ((client-socket (make-client-tls-socket "localhost" "10001"
					     :private-key (keypair-private client-keypair)
					     :certificates (list client-cert))))
  (define (ensure-read-n in n)
    (let-values (((out e) (open-string-output-port)))
      (let loop ((c n))
	(let ((s (get-string-n in c)))
	  (put-string out s)
	  (if (= (string-length s) c)
	      (e)
	      (loop (- c (string-length s))))))))
    
  (tls-socket-send client-socket (string->utf8 "certificate\r\n"))
  (let ((in/out (transcoded-port (tls-socket-port client-socket)
				 (native-transcoder))))
    (let ((n (string->number (get-line in/out))))
      (unless (zero? n)
	;; skip excess space in case of on windows
	(do ((c (lookahead-char in/out) (lookahead-char in/out)))
	    ((not (char-whitespace? c)))
	  (get-char in/out))
	(let ((cert (ensure-read-n in/out n)))
	  (test-assert (x509-certificate?
			(make-x509-certificate
			 (base64-decode-string cert :transcoder #f)))))))
    
    (put-string in/out "end\r\n"))
  (shutdown&close client-socket))

;; no certificate
(let ((client-socket (make-client-tls-socket "localhost" "10001")))
  (tls-socket-send client-socket (string->utf8 "certificate\r\n"))
  (let ((in/out (transcoded-port (tls-socket-port client-socket)
				 (native-transcoder))))
      (test-equal 0 (string->number (get-line in/out)))
      (put-string in/out "end\r\n"))
  (shutdown&close client-socket))

(let ((client-socket (make-client-tls-socket "localhost" "10001")))
  (tls-socket-nonblocking! client-socket)
  (test-equal "raw nonblocking socket-send"
	      (+ (string-length "wait") 2)
	      (tls-socket-send client-socket (string->utf8 "wait\r\n") 0))
  (test-equal "raw nonblocking socket-recv"
	      #f
	      (tls-socket-recv client-socket 
			       (+ (string-length "hello\r\n") 2) 0))
  (tls-socket-blocking! client-socket)
  (tls-socket-send client-socket (string->utf8 "test-end\r\n") 0)
  (thread-sleep! 2)
  ;; (tls-socket-close client-socket)
  (shutdown&close client-socket))

(thread-join! server-thread)
;;(test-assert "TLS server finish" (thread-join! server-thread))
(shutdown&close server-socket)

(test-end)
