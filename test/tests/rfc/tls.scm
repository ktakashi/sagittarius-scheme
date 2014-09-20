(import (rnrs) 
	(sagittarius threads) 
	(rfc tls)
	(rfc x.509) 
	(crypto)
	(srfi :19)
	(srfi :64))


(test-begin "RFC TLS")
(define keypair (generate-key-pair RSA :size 1024))

(define cert (make-x509-basic-certificate keypair 1
                      (make-x509-issuer '((C . "NL")))
                      (make-validity (current-date)
                             (current-date))
                      (make-x509-issuer '((C . "NL")))))


(define server (make-server-tls-socket "5000" (list cert)))
(define server-thread
  (let ((t (make-thread (lambda ()
              (let ((s (tls-socket-accept server)))
                (tls-socket-send s (string->utf8 "hello")))))))
    (thread-start! t))
  (thread-sleep! 2))

(let ((client (make-client-tls-socket "localhost" "5000")))
  (test-equal "TLS client" "hello" (utf8->string (tls-socket-recv client 50))))

(test-assert "TLS server finish" (thread-join! server-thread))

(test-end)
