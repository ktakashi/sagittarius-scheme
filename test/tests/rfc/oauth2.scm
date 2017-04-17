#!read-macro=sagittarius/regex
#!read-macro=sagittarius/bv-string
(import (rnrs)
	(rfc oauth2)
	(rfc http-connections)
	(clos user)
	(sagittarius)
	(net server)
	(crypto)
	(rfc x.509)
	(rfc :5322)
	(prefix (binary io) binary:)
	(sagittarius regex)
	(sagittarius socket)
	(rfc tls)
	(srfi :18)
	(srfi :19)
	(srfi :64))

(test-begin "OAuth 2.0")

(test-assert (oauth2-connection? (make-oauth2-http1-connection "api.twitter.com")))
(test-assert (oauth2-connection? (make-oauth2-http2-connection "api.twitter.com")))

(let ((conn (make-oauth2-http1-connection "api.twitter.com")))
  (test-assert (oauth2-connection? (open-oauth2-connection! conn)))
  (test-assert (oauth2-connection? (close-oauth2-connection! conn)))
  (test-assert (http-connection? (oauth2-connection-http-connection conn))))

(let ()
  (define json-string
    "{
       \"access_token\":\"2YotnFZFEjr1zCsicMWpAA\",
       \"token_type\":\"example\",
       \"expires_in\":3600,
       \"refresh_token\":\"tGzv3JOkF0XG5Qx2TlKWIA\",
       \"example_parameter\":\"example_value\"
     }")
  (test-assert (oauth2-access-token? (json-string->access-token json-string)))
  (let ((access-token (json-string->access-token json-string)))
    (test-equal "2YotnFZFEjr1zCsicMWpAA" (oauth2-access-token-access-token access-token))
    (test-equal "example" (oauth2-access-token-token-type access-token))
    (test-assert (time? (oauth2-access-token-expires-in access-token)))
    (test-equal time-duration (time-type (oauth2-access-token-expires-in access-token)))
    (test-equal "tGzv3JOkF0XG5Qx2TlKWIA" (oauth2-access-token-refresh-token access-token))
    (test-assert (not (oauth2-access-token-scope access-token)))
    (test-assert (hashtable? (oauth2-access-token-parameters access-token)))
    (let ((param (oauth2-access-token-parameters access-token)))
      (test-equal "example_value" (hashtable-ref param "example_parameter")))
    (test-assert (not (oauth2-access-token-expired? access-token)))))

(let ((access-token (make-oauth2-access-token "token" "exaple" -1 #f #f #f)))
  (test-assert (oauth2-access-token-expired? access-token)))

(test-equal "grant_type=foo&bar=bar"
	    (oauth2-compose-access-token-request-parameter "foo" :bar "bar"))
(test-error (oauth2-compose-access-token-request-parameter "foo" :bar))
(test-error (oauth2-compose-access-token-request-parameter "foo" "bar" "bar"))

(let ()
  (define json-string
    (string->utf8
     "{
       \"access_token\":\"2YotnFZFEjr1zCsicMWpAA\",
       \"token_type\":\"example\",
       \"expires_in\":3600,
       \"refresh_token\":\"tGzv3JOkF0XG5Qx2TlKWIA\",
       \"example_parameter\":\"example_value\"
     }"))
  (define +shutdown-port+ "17500")
  (define keypair (generate-key-pair RSA :size 1024))
  (define cert (make-x509-basic-certificate keypair 1
					    (make-x509-issuer '((C . "NL")))
					    (make-validity (current-date)
							   (current-date))
					    (make-x509-issuer '((C . "NL")))))
  
  (define config (make-server-config :shutdown-port +shutdown-port+
				     :secure? #t
				     :use-ipv6? #t
				     :certificates (list cert)))
  (define (put-error out)
    (put-bytevector out #*"HTTP/1.1 401 Unauthorized\r\n")
    (put-bytevector out #*"Content-Length: 5\r\n\r\n")
    (put-bytevector out #*"error"))
  (define (do-test-process out method path headers content)
    (test-equal "application/x-www-form-urlencoded"
		(rfc5322-header-ref headers "content-type"))
    (test-assert (rfc5322-header-ref headers "authorization"))
    (test-equal "POST" method)
    (cond ((string=? path "/error") (put-error out))
	  (else
	   (put-bytevector out #*"HTTP/1.1 200 OK\r\n")
	   (put-bytevector out #*"Content-Type: application/json\r\n")
	   (put-bytevector out #*"Content-Length: ")
	   (put-bytevector out
	    (string->utf8 (number->string (bytevector-length json-string))))
	   (put-bytevector out #*"\r\n\r\n")
	   (put-bytevector out json-string))))
  
  (define (handler server socket)
    (call-with-port (socket-port socket #f)
      (lambda (in/out)
	(let ((line (binary:get-line in/out)))
	  (cond ((#/(\w+)\s+([^\s]+)\s+HTTP\/([\d\.]+)/ line) =>
		 (lambda (m)
		   (let ((method (utf8->string (m 1)))
			 (path (utf8->string (m 2)))
			 (headers (rfc5322-read-headers in/out))
			 (content (get-bytevector-all in/out)))
		     (do-test-process in/out method path headers content))))
		;; something went terribly wrong
		(else (get-bytevector-all in/out) (put-error in/out)))))))
  (define server (make-simple-server "10080" handler :config config
				     :exception-handler print))

  (define conn (make-oauth2-http1-connection "localhost:10080"))
  (server-start! server :background #t)
  (thread-sleep! 0.1)
  
  (test-assert (oauth2-access-token?
		(oauth2-request-password-credentials-access-token
		 conn "/password_credentials" "username" "password")))
  (test-assert (oauth2-access-token?
		(oauth2-request-client-credentials-access-token
		 conn "/client_credentials" "credential")))
  (test-error oauth2-authorization-server-error?
	      (oauth2-request-client-credentials-access-token
	       conn "/error" "credential"))
  
  (make-client-socket "localhost" +shutdown-port+)
  (test-assert "finish simple server (2)" (wait-server-stop! server)))


(test-end)
