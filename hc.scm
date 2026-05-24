(import (rnrs)
	(net http-client)
	(util file)
	(util duration)
	(util concurrent)
	(sagittarius debug)
	(time)
	(srfi :1)
	(srfi :18)
	(srfi :19))

(define debugger (make-remote-debugger "0"))
(print (remote-debugger-port debugger))

(define (open-null-output-port i)
  (define (write! bv start count)
    ;;(format #t "data [~d]~%" i)
    count)
  (define (close) #t) ;; do nothing
  (make-custom-binary-output-port "null-port" write! #f #f close))


(define ((null-data-handler i))
  (let ((out (open-null-output-port i)))
    (values out (lambda (status #:_) #f))))

(define max-connection (* (cpu-count) 5))
(define request-count (* max-connection 20))
(define pooled-connection-manager
  (make-http-pooling-connection-manager
   (http-pooling-connection-config-builder
    ;; timeouts are basically random number, mostly taken from some other
    ;; libraries or whatever values 
    (dns-timeout (duration:of-seconds 30))	  ;; 30s
    (read-timeout (duration:of-seconds 5))	  ;; 120s
    (connection-timeout (duration:of-seconds 60)) ;; 60s
    (connection-request-timeout (duration:of-seconds 2))
    (time-to-live 10)
    (max-connection-per-route max-connection))))
(define http-client
  (http:client-builder
   (follow-redirects (http:redirect always))
   ;;(version (http:version http/1.1))
   (connection-manager pooled-connection-manager)))

(define (request i)
  (http:request-builder
   ;;(uri "https://h2o.examp1e.net/index.html")
   (uri "https://example.com")
   ;;(uri (format "https://www.bing.com?q=~d" i))
   ;;(timeout (duration:of-seconds 2))
   ;;(uri "https://www.google.com")
   ))

(define (run-request count)
  (define now (current-time))
  (define ((print-response-time start) r)
    #;(format #t "total: ~a, resp: ~a~%" (time-difference (current-time) start)
	    (http:response-time r)))
  (for-each (print-response-time now)
  (time (map (lambda (f) (future-get f))
    (map (lambda (i)
	   #;(http:client-send-async http-client (request i)
	    :data-handler (null-data-handler i))
	   (future-map (lambda (r)
			 (format #t "duration [~d] total ~a: response ~a~%" i
				 (time-difference (current-time) now)
				 (http:response-time r))
			 r)
		       (http:client-send-async http-client (request i)
		        :data-handler (null-data-handler i))))
	 (iota count))))))
#;(define (run-request count)
  (define now (current-time))
  (for-each (lambda (i)
	      (let ((r (http:client-send http-client (request i)
					 :data-handler (null-data-handler i))))
		(print "duration [" i "] total "
		       (time-difference (current-time) now)
		       ": response " (http:response-time r))))
	    (iota count)))
(run-request max-connection)
(print "************ connections are created **************")
(let ((count request-count))
  (run-request count)
  (print count))
;;(run-request 10)
;;(run-request 5)
(http:client-shutdown! http-client)
