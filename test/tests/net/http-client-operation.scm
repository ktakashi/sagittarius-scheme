#!read-macro=sagittarius/bv-string
(import (rnrs)
	(net http-client)
	(net server)
	(net http-server)
	(srfi :18)
	(srfi :64)
	(util concurrent))

(test-begin "net/http-client operation")

(define (start-server)
  (define (app req res)
    (let ((path (http-server:request-path req)))
      (print "  server received request on: " path)
      (cond ((string=? path "/slow")
	     (thread-sleep! 0.5)
	     (http-server:response-text! res "slow"))
	    ((string=? path "/discard")
	     (http-server:response-text! res "discard-this-body"))
	    ((string=? path "/stream")
	     (http-server:response-header-set! res "Content-Type" "text/plain")
	     (http-server:response-text! res "stream-body"))
	    ((string=? path "/sse")
	     (http-server:response-bytes! res #*"data: hello\n\n"
					 "text/event-stream"))
	    (else
	     (http-server:response-text! res "hello"))))
    res)
  (define server (make-http-server "0" app))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (do ()
      ((server-running? server))
    (thread-sleep! 0.05))
  server)

(let ((server (start-server)))
  (define (make-uri path)
    (format "http://localhost:~a~a" (server-port server) path))
  (define client (http:client-builder (version (http:version http/1.1))))

  (print "testing client operation")
  (let ((events '())
	(lock (make-mutex "operation-test-lock")))
    (define (record-event! e)
      (dynamic-wind
	  (lambda () (mutex-lock! lock))
	  (lambda () (set! events (cons e events)))
	  (lambda () (mutex-unlock! lock))))
    (define (event-seen? e)
      (dynamic-wind
	  (lambda () (mutex-lock! lock))
	  (lambda () (and (memq e events) #t))
	  (lambda () (mutex-unlock! lock))))

    (let-values (((f success failure) (make-piped-future)))
      (define response-status #f)
      (define response-headers '())
      (define response-body-parts '())
      (define (on-init request header-handler data-handler)
	(make-http:response-context request header-handler data-handler))
      (define on-finalize
	(lambda (ctx)
	  (define headers (http:make-headers))
	  (for-each (lambda (kv)
		      (hashtable-set! headers (car kv) (cdr kv)))
		    response-headers)
	  (http:response-builder
	   (status response-status)
	   (headers headers)
	   (body (if (null? response-body-parts)
		     #vu8()
		     (apply bytevector-append (reverse response-body-parts)))))))
      (let* ((request (http:request-builder
		       (method 'GET)
		       (uri (make-uri "/ok"))))
	     (operation
	      (http:client-start client request
	       :on-init on-init
	       :on-finalize on-finalize
	       :on-headers (lambda (op ctx status headers has-data?)
			     (record-event! 'headers)
			     (set! response-status status)
			     (set! response-headers headers))
	       :on-data (lambda (op ctx data end?)
			  (record-event! 'data)
			  (set! response-body-parts
				(cons data response-body-parts)))
	       :on-complete (lambda (op response)
			      (record-event! 'complete)
			      (success response))
	       :on-error (lambda (op e) (failure e)))))
	(test-assert "client-start returns operation"
		     (http:operation? operation))
	(test-assert "operation starts asynchronously"
		     (not (memq (http:operation-state operation)
				'(completed failed))))
	(let ((response (future-get f 5 #f)))
	  (test-assert "operation completed with response"
		       (http:response? response))
	  (test-equal "operation status" "200" (http:response-status response))
	  (test-equal "operation state completed"
		      'completed
		      (http:operation-state operation))
	  (test-assert "headers callback called" (event-seen? 'headers))
	  (test-assert "data callback called" (event-seen? 'data))
	  (test-assert "complete callback called" (event-seen? 'complete))))))

  (print "testing discarding response")
  (let-values (((f success failure) (make-piped-future)))
    (define chunk-count 0)
    (define byte-count 0)
    (define request (http:request-builder
		     (method 'GET) 
		     (uri (make-uri "/discard"))))
    (define operation
      (http:client-start client request
       :on-init (lambda (request header-handler data-handler)
		  (make-http:response-context request
					      header-handler data-handler))
       :on-headers (lambda (op ctx status headers has-data?) #t)
       :on-data (lambda (op ctx data end?)
		  (set! chunk-count (+ chunk-count 1))
		  (set! byte-count (+ byte-count (bytevector-length data))))
       :on-finalize (lambda (ctx)
		      (vector 'discarded chunk-count byte-count))
       :on-complete (lambda (op result) (success result))
       :on-error (lambda (op e) (failure e))))
    (let ((result (future-get f 5 #f)))
      (test-assert "discard context finalizes without http:response"
		   (vector? result))
      (test-equal "discard context marker" 'discarded (vector-ref result 0))
      (test-assert "discard context received at least one chunk"
		   (> (vector-ref result 1) 0))
      (test-assert "discard context counted body bytes"
		   (> (vector-ref result 2) 0))
      (test-equal "discard context operation state"
		  'completed
		  (http:operation-state operation))))

  (print "testing stream response")
  (let-values (((f success failure) (make-piped-future)))
    (define request (http:request-builder 
		     (method 'GET)
		     (uri (make-uri "/stream"))))
    (define operation
      (http:client-start client request
       :on-init (lambda (request header-handler data-handler)
		  (make-http:response-context request
					      header-handler data-handler))
       :on-headers (lambda (op ctx status headers has-data?)
		     (http:response-context-takeover! ctx 'http/1.1-connection))
       :on-finalize (lambda (ctx)
		      (http:response-context-takeover-resource ctx))
       :on-complete (lambda (op response) (success response))
       :on-error (lambda (op e) (failure e))))
    (let ((response (future-get f 5 #f)))
      (test-assert "takeover context returns stream response"
		   (http:stream-response? response))
      (test-equal "takeover stream body"
		  #*"stream-body"
		  (get-bytevector-all (http:response-body response)))
      (test-equal "takeover operation state"
		  'completed
		  (http:operation-state operation))
      (http:stream-response-close! response)))
  
  (print "testing sse")
  (let* ((request (http:request-builder (method 'GET) (uri (make-uri "/sse"))))
	 (response (http:client-send client request)))
    (test-assert "default SSE response uses stream response"
		 (http:stream-response? response))
    (test-equal "default SSE stream body"
		#*"data: hello\n\n"
		(get-bytevector-all (http:response-body response)))
    (http:stream-response-close! response))
  

  (print "testing takeover")
  (let* ((request (http:request-builder (method 'GET) (uri (make-uri "/ok"))))
	 (ctx (make-http:response-context request (lambda args #t)
					  (lambda args #t))))
    (test-assert "takeover request predicate"
		 (not (http:response-context-takeover-requested? ctx)))
    (http:response-context-takeover! ctx 'http/2-stream 'dummy-stream)
    (test-assert "http/2 stream takeover request predicate"
		 (http:response-context-takeover-requested? ctx))
    (test-eq "http/2 stream takeover kind"
	     'http/2-stream
	     (http:response-context-takeover-kind ctx))
    (test-eq "http/2 stream takeover resource"
	     'dummy-stream
	     (http:response-context-takeover-resource ctx)))
  
  (let ((completed 0)
	(failed 0))
    (define slow-request
      (http:request-builder (method 'GET) (uri (make-uri "/slow"))))
    (define operation
      (http:client-start client slow-request
			 :on-complete (lambda (op response)
					(set! completed (+ completed 1)))
			 :on-error (lambda (op e)
				     (set! failed (+ failed 1)))))
    (thread-sleep! 0.05)
    (test-assert "cancel returns true for active operation"
		 (http:operation-cancel! operation))
    (test-assert "cancel is idempotent"
		 (not (http:operation-cancel! operation)))
    (thread-sleep! 0.8)
    (test-equal "cancelled state" 'cancelled (http:operation-state operation))
    (test-equal "no completion callback after cancel" 0 completed)
    (test-equal "no error callback after cancel" 0 failed))
  
  (print "testing send-async")
  (let* ((request (http:request-builder (method 'GET) (uri (make-uri "/ok"))))
	 (response (future-get (http:client-send-async client request) 5 #f)))
    (test-assert "send-async adapter returns response"
		 (http:response? response))
    (test-equal "send-async adapter status" "200"
		(http:response-status response)))

  (print "testing send")
  (let* ((request (http:request-builder (method 'GET) (uri (make-uri "/ok"))))
	 (response (http:client-send client request)))
    (test-equal "send compatibility status" "200"
		(http:response-status response)))
  
  (http:client-shutdown! client)
  (server-stop! server))

(test-end)
