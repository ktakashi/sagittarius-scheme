(import (rnrs)
	(rpc json)
	(util file)
	(json)
	(srfi :64))

(test-begin "JSON RPC tests")

(test-assert "predicate (request)" (json-request? (make-json-request 'foo)))
(test-assert "predicate (response)" 
	     (json-response? (make-json-response "id" #f)))

(define request-data (string-append (current-directory)
				    "/test/data/json-rpc-request.json"))
(define response-data (string-append (current-directory)
				    "/test/data/json-rpc-response.json"))

(define-syntax test-json-rpc
  (syntax-rules ()
    ((_ file ctr rtc extra)
     (let ((str (file->string file)))
       (test-assert 'ctr (ctr str))
       (let ((rpc (ctr str))
	     (e   (json-read (open-string-input-port str))))
	 (test-equal 'rtc e (json-read (open-string-input-port (rtc rpc))))
	 (extra rpc))))))

(test-json-rpc request-data json-string->json-request json-request->json-string
	       (lambda (req)
		 (test-equal "id" "id1" (json-request-id req))
		 (test-equal "params" "parameter string" 
			     (json-request-params req))
		 (test-equal "method" "foo" (json-request-method req))))
(test-json-rpc response-data json-string->json-response 
	       json-response->json-string
	       (lambda (res)
		 (test-equal "id" "id1" (json-response-id res))
		 (test-equal "result" "result string" 
			     (json-response-result res))))


(test-end)