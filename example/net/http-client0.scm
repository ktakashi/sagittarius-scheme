(import (rnrs)
        (net http-client)
        (util concurrent))

(define client (http:client-builder
                (follow-redirects (http:redirect normal))))

(let ((future (http:client-send-async client
                                      (http:request-builder
                                       (uri "http://google.com")))))
  ;; do whatever you need to do during the above HTTP call
  (let ((response (future-get future)))
    (http:response-status response)  ;; -> 200
    (http:response-headers response) ;; -> header object
    (http:response-body response)    ;; -> body as bytevector
    ))
