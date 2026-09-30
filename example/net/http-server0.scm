(import (rnrs)
	(net server)
	(net http-server))

;; request handlers 
(define (post req res)
  (http-server:response-status-set! res 200)
  (http-server:response-bytes! res
    (http-server:request-body-bytevector req)
    (http-server:request-header-ref req "content-type")))

(define (get req res)
  (http-server:response-status-set! res 200)
  (http-server:response-cacheable?-set! res #t)
  (http-server:response-text! res "hello"))

;; Creating a router
(define router (make-http-server:router))
(http-server:router-add-route! router 'GET "/hello" get)
(http-server:router-add-route! router 'POST "/hello" post)

;; default HTTP
(define server (make-http-server "8080" (http-server:make-router-handler router)))

;; run the server
(server-start! server :background #f)

#|
# Check with `curl`
$ curl http://localhost:8080/hello
hello

$ curl http://localhost:8080/hello -X POST -d "foo"
foo
|#
