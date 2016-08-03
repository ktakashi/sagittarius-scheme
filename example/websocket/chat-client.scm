(import (rnrs)
	(sagittarius)
	(rfc websocket)
	(rfc uri)
	(getopt))

(define (connect-server host port path)
  (let* ((uri (uri-compose :scheme "ws" :host host :port port :path path)))
    (websocket-open
     (websocket-on-binary-message
      (websocket-on-text-message
       (websocket-on-error
	(websocket-on-open (make-websocket uri)
	 (lambda (ws) (print "CONNECTED!")))
	(lambda (ws e) (print "ERROR: " e)))
       (lambda (ws text) (newline) (print text) (format #t "> ~!")))
      (lambda (ws bin) (put-bytevector (standart-output-port) bin))))))

(define (chat ws)
  (let loop ()
    (format #t "> ~!")
    (let ((line (get-line (current-input-port))))
      (cond ((eof-object? line)
	     (websocket-close ws)
	     (display "Bye") (newline))
	    (else
	     (websocket-send ws line)
	     (loop))))))

(define (main args)
  (with-args (cdr args)
      ((port (#\p "port") #t "80")
       (host (#\h "host") #t "localhost")
       (path (#\r "room") #t "/chat"))
    (chat (connect-server host port path))))
