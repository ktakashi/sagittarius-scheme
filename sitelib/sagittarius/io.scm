;; -*- scheme -*-
(library (sagittarius io)
    (export call-with-input-string
	    call-with-output-string
	    with-input-from-string
	    with-input-from-port
	    with-output-to-string
	    with-output-to-port)
    (import (core)
	    (srfi :6 basic-string-ports)
	    (srfi :39 parameters))

  (define (call-with-input-string str proc)
    (proc (open-input-string str)))

  (define (call-with-output-string proc)
    (let ((port (open-output-string)))
      (proc port)
      (get-output-string port)))

  (define (with-input-from-string str thunk)
    (parameterize ((current-input-port (open-input-string str)))
      (thunk)))

  (define (with-input-from-port port thunk)
    (parameterize ((current-input-port port))
      (thunk)))

  (define (with-output-to-string thunk)
    (let ((port (open-output-string)))
      (parameterize ((current-output-port port)) (thunk))
      (get-output-string port)))

  (define (with-output-to-port port thunk)
    (parameterize ((current-output-port port))
      (thunk)))

)