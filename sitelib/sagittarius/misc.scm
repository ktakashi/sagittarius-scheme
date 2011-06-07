;; -*- scheme -*-
(library (sagittarius misc)
    (export output-port-width)
    (import (core)
	    (core base)
	    (sagittarius))

  (define *output-port-width* 79)
  ;; we don't provide specific port width from port.
  (define (output-port-width port . width)
    (if (null? width)
	*output-port-width*
	(set! *output-port-width* (car width))))
)
