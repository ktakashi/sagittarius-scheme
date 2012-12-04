;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme load)
    (export (rename (r7rs:load load)))
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius))

  (define r7rs:load
    (case-lambda
     ((file env)
      (call-with-input-file file
	(lambda (in)
	  (let loop ((e (read/ss in)))
	    (unless (eof-object? e)
	      (eval e env))))))
     ((file) (load file))))

)
