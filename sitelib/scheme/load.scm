;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme load)
    (export (rename r7rs:load load))
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (util file))

  (define r7rs:load
    (case-lambda
     ((file env)
      (let ((source (file->sexp-list file)))
	(for-each (lambda (e) (eval e env)) source)))
     ((file) (load file))))

)