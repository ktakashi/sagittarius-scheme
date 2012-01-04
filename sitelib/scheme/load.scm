;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme load)
    (export load)
    (import (rnrs)
	    (sagittarius)
	    (util file))

  (define load
    (case-lambda
     ((file env)
      (let ((source (file->sexp-list file)))
	(for-each (lambda (e) (eval e env)) source)))
     ((file) (load file))))

)