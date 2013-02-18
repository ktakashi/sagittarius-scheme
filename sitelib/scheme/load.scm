;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme load)
    (export (rename (r7rs:load load)))
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (sagittarius vm)
	    (srfi :39))

  (define r7rs:load
    (case-lambda
     ((file env)
      ;; To detect #!reader=... notation, we need to use
      ;; 'load' procedure, otherwise it can't detect it.
      (parameterize ((vm-current-library env))
	(r7rs:load file)))
     ((file) (load file))))

)
