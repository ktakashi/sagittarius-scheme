;; -*- mode:scheme; coding:utf-8; -*-
(library (crypto marker)
    (export <marker>)
    (import (rnrs)
	    (clos user))

  (define-class <marker> ()
    ((name :init-keyword :name)))

  )
