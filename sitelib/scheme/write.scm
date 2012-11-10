;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme write)
    (export (rename (write-cyclic write)
		    (write/ss write-shared)
		    (write write-simple))
	    display)
    (import (rnrs) (sagittarius)))

