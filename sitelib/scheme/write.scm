;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme write)
    (export (rename (write-cyclic write)
		    (write/ss write-shared)
		    (write write-simple))
	    display)
    (import (only (rnrs) display write)
	    (only (sagittarius) write/ss write-cyclic)))

