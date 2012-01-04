;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme write)
    (export (rename (write/ss write)
		    (write write-simple))
	    display)
    (import (rnrs) (sagittarius)))
	    