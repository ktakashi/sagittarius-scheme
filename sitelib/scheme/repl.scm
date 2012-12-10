;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme repl)
    (export interaction-environment)
    (import (only (rnrs) define quote)
	    (only (sagittarius vm) find-library))
  (define interaction-environment (find-library 'user #t))
)
