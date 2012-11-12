;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme process-context)
    (export command-line exit emergency-exit
	    get-environment-variable get-environment-variables)
    (import (only (rnrs) command-line exit)
	    (only (sagittarius) emergency-exit)
	    (srfi :98)))