;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme process-context)
    (export command-line exit
	    get-environment-variable get-environment-variables)
    (import (rnrs) (srfi :98)))