;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; secure.scm - Non Tomcrypt bindings
;;;
#!nounbound
(library (sagittarius crypto secure)
    (export safe-bytevector=?)
    (import (sagittarius dynamic-module))

(load-dynamic-module "sagittarius--secure")

)
