;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; ec.scm - Elliptic curve
;;; 

;; For convenience
#!nounbound
(library (math ec)
    (export :all)
    (import (sagittarius crypto math ec)))
