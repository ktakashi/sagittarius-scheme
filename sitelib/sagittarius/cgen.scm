;; -*- Scheme -*-
;; CGen, originally from Gauche
;;
;; This library just exports all variables from (sagittarius cgen cise) and
;; (sagittarius cgen unit). These 2 libraries do not depend on Sagittarius
;; headers. So it can be used for normal C generator (I guess).
#!compatible
(library (sagittarius cgen)
    (export :all)
    (import (sagittarius cgen cise)
	    (sagittarius cgen unit)))
