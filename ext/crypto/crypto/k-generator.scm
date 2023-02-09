;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; DSA K-generator
;;;

;; For backward compatibility
#!deprecated
(library (crypto k-generator)
    (export (rename (make-random-k-generator random-k-generator)
		    (make-hmac-k-generator determistic-k-generator)))
    (import (sagittarius crypto signatures k-generators)))
