;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme char normalization)
    (export string-ni<=? string-ni<? string-ni=?
	    string-ni>=? string-ni>?)
    (import (rnrs))
  ;; from mosh
  (define (string-normalize str)
    str)

  (define (make-ni proc)
    (lambda x (apply proc (map x string-normalize))))

  (define string-ni=? (make-ni string=?))
  (define string-ni<? (make-ni string<?))
  (define string-ni>? (make-ni string>?))
  (define string-ni<=? (make-ni string<=?))
  (define string-ni>=? (make-ni string>=?))

)