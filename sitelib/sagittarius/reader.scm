;; -*- mode: scheme; coding: utf-8; -*-
;; This file is a part of Sagittarius Scheme system.
#!compatible
(library (sagittarius reader)
    (export define-reader-macro
	    define-dispatch-macro
	    get-macro-character
	    set-macro-character
	    get-dispatch-macro-character
	    set-dispatch-macro-character)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius vm))

  (define-syntax define-reader-macro
    (syntax-rules ()
      ((_ name c proc)
       (define-reader-macro :define name c proc #f))
      ((_ name c proc non-term?)
       (define-reader-macro :define name c proc non-term?))
      ((k :define name c proc non-term?)
       (begin
	 (define name proc)
	 (unless (char? 'c)
	   (assertion-violation 'k
				(format "character requireb but got ~s" 'c)))
	 (unless (procedure? name)
	   (assertion-violation 'k
				(format "procedure requireb but got ~s" name)))
	 (%insert-macro-character c name (vm-current-library) non-term?)))))

  (define-syntax define-dispatch-macro
    (syntax-rules ()
      ((_ name c sc proc)
       (define-dispatch-macro :define name c sc proc #f))
      ((_ name c sc proc non-term?)
       (define-dispatch-macro :define name c sc proc non-term?))
      ((k :define name c sc proc non-term?)
       (begin
	 (define name proc)
	 (unless (and (char? 'c) (char? 'sc))
	   (assertion-violation 'k
				(format "character requireb but got ~s and ~s"
					'c 'sc)))
	 
	 (unless (procedure? name)
	   (assertion-violation 'k
				(format "procedure requireb but got ~s" name)))
	 (%insert-dispatch-macro-character c sc name (vm-current-library)
					   non-term?)))))
)