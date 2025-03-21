#!nounbound
;;;;
;; pass1: translation stage.
;; this stage translates s-expression to IForm which is from Gauche.
;; until this stage, every s-expression must be expanded by previous 
;; stage.

(library (sagittarius compiler pass1)
    (export pass1 init-pass1

	    make-bottom-p1env
	    p1env-library)
    (import (sagittarius compiler pass1 core)
	    (sagittarius compiler pass1 syntax)))
