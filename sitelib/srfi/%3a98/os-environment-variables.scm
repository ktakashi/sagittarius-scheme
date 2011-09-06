;;; -*- mode: scheme; coding: utf-8; -*-
;; SRFI 98 ported for Sagittarius Scheme by Takashi Kato

(library (srfi :98 os-environment-variables)
    (export (rename (getenv       get-environment-variable)
		    (getenv-alist get-environment-variables)))
    (import (sagittarius))
)