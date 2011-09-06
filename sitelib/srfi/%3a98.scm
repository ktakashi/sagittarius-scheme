;;; -*- mode: scheme; coding: utf-8; -*-
;; SRFI 98 ported for Sagittarius Scheme by Takashi Kato

(library (srfi :98)
    (export get-environment-variable
	    get-environment-variables)
    (import (srfi :98 os-environment-variables))
)