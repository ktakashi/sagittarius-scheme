;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax-case)
    (export syntax-case syntax datum->syntax)
    (import (core)
	    (sagittarius)
	    (sagittarius compiler)
	    (core base)
	    (core syntax pattern)
	    (core syntax template)
	    (core syntax helper))
  )