;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme repl)
    (export interaction-environment)
    (import (rename (sagittarius interactive)
		    (interactive-environment interaction-environment))))
