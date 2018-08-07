(import (rnrs)
	(text yaml builder)
	(text yaml nodes)
	(text yaml tags)
	(srfi :64))

(test-begin "YAML builder")

(define (->yaml sexp)
  (yaml->sexp (canonical-sexp->yaml-node sexp)))

(test-equal "string" (->yaml `(,+yaml-tag:str+ . "string")))

(test-end)
