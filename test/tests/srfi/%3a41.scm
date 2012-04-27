;; -*- mode:scheme; coding: utf-8; -*-

(import (rnrs)
	(srfi :64 testing)
	(srfi :41))

(define power-table
  (stream-of
   (stream-of (expt m n)
	      (m in (stream-from 1)))
   (n in (stream-from 2))))

(test-begin "SRFI-41 test")

(test-equal "power-table"
	    '(1 8 27 64 125 216 343 512 729 1000)
	    (stream->list 10 (stream-ref power-table 1)))
(test-end)

