#!read-macro=sagittarius/regex

(import (rnrs)
	(compression huffman)
	(util port)
	(srfi :13)
	(srfi :64))

(define table-definition
  " (65) |0              0 [1]
    (66) |10             2 [2]
    (67) |1100           c [4]
    (68) |11010         1a [5]
    (69) |11011         1b [5]
    (70) |11100         1c [5]
    (71) |11101         1d [5]
    (72) |11110         1e [5]
    (73) |111110        3e [6]
    (74) |111111        3f [7]")

(define rx #/.*\(\s*(\d+)\)\s+([01|]+)\s+(\S+)\s+\[\s*(\d+)\].*/) ;; |]))

(test-begin "Huffman coding")

(define (->bin s)
  (string-fold-right (lambda (c acc) 
		       (if (char=? c #\|)
			   acc
			   (cons (digit-value c) acc)))
		     '()
		     s))

(let* ((in (open-string-input-port table-definition))
       (table (list->vector
	       (port-map (lambda (line)
			   (cond ((rx line) =>
				  (lambda (m)
				    (vector
				     (string->number (m 1))
				     (->bin (m 2)))))))
			 (lambda () (get-line in))))))
  (test-assert "encoder" (procedure? (make-huffman-encoder table)))
  (test-assert "decoder" (procedure? (make-huffman-decoder table)))
  
  (let ((decoder (make-huffman-decoder table)))
    (test-equal "huffman decode" (string->utf8 "HEAD")
		(call-with-bytevector-output-port
		 (lambda (out)
		   (decoder (open-bytevector-input-port #vu8(#xf6 #xda))
			    out))))
    ;; this doesn't workd because of the filler issue
    ;; (see comment on sitelib/compression/huffman.scm
    #;
    (test-equal "huffman decode(2)" (string->utf8 "CAD")
		(call-with-bytevector-output-port
		 (lambda (out)
		   (decoder (open-bytevector-input-port #vu8(#x03 #x1a))
			    out)))))

  (let ((encoder (make-huffman-encoder table)))
    (test-equal "huffman encode" #vu8(#xf6 #xda)
		(call-with-bytevector-output-port
		 (lambda (out)
		   (encoder (open-bytevector-input-port (string->utf8 "HEAD"))
			    out))))
    ;; ditto
    #;
    (test-equal "huffman encode(2)" #vu8(#x03 #x1a)
		(call-with-bytevector-output-port
		 (lambda (out)
		   (encoder (open-bytevector-input-port (string->utf8 "CAD"))
			    out)))))
  )
(test-end)
