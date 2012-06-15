;; -*- coding: utf-8 -*- 
(add-load-path "./zlib")

(import (srfi :64 testing)
	(rnrs)
	(sagittarius)
	(sagittarius control)
	(rfc zlib))

(define (do-deflate file level)
  (call-with-bytevector-output-port
   (lambda (out)
     (define dout (open-deflating-output-port
		   out
		   :compression-level level))
     (call-with-port
      (open-file-input-port file (file-options) 'block)
      (lambda (in)
	(let loop ((r (get-bytevector-n in 1024)))
	  (unless (eof-object? r)
	    (put-bytevector dout r)
	    (loop (get-bytevector-n in 1024))))
	(close-port dout))))))

(define (do-inflate file)
  (call-with-port 
   (open-file-input-port file (file-options) 'block)
   (lambda (in)
     (define din (open-inflating-input-port in))
     (let ((bv (get-bytevector-all din)))
       (close-port din)
       bv))))

(define (compare-file-bytevector file bv)
  (let ((c (call-with-port
	    (open-file-input-port file (file-options) 'block)
	    (lambda (in)
	      (get-bytevector-all in)))))
    (bytevector=? c bv)))

;; the files compressed-1.bin ~ compressed-9.bin are generatedo by Gauche.
(test-begin "(run-zlib-test)")
(test-equal "deflate"
	    '(#t #t #t #t #t #t #t #t #t)
	    (map (lambda (level)
		   (compare-file-bytevector
		    (format (build-path (current-directory)
					"zlib/data/compressed-~a.bin") level)
		    (do-deflate (build-path (current-directory)
					    "zlib/data/data.txt") level)))
		 '(1 2 3 4 5 6 7 8 9)))
(test-equal "inflate"
	    '(#t #t #t #t #t #t #t #t #t)
	    (map (lambda (level)
		   (compare-file-bytevector
		    (build-path (current-directory) "zlib/data/data.txt")
		    (do-inflate
		     (format (build-path (current-directory)
					 "zlib/data/compressed-~a.bin")
			     level))))
		 '(1 2 3 4 5 6 7 8 9)))

(test-equal "inflate dictionary"
	    "abc"
	    (let1 bv (call-with-bytevector-output-port
		      (lambda (p)
			(let1 p2 (open-deflating-output-port 
				  p :dictionary (string->utf8 "abc"))
			  (put-bytevector p2 (string->utf8 "abc"))
			  (close-output-port p2))))
	      (utf8->string
	       (get-bytevector-all (open-inflating-input-port
				    (open-bytevector-input-port bv)
				    :dictionary (string->utf8 "abc"))))))
  
(test-end)
