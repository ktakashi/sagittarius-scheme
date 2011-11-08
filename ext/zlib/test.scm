;; -*- coding: utf-8 -*- 
(add-load-path "./zlib")
(library (zlib test)
    (export run-zlib-test)
    (import (srfi :64 testing)
	    (rnrs)
	    (sagittarius)
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
  (define (run-zlib-test)
    (test-equal "deflate"
		'(#t #t #t #t #t #t #t #t #t)
		(map (lambda (level)
		       (compare-file-bytevector
			(format "./zlib/compressed-~a.bin" level)
			(do-deflate "./zlib/rfc/zlib.scm" level)))
		     '(1 2 3 4 5 6 7 8 9)))
    (test-equal "inflate"
		'(#t #t #t #t #t #t #t #t #t)
		(map (lambda (level)
		       (compare-file-bytevector
			"./zlib/rfc/zlib.scm"
			(do-inflate
			 (format "./zlib/compressed-~a.bin" level))))
		     '(1 2 3 4 5 6 7 8 9)))
    
    )

)