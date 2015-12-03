;;; -*- Scheme -*-
;;;
;;; base64.scm - base64 encoding/decoding routine
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
#!core
(library (rfc base64)
    (export base64-encode base64-encode-string
	    base64-decode base64-decode-string

	    open-base64-encode-output-port
	    open-base64-encode-input-port

	    open-base64-decode-output-port
	    open-base64-decode-input-port)
    (import (rnrs) (rnrs r5rs)
	    (sagittarius)
	    (sagittarius control))

  (define *decode-table*
    ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
    #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  62  #f  #f  #f  63  
    ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
      52  53  54  55  56  57  58  59  60  61  #f  #f  #f  #f  #f  #f
    ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
      #f  0   1   2   3   4   5   6   7   8   9   10  11  12  13  14
    ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
      15  16  17  18  19  20  21  22  23  24  25  #f  #f  #f  #f  #f
    ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
      #f  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
    ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
      41  42  43  44  45  46  47  48  49  50  51  #f  #f  #f  #f  #f
      ))

  (define *encode-table*
    (vector-map char->integer
     ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
     #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
       ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
       #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
       ;;32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
       #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
       ;;48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
       #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/
       ;;pad 
       #\= )))

  (define (base64-decode-string string
				:key (transcoder (make-transcoder 
						  (utf-8-codec) 'none)))
      (or (string? string)
	  (assertion-violation 'base64-decode-string
			       (format "string required, but got ~s" string)
			       string))
      (let ((bv (base64-decode (string->utf8 string))))
	(if transcoder
	    (bytevector->string bv transcoder)
	    bv)))

  (define (base64-decode in)
    (if (bytevector? in)
	(base64-decode (open-bytevector-input-port in))
	(call-with-bytevector-output-port
	 (lambda (out)
	   (base64-decode-impl in out)))))

  (define (base64-decode-impl in out)
    (let-syntax ((lookup (syntax-rules ()
			   ((_ b)
			    (and (< 32 b 128)
				 (vector-ref *decode-table* (- b 32)))))))
      (define (d0 b)
	(cond ((eof-object? b))
	      ((= b #x1d)) ;; =
	      ((lookup b) => (lambda (v) (d1 (get-u8 in) v)))
	      (else (d0 (get-u8 in)))))
      
      (define (d1 b hi)
	(cond ((eof-object? b))
	      ((= b #x1d)) ;; =
	      ((lookup b) => (lambda (lo)
			       (put-u8 out (+ (* hi 4) (quotient lo 16)))
			       (d2 (get-u8 in) (modulo lo 16))))
	      (else (d1 (get-u8 in) hi))))

      (define (d2 b hi)
	(cond ((eof-object? b))
	      ((= b #x1d)) ;; =
	      ((lookup b) => (lambda (lo)
			       (put-u8 out (+ (* hi 16) (quotient lo 4)))
			       (d3 (get-u8 in) (modulo lo 4))))
	      (else (d2 (get-u8 in) hi))))

      (define (d3 b hi)
	(cond ((eof-object? b))
	      ((= b #x1d)) ;; =
	      ((lookup b) => (lambda (lo)
			       (put-u8 out (+ (* hi 64) lo))
			       (d0 (get-u8 in))))
	      (else (d2 (get-u8 in) hi))))
      (d0 (get-u8 in))))    

  (define (base64-encode-string string :key
				(transcoder (make-transcoder 
					     (utf-8-codec) 'none))
				(line-width 76))
    (or (string? string)
	(assertion-violation 'base64-encode-string
			     (format "string required, but got ~s" string)
			     string))
    (utf8->string
     (base64-encode (string->bytevector string transcoder)
		    :line-width line-width)))

  (define (base64-encode in :key (line-width 76))
    (if (bytevector? in)
	(base64-encode (open-bytevector-input-port in) :line-width line-width)
	(call-with-bytevector-output-port
	 (lambda (out)
	   (base64-encode-impl in out line-width)))))

  (define (base64-encode-impl in out line-width)
    (define max-col (and line-width
			 (> line-width 0)
			 (- line-width 1)))
    (letrec-syntax ((emit* (syntax-rules ()
			     ((_ col) col)
			     ((_ col idx idx2 ...)
			      (begin
				(put-u8 out (vector-ref *encode-table* idx))
				(let ((col2 (cond ((eqv? col max-col)
						   ;; newline
						   (put-u8 out #x0a) 0)
						  (else (+ col 1)))))
				  (emit* col2 idx2 ...)))))))
      (define (e0 c col)
	(cond ((eof-object? c))
	      (else
	       (e1 (get-u8 in) (modulo c 4) (emit* col (quotient c 4))))))

      (define (e1 c hi col)
	(cond ((eof-object? c)
	       (emit* col (* hi 16) 64 64))
	      (else
	       (e2 (get-u8 in) (modulo c 16)
		   (emit* col (+ (* hi 16) (quotient c 16)))))))

      (define (e2 c hi col)
	(cond ((eof-object? c)
	       (emit* col (* hi 4) 64))
	      (else
	       (e0 (get-u8 in)
		   (emit* col (+ (* hi 4) (quotient c 64)) (modulo c 64))))))

      (e0 (get-u8 in) 0)))

  ;; basically the same as above but it input length is unknown
  ;; always encode 3 bytes to 4 bytes
  (define (make-base64-encoder real-put line-width)
    (define max-col (and line-width (> line-width 0) (- line-width 1)))
    (define col 0)
    (define (check-col)
      (when max-col
	(if (= col max-col)
	    (begin
	      (real-put -1) ;; so that implementation may choose end line
	      (set! col 0))
	    (set! col (+ col 1)))))
    (define (put i)
      (real-put i)
      (check-col))

    (lambda (b0 b1 b2)
      (define lshift bitwise-arithmetic-shift-left)
      (define rshift bitwise-arithmetic-shift-right)

      (when (>= b0 0)
	(put (rshift (bitwise-and #xFC b0) 2))
	(let ((b (lshift (bitwise-and #x03 b0) 4)))
	  (cond ((negative? b1)
		 (put b) (put 64) (put 64))
		(else
		 (put (bitwise-ior b (rshift (bitwise-and #xF0 b1) 4)))
		 (let ((b (lshift (bitwise-and #x0F b1) 2)))
		   (cond ((negative? b2)
			  (put b) (put 64))
			 (else
			  (put (bitwise-ior b (rshift (bitwise-and #xC0 b2) 6)))
			  (put (bitwise-and #x3F b2)))))))))))

  (define (open-base64-encode-output-port sink :key (owner? #f) (line-width #f))
    (define buffer (make-bytevector 3 0))
    (define buffer-count 0)

    (define (fill-buffer bv start count)
      (define size (min (- 3 buffer-count) count))
      (bytevector-copy! bv start buffer buffer-count size)
      (set! buffer-count (+ buffer-count size))
      size)

    (define (put i) 
      (if (negative? i)
	  (put-u8 sink #x0a)
	  (put-u8 sink (vector-ref *encode-table* i))))
    (define encoder (make-base64-encoder put line-width))

    (define (process-encode)
      (define (get n)
	(or (and (> buffer-count n) (bytevector-u8-ref buffer n)) -1))
      (define b0 (get 0))
      (define b1 (get 1))
      (define b2 (get 2))
      (encoder b0 b1 b2)
      (set! buffer-count 0))

    (define (write! bv start count) 
      (let loop ((start start) (rest count))
	(if (zero? rest)
	    count
	    (let ((n (fill-buffer bv start rest)))
	      (when (= buffer-count 3) (process-encode))
	      (loop (+ start n) (- rest n))))))
    
    (define (close) 
      ;; do the last
      (process-encode)
      (flush-output-port sink)
      (when owner? (close-port sink)))
    (make-custom-binary-output-port "base64-encode-output-port"
				    write! #f #f close))

  ;; TODO I don't think this has good performance
  (define (open-base64-encode-input-port source
					 :key (owner? #f) (line-width #f))
    ;; max length = when line-width is 1
    (define buffer (make-bytevector 8 0))
    (define buffer-count 0)

    (define (fill-buffer! bv start count)
      (define size (min buffer-count count))
      (bytevector-copy! buffer 0 bv start size)
      (set! buffer-count (- buffer-count size))
      (bytevector-copy! buffer size buffer 0 buffer-count)
      size)

    (define (put i)
      (bytevector-u8-set! buffer buffer-count 
			  (if (negative? i)
			      #x0a
			      (vector-ref *encode-table* i)))
      (set! buffer-count (+ buffer-count 1)))

    (define encoder (make-base64-encoder put line-width))

    (define (process-encode)
      (define (get prev) 
	(if (negative? prev)
	    -1
	    (let ((b (get-u8 source)))
	      (if (eof-object? b)
		  -1
		  b))))
      (define b0 (get 0))
      (define b1 (get b0))
      (define b2 (get b1))
		    
      (encoder b0 b1 b2))

    (define (read! bv start count) 
      (let loop ((start start) (set 0))
	(cond ((= set count) count)
	      ((not (zero? buffer-count))
	       (let ((n (fill-buffer! bv start (- count set))))
		 (loop (+ start n) (+ set n))))
	      (else
	       (process-encode)
	       (let ((n (fill-buffer! bv start (- count set))))
		 (if (zero? n)
		     set
		     (loop (+ start n) (+ set n))))))))
    
    (define (close) (when owner? (close-port source)))
    (make-custom-binary-input-port "base64-encode-input-port"
				   read! #f #f close))

  ;; decode port
  (define (make-base64-decoder put)
    (lambda (b0 b1 b2 b3)
      (define (check b) (>= b 0))
      (define lshift bitwise-arithmetic-shift-left)
      (define rshift bitwise-arithmetic-shift-right)
      
      (when (and (check b0) (check b1))
	(put (bitwise-and (bitwise-ior (lshift b0 2) (rshift b1 4)) #xFF))
	(when (check b2)
	  (put (bitwise-and (bitwise-ior (lshift b1 4) (rshift b2 2)) #xFF))
	  (when (check b3)
	    (put (bitwise-and (bitwise-ior (lshift b2 6)  b3) #xFF)))))))
      
  ;; TODO maybe we want to make buffer size bigger for performance?
  (define (open-base64-decode-input-port source :key (owner? #f))
    (define output-buffer (make-bytevector 3))
    (define output-buffer-size 0)
    (define (put b)
      (bytevector-u8-set! output-buffer output-buffer-size b)
      (set! output-buffer-size (+ output-buffer-size 1)))
    (define decoder (make-base64-decoder put))
    ;; Should we raise an error if the input is not multiple of 4?
    (define (decode1)
      (define (get)
	(let ((b (get-u8 source)))
	  (cond ((eof-object? b) -1)
		((and (< 32 b 128) (vector-ref *decode-table* (- b 32))))
		(else (get)))))
      (define b0 (get))
      (define b1 (get))
      (define b2 (get))
      (define b3 (get))
      (decoder b0 b1 b2 b3)
      output-buffer-size)

    (define (read! bv start count)
      (define (copy-buffer! i n count)
	(let ((size (min n count)))
	  (bytevector-copy! output-buffer 0 bv i size)
	  (set! output-buffer-size (- output-buffer-size size))
	  ;; TODO should we manage position instead of sliding?
	  (bytevector-copy! output-buffer size
			    output-buffer 0 output-buffer-size)
	  size))
      (let loop ((i start) (set 0))
	(cond ((= set count) count)
	      ((not (zero? output-buffer-size))
	       (let ((size (copy-buffer! i output-buffer-size (- count set))))
		 (loop (+ i size) (+ set size))))
	      (else
	       (let ((n (decode1)))
		 (if (zero? n)
		     set
		     (let ((size (copy-buffer! i n (- count set))))
		       (loop (+ i size) (+ set size)))))))))

    (define (close) (when owner? (close-port source)))

    (make-custom-binary-input-port "base64-decode-input-port"
				   read! #f #f close))
  
  (define (open-base64-decode-output-port sink :key (owner? #f))
    (define (put b) (put-u8 sink b))
    (define decoder (make-base64-decoder put))
    
    (define buffer (make-bytevector 4))
    (define buffer-size 0)
    (define (fill-buffer bv start count)
      (define size (min (- 4 buffer-size) count))
      (let loop ((i 0))
	(if (= i size)
	    size
	    (let ((b (bytevector-u8-ref bv (+ start i))))
	      (cond ((and (< 32 b 128) (vector-ref *decode-table* (- b 32))) =>
		     (lambda (b) 
		       (bytevector-u8-set! buffer buffer-size b)
		       (set! buffer-size (+ buffer-size 1)))))
	      (loop (+ i 1))))))

    (define (write! bv start count)
      (let loop ((i start) (set 0))
	(cond ((= buffer-size 4)
	       (decoder (bytevector-u8-ref buffer 0)
			(bytevector-u8-ref buffer 1)
			(bytevector-u8-ref buffer 2)
			(bytevector-u8-ref buffer 3))
	       (set! buffer-size 0)
	       (loop i set))
	      ((= set count) count)
	      (else
	       (let ((n (fill-buffer bv i (- count set))))
		 (loop (+ i n) (+ set n)))))))

    (define (close) 
      (define (get n)
	(or (and (> buffer-size n) (bytevector-u8-ref buffer n)) -1))
      (unless (zero? buffer-size)
	(decoder (get 0) (get 1) (get 2) (get 3)))
      (flush-output-port sink)
      (when owner? (close-port sink)))

    (make-custom-binary-output-port "base64-decode-output-port"
				    write! #f #f close))

)
