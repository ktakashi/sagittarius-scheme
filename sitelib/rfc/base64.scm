;;; -*- Scheme -*-
;;;
;;; base64.scm - base64 encoding/decoding routine
;;;  
;;;   Copyright (c) 2010-2020  Takashi Kato  <ktakashi@ymail.com>
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

;; ref: https://tools.ietf.org/html/rfc4648
#!nounbound
(library (rfc base64)
    (export base64-encode base64-encode-string
	    base64-decode base64-decode-string
	    
	    base64url-encode base64url-encode-string
	    base64url-decode base64url-decode-string
	    
	    open-base64-encode-output-port
	    open-base64-encode-input-port
	    open-base64url-encode-output-port
	    open-base64url-encode-input-port

	    open-base64-decode-output-port
	    open-base64-decode-input-port
	    open-base64url-decode-output-port
	    open-base64url-decode-input-port

	    ;; For custom encoder/decoder
	    (rename (*decode-table* *base64-decode-table*)
		    (*encode-table* *base64-encode-table*)
		    (*decode-url-table* *base64-decode-url-table*)
		    (*encode-url-table* *base64-encode-url-table*))
	    make-base64-decoder make-base64-encoder
	    )
    (import (rnrs)
	    (sagittarius))

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

  ;; base64url
  (define *decode-url-table*
    ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
    #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  62  #f  #f  
    ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
      52  53  54  55  56  57  58  59  60  61  #f  #f  #f  #f  #f  #f
    ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
      #f  0   1   2   3   4   5   6   7   8   9   10  11  12  13  14
    ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
      15  16  17  18  19  20  21  22  23  24  25  #f  #f  #f  #f  63
    ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
      #f  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
    ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
      41  42  43  44  45  46  47  48  49  50  51  #f  #f  #f  #f  #f
      ))

  (define *encode-url-table*
    (vector-map char->integer
       ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
     #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
       ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
       #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
       ;;32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
       #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
       ;;48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
       #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\_
       ;;pad 
       #\= )))

  (define utf8-transcoder (make-transcoder (utf-8-codec) 'none))
  (define-syntax define-decode-string
    (syntax-rules ()
      ((_ name decoder)
       (define (name string :key (transcoder utf8-transcoder))
	 (or (string? string)
	     (assertion-violation 'name
				  (format "string required, but got ~s" string)
				  string))
	 (let ((bv (decoder (string->utf8 string))))
	   (if transcoder
	       (bytevector->string bv transcoder)
	       bv))))))
  (define-decode-string base64-decode-string base64-decode)
  (define-decode-string base64url-decode-string base64url-decode)

  (define-syntax define-decode
    (syntax-rules ()
      ((_ name table)
       (define (name in)
	 (if (bytevector? in)
	     (name (open-bytevector-input-port in))
	     (call-with-bytevector-output-port
	      (lambda (out)
		(base64-decode-impl in out table))))))))
  (define-decode base64-decode *decode-table*)
  (define-decode base64url-decode *decode-url-table*)

  (define (base64-decode-impl in out decode-table)
    (define (put b) (put-u8 out b))
    (define (get) (get-u8 in))
    (define decoder (make-base64-decoder :decode-table decode-table))
    (do () ((decoder get put))))
  
  ;; decode port
  (define (make-base64-decoder :key (decode-table *decode-table*))
    (define buffer (make-bytevector 4))
    (define buffer-size 0)
    
    (lambda (get put)
      (define (fill!)
	(let loop ()
	  (if (= buffer-size 4)
	      'full
	      (let ((b (get)))
		(cond ((eof-object? b) 'end)
		      ((negative? b) 'cont) ;; keep		
		      ((and (< 32 b 128) (vector-ref decode-table (- b 32))) =>
		       (lambda (b)
			 (bytevector-u8-set! buffer buffer-size b)
			 (set! buffer-size (+ buffer-size 1))
			 (loop)))
		      (else (loop)))))))
      (define (check b) (>= b 0))
      (define lshift bitwise-arithmetic-shift-left)
      (define rshift bitwise-arithmetic-shift-right)
      (define (decode put b0 b1 b2 b3)
	(when (and (check b0) (check b1))
	  (put (bitwise-and (bitwise-ior (lshift b0 2) (rshift b1 4)) #xFF))
	  (when (check b2)
	    (put (bitwise-and (bitwise-ior (lshift b1 4) (rshift b2 2)) #xFF))
	    (when (check b3)
	      (put (bitwise-and (bitwise-ior (lshift b2 6)  b3) #xFF))))))
      (define (do-it put size)
	(case size
	  ;; we at least need 2 bytes ;)
	  ((2) (decode put
		       (bytevector-u8-ref buffer 0)
		       (bytevector-u8-ref buffer 1)
		       -1 -1))
	  ((3) (decode put
		       (bytevector-u8-ref buffer 0)
		       (bytevector-u8-ref buffer 1)
		       (bytevector-u8-ref buffer 2)
		       -1))
	  ((4) (decode put
		       (bytevector-u8-ref buffer 0)
		       (bytevector-u8-ref buffer 1)
		       (bytevector-u8-ref buffer 2)
		       (bytevector-u8-ref buffer 3)))))
      (case (fill!)
	((full) (do-it put buffer-size) (set! buffer-size 0) #f)
	((end)  (do-it put buffer-size) (set! buffer-size 0) #t)
	((cont) #f))))
  
  (define-syntax define-encode-string
    (syntax-rules ()
      ((_ name encoder lw pad)
       (define (name string :key (transcoder utf8-transcoder)
				 (line-width lw)
				 (padding? pad))
	 (or (string? string)
	     (assertion-violation 'name
				  (format "string required, but got ~s" string)
				  string))
	 (utf8->string
	  (encoder (string->bytevector string transcoder)
		   :line-width line-width :padding? padding?))))))
  (define-encode-string base64-encode-string base64-encode 76 #t)
  (define-encode-string base64url-encode-string base64url-encode #f #f)

  (define-syntax define-encode
    (syntax-rules ()
      ((_ name table lw pad)
       (define (name in :key (line-width lw) (padding? pad))
	 (if (bytevector? in)
	     (name (open-bytevector-input-port in) 
		   :line-width line-width :padding? padding?)
	     (let-values (((out e) (open-bytevector-output-port)))
	       (base64-encode-impl in out line-width padding? table)
	       (e)))))))
  (define-encode base64-encode *encode-table* 76 #t)
  (define-encode base64url-encode *encode-url-table* #f #f)
  
  (define (base64-encode-impl inp out line-width padding? encode-table)
    (define (put v) (put-u8 out (or v #x0a)))
    (define (get) (get-u8 inp))
    (define encoder
      (make-base64-encoder :encode-table encode-table
			   :line-width line-width
			   :padding? padding?))
    (do () ((encoder get put))))

  (define (make-base64-encoder :key (encode-table *encode-table*)
				    (line-width 76)
				    (padding? #t))
    (define max-col (and line-width (> line-width 0) (- line-width 1)))
    (define col 0)
    (define buffer (make-bytevector 3))
    (define buffer-size 0)

    (lambda (get real-put)
      (define (check-col)
	(when max-col
	  (if (= col max-col)
	      (begin
		(real-put #f) ;; so that implementation may choose end line
		(set! col 0))
	      (set! col (+ col 1)))))
      (define (put i)
	(real-put (vector-ref encode-table i))
	(check-col))
      (define (fill!)
	(define (ret v size)
	  (set! buffer-size size)
	  v)
	(let loop ((i buffer-size))
	  (if (= i 3)
	      (ret 'full i)
	      (let ((b (get)))
		(cond ((eof-object? b) (ret 'end i))
		      ((negative? b) (ret 'cont i))
		      (else (bytevector-u8-set! buffer i b) (loop (+ i 1))))))))

      (define lshift bitwise-arithmetic-shift-left)
      (define rshift bitwise-arithmetic-shift-right)
      (define ior bitwise-ior)
      (define (encode b0 b1 b2)
	(when (>= b0 0)
	  (put (rshift (bitwise-and #xFC b0) 2))
	  (let ((b (lshift (bitwise-and #x03 b0) 4)))
	    (cond ((negative? b1) (put b) (when padding? (put 64) (put 64)))
		  (else
		   (put (ior b (rshift (bitwise-and #xF0 b1) 4)))
		   (let ((b (lshift (bitwise-and #x0F b1) 2)))
		     (cond ((negative? b2) (put b) (when padding? (put 64)))
			   (else
			    (put (ior b (rshift (bitwise-and #xC0 b2) 6)))
			    (put (bitwise-and #x3F b2))))))))))
      (define (do-it size)
	(case size
	  ((1) (encode (bytevector-u8-ref buffer 0)
		       -1
		       -1))
	  ((2) (encode (bytevector-u8-ref buffer 0)
		       (bytevector-u8-ref buffer 1)
		       -1))
	  ((3) (encode (bytevector-u8-ref buffer 0)
		       (bytevector-u8-ref buffer 1)
		       (bytevector-u8-ref buffer 2)))))
      (case (fill!)
	((full) (do-it buffer-size) (set! buffer-size 0) #f)
	((end)  (do-it buffer-size) (set! buffer-size 0) #t)
	((cont) #f))))

  (define (open-base64-encode-output-port sink
					  :key (owner? #f) (line-width #f)
					       (padding? #t))
    (open-base64-encode-output-port/encode-table sink *encode-table*
      :owner? owner? :line-width line-width :padding? padding?))
  (define (open-base64url-encode-output-port sink
					     :key (owner? #f) (line-width #f)
					          (padding? #f))
    (open-base64-encode-output-port/encode-table sink *encode-url-table*
      :owner? owner? :line-width line-width :padding? padding?))

  (define (open-base64-encode-output-port/encode-table
	   sink encode-table :key (owner? #f) (line-width #f) (padding? #t))
    (define buffer (make-bytevector 3 0))
    (define buffer-count 0)

    (define (fill-buffer bv start count)
      (define size (min (- 3 buffer-count) count))
      (bytevector-copy! bv start buffer buffer-count size)
      (set! buffer-count (+ buffer-count size))
      size)

    (define (put v) (put-u8 sink (or v #x0a)))
    (define encoder (make-base64-encoder :encode-table encode-table
					 :line-width line-width
					 :padding? padding?))

    (define (process-encode)
      (define (get/index n)
	(or (and (> buffer-count n) (bytevector-u8-ref buffer n))
	    (eof-object)))
      (define i 0)
      (define (get)
	(let ((r (get/index i)))
	  (set! i (mod (+ i 1) 3))
	  r))
      (encoder get put)
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
					 :key (owner? #f) (line-width #f)
					      (padding? #t))
    (open-base64-encode-input-port/encode-table source *encode-table*
     :owner? owner? :line-width line-width :padding? padding?))
  (define (open-base64url-encode-input-port source
					 :key (owner? #f) (line-width #f)
					      (padding? #f))
    (open-base64-encode-input-port/encode-table source *encode-url-table*
     :owner? owner? :line-width line-width :padding? padding?))
  (define (open-base64-encode-input-port/encode-table source encode-table
					 :key (owner? #f) (line-width #f)  
					      (padding? #t))
    ;; max length = when line-width is 1
    (define buffer (make-bytevector 8 0))
    (define buffer-count 0)

    (define (fill-buffer! bv start count)
      (define size (min buffer-count count))
      (bytevector-copy! buffer 0 bv start size)
      (set! buffer-count (- buffer-count size))
      (bytevector-copy! buffer size buffer 0 buffer-count)
      size)

    (define (put v)
      (bytevector-u8-set! buffer buffer-count (or v #x0a))
      (set! buffer-count (+ buffer-count 1)))

    (define encoder (make-base64-encoder :encode-table encode-table
					 :line-width line-width
					 :padding? padding?))

    (define (process-encode)
      (define prev 0)
      (define (get)
	(define (do-it)
	  (if (eof-object? prev)
	      prev
	      (get-u8 source)))
	(let ((r (do-it)))
	  (set! prev r)
	  r))
      (encoder get put))

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
      
  ;; TODO maybe we want to make buffer size bigger for performance?
  (define (open-base64-decode-input-port source :key (owner? #f))
    (open-base64-decode-input-port/decode-table source *decode-table*
						:owner? owner?))
  (define (open-base64url-decode-input-port source :key (owner? #f))
    (open-base64-decode-input-port/decode-table source *decode-url-table*
						:owner? owner?))

  (define (open-base64-decode-input-port/decode-table source decode-table
						      :key (owner? #f))
    (define output-buffer (make-bytevector 3))
    (define output-buffer-size 0)
    (define (put b)
      (bytevector-u8-set! output-buffer output-buffer-size b)
      (set! output-buffer-size (+ output-buffer-size 1)))
    (define decoder (make-base64-decoder :decode-table decode-table))
    ;; Should we raise an error if the input is not multiple of 4?
    (define (decode1)
      (define (get) (get-u8 source))
      (decoder get put)
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
    (open-base64-decode-output-port/decode-table sink *decode-table*
						 :owner? owner?))
  (define (open-base64url-decode-output-port sink :key (owner? #f))
    (open-base64-decode-output-port/decode-table sink *decode-url-table*
						 :owner? owner?))

  (define (open-base64-decode-output-port/decode-table sink decode-table
						       :key (owner? #f))
    (define (put b) (put-u8 sink b))
    (define decoder (make-base64-decoder :decode-table decode-table))
    
    (define buffer (make-bytevector 4))
    (define buffer-size 0)
    (define (write! bv start count)
      (define i start)
      (define (get)
	(cond ((>= i count) -1)
	      (else
	       (let ((r (bytevector-u8-ref bv i)))
		 (set! i (+ i 1))
		 r))))
      (let loop ()
	(decoder get put)
	(if (= i count)
	    count
	    (loop))))

    (define (close)
      (decoder (lambda () (eof-object)) put)
      (flush-output-port sink)
      (when owner? (close-port sink)))

    (make-custom-binary-output-port "base64-decode-output-port"
				    write! #f #f close))
  
)
