;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; base-n.scm - Base N encoding/decoding framework
;;;  
;;;   Copyright (c) 2024  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc base-n)
    (export make-make-base-n-encoder make-make-base-n-decoder
	    make-base-n-encode make-base-n-decode
	    make-base-n-encode-string make-base-n-decode-string
	    make-base-n-encode-output-port-opener
	    make-base-n-encode-input-port-opener
	    make-base-n-decode-output-port-opener
	    make-base-n-decode-input-port-opener

	    base-n-encode-table->decode-table)
    (import (rnrs)
	    (sagittarius))

(define utf8-transcoder (make-transcoder (utf-8-codec) 'none))
(define ((make-base-n-encode-string encode) string
	 :key (transcoder utf8-transcoder)
	 :allow-other-keys options)
  (unless (string? string)
    (assertion-violation 'base-n-encode-string
			 (format "string required, but got ~s" string) string))
  (utf8->string (apply encode (string->bytevector string transcoder) options)))

(define ((make-base-n-decode-string decode) string
	 :key (transcoder utf8-transcoder)
	 :allow-other-keys options)
  (unless (string? string)
    (assertion-violation 'base-n-decode-string
			 (format "string required, but got ~s" string) string))
  (let ((bv (apply decode (string->utf8 string) options)))
    (if transcoder
	(bytevector->string bv transcoder)
	bv)))

(define (make-base-n-encode make-encoder . default-options)
  (define (encode in . encoder-options)
    (define (rec)
      (let-values (((out e) (open-bytevector-output-port)))
	(define (put v) (put-u8 out (or v #x0a)))
	(define (get) (get-u8 in))
	;; the append order is depending on the compiler...
	;; TODO maybe merge it properly
	(define encoder
	  (apply make-encoder (append default-options encoder-options)))
	(do () ((encoder get put) (e)))))
    (if (bytevector? in)
	(apply encode (open-bytevector-input-port in) encoder-options)
	(rec)))
  encode)

(define (make-base-n-decode make-decoder . default-options)
  (define (decode in . decoder-options)
    (define (rec)
      (let-values (((out e) (open-bytevector-output-port)))
	(define (put v) (put-u8 out v))
	(define (get) (get-u8 in))
	(define decoder
	  (apply make-decoder (append default-options decoder-options)))
	(do () ((decoder get put) (e)))))
    (if (bytevector? in)
	(apply decode (open-bytevector-input-port in) decoder-options)
	(rec)))
  decode)

(define (base-n-encode-table->decode-table table :optional (size 128))
  (define table-size (- (vector-length table) 1)) ;; exclude pad
  (do ((i 0 (+ i 1)) (r (make-vector size #f)))
      ((= i table-size) r)
    (vector-set! r (vector-ref table i) i)))

;; exact log2
(define (elog2 n)
  (let ((r (exact (log n 2))))
    (unless (integer? r) (assertion-violation 'base-n "N must be power of 2"))
    r))

;; Framework for Base N (16, 32, or 64) encoder
(define ((make-make-base-n-encoder encode n :key (table :encode-table #f))
	 :key (encode-table table) (line-width 76) (linefeeds #f) (padding? #t))
  (define max-col (and line-width (> line-width 0) (- line-width 1)))
  (define col 0)
  (define max-buffer-size (denominator (/ 8 (elog2 n))))
  (define buffer (make-bytevector max-buffer-size))
  (define buffer-size 0)

  (case-lambda
   (() n)
   ((get real-put)
    (define (check-col)
      (when max-col
	(if (= col max-col)
	    (begin
	      (if linefeeds
		  (for-each real-put linefeeds)
		  (real-put #f))
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
	(if (= i max-buffer-size)
	    (ret 'full i)
	    (let ((b (get)))
	      (cond ((eof-object? b) (ret 'end i))
		    ((negative? b) (ret 'cont i))
		    (else (bytevector-u8-set! buffer i b) (loop (+ i 1))))))))
    (case (fill!)
      ((full) (encode put buffer buffer-size padding?) (set! buffer-size 0) #f)
      ((end)
       ;; Fill unused buffer with 0
       (unless (= buffer-size max-buffer-size)
	 (bytevector-fill! buffer 0 buffer-size max-buffer-size))
       (encode put buffer buffer-size padding?) (set! buffer-size 0) #t)
      ((cont) #f)))))

(define ((make-make-base-n-decoder decode n :key (table :decode-table #f))
	 :key (decode-table table))
  (define max-buffer-size (numerator (/ 8 (elog2 n))))
  (define buffer (make-bytevector max-buffer-size))
  (define buffer-size 0)
  
  (case-lambda
   (() n)
   ((get put)
    (define (fill!)
      (let loop ()
	(if (= buffer-size max-buffer-size)
	    'full
	    (let ((b (get)))
	      (cond ((eof-object? b) 'end)
		    ((negative? b) 'cont) ;; keep		
		    ((and (< b 128) (vector-ref decode-table b)) =>
		     (lambda (b)
		       (bytevector-u8-set! buffer buffer-size b)
		       (set! buffer-size (+ buffer-size 1))
		       (loop)))
		    (else (loop)))))))
    (case (fill!)
      ((full) (decode put buffer buffer-size) (set! buffer-size 0) #f)
      ((end)  (decode put buffer buffer-size) (set! buffer-size 0) #t)
      ((cont) #f)))))

(define ((make-base-n-encode-output-port-opener make-encoder)
	 sink :key (owner? #f) :allow-other-keys encoder-options)
  (define encoder (apply make-encoder encoder-options))
  (define n (encoder))
  (define max-buffer-size (denominator (/ 8 (elog2 n))))
  (define buffer (make-bytevector max-buffer-size))
  (define buffer-count 0)
  (define (fill-buffer bv start count)
    (define size (min (- max-buffer-size buffer-count) count))
    (bytevector-copy! bv start buffer buffer-count size)
    (set! buffer-count (+ buffer-count size))
    size)
  
  (define (put v) (put-u8 sink (or v #x0a)))
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
	    (when (= buffer-count max-buffer-size) (process-encode))
	    (loop (+ start n) (- rest n))))))
  
  (define (close) 
    ;; do the last
    (process-encode)
    (flush-output-port sink)
    (when owner? (close-port sink)))
  (make-custom-binary-output-port 
   (string-append "base" (number->string n) "-encode-output-port")
   write! #f #f close))

(define ((make-base-n-encode-input-port-opener make-encoder)
	 source :key (owner? #f) (linefeeds #f) 
		:allow-other-keys encoder-options)
  (define encoder (apply make-encoder encoder-options))
  (define n (encoder))
  (define max-buffer-size
    (* (numerator (/ 8 (elog2 n))) (+ (if linefeeds (length linefeeds) 1) 1)))
  (define buffer (make-bytevector max-buffer-size))
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
  (make-custom-binary-input-port
   (string-append "base" (number->string n) "-encode-input-port")
   read! #f #f close))

(define ((make-base-n-decode-output-port-opener make-decoder)
	 sink :key (owner? #f) :allow-other-keys decoder-options)
  (define decoder (apply make-decoder decoder-options))
  (define n (decoder))
  (define max-buffer-size (denominator (/ 8 (elog2 n))))
  (define buffer (make-bytevector max-buffer-size))
  (define buffer-size 0)
  (define (put b) (put-u8 sink b))
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

  (make-custom-binary-output-port
   (string-append "base" (number->string 64) "-decode-output-port")
   write! #f #f close))

(define ((make-base-n-decode-input-port-opener make-decoder)
	 source :key (owner? #f) :allow-other-keys decoder-options)
  (define decoder (apply make-decoder decoder-options))
  (define n (decoder))
  (define max-buffer-size (denominator (/ 8 (- (bitwise-length n) 1))))
  (define output-buffer (make-bytevector max-buffer-size))
  (define output-buffer-size 0)
  (define (put b)
    (bytevector-u8-set! output-buffer output-buffer-size b)
    (set! output-buffer-size (+ output-buffer-size 1)))
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

  (make-custom-binary-input-port
   (string-append "base" (number->string n) "-decode-input-port")
   read! #f #f close))

)
