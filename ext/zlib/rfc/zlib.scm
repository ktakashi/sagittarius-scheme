;;; -*- mode:scheme; coding: utf-8 -*-
;;;
;;; zlib.scm - RFC1950 zlib library
;;;  
;;;   Copyright (c) 2010-2025  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (rfc zlib)
    (export ;; open-deflating-input-port
	    open-deflating-output-port
	    open-inflating-input-port
	    open-inflating-output-port
	    ;; flush flags
	    Z_NO_FLUSH
	    Z_PARTIAL_FLUSH
	    Z_SYNC_FLUSH
	    Z_FULL_FLUSH
	    Z_BLOCK
	    Z_TREES
	    ;; result states
	    Z_OK
	    Z_STREAM_END
	    Z_NEED_DICT
	    Z_ERRNO
	    Z_STREAM_ERROR
	    Z_DATA_ERROR
	    Z_MEM_ERROR
	    Z_BUF_ERROR
	    Z_VERSION_ERROR
	    ;; compression flags
	    Z_NO_COMPRESSION
	    Z_BEST_SPEED
	    Z_BEST_COMPRESSION
	    Z_DEFAULT_COMPRESSION
	    ;; strategy flags
	    Z_FILTERED
	    Z_HUFFMAN_ONLY
	    Z_RLE
	    Z_FIXED
	    Z_DEFAULT_STRATEGY
	    ;; data types
	    Z_BINARY
	    Z_TEXT
	    Z_ASCII
	    Z_UNKNOWN
	    ;; condition
	    &zlib-error zlib-error?
	    condition-z-stream
	    ;; utility
	    ;; well usually we raise condition with this message,
	    ;; so this is actually not needed.
	    zlib-error-message

	    deflate-bytevector
	    inflate-bytevector
	    crc32 adler32
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius dynamic-module)
	    (sagittarius control))
(load-dynamic-module "sagittarius--zlib")

(define-condition-type &zlib-error &error make-zlib-error
  zlib-error?
  (z-stream condition-z-stream))

(define (raise-z-stream-error z-stream who . irritants)
  (raise (condition (make-zlib-error z-stream)
		    (make-who-condition who)
		    (make-message-condition (zlib-error-message z-stream))
		    (make-irritants-condition irritants))))

;; For now, we don't implement this, I'll do it if there's a demand
;; uncompress input port -> compress input port
;; (define (open-deflating-input-port (source (and binary-port? input-port?))
;; 	   :key (compression-level Z_DEFAULT_COMPRESSION)
;; 		(buffer-size 4096)
;; 		(window-bits 15)
;; 		(memory-level 8)
;; 		(strategy Z_DEFAULT_STRATEGY)
;; 		(dictionary #f)
;; 		(owner? #f))
;;   (define out-buffer (make-bytevector buffer-size 0))
;;   (define in-buffer (make-bytevector buffer-size 0))
;;   (define z-stream
;;     (deflate-init compression-level window-bits memory-level strategy))
;;   (define current-pos 0)
;;   (define out-buffer-size 0)
;;   (define (fill-buffer!)
;;     (let ((n (get-bytevector-n! source in-buffer 0 buffer-size)))
;;       (cond ((eof-object? n) (set! out-buffer-size 0))
;; 	    )))
    
;;   (define (read! bv start count)
;;     (let loop ((offset start) (rest count))
;;       (when (= current-pos out-buffer-size) (fill-buffer!))
;;       (if (zero? out-buffer-size)
;; 	  (- count rest) ;; nothing to read
;; 	  (let ((s (min rest (- out-buffer-size current-pos))))
;; 	    (bytevector-copy! out-buffer current-pos bv offset s)
;; 	    (set! current-pos (+ current-pos s))
;; 	    (if (= s rest)
;; 		count
;; 		(loop (+ start s) (- rest s)))))))
;;   (define (close)

;;     (when owner? (close-input-port source)))
;;   (when dictionary (deflate-set-dictionary z-stream dictionary))
;;   (make-custom-binary-input-port "deflating-output-port" read! #f #f close))
  
;; uncompress output port -> compress output port
(define (open-deflating-output-port (sink (and binary-port? output-port?))
	   :key (compression-level Z_DEFAULT_COMPRESSION)
		(buffer-size 4096)
		(window-bits 15)
		(memory-level 8)
		(strategy Z_DEFAULT_STRATEGY)
		(dictionary #f)
		(owner? #f))
  (let ((out-buffer (make-bytevector buffer-size 0))
        (in-buffer  (make-bytevector buffer-size 0))
        (flush Z_NO_FLUSH)
        (current-pos 0)
        (z-stream
	 (deflate-init compression-level window-bits memory-level strategy)))
    (define (flush! in-buffer force?)
      (define (impl in-buffer. count)
        (let loop ((total 0))
          (let ((ret (deflate z-stream in-buffer. out-buffer flush)))
            (unless (= ret Z_OK)
      	      (raise-z-stream-error z-stream 'write!))
            (unless (zero? (zstream-avail-out z-stream))
      	      (set! flush Z_NO_FLUSH))
            (let* ((nread (zstream-read-count z-stream in-buffer.))
      		   (nwrite (zstream-write-count z-stream out-buffer))
      		   (total (+ total nread)))
      	      (when (> nwrite 0)
      		(put-bytevector sink out-buffer 0 nwrite))
      	      (if (and force? (< total count))
      		  (loop (+ total nread))
      		  total)))))
      (let ((in-buffer. (if (= current-pos buffer-size)
      			    in-buffer
      			    (let ((r (make-bytevector current-pos 0)))
      			      (bytevector-copy! in-buffer 0 r 0 current-pos)
      			      r))))
        (when (and (= flush Z_NO_FLUSH) force?)
          (set! flush Z_SYNC_FLUSH))
        (let ((nwrote (impl in-buffer. current-pos)))
          (if (and (>= nwrote 0) (< nwrote current-pos))
      	      (let ((diff (- current-pos nwrote)))
      		(bytevector-copy! in-buffer nwrote
      				  in-buffer 0 diff)
      		(set! current-pos diff))
      	      (set! current-pos 0)))))

    (define (write! bv start count)
      (define (rec by start count diff)
        (cond ((< (+ current-pos count) buffer-size)
      	       (bytevector-copy! bv start in-buffer current-pos count)
      	       (set! current-pos (+ current-pos count))
      	       (+ count diff))
      	      (else
      	       (let ((n (- buffer-size current-pos)))
		 (bytevector-copy! bv start in-buffer current-pos n)
		 (set! current-pos (+ current-pos n))
      		 (flush! in-buffer #f)
      		 (rec bv (+ start n) (- count n) (+ n diff))))))
      (rec bv start count 0))

    (define (close)
      (flush! in-buffer #t)
      (let ((in-buffer. (make-bytevector current-pos 0)))
        (bytevector-copy! in-buffer 0 in-buffer. 0 current-pos)
        (do ((r (deflate z-stream in-buffer. out-buffer Z_FINISH)
      		(deflate z-stream in-buffer. out-buffer Z_FINISH)))
            ((= r Z_STREAM_END) r)
          (unless (or (= r Z_OK) (= r Z_STREAM_END))
            (raise-z-stream-error z-stream 'close r))
          (put-bytevector sink out-buffer 0
      			  (zstream-write-count z-stream out-buffer)))
        (put-bytevector sink out-buffer 0
      			(zstream-write-count z-stream out-buffer))
        (unless (= (deflate-end z-stream) Z_OK)
          (raise-z-stream-error z-stream 'close (deflate-end z-stream)))
        ;; flush sink
        (flush-output-port sink)
        ;; if the deflating port is owner, we need to close the port.
        (when owner? (close-output-port sink))))

    (when dictionary (deflate-set-dictionary z-stream dictionary))
    (make-custom-binary-output-port "deflating-port" write! #f #f close)))


(define (open-inflating-input-port (source (and binary-port? input-port?))
      				   :key (buffer-size 4096)
      				   (window-bits 15)
      				   (dictionary #f)
      				   (owner? #f))
  (let ((in-buffer (make-bytevector buffer-size 0))
        (out-buffer (make-bytevector buffer-size 0))
        (current-pos 0)
        (start (and (port-has-port-position? source) (port-position source)))
        (offset 0)
        (out-buffer-size -1)
        (next-in 0)
        (stream-end? #f)
        (z-stream (inflate-init window-bits)))

    (define (close)
      (let ((r (inflate-end z-stream Z_FINISH)))
        (unless (= r Z_OK)
          (raise-z-stream-error z-stream 'inflate-end))
        (when (and start (port-has-set-port-position!? source))
          ;; set the port position to proper offset
          (set-port-position! source (+ start offset)))
        ;; when the inflating port is owner, we need to close source port.
        (when owner? (close-input-port source))))

    (define (read! bv start count)
      (define (rec bv start count diff)
        (let ((nread 0))
          (when (> current-pos out-buffer-size)
            (set! nread (fill-buffer!)))
          (cond ((and (zero? nread)
      		      (= current-pos out-buffer-size)
      		      stream-end?)
      		 0)
      		((< (+ current-pos count) out-buffer-size)
      		 (bytevector-copy! out-buffer current-pos bv start count)
      		 (set! current-pos (+ current-pos count))
      		 (+ count diff))
      		((and (< current-pos out-buffer-size)
      		      (>= (+ current-pos count) out-buffer-size))
      		 (let ((n (- out-buffer-size current-pos)))
      		   ;;(format #t "~a:~a~%" out-buffer-size current-pos)
      		   (bytevector-copy! out-buffer current-pos bv start n)
      		   (let ((n2 (fill-buffer!)))
      		     (if (zero? n2)
      			 (begin
      			   (set! current-pos (+ current-pos n))
      			   (+ diff n))
      			 (rec bv (+ start n) (- count n) (+ diff n))))))
      		(else
      		 (assertion-violation 'read! "what's the condition"
      				      `((current-pos ,current-pos)
      					(out-buffer-size ,out-buffer-size)
      					(start ,start) (count ,count)
      					(stream-end? ,stream-end?)))))))
      (rec bv start count 0))

    (define (fill-buffer!)
      (let* ((len (- buffer-size next-in))
             (n (get-bytevector-n! source in-buffer next-in len)))
        (cond ((and (eof-object? n) (zero? next-in))
      	       (set! stream-end? #t)
      	       0)
      	      (else
	       (if (eof-object? n) (set! n 0))
	       (let ((in-buffer.
		      (if (= (+ n next-in) buffer-size)
      			  in-buffer
      			  (bytevector-copy in-buffer 0 (+ next-in n)))))
		 (let-values (((avail-in nwrite end?)
			       (fill-inflating-buffer! z-stream dictionary
						       in-buffer. out-buffer)))
		   (set! stream-end? end?)
		   (set! offset (+ offset (- buffer-size (- len n) avail-in)))
		   (set! next-in avail-in)
		   (cond ((> avail-in 0)
			  ;; copy the remaining values back to the
			  ;; original buffer
      			  (bytevector-copy! in-buffer.
			    (- (bytevector-length in-buffer.) avail-in)
      			    in-buffer 0 avail-in))
			 ;; all the input buffer is inflated, so fill null
      			 (else (bytevector-fill! in-buffer 0)))
		   (cond ((and (zero? avail-in) (zero? nwrite))
			  (fill-buffer!))
			 (else
			  (set! current-pos 0)
      			  (set! out-buffer-size nwrite)
			  nwrite))))))))

    (when dictionary (inflate-set-dictionary z-stream dictionary))

    (make-custom-binary-input-port "inflate input port" read! #f #f close)))

(define (open-inflating-output-port (sink (and binary-port? output-port?))
				    :key (buffer-size 4096)
					 (window-bits 15)
					 (dictionary #f)
					 (owner? #f))
  (define in-buffer (make-bytevector buffer-size 0))
  (define out-buffer (make-bytevector buffer-size 0))
  (define current-pos 0)
  (define z-stream (inflate-init window-bits))
  
  (define (close)
    (unless (zero? current-pos)
      ;; TODO support size on inflate so that we don't have to
      ;;      copy bytevector number of times here
      (let loop ((buf (bytevector-copy in-buffer 0 current-pos)))
	(let-values (((avail-in nwrite end?)
		      (fill-inflating-buffer! z-stream dictionary
					      buf out-buffer)))
	  (put-bytevector sink out-buffer 0 nwrite)
	  (unless (or end? (zero? avail-in))
	    (loop (bytevector-copy buf (- (bytevector-length buf) avail-in)))))))
    (unless (= (inflate-end z-stream Z_FINISH) Z_OK)
      (raise-z-stream-error z-stream 'inflate-end))
    (when owner? (close-output-port sink)))
  
  (define (write! bv start count)
    (let loop ((offset start) (rest count))
      (let ((s (min (- buffer-size current-pos) rest)))
	(bytevector-copy! bv offset in-buffer current-pos s)
	(set! current-pos (+ current-pos s))
	(when (= current-pos buffer-size)
	  (let-values (((avail-in nwrite end?)
			(fill-inflating-buffer! z-stream dictionary
						in-buffer out-buffer)))
	    (set! current-pos avail-in)
	    (unless (zero? avail-in)
	      ;; shift remaining to the front
	      (bytevector-copy! in-buffer (- buffer-size avail-in)
				in-buffer 0 avail-in))
	    (put-bytevector sink out-buffer 0 nwrite)))
	(if (= s rest)
	    count
	    (loop (+ offset s) (- rest s))))))

  (when dictionary (inflate-set-dictionary z-stream dictionary))
  (make-custom-binary-output-port "inflate output port" write! #f #f close))

;; core inflate logic
(define (fill-inflating-buffer! z-stream dictionary in-buffer out-buffer)
  (let loop ((in-buffer. in-buffer))
    (let ((r (inflate z-stream in-buffer. out-buffer Z_SYNC_FLUSH))
      	  (avail-in (zstream-avail-in z-stream))
      	  (nwrite (zstream-write-count z-stream out-buffer)))
      (cond ((= r Z_STREAM_ERROR) (raise-z-stream-error z-stream 'inflate))
	    ((= r Z_NEED_DICT)
	     (unless dictionary
	       (error 'inflate "dictionary is required for this stream"))
      	     (let ((r (inflate-set-dictionary z-stream dictionary)))
      	       (unless (= r Z_OK) (raise-z-stream-error z-stream 'inflate))
      	       (let ((avail-in (zstream-avail-in z-stream)))
		 (cond ((> avail-in 0)
			;; shift the rest of the input to the front
      			(bytevector-copy! in-buffer.
			  (- (bytevector-length in-buffer.) avail-in)
      			  in-buffer. 0 avail-in)
		       ;; dictionary is passed, try again
			(loop in-buffer.))
		       (else (values avail-in nwrite #f))))))
      	    (else
      	     (when (and (= r Z_DATA_ERROR) (<= nwrite 0))
      	       (raise-z-stream-error z-stream 'inflate-data-error))
	     (values avail-in nwrite (= r Z_STREAM_END)))))))

(define (deflate-bytevector bv . args)
  (let-values (((out e) (open-bytevector-output-port)))
    (let ((p (apply open-deflating-output-port out :owner? #f args)))
      (put-bytevector p bv)
      (close-port p)
      (e))))

(define (inflate-bytevector bv . args)
  (let ((p (apply open-inflating-input-port
      		  (open-bytevector-input-port bv) :owner? #t args)))
    (get-bytevector-all p)))

)
