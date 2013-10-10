;;; -*- mode:scheme; coding: utf-8 -*-
;;;
;;; zlib.scm - RFC1950 zlib library
;;;  
;;;   Copyright (c) 2000-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (rfc zlib)
    (export open-deflating-output-port
	    open-inflating-input-port
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

  (define (raise-z-stream-error z-stream who message . irritants)
    (raise
     (apply condition
	    (filter values
		    (list (make-zlib-error z-stream)
			  (and who (make-who-condition who))
			  (make-message-condition message)
			  (make-irritants-condition irritants))))))

  (define (open-deflating-output-port sink
			   :key (compression-level Z_DEFAULT_COMPRESSION)
				(buffer-size 4096)
				(window-bits 15)
				(memory-level 8)
				(strategy Z_DEFAULT_STRATEGY)
				(dictionary #f)
				(owner? #f))
    (or (and (binary-port? sink)
	     (output-port? sink))
	(assertion-violation 'open-deflating-output-port
			     "binary-output-port required"
			     sink))
    (let ((out-buffer (make-bytevector buffer-size 0))
	  (in-buffer  (make-bytevector buffer-size 0))
	  (flush Z_NO_FLUSH)
	  (current-pos 0)
	  (z-stream   (deflate-init compression-level window-bits memory-level
			strategy)))
      (define (flush! in-buffer force?)
	(define (impl in-buffer. count)
	  (let loop ((total 0))
	    (let ((ret (deflate z-stream in-buffer. out-buffer flush)))
	      (unless (= ret Z_OK)
		(raise-z-stream-error z-stream 'write!
				      (zlib-error-message z-stream)))
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
		 (let ((n (do ((i 0 (+ i 1)))
			      ((= current-pos buffer-size) i)
			    (bytevector-u8-set! in-buffer current-pos
						(bytevector-u8-ref bv
								   (+ start i)))
			    (set! current-pos (+ current-pos 1)))))
		   (flush! in-buffer #f)
		   (rec bv (+ start n) (- count n) n)))))
	  (rec bv start count 0))

      (define (close)
	(flush! in-buffer #t)
	(let ((in-buffer. (make-bytevector current-pos 0)))
	  (bytevector-copy! in-buffer 0 in-buffer. 0 current-pos)
	  (do ((r (deflate z-stream in-buffer. out-buffer Z_FINISH)
		  (deflate z-stream in-buffer. out-buffer Z_FINISH)))
	      ((= r Z_STREAM_END) r)
	    (unless (or (= r Z_OK) (= r Z_STREAM_END))
	      (raise-z-stream-error z-stream 'close
				    (zlib-error-message z-stream)
				    r))
	    (put-bytevector sink out-buffer 0
			    (zstream-write-count z-stream out-buffer)))
	  (put-bytevector sink out-buffer 0
			  (zstream-write-count z-stream out-buffer))
	  (unless (= (deflate-end z-stream) Z_OK)
	    (raise-z-stream-error z-stream 'close
				  (zlib-error-message z-stream)
				  (deflate-end z-stream)))
	  ;; flush sink
	  (flush-output-port sink)
	  ;; if the deflating port is owner, we need to close the port.
	  (if owner?
	      (close-output-port sink))))

      (when dictionary
	(deflate-set-dictionary z-stream dictionary))

      (make-custom-binary-output-port "deflating-port"
				      write!
				      #f
				      #f
				      close)))

  (define (open-inflating-input-port source
				     :key (buffer-size 4096)
					  (window-bits 15)
					  (dictionary #f)
					  (owner? #f))
    (or (and (binary-port? source)
	     (input-port? source))
	(assertion-violation 'open-inflating-input-port
			     "binary-input-port required"
			     source))
    (let ((in-buffer (make-bytevector buffer-size 0))
	  (out-buffer (make-bytevector buffer-size 0))
	  (current-pos 0)
	  (out-buffer-size -1)
	  (next-in 0)
	  (stream-end? #f)
	  (z-stream (inflate-init window-bits)))

      (define (close)
	(let ((r (inflate-end z-stream Z_FINISH)))
	  (unless (= r Z_OK)
	    (raise-z-stream-error z-stream 'inflate-end
				  (zlib-error-message z-stream)))
	  ;; when the inflating port is owner, we need to close source port.
	  (if owner?
	      (close-input-port source))))

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
		   (let ((n (do ((i 0 (+ i 1))
				 (index1 start (+ index1 1))
				 (index2 current-pos (+ index2 1)))
				((= index2 out-buffer-size) i)
			      (bytevector-u8-set! bv index1
						  (bytevector-u8-ref out-buffer
								     index2))))
			 (n2 (fill-buffer!)))
		     (if (zero? n2)
			 (begin
			   (set! current-pos (+ current-pos n))
			   n)
			 (rec bv (+ start n) (- count n) n))))
		  (else
		   (assertion-violation 'read!
					"what's the condition"
					current-pos out-buffer-size
					start count
					stream-end?))
		   )))
	(rec bv start count 0))

      (define (fill-buffer!)
	(let ((n (get-bytevector-n! source in-buffer next-in
				    (- buffer-size next-in))))
	  (cond ((and (eof-object? n)
		      (zero? next-in))
		 (set! stream-end? #t)
		 0)
		(else
		 (if (eof-object? n) (set! n 0))
		 (let loop ((in-buffer. 
			     (if (= (+ n next-in) buffer-size)
				 in-buffer
				 (let ((bv (make-bytevector (+ next-in n) 0)))
				   (bytevector-copy! in-buffer 0
						     bv 0 (+ next-in n))
				   bv))))
		   (let* ((r (inflate z-stream in-buffer.
				      out-buffer Z_SYNC_FLUSH))
			  (avail-in (zstream-avail-in z-stream))
			  (nwrite (zstream-write-count z-stream out-buffer)))
		     (when (= r Z_STREAM_ERROR)
		       (raise-z-stream-error z-stream 'inflate
					     (zlib-error-message z-stream)))
		     (cond ((> avail-in 0)
			    (set! next-in avail-in)
			    (bytevector-copy! in-buffer. 
					      (- (bytevector-length in-buffer.)
						 avail-in)
					      in-buffer 0 avail-in))
			   (else
			    (set! next-in 0)
			    (bytevector-fill! in-buffer 0)))
		     (if (= r Z_NEED_DICT)
			 (let ((r (inflate-set-dictionary z-stream
							  dictionary)))
			   (unless (= r Z_OK)
			     (raise-z-stream-error z-stream 'inflate
						   (zlib-error-message
						    z-stream)))
			   (let ((avail-in (zstream-avail-in z-stream)))
			     (bytevector-copy! in-buffer 0
					       in-buffer. 0 avail-in)
			     (cond ((> avail-in 0)
				    (loop in-buffer.))
				   (else
				    (set! current-pos 0)
				    (set! out-buffer-size nwrite)
				    nwrite))))
			 (begin
			   (cond ((= r Z_STREAM_END)
				  (set! stream-end? #t))
				 ((and (= r Z_DATA_ERROR)
				       (<= nwrite 0))
				  (raise-z-stream-error
				   z-stream 'inflate-data-error
				   (zlib-error-message z-stream))))
			   (set! current-pos 0)
			   (set! out-buffer-size nwrite)
			   nwrite))))))))

      (when dictionary
	(inflate-set-dictionary z-stream dictionary))

      (make-custom-binary-input-port "inflate port"
				     read!
				     #f
				     #f
				     close)))

  (define (deflate-bytevector bv . args)
    (call-with-bytevector-output-port
     (^p (let1 p2 (apply open-deflating-output-port p args)
	   (put-bytevector p2 bv)
	   (close-output-port p2)))))

  (define (inflate-bytevector bv . args)
    (call-with-bytevector-output-port
     (^p (let1 p2 (apply open-inflating-input-port p args)
	   (put-bytevector p2 bv)
	   (close-output-port p2)))))
	
)