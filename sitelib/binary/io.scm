;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; binary/io.scm - Binary IO.
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

;; binary port utility
;; in real world you sometimes want to treat
;; binary port like textual port (mostly get-line).
;; this library exports those convenient procedures
(library (binary io)
    (export get-until
	    get-line
	    lookahead-next-u8
	    (rename (lookahead-next-u8 peek-next-u8))
	    ;; utilities
	    put-s8  get-s8 ;; this isn't in (rnrs)
	    put-u16 put-s16 get-u16 get-s16
	    put-u32 put-s32 get-u32 get-s32
	    put-u64 put-s64 get-u64 get-s64
	    put-f32 put-f64 get-f32 get-f64

	    ;; memory efficient(?) ports
	    input-port->chunked-binary-input-port
	    ->chunked-binary-input-port
	    +default-chunk-size+
	    )
    (import (except (rnrs) get-line)
	    (sagittarius)
	    (clos user)
	    (sagittarius object))

  (define (lookahead-next-u8 in) (get-u8 in) (lookahead-u8 in))

  (define (get-until bin bv-mark)
    (let-values (((out extract) (open-bytevector-output-port)))
      (define (finish bv/false) (values (extract) bv/false))
      (let* ((buf-len (bytevector-length bv-mark))
	     (buf     (make-bytevector buf-len)))
	(let loop ((b (lookahead-u8 bin)))
	  (cond ((eof-object? b) (finish #f))
		((= b (bytevector-u8-ref bv-mark 0))
		 ;; check
		 (let loop2 ((i 0) (b2 b))
		   (cond ((= i buf-len) (finish buf)) ;; we are done
			 ((eqv? (bytevector-u8-ref bv-mark i) b2)
			  (bytevector-u8-set! buf i b2)
			  (get-u8 bin)
			  (if (= (+ i 1) buf-len)
			      (finish buf)
			      (loop2 (+ i 1) (lookahead-u8 bin))))
			 (else
			  (put-bytevector out buf 0 i)
			  (loop b2)))))
		(else
		 (put-u8 out b)
		 (loop (lookahead-next-u8 bin))))))))

  ;; default \n = #x0a
  (define (get-line bin :key (eol #vu8(#x0a)) (transcoder #f))
    (let-values (((r _) (get-until bin eol)))
      (if transcoder
	  (bytevector->string r transcoder)
	  r)))

  ;; other utilities
  (define-syntax define-put&get
    (lambda (x)
      (define (->syntax k s)
	(datum->syntax k (string->symbol s)))
      (define (make-put&get k type)
	(let ((s (symbol->string (syntax->datum type))))
	  (list (->syntax k (string-append "bytevector-" s "-set!"))
		(->syntax k (string-append "bytevector-" s "-ref")))))
      (define (make-names k type)
	(let ((s (symbol->string (syntax->datum type))))
	  (list (->syntax k (string-append "put-" s))
		(->syntax k (string-append "get-" s)))))
      (define (get-size type)
	(let ((s (symbol->string (syntax->datum type))))
	  (div (string->number (string-copy s 1)) 8)))
      (syntax-case x ()
	((k type)
	 (with-syntax (((put get) (make-put&get #'k #'type))
		       ((pname gname) (make-names #'k #'type))
		       (size (get-size #'type)))
	   #'(begin
	       (define (pname out v endian)
		 (let ((buf (make-bytevector size)))
		   (put buf 0 v endian)
		   (put-bytevector out buf)))
	       (define (gname in endian)
		 (let ((buf (get-bytevector-n in size)))
		   (get buf 0 endian)))))))))

  (define (put-s8 out s8)
    (unless (<= -128 s8 127) (error 'put-s8 "out of range" s8))
    (if (< s8 128)
	(put-u8 out (bitwise-and s8 #xFF))
	(put-u8 out s8)))
  (define (get-s8 in)
    (let ((r (get-u8 in)))
      (if (< r 128)
	  r
	  ;; return two's complement
	  (- (bitwise-and r #x7F) 128))))

  (define-put&get u16)
  (define-put&get s16)
  (define-put&get u32)
  (define-put&get s32)
  (define-put&get u64)
  (define-put&get s64)
  ;; flonum needs to be treated differently ... 
  (define (put-f32 out v endian)
    (let ((buf (make-bytevector 4)))
      (bytevector-ieee-single-set! buf 0 v endian)
      (put-bytevector out buf)))
  (define (get-f32 in endian)
    (let ((buf (get-bytevector-n in 4)))
      (bytevector-ieee-single-ref buf 0 endian)))
  (define (put-f64 out v endian)
    (let ((buf (make-bytevector 8)))
      (bytevector-ieee-double-set! buf 0 v endian)
      (put-bytevector out buf)))
  (define (get-f64 in endian)
    (let ((buf (get-bytevector-n in 8)))
      (bytevector-ieee-double-ref buf 0 endian)))


  ;; built in bytevector input port would requires length of input date
  ;; however it would allocate huge amount of data.
  ;; e.g.) (open-bytevector-input-port (receive-all-packet socket))
  ;;  assume receive-all-packet receive more than 100MB of data.
  ;; in this situation, we want to make chunked port so that allocator
  ;; won't allocate huge chunk of memory.
  ;; NOTE: this is only for practical reason.
  (define-class <chunked-buffer> ()
    ((position :init-value 0) ;; index of vector
     (offset   :init-value 0) ;; offset of bytevector
     (chunks   :init-keyword :chunks)))

  ;; buffer size default 4096
  (define-constant +default-chunk-size+ 4096)
  ;; threshold is how much bytes this needs to read from source port
  ;; default #f (infinite).
  (define (input-port->chunked-binary-input-port 
	   in :key (chunk-size +default-chunk-size+) (threshold #f))
    (define (read-as-chunk chunk-size)
      (define (next-chunk-size read-size)
	(if threshold (min (- threshold read-size) chunk-size) chunk-size))

      (let loop ((chunk-size (next-chunk-size 0))
		 (read-size   0)
		 (r '()))
	(let ((buf (get-bytevector-n in chunk-size)))
	  (if (eof-object? buf) (list->vector (reverse! r))
	      (let ((len (bytevector-length buf)))
		(if (or (< len chunk-size)
			(and threshold (>= (+ read-size len) threshold)))
		    (list->vector (reverse! (cons buf r)))
		    (let ((read-size (+ read-size len)))
		      (loop (next-chunk-size read-size)
			    read-size
			    (cons buf r)))))))))
    (->chunked-binary-input-port read-as-chunk :chunk-size chunk-size))

  ;; in must be a procedure which takes chunk-size
  (define (->chunked-binary-input-port read-as-chunk
				       :key (chunk-size +default-chunk-size+))
    (let ((chunked-port (make <chunked-buffer>
			  :chunks (read-as-chunk chunk-size))))
      (define (read! bv start count)
	(define (last-chunk? chunks position)
	  (= (vector-length chunks) (+ position 1)))
	(let loop ((start start) (copied 0) (count count))
	  (let ((offset   (~ chunked-port 'offset))
		(position (~ chunked-port 'position))
		(chunks  (~ chunked-port 'chunks)))
	    (if (= (vector-length chunks) position)
		0
		(let* ((current-chunk (vector-ref chunks position))
		       (chunk-size (bytevector-length current-chunk))
		       (diff (- chunk-size offset)))
		  (cond ((>= diff count)
			 (bytevector-copy! current-chunk offset bv start count)
			 (cond ((and (not (last-chunk? chunks position))
				     (= (+ offset count) chunk-size))
				(set! (~ chunked-port 'offset) 0)
				(set! (~ chunked-port 'position)
				      (+ position 1)))
			       (else
				(set! (~ chunked-port 'offset)
				      (+ offset count))))
			 (+ count copied))
			((not (last-chunk? chunks position))
			 (bytevector-copy! current-chunk offset bv start diff)
			 (set! (~ chunked-port 'offset) 0)
			 (set! (~ chunked-port 'position) (+ position 1))
			 (loop (+ start diff) (+ diff copied) (- count diff)))
			(else
			 ;; last chunk and not enough
			 (bytevector-copy! current-chunk offset bv start diff)
			 (set! (~ chunked-port 'offset) (+ offset count))
			 (set! (~ chunked-port 'position) (+ position 1))
			 (+ diff copied))))))))
      (define (get-position)
	(let ((offset (~ chunked-port 'offset))
	      (position (~ chunked-port 'position)))
	  (+ offset (* position chunk-size))))
      (define (set-position! pos)
	(let ((index (div pos chunk-size))
	      (offset (mod pos chunk-size))
	      (chunks (~ chunked-port 'chunks)))
	  (cond ((>= index (vector-length chunks))
		 (let ((last (- (vector-length chunks) 1)))
		   (set! (~ chunked-port 'position) last)
		   (set! (~ chunked-port 'offset)
			 (bytevector-length (vector-ref chunks last)))))
		(else
		 (set! (~ chunked-port 'position) index)
		 (set! (~ chunked-port 'offset) offset)))))
      (define (close) (set! (~ chunked-port 'chunks) #f))
      (make-custom-binary-input-port "chunked-binary-input-port"
				     read! get-position set-position! close)))
)
