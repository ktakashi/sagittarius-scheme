;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; binary/io.scm - Binary IO.
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
	    ;; n variable
	    put-u* put-s* get-u* get-s*

	    ;; memory efficient(?) ports
	    input-port->chunked-binary-input-port
	    ->chunked-binary-input-port
	    open-chunked-binary-input/output-port
	    ->chunked-binary-input/output-port
	    +default-chunk-size+

	    ->size-limit-binary-input-port
	    )
    (import (except (rnrs) get-line)
	    (sagittarius)
	    (clos user)
	    (sagittarius object)
	    (only (srfi :43 vectors) vector-copy!))

  (define (lookahead-next-u8 in) (get-u8 in) (lookahead-u8 in))

  (define (get-until bin bv-mark)
    (let ((bv (get-bytevector-until bin bv-mark)))
      (values bv (if (eof-object? bv) #f bv-mark))))

  ;; default \n = #x0a
  (define (get-line bin :key (eol #vu8(#x0a)) (transcoder #f))
    (if (eof-object? (lookahead-u8 bin))
	(eof-object)
	(let-values (((r _) (get-until bin eol)))
	  (if transcoder
	      (bytevector->string r transcoder)
	      r))))

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

  ;; 16 bits and 32 bits are defined in C for better performance
  ;; I think these 2 are the most used ones.
  ;;(define-put&get u16)
  ;;(define-put&get s16)
  ;;(define-put&get u32)
  ;;(define-put&get s32)

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

  (define-syntax define-put&get*
    (lambda (x)
      (define (->syntax k v) (datum->syntax k v))
      (define (->names k name)
	(let ((get (string->symbol (format "get-~a*" (syntax->datum name))))
	      (put (string->symbol (format "put-~a*" (syntax->datum name)))))
	  (->syntax k (list get put))))
      (define (->set&ref k name)
	(let ((set (string->symbol (format "bytevector-~aint-set!"
					   (syntax->datum name))))
	      (ref (string->symbol (format "bytevector-~aint-ref"
					   (syntax->datum name)))))
	  (->syntax k (list set ref))))
      (syntax-case x ()
	((k name)
	 (with-syntax (((get put) (->names #'k #'name))
		       ((set! ref) (->set&ref #'k #'name)))
	   #'(begin
	       (define (put out v n endian)
		 (let ((bv (make-bytevector n)))
		   (set! bv 0 v endian n)
		   (put-bytevector out bv)))
	       (define (get in n endian)
		 (let ((bv (get-bytevector-n in n)))
		   (ref bv 0 endian n)))))))))

  (define-put&get* u)
  (define-put&get* s)

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
  ;; for input/output port
  (define-class <chunked-output-buffer> (<chunked-buffer>)
    ((buffer-size :init-value 0)
     (threshold :init-value 0)))

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
      (define (close) (set! (~ chunked-port 'chunks) #f))
      (make-custom-binary-input-port "chunked-binary-input-port"
				     (%read! chunked-port)
				     (%get-position chunked-port chunk-size) 
				     (%set-position! chunked-port chunk-size)
				     close)))

  ;; should we take buffer size so that we can pre-allocate?
  (define (open-chunked-binary-input/output-port 
	   :key (chunk-size +default-chunk-size+))
    (define (read-as-chunk ignore) #())
    (->chunked-binary-input/output-port read-as-chunk :chunk-size chunk-size))
  ;; we don't need only output port (built-in bytevector output port
  ;; isn't so bad)
  (define (->chunked-binary-input/output-port read-as-chunk
	   :key (chunk-size +default-chunk-size+))
    (let ((chunked-port (make <chunked-output-buffer>
			  :chunks (read-as-chunk chunk-size))))
      (define (align-chunk)
	;; last chunk check
	(let ((chunks (~ chunked-port 'chunks)))
	  (unless (zero? (vector-length chunks))
	    (let ((chunk (vector-ref chunks (- (vector-length chunks) 1))))
	      (unless (= (bytevector-length chunk) chunk-size)
		(let ((buf (make-bytevector chunk-size)))
		  (bytevector-copy! chunk 0 buf 0 (bytevector-length chunk))
		  (vector-set! chunks (- (vector-length chunks) 1) buf)))
	      (set! (~ chunked-port 'buffer-size) 
		    (* (vector-length chunks) chunk-size))))))
      (define get-position (%get-position chunked-port chunk-size))
      (define %read (%read! chunked-port))
      (define (read! bv start count)
	(let ((pos (get-position))
	      (threshold (~ chunked-port 'threshold)))
	  ;; if pos = then sof
	  ;; if pos + count > threshold then reduce amount
	  (cond ((= pos threshold) 0)
		((> (+ pos count) threshold)
		 (%read bv start (- threshold pos)))
		(else (%read bv start count)))))
      (define (expand! n)
	(let* ((chunks (~ chunked-port 'chunks))
	       (new-chunks (make-vector (+ (vector-length chunks) n))))
	  ;; a bit of stupid kludge for empty chunk...
	  (unless (zero? (vector-length chunks))
	    (vector-copy! new-chunks 0 chunks))
	  ;; expand!
	  (do ((i 0 (+ i 1)))
	      ((= i n))
	    (vector-set! new-chunks (+ (vector-length chunks) i)
			 (make-bytevector chunk-size 0)))
	  (set! (~ chunked-port 'chunks) new-chunks)
	  (set! (~ chunked-port 'buffer-size) 
		(* (vector-length new-chunks) chunk-size))))
      (define (write! bv start count)
	(let ((pos (get-position))
	      (size (~ chunked-port 'buffer-size)))
	  (when (>= (+ pos count) size)
	    ;; compute how many chuns required for this
	    (let ((required (ceiling (/ (+ pos count) chunk-size)))
		  (size (vector-length (~ chunked-port 'chunks))))
	      (expand! (- required size))))
	  ;; update threshold, when write! is adding data not updating
	  (when (< (~ chunked-port 'threshold) (+ pos count))
	    (set! (~ chunked-port 'threshold) (+ pos count)))
	  ;; now we have enough buffer so just fill
	  (let ((chunks (~ chunked-port 'chunks)))
	    (let loop ((offset (~ chunked-port 'offset))
		       (position (~ chunked-port 'position))
		       (start start)
		       (count count)
		       (written 0))
	      (let ((chunk (vector-ref chunks position)))
		(cond ((>= (- chunk-size offset) count)
		       (bytevector-copy! bv start chunk offset count)
		       (set! (~ chunked-port 'position) position)
		       (set! (~ chunked-port 'offset) (+ offset count))
		       (+ written count))
		      (else 
		       (let ((diff (- chunk-size offset)))
			 (bytevector-copy! bv start chunk offset diff)
			 (loop 0 (+ position 1) (+ start diff) (- count diff)
			       (+ written diff))))))))))
      ;; if it's overflow then we need to expand chunks
      (define %set (%set-position! chunked-port chunk-size))
      (define (set-position! pos)
	(let ((size (~ chunked-port 'buffer-size)))
	  (when (>= pos size)
	    (let ((required (ceiling (/ pos chunk-size))))
	      (expand! (- required (vector-length (~ chunked-port 'chunks))))
	      (set! (~ chunked-port 'threshold) pos)))
	  (%set pos)))
      (define (close) (set! (~ chunked-port 'chunks) #f))
      (set! (~ chunked-port 'threshold)
	    (let* ((chunks (~ chunked-port 'chunks))
			    (len (vector-length chunks)))
	      (let loop ((i 0) (c 0))
		(if (= i len)
		    c
		    (loop (+ i 1) 
			  (+ c (bytevector-length (vector-ref chunks i))))))))
      ;; expand last chunk if needed
      (align-chunk)
      (make-custom-binary-input/output-port
       "chunked-binary-input/output-port"
       read! write! get-position set-position! close)))

  ;; common procedure
  ;; read must consider threshold for input/output...
  (define (%read! chunked-port)
    (lambda (bv start count)
      (define chunks  (~ chunked-port 'chunks))
      (define chunks-length (vector-length chunks))
      (define (last-chunk? position) (= chunks-length (+ position 1)))
      (let loop ((start start) (copied 0) (count count))
	(let ((offset   (~ chunked-port 'offset))
	      (position (~ chunked-port 'position)))
	  (if (= chunks-length position)
	      0
	      (let* ((current-chunk (vector-ref chunks position))
		     (chunk-size (bytevector-length current-chunk))
		     (diff (- chunk-size offset)))
		(cond ((>= diff count)
		       (bytevector-copy! current-chunk offset bv start count)
		       (cond ((and (not (last-chunk? position))
				   (= (+ offset count) chunk-size))
			      (slot-set! chunked-port 'offset 0)
			      (slot-set! chunked-port 'position (+ position 1)))
			     (else
			      (slot-set! chunked-port 'offset
					 (+ offset count))))
		       (+ count copied))
		      ((not (last-chunk? position))
		       (bytevector-copy! current-chunk offset bv start diff)
		       (slot-set! chunked-port 'offset 0)
		       (slot-set! chunked-port 'position (+ position 1))
		       (loop (+ start diff) (+ diff copied) (- count diff)))
		      (else
		       ;; last chunk and not enough
		       (bytevector-copy! current-chunk offset bv start diff)
		       (slot-set! chunked-port 'offset (+ offset count))
		       (slot-set! chunked-port 'position (+ position 1))
		       (+ diff copied)))))))))
  (define (%get-position chunked-port chunk-size)
    (lambda ()
      (let ((offset (~ chunked-port 'offset))
	    (position (~ chunked-port 'position)))
	(+ offset (* position chunk-size)))))
  (define (%set-position! chunked-port chunk-size)
    (lambda (pos)
      (let* ((index (div pos chunk-size))
	     (offset (mod pos chunk-size))
	     (chunks (~ chunked-port 'chunks))
	     (chunk-len (vector-length chunks)))
	(cond ((and (> chunk-len 0) (>= index chunk-len))
	       (let ((last (- (vector-length chunks) 1)))
		 (set! (~ chunked-port 'position) last)
		 (set! (~ chunked-port 'offset)
		       (bytevector-length (vector-ref chunks last)))))
	      (else
	       (set! (~ chunked-port 'position) index)
	       (set! (~ chunked-port 'offset) offset))))))

  ;; size limited port
  ;; this itself might not be so useful but if there is an API
  ;; which takes input port and reads until EOF, then this can
  ;; be used.
  ;; 
  ;; use case:
  ;;  a port contains multiple records and an API reads it but
  ;;  expects the given port ends with EOF. if the size of the
  ;;  record is known then this can be used instead of get all
  ;;  bytes ahead and convert it to bytevector input port.
  (define (->size-limit-binary-input-port iport size)
    (define size-left size)
    (define original-position (if (port-has-port-position? iport)
				  (port-position iport)
				  #f))

    (define (read! bv start count)
      (let ((reading-size (min count size-left)))
	(if (<= reading-size 0)
	    0 ;; don't call get-bytevector-n! for performance
	    (let ((ret (get-bytevector-n! iport bv start reading-size)))
	      (cond ((eof-object? ret) 0) ;; well size was too big but hey
		    ((zero? ret) 0)	;; count was 0?
		    (else
		     (set! size-left (- size-left ret))
		     ret))))))
    (define position
      (and original-position
	   (lambda () (- size size-left))))
    
    (define set-position!
      (and original-position
	   (port-has-set-port-position!? iport)
	   (lambda (pos)
	     (cond ((> pos size) 
		    (set! size-left 0)
		    (set-port-position! iport (+ original-position size)))
		   (else
		    (set! size-left (- size pos))
		    (set-port-position! iport (+ original-position pos)))))))
    ;; should we take keyword argument to close iport?
    (define (close) #t)
    ;; Sagittarius specific remove if you want to make this
    ;; R6RS compliant.
    (define (ready) (port-ready? iport))
    (make-custom-binary-input-port "limited-size port" 
				   read! position set-position! close ready))
  
)
