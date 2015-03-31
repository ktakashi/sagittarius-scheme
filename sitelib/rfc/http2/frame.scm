;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http2/frame.scm - HTTP2 frame
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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


(library (rfc http2 frame)
    (export fill-http2-frame-buffer!
	    read-http2-frame
	    ;; TODO rename this...
	    write-http2-frame!

	    frame-buffer? make-frame-buffer
	    frame-buffer-buffer frame-buffer-size
	    update-frame-buffer!
	    ->frame-buffer-output-port
	    frame-buffer-can-store?

	    (rename (+initial-frame-buffer-size+
		     +http2-initial-frame-buffer-size+)
		    (+max-frame-buffer-size+
		     +http2-max-frame-buffer-size+))
	    ;; constants
	    +http2-frame-type-data+
	    +http2-frame-type-headers+
	    +http2-frame-type-priority+
	    +http2-frame-type-rst-stream+
	    +http2-frame-type-settings+
	    +http2-frame-type-push-promise+
	    +http2-frame-type-ping+
	    +http2-frame-type-goaway+
	    +http2-frame-type-window-update+
	    +http2-frame-type-continuation+
	    ;; frame types
	    http2-frame?
	    http2-frame-type
	    http2-frame-flags
	    http2-frame-stream-identifier
	    ;; DATA
	    http2-frame-data? make-http2-frame-data http2-frame-data-data
	    ;; HEADERS
	    http2-frame-headers? make-http2-frame-headers
	    http2-frame-headers-stream-dependency http2-frame-headers-weight
	    http2-frame-headers-headers
	    ;; PRIORISY
	    http2-frame-priority? make-http2-frame-priority
	    http2-frame-priority-stream-dependency http2-frame-priority-weight
	    ;; RST-STREAM
	    http2-frame-rst-stream? make-http2-frame-rst-stream
	    http2-frame-rst-stream-error-code
	    ;; SETTINGS
	    http2-frame-settings? make-http2-frame-settings
	    http2-frame-settings-settings
	    ;; PUSH_PROMISE
	    http2-frame-push-promise? make-http2-frame-push-promise
	    http2-frame-push-promise-pushed-promise-id
	    http2-frame-push-promise-headers
	    ;; PING
	    http2-frame-ping? make-http2-frame-ping
	    http2-frame-ping-opaque-data
	    ;; GOARAY
	    http2-frame-goaway? make-http2-frame-goaway
	    http2-frame-goaway-last-stream-id http2-frame-goaway-error-code
	    http2-frame-goaway-data
	    ;; WINDOW_UPDATE
	    http2-frame-window-update? make-http2-frame-window-update
	    http2-frame-window-update-window-size-increment
	    ;; CONTINUATION
	    http2-frame-continuation? make-http2-frame-continuation
	    http2-frame-continuation-headers
	    )
    (import (rnrs)
	    (sagittarius)
	    (binary io)
	    (rfc http2 hpack)
	    (rfc http2 conditions))

  (define-constant +initial-frame-buffer-size+ #x4000)
  ;; Don't use this much but we can do it.
  (define-constant +max-frame-buffer-size+     #xffffff)

  (define-record-type (<frame-buffer> make-frame-buffer frame-buffer?)
    (fields (mutable buffer frame-buffer-buffer frame-buffer-buffer-set!)
	    ;; buffer size (not buffer itself but read size)
	    (mutable size   frame-buffer-size   frame-buffer-size-set!))
    (protocol (lambda (p)
		(case-lambda
		 (() (p (make-bytevector +initial-frame-buffer-size+) 0))
		 ((size)
		  (unless (<= +initial-frame-buffer-size+
			      size 
			      +max-frame-buffer-size+)
		    (http2-protocol-error 'make-frame-buffer
					  "Frame size out of range" size))
		  (p (make-bytevector size) 0))))))
  (define (update-frame-buffer! buffer size)
    (unless (<= +initial-frame-buffer-size+ size +max-frame-buffer-size+)
      (http2-protocol-error 'update-frame-buffer!
			    "Frame size out of range" size))
    (let ((buf (frame-buffer-buffer buffer))
	  (siz (frame-buffer-size buffer))
	  (new (make-bytevector size)))
      ;; TODO check shrink
      ;; TODO should we actually copy?
      (bytevector-copy! buf 0 new 0 siz)
      (frame-buffer-buffer-set! buffer new)
      buffer))
  (define (yeild-buffer buffer padding?)
    (let ((buf (frame-buffer-buffer buffer))
	  (size (frame-buffer-size buffer)))
      (if padding?
	  (let ((plen (bytevector-u8-ref buf 0)))
	    (bytevector-copy buf 1 (- size plen)))
	  (bytevector-copy buf 0 size))))

  #|
  Frame format
    +-----------------------------------------------+
    |                 Length (24)                   |
    +---------------+---------------+---------------+
    |   Type (8)    |   Flags (8)   |
    +-+-------------+---------------+-------------------------------+
    |R|                 Stream Identifier (31)                      |
    +=+=============================================================+
    |                   Frame Payload (0...)                      ...
    +---------------------------------------------------------------+
  |#
  ;; returns 3 values and updates given frame buffer.
  (define (fill-http2-frame-buffer! in buffer)
    (define (read-u24 in)
      (let ((bv (get-bytevector-n in 3)))
	(bytevector->integer bv)))
    (unless (frame-buffer? buffer)
      (assertion-violation 'fill-http2-frame-buffer!
			   "frame buffer required" buffer))
    (let ((len (read-u24 in))
	  (type (get-u8 in))
	  (flags (get-u8 in))
	  (si   (get-u32 in 'big))
	  (buf (frame-buffer-buffer buffer)))
      (when (> len (bytevector-length buf))
	(http2-frame-size-error 'fill-http2-frame-buffer!
				"Frame size exceed SETTINGS_MAX_FRAME_SIZE"
				len))
      (get-bytevector-n! in buf 0 len)
      (frame-buffer-size-set! buffer len)
      (values type flags si)))

  (define-record-type http2-frame
    ;; fields are immutable for reading
    (fields type
	    flags
	    stream-identifier))

  (define-syntax define-http2-frame
    (lambda (x)
      (define (->const name)
	(let ((str (symbol->string (syntax->datum name))))
	  (string->symbol (string-append "+http2-frame-type-" str "+"))))
      (define (->name name)
	(let ((str (symbol->string (syntax->datum name))))
	  (string->symbol (string-append "http2-frame-" str))))
      (syntax-case x ()
	((_ name code ?fields ...)
	 (with-syntax ((const (datum->syntax #'k (->const #'name)))
		       (type (datum->syntax #'k (->name #'name))))
	   #'(begin
	       (define-constant const code)
	       (define-record-type type
		 (parent http2-frame)
		 (sealed #t)
		 (fields ?fields ...)
		 (protocol
		  (lambda (p)
		    (lambda (flags si ?fields ...)
		      ((p code type si) ?fields ...)))))))))))

  (define-http2-frame data          #x0 data)
  (define-http2-frame headers       #x1 stream-dependency weight headers)
  (define-http2-frame priority      #x2 stream-dependency weight)
  (define-http2-frame rst-stream    #x3 error-code)
  (define-http2-frame settings      #x4 settings)
  (define-http2-frame push-promise  #x5 pushed-promise-id headers)
  (define-http2-frame ping          #x6 opaque-data)
  (define-http2-frame goaway        #x7 last-stream-id error-code data)
  (define-http2-frame window-update #x8 window-size-increment)
  (define-http2-frame continuation  #x9 headers)

  ;; always big endian
  (define (put-length out n) (put-bytevector out (integer->bytevector n 3)))
  (define (write-http2-frame! out type flags si buffer)
    (let ((buf (frame-buffer-buffer buffer))
	  (siz (frame-buffer-size buffer)))
      (put-length out siz)
      (put-u8 out type)
      (put-u8 out flags)
      (put-u32 out si 'big)
      (put-bytevector out buf 0 siz)
      (flush-output-port out)))

  ;; for convenience
  (define (frame-buffer-can-store? frame-buffer count)
    (let ((size (frame-buffer-size frame-buffer)))
      (< (+ size count) 
	 (bytevector-length (frame-buffer-buffer frame-buffer)))))

  (define-condition-type &frame-buffer-overflow &i/o
    make-frame-buffer-overflow frame-buffer-overflow?
    (data frame-buffer-overflow-data))
  (define (frame-buffer-overflow data)
    (raise (condition (list (make-frame-buffer-overflow data)
			    (make-who-condition 'frame-buffer-port)
			    (make-message-condition "frame buffer overflow")))))
  
  (define (->frame-buffer-output-port frame-buffer)
    ;; for updating buffer
    (define (buffer) (frame-buffer-buffer frame-buffer))
    (define (buffer-size) (frame-buffer-size frame-buffer))
    (define (update-size! count)
      (let ((size (frame-buffer-size frame-buffer)))
	(frame-buffer-size-set! frame-buffer (+ size count))))
    (define (can-write? count)
      (frame-buffer-can-store? frame-buffer count))

    (define (write! bv start count)
      (unless (can-write? count)
	(frame-buffer-overflow (bytevector-copy bv start count)))
      (bytevector-copy! bv start (buffer) (buffer-size) count)
      (update-size! count)
      count)
    (define position buffer-size)
    (define (set-position! pos)
      (unless (<= 0 pos (buffer-size))
	(raise (condition (make-i/o-invalid-position-error pos)
			  (make-who-condition 'frame-buffer-port)
			  (make-message-condition "invalid position"))))
      (update-size! pos))
    (define (close!) #t)
    (make-custom-binary-output-port "frame-buffer-port"
				    write! position set-position! close!))

  ;; Reading HTTP2 frame
  ;; - buffer needs to be explicitly passed
  ;; - hpack-reader is used only for HEADER and PUSH_PROMISE
  (define (read-http2-frame in buffer hpack-reader)
    (let-values (((type flags si) (fill-http2-frame-buffer! in buffer)))
      ((vector-ref *http2-frame-convertors* type) 
       flags si buffer hpack-reader)))

  (define *http2-frame-convertors* (make-vector 10))
  (define-syntax define-frame-converter
    (syntax-rules ()
      ((_ code (name . args) body ...)
       (define name
	 (let ((p (lambda args body ...)))
	   (vector-set! *http2-frame-convertors* code p)
	   p)))))
  
  (define-frame-converter +http2-frame-type-data+ 
    (data-converter flags si buffer hpack-reader)
    (make-http2-frame-data flags si 
			   (yeild-buffer buffer (bitwise-bit-set? flags 3))))

  (define (parse-header-block flags buffer hpack-reader weight?)
    (define (read-headers bv size padding? offset hpack-reader)
      (let ((in (if padding?
		    (let ((pad (bytevector-u8-ref bv 0)))
		      (open-bytevector-input-port bv #f (+ 1 offset)
						  (- size pad)))
		    (open-bytevector-input-port bv #f offset size))))
	(hpack-reader in)))
    (define (read-dependency&weight bv offset)
      (values (bytevector-u32-ref bv offset (endianness big))
	      (if weight? (bytevector-u8-ref bv (+ offset 4)) #f)))

    (let ((buf (frame-buffer-buffer buffer))
	  (size (frame-buffer-size buffer))
	  (padding? (bitwise-bit-set? flags 3))
	  (priority? (if weight? (bitwise-bit-set? flags 5) #f)))
      (let-values (((d w) (if (or priority? (not weight?))
			      (read-dependency&weight buf (if padding? 1 0))
			      (values #f #f))))
	(values d w
		(read-headers buf size padding? 
			      (cond (priority? 5)
				    ((not weight?) 4)
				    (else 0))
			      hpack-reader)))))

  (define-frame-converter +http2-frame-type-headers+
    (headers-converter flags si buffer hpack-reader)
    (let-values (((d w headers)
		  (parse-header-block flags buffer hpack-reader #t)))
	(make-http2-frame-headers flags si d w headers)))

   (define-frame-converter +http2-frame-type-priority+
     (priority-converter flags si buffer hpack-reader)
     (let* ((buf (frame-buffer-buffer buffer))
	    (sd (bytevector-u32-ref buf 0 (endianness big)))
	    (w  (bytevector-u8-ref buf 4)))
       (make-http2-frame-priority flags si sd w)))
   (define-frame-converter +http2-frame-type-rst-stream+
     (rst-stream-converter flags si buffer hpack-reader)
     (let ((buf (frame-buffer-buffer buffer)))
       (make-http2-frame-rst-stream flags si
	(bytevector-u32-ref buf 0 (endianness big)))))
   (define-frame-converter +http2-frame-type-settings+
     (settings-converter flags si buffer hpack-reader)
     (define (parse-settings settings size)
       (unless (zero? (mod size 6))
	 (http2-frame-size-error 'settings-converter
			   "SETTINGS must be multiple of 6"
			   (bytevector-copy settings 0 size)))
       ;; TODO parse it properly
       '())
     (make-http2-frame-settings flags si
      (parse-settings (frame-buffer-buffer buffer)
		      (frame-buffer-size buffer))))
   (define-frame-converter +http2-frame-type-push-promise+
     (push-promise-converter flags si buffer hpack-reader)
     (let-values (((d w headers)
		   (parse-header-block flags buffer hpack-reader #f)))
	(make-http2-frame-push-promise flags si d headers)))

   (define-frame-converter +http2-frame-type-ping+
     (ping-converter flags si buffer hpack-reader)
     (make-http2-frame-ping flags si (yeild-buffer buffer #f)))

   (define-frame-converter +http2-frame-type-goaway+
     (goaway-converter flags si buffer hpack-reader)
     (let* ((buf (frame-buffer-buffer buffer))
	    (size (frame-buffer-size buffer))
	    (sid (bytevector-u32-ref buf 0 (endianness big)))
	    (code (bytevector-u32-ref buf 4 (endianness big))))
       (make-http2-frame-goaway flags si sid code 
				(bytevector-copy buf 8 size))))

   (define-frame-converter +http2-frame-type-window-update+
     (window-update-converter flags si buffer hpack-reader)
     (let ((buf (frame-buffer-buffer buffer)))
       (make-http2-frame-window-update flags si
	(bytevector-u32-ref buf 0 (endianness big)))))

   (define-frame-converter +http2-frame-type-continuation+
     (continuation-converter flags si buffer hpack-reader)
     (let ((buf (frame-buffer-buffer buffer))
	   (size (frame-buffer-size buffer)))
       (make-http2-frame-continuation flags si 
	(hpack-reader (open-bytevector-input-port buf #f 0 size)))))

)
