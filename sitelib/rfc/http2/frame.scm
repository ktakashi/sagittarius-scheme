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
    (export fill-http2-frame-buffer! ;; for testing
	    read-http2-frame

	    store-frame-to-frame-buffer! ;; for testing
	    write-http2-frame

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
	    ;; SETTINGS
	    +http2-settings-header-table-size+
	    +http2-settings-enable-push+
	    +http2-settings-max-concurrent-streams+
	    +http2-settings-initial-window-size+
	    +http2-settings-max-frame-size+
	    +http2-settings-max-header-list-size+

	    ;; frame types
	    http2-frame?
	    http2-frame-type
	    http2-frame-flags
	    http2-frame-stream-identifier
	    ;; helper
	    http2-frame-end-stream?
	    http2-frame-end-headers?

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
	    (rfc http2 conditions)
	    (rename (util buffer)
		    (pre-allocated-buffer-buffer frame-buffer-buffer)
		    (pre-allocated-buffer-size frame-buffer-size)
		    (binary-pre-allocated-buffer? frame-buffer?)
		    (binary-pre-allocated-buffer-can-store?
		     frame-buffer-can-store?)
		    (->binary-pre-allocated-buffer-output-port
		     ->frame-buffer-output-port)))

  ;; constants
  (define-constant +initial-frame-buffer-size+ #x4000)
  ;; Don't use this much but we can do it.
  (define-constant +max-frame-buffer-size+     #xffffff)

  (define make-frame-buffer
    (case-lambda
     (() (make-binary-pre-allocated-buffer
	  (make-bytevector +initial-frame-buffer-size+)))
     ((size)
      (unless (<= +initial-frame-buffer-size+ size +max-frame-buffer-size+)
	(http2-protocol-error 'make-frame-buffer
			      "Frame size out of range" size))
      (make-binary-pre-allocated-buffer
       (make-bytevector +max-frame-buffer-size+)))))

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
      (binary-pre-allocated-buffer-swap! buffer new siz)
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
      (binary-pre-allocated-buffer-get-bytevector-n! buffer in len 0)
      (values type flags si)))

  (define-record-type http2-frame
    ;; fields are immutable for reading
    (fields type
	    flags
	    stream-identifier))

  ;; sort of common flags among the frames
  (define (http2-frame-end-stream? frame)
    (and (or (http2-frame-data? frame)
	     (http2-frame-headers? frame))
	 (bitwise-bit-set? (http2-frame-flags frame) 0)))
  (define (http2-frame-end-headers? frame)
    (and (or (http2-frame-headers? frame)
	     (http2-frame-push-promise? frame)
	     (http2-frame-continuation? frame)
	 (bitwise-bit-set? (http2-frame-flags frame) 2))))

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
		      ((p code flags si) ?fields ...)))))))))))

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

  ;; SETTINGS
  (define-constant +http2-settings-header-table-size+      #x1)
  (define-constant +http2-settings-enable-push+            #x2)
  (define-constant +http2-settings-max-concurrent-streams+ #x3)
  (define-constant +http2-settings-initial-window-size+    #x4)
  (define-constant +http2-settings-max-frame-size+         #x5)
  (define-constant +http2-settings-max-header-list-size+   #x6)

  ;; hpack-context is required for HEADER, PUSH_PROMISE and CONTINUATION
  ;; NOTE: must be a row context to count
  (define (write-http2-frame out buffer frame end? hpack-context)
    (pre-allocated-buffer-reset! buffer)
    (let ((next (store-frame-to-frame-buffer! frame buffer end? hpack-context)))
      (put-bytevector out (frame-buffer-buffer buffer) 0
		      (frame-buffer-size buffer))
      (flush-output-port out)
      (when next (write-http2-frame out buffer next end? hpack-context))))

  (define (store-frame-to-frame-buffer! frame buffer end? hpack-context)
    ((vector-ref *http2-buffer-convertors* (http2-frame-type frame))
     frame buffer end? hpack-context))

  (define *http2-buffer-convertors* (make-vector 10))
  (define-syntax define-buffer-converter
    (syntax-rules ()
      ((_ code (name . args) body ...)
       (define name
	 (let ((name (lambda args body ...)))
	   (vector-set! *http2-buffer-convertors* code name)
	   name)))))

  ;; Storing frame to buffer
  ;; helpers
  (define-constant +frame-common-size+ 9)
  (define (put-frame-common buf length type flags si)
    (binary-pre-allocated-buffer-put-bytevector! buf
     (integer->bytevector length 3))
    (binary-pre-allocated-buffer-put-u8! buf type)
    (binary-pre-allocated-buffer-put-u8! buf flags)
    (binary-pre-allocated-buffer-put-u32! buf si (endianness big)))

  (define-buffer-converter +http2-frame-type-data+
    (buffer-converter-data frame buffer end? ctx)
    (let* ((type (http2-frame-type frame))
	   (flags (http2-frame-flags frame))
	   (si (http2-frame-stream-identifier frame))
	   ;; data
	   (data (http2-frame-data-data frame))
	   (buf  (frame-buffer-buffer buffer))
	   (data-size (bytevector-length data))
	   (buf-size  (- (bytevector-length buf) +frame-common-size+)))
      ;; flags
      ;; for now we don't do padding
      (let ((end? (and end? (>= buf-size data-size)))
	    (len  (min buf-size data-size)))
	(put-frame-common buffer len type (if end? 1 0) si)
	(binary-pre-allocated-buffer-put-bytevector! buffer data 0 len)
	(and (< buf-size data-size)
	     ;; TODO maybe we want to use shared data structure...
	     (make-http2-frame-data flags si (bytevector-copy data len))))))

  (define-buffer-converter +http2-frame-type-headers+
    (buffer-converter-headers frame buffer end? ctx)
    (let* ((type (http2-frame-type frame))
	   (flags (http2-frame-flags frame))
	   (si (http2-frame-stream-identifier frame))
	   ;; data
	   (headers (http2-frame-headers-headers frame))
	   (deps (http2-frame-headers-stream-dependency frame))
	   (weight (http2-frame-headers-weight frame))
	   (buf  (frame-buffer-buffer buffer))
	   (data-size (count-hpack-bytes ctx headers))
	   (buf-size  (- (bytevector-length buf) +frame-common-size+)))
      (when (> data-size buf-size)
	(error 'buffer-converter-headers
	       "continuation header is not supported"))
      ;; flags
      ;; for now we don't do padding
      (let ((priority? (and deps weight))
	    (out (->frame-buffer-output-port buffer)))
	(put-frame-common buffer data-size type
			  (bitwise-ior (if end? 1 0) 4 (if priority? 20 0))
			  si)
	(write-hpack out ctx headers)
	#f)))

;; later
;;   (define-buffer-converter +http2-frame-type-priority+
;;     (buffer-converter-priority frame buffer end? ctx))
;;   (define-buffer-converter +http2-frame-type-rst-stream+
;;     (buffer-converter-rst-stream frame buffer end? ctx))
   (define-buffer-converter +http2-frame-type-settings+
     (buffer-converter-settings frame buffer end? ctx)
     (let* ((type (http2-frame-type frame))
	    (flags (http2-frame-flags frame))
	    (si (http2-frame-stream-identifier frame))
	    (settings (http2-frame-settings-settings frame)))
       ;; sanity check
       (unless (zero? si)
	 (http2-protocol-error 'write-http2-frame
			       "SETTINGS got non zero stream identifier" si))
       (when (and (bitwise-bit-set? flags 0)
		  (not (null? settings)))
	 (http2-frame-size-error 'write-http2-frame
				 "SETTINGS with ACK must not have settings"))
       (put-frame-common buffer (* (length settings) 6) type flags si)
       (let loop ((settings settings))
	 (if (null? settings)
	     #f
	     (let ((s (car settings)))
	       (binary-pre-allocated-buffer-put-u16! buffer (car s)
						     (endianness big))
	       (binary-pre-allocated-buffer-put-u32! buffer (cadr s)
						     (endianness big))
	       (loop (cdr settings))))
	 )))
;;   (define-buffer-converter +http2-frame-type-push-promise+
;;     (buffer-converter-push-promise frame buffer end? ctx))
;;   (define-buffer-converter +http2-frame-type-ping+
;;     (buffer-converter-ping frame buffer end? ctx))
;;   (define-buffer-converter +http2-frame-type-goaway+
;;     (buffer-converter-goaway frame buffer end? ctx))
;;   (define-buffer-converter +http2-frame-type-window-update+
;;     (buffer-converter-window-update frame buffer end? ctx))
;;   (define-buffer-converter +http2-frame-type-continuation+
;;     (buffer-converter-continuation frame buffer end? ctx))

  ;; Reading HTTP2 frame
  ;; - buffer needs to be explicitly passed
  ;; - hpack-reader is used only for HEADER, PUSH_PROMISE and CONTINUATION
  (define (read-http2-frame in buffer hpack-context)
    (let-values (((type flags si) (fill-http2-frame-buffer! in buffer)))
      ((vector-ref *http2-frame-convertors* type)
       flags si buffer hpack-context)))

  (define *http2-frame-convertors* (make-vector 10))
  (define-syntax define-frame-converter
    (syntax-rules ()
      ((_ code (name . args) body ...)
       (define name
	 (let ((name (lambda args body ...)))
	   (vector-set! *http2-frame-convertors* code name)
	   name)))))

  (define-frame-converter +http2-frame-type-data+
    (data-converter flags si buffer hpack-context)
    (make-http2-frame-data flags si
			   (yeild-buffer buffer (bitwise-bit-set? flags 3))))

  (define (parse-header-block flags buffer hpack-context weight?)
    (define (read-headers bv size padding? offset hpack-context)
      (let ((in (if padding?
		    (let ((pad (bytevector-u8-ref bv 0)))
		      (open-bytevector-input-port bv #f (+ 1 offset)
						  (- size pad)))
		    (open-bytevector-input-port bv #f offset size))))
	(read-hpack in hpack-context)))
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
			      hpack-context)))))

  (define-frame-converter +http2-frame-type-headers+
    (headers-converter flags si buffer hpack-context)
    (let-values (((d w headers)
		  (parse-header-block flags buffer hpack-context #t)))
	(make-http2-frame-headers flags si d w headers)))

   (define-frame-converter +http2-frame-type-priority+
     (priority-converter flags si buffer hpack-context)
     (let* ((buf (frame-buffer-buffer buffer))
	    (sd (bytevector-u32-ref buf 0 (endianness big)))
	    (w  (bytevector-u8-ref buf 4)))
       (make-http2-frame-priority flags si sd w)))
   (define-frame-converter +http2-frame-type-rst-stream+
     (rst-stream-converter flags si buffer hpack-context)
     (let ((buf (frame-buffer-buffer buffer)))
       (make-http2-frame-rst-stream flags si
	(bytevector-u32-ref buf 0 (endianness big)))))
   (define-frame-converter +http2-frame-type-settings+
     (settings-converter flags si buffer hpack-context)
     (define (parse-settings settings size)
       (unless (zero? (mod size 6))
	 (http2-frame-size-error 'settings-converter
			   "SETTINGS must be multiple of 6"
			   (bytevector-copy settings 0 size)))
       (let loop ((i 0) (r '()))
	 (if (= i size)
	     (reverse! r)
	     (let ((id (bytevector-u16-ref settings i (endianness big)))
		   (v (bytevector-u32-ref settings (+ i 2) (endianness big))))
	       ;; TODO convert identifier to readable symbol
	       (loop (+ i 6) (acons id v r))))))
     (unless (zero? si)
       (http2-protocol-error 'read-http2-frame
			     "SETTINGS got non zero stream identifier" si))
     (when (and (bitwise-bit-set? flags 0)
		(not (zero? (frame-buffer-size buffer))))
       (http2-frame-size-error 'read-http2-frame
			       "SETTINGS with ACK has non zero data"))
     (make-http2-frame-settings flags si
      (parse-settings (frame-buffer-buffer buffer)
		      (frame-buffer-size buffer))))
   (define-frame-converter +http2-frame-type-push-promise+
     (push-promise-converter flags si buffer hpack-context)
     (let-values (((d w headers)
		   (parse-header-block flags buffer hpack-context #f)))
	(make-http2-frame-push-promise flags si d headers)))

   (define-frame-converter +http2-frame-type-ping+
     (ping-converter flags si buffer hpack-context)
     (make-http2-frame-ping flags si (yeild-buffer buffer #f)))

   (define-frame-converter +http2-frame-type-goaway+
     (goaway-converter flags si buffer hpack-context)
     (let* ((buf (frame-buffer-buffer buffer))
	    (size (frame-buffer-size buffer))
	    (sid (bytevector-u32-ref buf 0 (endianness big)))
	    (code (bytevector-u32-ref buf 4 (endianness big))))
       (make-http2-frame-goaway flags si sid code
				(bytevector-copy buf 8 size))))

   (define-frame-converter +http2-frame-type-window-update+
     (window-update-converter flags si buffer hpack-context)
     (let ((buf (frame-buffer-buffer buffer)))
       (make-http2-frame-window-update flags si
	(bytevector-u32-ref buf 0 (endianness big)))))

   (define-frame-converter +http2-frame-type-continuation+
     (continuation-converter flags si buffer hpack-context)
     (let ((buf (frame-buffer-buffer buffer))
	   (size (frame-buffer-size buffer)))
       (make-http2-frame-continuation flags si
	(read-hpack (open-bytevector-input-port buf #f 0 size) hpack-context))))

)
