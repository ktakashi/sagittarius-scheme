;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; util/buffer.scm - Buffer utilities
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

(library (util buffer)
    (export <pre-allocated-buffer> pre-allocated-buffer?
	    pre-allocated-buffer-buffer
	    pre-allocated-buffer-size
	    pre-allocated-buffer-reset!
	    ;; condition
	    &pre-allocated-buffer-overflow
	    pre-allocated-buffer-overflow?
	    pre-allocated-buffer-overflow-data
	    ;; type specific
	    make-binary-pre-allocated-buffer binary-pre-allocated-buffer?
	    binary-pre-allocated-buffer-put-u8!
	    binary-pre-allocated-buffer-put-u16!
	    binary-pre-allocated-buffer-put-u32!
	    binary-pre-allocated-buffer-put-u64!
	    binary-pre-allocated-buffer-put-s8!
	    binary-pre-allocated-buffer-put-s16!
	    binary-pre-allocated-buffer-put-s32!
	    binary-pre-allocated-buffer-put-s64!
	    binary-pre-allocated-buffer-put-f32!
	    binary-pre-allocated-buffer-put-f64!
	    binary-pre-allocated-buffer-put-bytevector!
	    binary-pre-allocated-buffer-can-store?
	    binary-pre-allocated-buffer-swap!
	    binary-pre-allocated-buffer-get-bytevector-n!
	    crop-binary-buffer
	    ->binary-pre-allocated-buffer-output-port)
    (import (rnrs))

  ;; super class
  (define-record-type (<pre-allocated-buffer> %dummy pre-allocated-buffer?)
    (fields (mutable buffer pre-allocated-buffer-buffer
		     pre-allocated-buffer-buffer-set!)
	    ;; buffer size (not buffer itself but read size)
	    (mutable size  pre-allocated-buffer-size
		     pre-allocated-buffer-size-set!)))
  (define (pre-allocated-buffer-reset! buffer)
    (pre-allocated-buffer-size-set! buffer 0))

  ;; condition
  (define-condition-type &pre-allocated-buffer-overflow &error
    make-pre-allocated-buffer-overflow pre-allocated-buffer-overflow?
    (data pre-allocated-buffer-overflow-data))
  (define (pre-allocated-buffer-overflow who data)
    (raise 
     (apply condition (list (make-pre-allocated-buffer-overflow data)
			    (make-who-condition who)
			    (make-message-condition "frame buffer overflow")))))

  ;; we can also make textual-pre-allocated-buffer
  ;; but for now only binary. (I don't see much needs for that)
  (define-record-type binary-pre-allocated-buffer
    (parent <pre-allocated-buffer>)
    (sealed #t)
    (protocol
     (lambda (p)
       (lambda (buf)
	 (unless (bytevector? buf)
	   (assertion-violation 'make-binary-pre-allocated-buffer
				"bytevector required" buf))
	 ((p) buf 0)))))
  (define (binary-pre-allocated-buffer-can-store? buffer count
					      :optional (offset #f))
    (let ((size (if offset offset (pre-allocated-buffer-size buffer))))
      (<= (+ size count)
	  (bytevector-length (pre-allocated-buffer-buffer buffer)))))
  
  (define (crop-binary-buffer buffer)
    (let ((buf (pre-allocated-buffer-buffer buffer))
	  (size (pre-allocated-buffer-size buffer)))
      (bytevector-copy buf 0 size)))

  (define (binary-pre-allocated-buffer-swap! buffer new-buf size)
    (pre-allocated-buffer-buffer-set! bufer new-buf)
    (pre-allocated-buffer-size-set! bufer size))

  ;; internal
  (define (update-size! binary-buffer size)
    (pre-allocated-buffer-size-set! binary-buffer size))
  ;; 
  (define (binary-pre-allocated-buffer-put-bytevector! binary-buffer bv 
	   :optional (start 0) (count (bytevector-length bv)) (offset #f))
    (define buffer (pre-allocated-buffer-buffer binary-buffer))
    (define buffer-size (pre-allocated-buffer-size binary-buffer))

    (unless (binary-pre-allocated-buffer-can-store? binary-buffer count offset)
      (pre-allocated-buffer-overflow 'binary-pre-allocated-buffer-put-bytevector!
				     bv))
    (let ((pos (if offset offset buffer-size)))
      (bytevector-copy! bv start buffer pos count)
      (update-size! binary-buffer (+ pos count))))

  (define (binary-pre-allocated-buffer-put-u8! binary-buffer u8 
					       :optional (offset #f))
    (define buffer (pre-allocated-buffer-buffer binary-buffer))
    (define buffer-size (pre-allocated-buffer-size binary-buffer))

    (unless (binary-pre-allocated-buffer-can-store? binary-buffer 1 offset)
      (pre-allocated-buffer-overflow 'binary-pre-allocated-buffer-put-u8!
				     u8))
    (let ((pos (if offset offset buffer-size)))
      (bytevector-u8-set! buffer pos u8)
      (update-size! binary-buffer (+ pos 1))))
  (define (binary-pre-allocated-buffer-put-s8! binary-buffer s8
					   :optional (offset #f))
    (define buffer (pre-allocated-buffer-buffer binary-buffer))
    (define buffer-size (pre-allocated-buffer-size binary-buffer))
    (unless (binary-pre-allocated-buffer-can-store? binary-buffer 1 offset)
      (pre-allocated-buffer-overflow 'binary-pre-allocated-buffer-put-s8!
				     s8))
    (let ((pos (if offset offset buffer-size)))
      (bytevector-s8-set! buffer pos s8)
      (update-size! binary-buffer (+ pos 1))))

  (define-syntax define-put!
    (syntax-rules ()
      ((_ name setter size)
       (define (name binary-buffer v endian :optional (offset #f))
	 (define buffer (pre-allocated-buffer-buffer binary-buffer))
	 (define buffer-size (pre-allocated-buffer-size binary-buffer))

	 (unless (binary-pre-allocated-buffer-can-store? binary-buffer size offset)
	   (pre-allocated-buffer-overflow 'name v))
	 (let ((pos (if offset offset buffer-size)))
	   (setter buffer pos v endian)
	   (update-size! binary-buffer (+ pos size)))))))
  (define-put! binary-pre-allocated-buffer-put-u16! bytevector-u16-set! 2)
  (define-put! binary-pre-allocated-buffer-put-u32! bytevector-u32-set! 4)
  (define-put! binary-pre-allocated-buffer-put-u64! bytevector-u64-set! 8)
  (define-put! binary-pre-allocated-buffer-put-s16! bytevector-s16-set! 2)
  (define-put! binary-pre-allocated-buffer-put-s32! bytevector-s32-set! 4)
  (define-put! binary-pre-allocated-buffer-put-s64! bytevector-s64-set! 8)
  (define-put! binary-pre-allocated-buffer-put-f32! bytevector-ieee-single-set! 4)
  (define-put! binary-pre-allocated-buffer-put-f64! bytevector-ieee-double-set! 8)

  (define (binary-pre-allocated-buffer-get-bytevector-n! buffer in n
							 :optional (offset #f))
    (let ((pos (if offset offset (pre-allocated-buffer-size buffer)))
	  (buf (pre-allocated-buffer-buffer buffer)))
      (get-bytevector-n! in buf pos n)
      (update-size! buffer (+ pos n))))

  ;; port conversion
  (define (->binary-pre-allocated-buffer-output-port binary-buffer)
    (define (write! bv start count)
      (binary-pre-allocated-buffer-put-bytevector! binary-buffer bv start count)
      count)
    (define (position) (binary-pre-allocated-buffer-size binary-buffer))
    (define (set-position! pos)
      (define (buffer-size) (binary-pre-allocated-buffer-size binary-buffer))
      (unless (<= 0 pos (buffer-size))
	(raise (condition (make-i/o-invalid-position-error pos)
			  (make-who-condition 'binary-pre-allocated-buffer-port)
			  (make-message-condition "invalid position"))))
      (update-size! binary-buffer pos))
    (define (close!) #t)
    (make-custom-binary-output-port "binary-pre-allocated-buffer-port"
				    write! position set-position! close!))

)
