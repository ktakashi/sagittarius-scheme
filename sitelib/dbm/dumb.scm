;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; dbm/dumb.scm - A dumb and slow but simple dbm clone
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

;; ported from Python dumbdbm
(library (dbm dumb)
    (export <dumb>)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (dbm)
	    (dbm private)
	    (binary pack)
	    (srfi :26 cut))

  (define-class <dumb-meta> (<dbm-meta>) ())
  (define-class <dumb> (<dbm>) 
    ;; we read/write all data at once...
    ((kv :init-form (make-string-hashtable)))
    :metaclass <dumb-meta>)

  (define-method dbm-open ((self <dumb>))
    (call-next-method)
    ;; it's so dumb so won't check rw-mode ...
    (let ((path (slot-ref self 'path)))
      (when (file-exists? path)
	(call-with-input-file path
	  (cut read-dumbdbm! <> (slot-ref self 'kv))
	  :transcoder #f)))
    self)

  (define-method dbm-close ((self <dumb>))
    (unless (eqv? (slot-ref self 'rw-mode) :read)
      (call-with-port (open-file-output-port (slot-ref self 'path)
					     (file-options no-fail))
	(cut write-dumbdbm <> (slot-ref self 'kv))))
    (slot-set! self 'kv #f))

  (define-method dbm-closed? ((self <dumb>)) (not (slot-ref self 'kv)))

  ;;; accessors
  (define-method dbm-put! ((self <dumb>) key value)
    (hashtable-set! (slot-ref self 'kv) (%dbm-k2s self key)
		    (%dbm-v2s self value)))

  (define-method dbm-get ((self <dumb>) key :optional args)
    (cond ((hashtable-ref (slot-ref self 'kv) (%dbm-k2s self key) #f) =>
	   (lambda (v) (%dbm-s2v self v)))
	  ((not (undefined? args)) args) ;; fallback
	  (else (error 'dbm-get "no data for given key"
		       key (slot-ref self 'path)))))

  (define-method dbm-exists? ((self <dumb>) key)
    (hashtable-ref (slot-ref self 'kv) (%dbm-k2s self key) #f))

  (define-method dbm-delete! ((self <dumb>) key)
    (hashtable-delete! (slot-ref self 'kv) (%dbm-k2s self key)))

  ;;; iterations
  (define-method dbm-fold ((self <dumb>) proc knil)
    (let ((kv (slot-ref self 'kv)))
      (let loop ((keys (hashtable-keys-list kv))
		 (r knil))
	(if (null? keys)
	    r
	    (let* ((k (car keys))
		   (v (hashtable-ref kv k #f)))
	      (loop (cdr keys) 
		    (proc (%dbm-s2k self k) (%dbm-s2v self v) r)))))))

  ;; metaoperations
  (define-method dbm-db-exists? ((class <dumb-meta>) name)
    (file-exists? name))
  (define-method dbm-db-remove ((class <dumb-meta>) name)
    (file-delete name))

  (define-method dbm-db-copy ((class <dumb-meta>) from to . ignore)
    (copy-file from to))

  (define-method dbm-db-move ((class <dumb-meta>) from to . ignore)
    (copy-file from to)
    (file-delete from))

  ;; it's simple pair of length value records
  ;; NOTE
  ;; key-length: 2 byte
  ;; value-length: 4 byte
  (define (read-dumbdbm! in store)
    (let loop ((b (lookahead-u8 in)))
      (unless (eof-object? b)
	;; if something happens then invalid format
	(let* ((kl (get-unpack in "!uS"))
	       (kv (get-bytevector-n kl))
	       (vl (get-unpack in "!uL"))
	       (vv (get-bytevector-n vl)))
	  (hashtable-set! store (utf8->string kv) (utf8->string vv))))))

  (define (write-dumbdbm out store)
    (for-each (lambda (k) 
		(let* ((v (hashtable-ref store k))
		       (kv (string->utf8 k))
		       (vv (string->utf8 v)))
		  (put-bytevector out (pack "!uS" (bytevector-length kv)))
		  (put-bytevector out kv)
		  (put-bytevector out (pack "!uL" (bytevector-length vv)))
		  (put-bytevector out vv)))
	      (hashtable-keys-list store)))
)