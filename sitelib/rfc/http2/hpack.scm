;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http2/hpack - HPACK
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

(library (rfc http2 hpack)
    (export hpack-huffman-encoder
	    hpack-huffman-decoder
	    ;; for convenience
	    hpack-huffman->bytevector
	    bytevector->hpack-huffman

	    ;; hpack context
	    make-hpack-context
	    ;; read
	    make-hpack-reader
	    read-hpack
	    )
    (import (rnrs)
	    (sagittarius) ;; for include
	    (compression huffman)
	    (util bytevector)
	    ;; for lightweight queue (I don't think we need deque)
	    (srfi :117 list-queues))

  (define-constant +code-table+ (include "hpack-code-table.scm"))
  (define-constant +raw-static-table+ (include "hpack-static-table.scm"))
  ;; need to compute offset
  (define-constant +static-table-size+ (vector-length +raw-static-table+))

  (define static-decode-table
    (let loop ((i 0) (r '()))
      (if (= i +static-table-size+)
	  r
	  (let ((e (vector-ref +raw-static-table+ i)))
	    (loop (+ i 1)
		  (acons (vector-ref e 0)
			 (cons (vector-ref e 1) (vector-ref e 2))
			 r))))))

  (define hpack-huffman-encoder (make-huffman-encoder +code-table+))
  (define hpack-huffman-decoder (make-huffman-decoder +code-table+))

  (define (hpack-huffman->bytevector hpack)
    (call-with-bytevector-output-port
     (lambda (out)
       (hpack-huffman-decoder (open-bytevector-input-port hpack) out))))

  (define (bytevector->hpack-huffman bv)
    (call-with-bytevector-output-port
     (lambda (out)
       (hpack-huffman-encoder (open-bytevector-input-port bv) out))))

  ;; kinda lazy to type ctr, pred and accessor
  (define-record-type dynamic-table
    (fields (mutable header-fields)
	    (mutable max-size)
	    (mutable current-size))
    (protocol (lambda (p)
		(lambda (max-size)
		  (let ((fields (list-queue)))
		    (p fields max-size 0))))))
  (define make-entry cons)
  (define entry-name car)
  (define entry-value cdr)
  (define (compute-entry-size e)
    (let ((n-size (bytevector-length (entry-name e)))
	  (v-size (bytevector-length (entry-value e))))
      (+ n-size v-size 32)))

  (define (evict-table-entries! table new-entry-size)
    (let ((max-size (dynamic-table-max-size table))
	  (fields (dynamic-table-header-fields table)))
      (let loop ((size (dynamic-table-current-size table)))
	(if (zero? size)
	    (dynamic-table-current-size-set! table 0)
	    (let* ((e (list-queue-remove-back! fields))
		   (new-size (- size (compute-entry-size e))))
	      (if (>= max-size (+ new-size new-entry-size))
		  (dynamic-table-current-size-set! table new-size)
		  (loop new-size)))))))
  (define (dynamic-entry-put! table e)
    (let ((max-size (dynamic-table-max-size table))
	  (current-size (dynamic-table-current-size table))
	  (entry-size (compute-entry-size e))
	  (fields (dynamic-table-header-fields table)))
      (when (> (+ current-size entry-size) max-size)
	(evict-table-entries! table entry-size))
      ;; may reduced
      (let ((current-size (dynamic-table-current-size table)))
	(list-queue-add-front! fields e)
	;; TODO what should we do if one entry is bigger than
	;;      max size?
	(dynamic-table-current-size-set! table (+ current-size entry-size)))))

  ;; hpack context
  ;; we need this to manage dynamic table
  (define-record-type hpack-context
    (fields dynamic-table
	    header-list)
    (protocol (lambda (p)
		(lambda (max-size)
		  (let ((table (make-dynamic-table max-size))
			(queue (list-queue)))
		    (p table queue))))))
  (define (append-header-list! context e)
    (let ((header-list (hpack-context-header-list context)))
      (list-queue-add-back! header-list e)))
  (define (dynamic-table-put! context name value)
    (let ((e (make-entry name value))
	  (table (hpack-context-dynamic-table context)))
      (dynamic-entry-put! table e)
      (append-header-list! context e)))

  (define (lookup-table context index)
    (if (> index +static-table-size+)
	;; TODO this takes O(n) make it O(1) somehow
	(let ((table (hpack-context-dynamic-table context))
	      (dynamic-index (- index +static-table-size+ 1)))
	  (let loop ((i 0) (entries (list-queue-list 
				     (dynamic-table-header-fields table))))
	    ;; (format #t "~a:~a~%" i (utf8->string (caar entries)))
	    (if (= i dynamic-index)
		(car entries)
		(loop (+ i 1) (cdr entries)))))
	(cond ((assv index static-decode-table) => cdr)
	      (else (error 'lookup-table "[INTERNAL] invalid static table")))))

  ;; reader is easy
  (define (make-hpack-reader context)
    ;; port must be limited
    (lambda (in)
      (read-hpack in context)))

  (define (read-hpack in context)
    (define (read-name&value in index)
      (let ((name (if (zero? index)
		      (read-hpack-string in)
		      (entry-name (lookup-table context index))))
	    (value (read-hpack-string in)))
	(values name value)))
    (define read-index read-hpack-integer)
    (let loop ((r '()))
      (let ((b (get-u8 in)))
	(cond 
	 ((eof-object? b) (reverse! r))
	 ;; 6.1 Indexed Header Field Representation
	 ((= (bitwise-and b #x80) #x80) 
	  (let ((index (read-index in b #x7F)))
	    (loop (cons (lookup-table context index) r))))
	 ;; 6.2.1 Literal Header Field with Incremental Indexing
	 ((= (bitwise-and b #x40) #x40)
	  (let ((index (read-index in b #x3F)))
	    (let-values (((name value) (read-name&value in index)))
	      (dynamic-table-put! context name value)
	      (loop (acons name value r)))))
	 ;; 6.3 Dynamic Table Size Update
	 ((= (bitwise-and b #x20) #x20)
	  (let ((new-size (read-hpack-integer in b #x1F))
		(table (hpack-context-dynamic-table context)))
	    (dynamic-table-max-size-set! table new-size)
	    (evict-table-entries! table 0)
	    (loop r)))
	 (else
	  (let ((type (bitwise-and b #xF0))
		(index (read-index in b #x0F)))
	    (cond 
	     ;; 6.2.2 Literal Header Field without Indexing
	     ((zero? type) 
	      (let-values (((name value) (read-name&value in index)))
		(let ((e (make-entry name value)))
		  (append-header-list! context e)
		  (loop (acons name value r)))))
	     ;; 6.2.3 Literal Header Field never Indexed
	     ((= type #x10) 
	      ;; for transfering, 
	      ;; TODO handle this properly
	      (let-values (((name value) (read-name&value in index)))
		(let ((e (make-entry name value)))
		  (append-header-list! context e)
		  ;; maybe we need to add never indexed field to the context?
		  (loop (acons name value r)))))
	     (else
	      (error 'read-hpack
		     "unknown HPACK type" b)))))))))

  (define (read-hpack-integer in first-byte prefix)
    #|
      decode I from the next N bits
      if I < 2^N - 1, return I
      else
          M = 0
          repeat
              B = next octet
              I = I + (B & 127) * 2^M
              M = M + 7
          while B & 128 == 128
          return I
    |#
    (define (read-variable-integer in)
      (let loop ((M 0) (B (get-u8 in)) (I 0))
	(let ((i (+ I (* (bitwise-and B 127) (expt 2 M)))))
	  (if (= (bitwise-and B 128) 128)
	      (loop (+ M 7) (get-u8 in) i)
	      i))))
    (let ((v (bitwise-and first-byte prefix)))
      (if (= v prefix)
	  ;; read variable length
	  (read-variable-integer in)
	  v)))

  (define (read-hpack-string in)
    (let* ((b (get-u8 in))
	   (len (read-hpack-integer in b #x7F)))
      (let ((bv (get-bytevector-n in len)))
	(if (= (bitwise-and b #x80) #x80)
	    (hpack-huffman->bytevector bv)
	    bv))))
)
