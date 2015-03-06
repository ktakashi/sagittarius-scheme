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
	    update-hpack-table-size!
	    ;; read
	    make-hpack-reader
	    read-hpack
	    
	    ;; write
	    make-hpack-writer
	    write-hpack
	    )
    (import (rnrs)
	    (sagittarius) ;; for include
	    (sagittarius control)
	    (compression huffman)
	    (binary io)
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
			 (list (vector-ref e 1) (vector-ref e 2))
			 r))))))

  (define-values (static-encode-kv-table static-encode-v-table)
    (let ((kv (make-equal-hashtable +static-table-size+))
	  ;; generic hashtable is actually heavier
	  (v  (make-equal-hashtable +static-table-size+)))
      (let loop ((i 0))
	(if (= i +static-table-size+)
	    (values kv v)
	    (let ((e (vector-ref +raw-static-table+ i))
		  (i (+ i 1)))
	      (unless (zero? (bytevector-length (vector-ref e 2)))
		(hashtable-set! kv (cons (vector-ref e 1) (vector-ref e 2)) i))
	      ;; use the first one (seems like that on example of spec)
	      (unless (hashtable-contains? v (vector-ref e 1))
		(hashtable-set! v (vector-ref e 1) i))
	      (loop i))))))

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
  (define make-entry list)
  (define entry-name car)
  (define entry-value cadr)
  (define entry-value/attr cdr)
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
  (define (dynamic-table-contains? context name value)
    (let ((table (hpack-context-dynamic-table context)))
      (exists (lambda (e)
		(and (bytevector=? name (entry-name e))
		     (bytevector=? value (entry-value e))))
	      (list-queue-list (dynamic-table-header-fields table)))))
      
      

  (define (lookup-decode-table context index)
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
	      (else (error 'lookup-decode-table
			   "[INTERNAL] invalid static table")))))

  ;; needed for connection
  (define (update-hpack-table-size! context size)
    (let1 table (hpack-context-dynamic-table context)
      (dynamic-table-max-size-set! table new-size)
      (evict-table-entries! table 0)))

  ;; reader is easy
  (define (make-hpack-reader context)
    ;; port must be limited
    (lambda (in)
      (read-hpack in context)))

  (define (read-hpack in context)
    (define (read-name&value in index)
      (let ((name (if (zero? index)
		      (read-hpack-string in)
		      (entry-name (lookup-decode-table context index))))
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
	    (loop (cons (lookup-decode-table context index) r))))
	 ;; 6.2.1 Literal Header Field with Incremental Indexing
	 ((= (bitwise-and b #x40) #x40)
	  (let ((index (read-index in b #x3F)))
	    (let-values (((name value) (read-name&value in index)))
	      (dynamic-table-put! context name value)
	      (loop (acons name (list value) r)))))
	 ;; 6.3 Dynamic Table Size Update
	 ((= (bitwise-and b #x20) #x20)
	  (let ((new-size (read-hpack-integer in b #x1F))
		(table (hpack-context-dynamic-table context)))
	    (when (> new-size (dynamic-table-max-size table))
	      (error 'read-hpack
		     "New dynamic table size is bigger than current size"
		     new-size))
	    (update-hpack-table-size! context new-size)
	    (loop r)))
	 (else
	  (let ((type (bitwise-and b #xF0))
		(index (read-index in b #x0F)))
	    (cond 
	     ;; 6.2.2 Literal Header Field without Indexing
	     ((zero? type) 
	      (let-values (((name value) (read-name&value in index)))
		(let ((e (make-entry name value :no-indexing)))
		  (append-header-list! context e)
		  (loop (acons name (entry-value/attr e) r)))))
	     ;; 6.2.3 Literal Header Field never Indexed
	     ((= type #x10) 
	      ;; for transfering, 
	      ;; TODO handle this properly
	      (let-values (((name value) (read-name&value in index)))
		(let ((e (make-entry name value :never-indexed)))
		  (append-header-list! context e)
		  ;; maybe we need to add never indexed field to the context?
		  (loop (acons name (entry-value/attr e) r)))))
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
      (let loop ((M 0) (B (get-u8 in)) (I prefix))
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
      (if (= (bitwise-and b #x80) #x80)
	  ;; should we do this much? seems kinda overkill...
	  (call-with-bytevector-output-port
	   (lambda (out)
	     (hpack-huffman-decoder (->size-limit-binary-input-port in len)
				    out)))
	  (get-bytevector-n in len))))

  ;; FIXME this *always* takes O(n) of dynamic table entries
  (define (lookup-encode-table context name value)
    (define (lookup-dynamic-table context name value)
      (let ((table (hpack-context-dynamic-table context)))
	(let loop ((i (+ 1 +static-table-size+))
		   (entries (list-queue-list
			     (dynamic-table-header-fields table))))
	  (if (null? entries)
	      #f
	      (let ((e (car entries)))
		(if (and (bytevector=? name (entry-name e))
			 (bytevector=? value (entry-value e)))
		    i
		    (loop (+ i 1) (cdr entries))))))))
    (let ((e (cons name value)))
      ;; dynamic table must be first.
      (cond ((lookup-dynamic-table context name value) =>
	     (lambda (i) (values i 'indexed)))
	    ((hashtable-ref static-encode-kv-table e #f) =>
	     (lambda (i) (values i 'indexed)))
	    ((hashtable-ref static-encode-v-table name #f) =>
	     (lambda (i) (values i 'name)))
	    (else (values #f #f)))))
	     

  (define (make-hpack-writer context)
    (lambda (out hpack)
      (write-hpack out context hpack)))

  ;; writer
  ;; hpack is a list of following structure
  ;;  hpack   ::= (element ...)
  ;;  element ::= (name value attrs ...) | (:table-size-update n)
  ;;  name    ::= bytevector
  ;;  value   ::= bytevector
  ;;  attrs   ::= :no-indexing | :never-indexed | :no-huffman
  (define (write-hpack out context hpack)
    (define (prefix&bits attrs)
      (cond ((memq :no-indexing attrs)   (values #x0 4))
	    ((memq :never-indexed attrs) (values #x1 4))
	    (else                        (values #x40 6)))) ;; literal
    (dolist (e hpack)
      (if (memq :table-size-update e)
	  (write-hpack-integer out (cadr e) #x20 5)
	  (let ((name (car e))
		(value (cadr e))
		(attrs (cddr e)))
	    (let-values (((index indexing-type)
			  (lookup-encode-table context name value)))
	      (cond ((eq? indexing-type 'indexed)
		     (write-hpack-integer out index #x80 7)) ;; 6.1
		    ((eq? indexing-type 'name)
		     (let-values (((prefix bits) (prefix&bits attrs)))
		       (when (and (= prefix #x40)
				  (not (dynamic-table-contains? 
					context name value)))
			 (dynamic-table-put! context name value))
		       (write-hpack-integer out index prefix bits)
		       (write-hpack-string out value (memq :no-huffman attrs))))
		    (else
		     ;; literal
		     (let-values (((prefix bits) (prefix&bits attrs)))
		       (write-hpack-integer out 0 prefix bits)
		       (let ((no-huffman? (memq :no-huffman attrs)))
			 (unless (dynamic-table-contains? context name value)
			   (dynamic-table-put! context name value))
			 (write-hpack-string out name no-huffman?)
			 (write-hpack-string out value no-huffman?))))))))))

  (define (write-hpack-integer out n prefix bits)
    (define (mask n) (- (expt 2 n) 1))
    #|
      if I < 2^N - 1, encode I on N bits
      else
          encode (2^N - 1) on N bits
          I = I - (2^N - 1)
          while I >= 128
               encode (I % 128 + 128) on 8 bits
               I = I / 128
          encode I on 8 bits
    |#
    (define (write-variable-length out n prefix bits)
      (define 2^n-1 (mask bits))
      (put-u8 out (bitwise-ior prefix 2^n-1))
      (do ((I (- n 2^n-1) (div I 128)))
	  ((< I 128)
	   (put-u8 out I))
	(put-u8 out (+ (mod I 128) 128))))
    (if (> (bitwise-length n) bits)
	(write-variable-length out n prefix bits)
	(put-u8 out (bitwise-ior prefix (bitwise-and n (mask bits))))))

  (define (write-hpack-string out value no-huffman?)
    (cond (no-huffman?
	   (write-hpack-integer out (bytevector-length value) #x00 7)
	   (put-bytevector out value))
	  (else
	   (let ((bv (bytevector->hpack-huffman value)))
	     (write-hpack-integer out (bytevector-length bv) #x80 7)
	     (put-bytevector out bv)))))
)
