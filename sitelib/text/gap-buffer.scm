;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/gap-buffer.scm - GAP buffer
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

;; ported from the implementation of Hsin Tsao.
;; Original copying is here:
;; 
;;  *  Author: Hsin Tsao (stsao@lazyhacker.com)
;;  *  Version: 1.0 (June 12, 2003)
;;  *
;;  *  A text buffer class using the buffer-gap technique for managing
;;  *  the text stored in the buffer.
;;  *
;;  *  Portions of this work derived from Joseph Allen's usenet
;;  *  postings on comp.editor that was released to the public
;;  *  domain.
;;  *
;;  *
;;  *  There are no restrictions on the use of this code other
;;  *  than to include my name in any derived work.  There are
;;  *  no warranty for this obviously, but you are welcomed
;;  *  to modify, correct, or adapt the code to your needs.  The
;;  *  author appreciates if you are willing to submit corrections
;;  *  or suggestions for improvement.
;;  *  
;;  *
;;  *  http://www.lazyhacker.com


;; We hold the content as bytevector instead of vector so that
;; binary editting can also be possible.
(library (text gap-buffer)
    (export <gap-buffer> make-gap-buffer gap-buffer?
	    gap-buffer-copy
	    file->gap-buffer
	    string->gap-buffer

	    ;; TODO should we export raw procedures?
	    ;; API names are taken from Gauche
	    gap-buffer-gap-start
	    gap-buffer-gap-end
	    gap-buffer-capacity
	    gap-buffer-content-length
	    gap-buffer-ref	  ; gap-buffer-raw-ref
	    gap-buffer-insert!		; gap-buffer-raw-insert!
	    gap-buffer-move!
	    gap-buffer-delete!
	    gap-buffer-change!		; gap-buffer-raw-change!
	    ;; these doesn't have raw procedure
	    gap-buffer-clear!
	    gap-buffer-copy

	    gap-buffer->string		; gap-buffer->bytevector
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (srfi :43)) ;; for vector-copy!

(define-constant +default-gap-delta+ 8)
(define (get-delta n) (expt 2 (bitwise-length n)))

(define-record-type (<gap-buffer> make-gap-buffer gap-buffer?)
  (fields (immutable gap-delta gb-gap-delta) ;; internal
	  (mutable point gb-point gb-point-set!)
	  (mutable buffer gb-buffer gb-buffer-set!)
	  (mutable gap-start gap-buffer-gap-start gb-gap-start-set!)
	  (mutable gap-end gap-buffer-gap-end gb-gap-end-set!))
  (protocol (lambda (p)
	      (lambda (:optional (initial-capacity +default-gap-delta+)
				 (gap-delta +default-gap-delta+))
		;; make sure we have 2^2n buffer size
		(let ((size (get-delta initial-capacity)))
		  (p gap-delta 
		     0
		     (make-vector size)
		     0
		     size))))))

;; TODO should we separate binary and text?
(define (file->gap-buffer file
			  :optional (pos 0) (whence 'end)
				    (transcoder (native-transcoder))
				    (gap-delta +default-gap-delta+))
  (let-values (((get conv) (if transcoder 
			       (values get-char char->integer)
			       (values get-u8 values))))
    (let* ((size (file-size-in-bytes file))
	   (gb (make-gap-buffer size gap-delta))
	   (in (open-file-input-port file (file-options no-fail)
				     (buffer-mode buffer) transcoder))
	   (buf (gb-buffer gb)))
      (let loop ((u (get in)) (i 0))
	(cond ((eof-object? u) 
	       (gb-gap-start-set! gb i)
	       (gb-point-set! gb i))
	      (else
	       (vector-set! buf i (conv u))
	       (loop (get in) (+ i 1)))))
      (close-port in)
      (gap-buffer-move! gb pos whence))))

;; API signature is taken from Gauche
(define (string->gap-buffer s 
			    :optional (pos 0) (whence 'end)
				      (start 0) end
				      (gap-delta +default-gap-delta+))
  (let* ((end (if (undefined? end) (string-length s) end))
	 (size (- end start))
	 (gb (make-gap-buffer size gap-delta))
	 (buf (gb-buffer gb)))
    (let loop ((i 0))
      (unless (= i size)
	(vector-set! buf i (char->integer (string-ref s (+ i start))))
	(loop (+ i 1))))
    (gb-gap-start-set! gb size)
    (gb-point-set! gb size)
    (gap-buffer-move! gb pos whence)))

(define (gap-buffer-capacity gb) (vector-length (gb-buffer gb)))

(define (gap-buffer-content-length gb)
  (let* ((bv (gb-buffer gb))
	 (len (vector-length bv))
	 (start (gap-buffer-gap-start gb))
	 (end (gap-buffer-gap-end gb)))
    (- len (- end start))))

(define-syntax define-ref
  (syntax-rules ()
    ((_ name conv)
     (define (name gb index :optional fallback)
       (define (oob)
	 (if (undefined? fallback)
	     (error 'gap-buffer-raw-ref "index out of range" index)
	     fallback))
       (define (ret i) (conv (vector-ref (gb-buffer gb) i)))
       (cond ((< index 0) (oob))
	     ((< index (gap-buffer-gap-start gb))
	      (ret index))
	     ((< index (gap-buffer-content-length gb))
	      (ret (+ index (gap-buffer-gap-size gb))))
	     (else (oob)))))))

(define-ref gap-buffer-raw-ref values)
(define-ref gap-buffer-ref     integer->char)


(define-syntax define-insert!
  (syntax-rules ()
    ((_ name pred ctr conv length ref)
     (define (name gb content)
       (if (pred content)
	   (let ((p (gb-point gb))
		 (s (gap-buffer-gap-start gb))
		 (e (gap-buffer-gap-end gb))
		 (l (length content)))
	     (cond ((not (= p s)) (name (%move-gap-to-point! gb) content))
		   ((> l (- e s))
		    (name (%expand-gap! gb (length content)) content))
		   (else
		    ;; insert it
		    (let ((buf (gb-buffer gb))
			  (len (length content)))
		      (let loop ((i 0))
			(unless (= i len)
			  (vector-set! buf (+ i s) (conv (ref content i)))
			  (loop (+ i 1))))
		      (gb-gap-start-set! gb (+ s len))
		      (gb-point-set! gb (+ s len))
		      (%move-gap-to-point! gb)))))
	   (name gb (ctr content)))))))
(define (bytevector . args)
  (let* ((len (length args))
	 (bv (make-bytevector len 0)))
    (let loop ((i 0) (args args))
      (if (= i len)
	  bv
	  (let ((u8 (car args)))
	    (bytevector-u8-set! bv i u8)
	    (loop (+ i 1) (cdr args)))))))
(define-insert! gap-buffer-raw-insert! bytevector? bytevector values
  bytevector-length bytevector-u8-ref)
(define-insert! gap-buffer-insert! string? string char->integer string-length
  string-ref)

(define (gap-buffer-move! gb offset :optional (whence 'beginning))
  (let ((p (case whence
	     ((beginning) offset)
	     ((current) (+ (gap-buffer-gap-start gb) offset))
	     ((end) (+ offset (gap-buffer-content-length gb)))
	     (else (assertion-violation 'gap-buffer-raw-move!
		    "whence must be one of 'beginning 'current or 'end" 
		    whence))))
	(s (gap-buffer-gap-start gb)))
    (gb-point-set! gb (if (> p s)
			  ;; check if the offset overflows or not
			  ;; if we don't do this here, then the error
			  ;; would be raised when actual buffer operation
			  ;; is done. and it makes my life miserable.
			  ;; TODO should we allow this by expanding buffer?
			  (let* ((e (gap-buffer-gap-end gb))
				 (r (+ p (- e s))))
			    (when (> r (vector-length (gb-buffer gb)))
			      (error 'gap-buffer-move! "invalid offset" 
				     offset whence))
			    r)
			  p))
    (%move-gap-to-point! gb)))

;; delete
(define (gap-buffer-delete! gb size)
  (unless (= (gb-point gb) (gap-buffer-gap-start gb))
    (%move-gap-to-point! gb))
  (gb-gap-end-set! gb (+ (gap-buffer-gap-end gb) size))
  gb)

;; change! delete size and insert content
(define (gap-buffer-raw-change! gb size content)
  (gap-buffer-delete! gb size)
  (gap-buffer-raw-insert! gb content))
(define (gap-buffer-change! gb size content)
  (gap-buffer-delete! gb size)
  (gap-buffer-insert! gb content))

;; clear
(define (gap-buffer-clear! gb) 
  (gb-gap-start-set! gb 0) 
  (gb-gap-end-set! gb (vector-length (gb-buffer gb))))

;; copy
;; we do rather awkwardly
(define (gap-buffer-copy gb)
  (let* ((buf (gb-buffer gb))
	 (len (vector-length buf))
	 (delta (gb-gap-delta gb))
	 (r (make-gap-buffer len delta)))
    (vector-copy! buf 0 (gb-buffer r) 0 len)
    (gb-point-set! r (gb-point gb))
    (gb-gap-start-set! r (gap-buffer-gap-start gb))
    (gb-gap-end-set! r (gap-buffer-gap-end gb))
    r))

;; converter
;; TODO do we want ->generator like Gauche?
(define (gap-buffer->string gb :optional (start 0) (end +inf.0))
  (let ((count (- (min (gap-buffer-content-length gb) end) start)))
    (when (< count 0) 
      (assertion-violation 'gap-buffer->string "start is too large" start))
    (let-values (((out extract) (open-string-output-port)))
      (let loop ((index start))
	(cond ((= index count) (extract))
	      (else
	       (put-char out (gap-buffer-ref gb index))
	       (loop (+ index 1))))))))

;; should we expose this?
(define (gap-buffer-gap-size gb)
  (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))

;; internal
;; These internal procedure doesn't consider size of content (e.g. char = 4)
;; just uses byte
(define (%expand-gap! gb size)
  (when (> size (gap-buffer-gap-size gb))
    (let* ((n (+ size (gb-gap-delta gb)))
	   (obuf (vector-length (gb-buffer gb)))
	   (buf (gb-buffer (%expand-gap-buffer! gb n)))
	   (e (gap-buffer-gap-end gb))
	   (s (gap-buffer-gap-start gb)))
      ;; gap might be huge...
      (let loop ((i (- (vector-length buf) 1)) (j (- obuf 1)))
	(if (< j e)
	    (gb-gap-end-set! gb (+ i 1))
	    (begin
	      (vector-set! buf i (vector-ref buf j))
	      (loop (- i 1) (- j 1)))))))
  gb)
      
(define (%expand-gap-buffer! gb size)
  (let* ((bv (gb-buffer gb))
	 (len (vector-length bv)))
    (when (> (+ len size) (gap-buffer-content-length gb))
      (let* ((new-size (+ len size))
	     (new-buffer (make-vector new-size)))
	(vector-copy! new-buffer 0 bv 0 len)
	(gb-buffer-set! gb new-buffer)))
    gb))

(define (%move-gap-to-point! gb)
  (let ((p (gb-point gb))
	(s (gap-buffer-gap-start gb))
	(e (gap-buffer-gap-end gb)))
    (cond ((= p s))
	  ((= p e) (gb-point-set! gb s))
	  ((< p s)
	   ;; move gap towards the left
	   (let ((buf (gb-buffer gb)))
	     (vector-copy! buf (+ p (- e s)) buf p s)
	     (gb-gap-start-set! gb p)
	     (gb-gap-end-set! gb (- e (- s p)))))
	  (else
	   (let ((buf (gb-buffer gb)))
	     (vector-copy! buf s buf e p)
	     (gb-gap-start-set! gb (+ s (- p e)))
	     (gb-gap-end-set! gb p)
	     (gb-point-set! gb (gap-buffer-gap-start gb)))))
    gb))
)
	    
	    
