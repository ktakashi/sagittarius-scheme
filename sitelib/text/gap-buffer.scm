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
	    ;; binary-file->gap-buffer ;; need to ponder
	    text-file->gap-buffer
	    string->gap-buffer

	    ;; TODO should we export raw procedures?
	    ;; API names are taken from Gauche
	    gap-buffer-content-length  ; gap-buffer-raw-content-length
	    gap-buffer-ref	  ; gap-buffer-raw-ref
	    gap-buffer-insert!		; gap-buffer-raw-insert!
	    gap-buffer-move!		; gap-buffer-raw-move!
	    gap-buffer-delete!		; gap-buffer-raw-delete!
	    gap-buffer-change!		; gap-buffer-raw-change!
	    ;; these doesn't have raw procedure
	    gap-buffer-clear!
	    gap-buffer-copy

	    gap-buffer->string		; gap-buffer->bytevector
	    )
    (import (rnrs)
	    (sagittarius))

(define-constant +default-gap-delta+ 8)
(define (get-delta n) (expt 4 (- (bitwise-length n) 1)))

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
		;; NB: creating bytevector is represents u32 thus
		;;     size is multiple of 4
		(let ((size (get-delta initial-capacity)))
		  (p gap-delta 
		     0
		     (make-bytevector size)
		     0
		     size))))))

;; a bit of waste of memory. but for now it's ok
(define (binary-file->gap-buffer file :optional (gap-delta +default-gap-delta+))
  (let* ((size (file-size-in-bytes file))
	(gb (make-gap-buffer size gap-delta)))
    (let ((in (open-file-input-port file)))
      (gb-gap-start-set! gb (get-bytevector-n! in (gb-buffer gb) 0 size))
      (gb-point-set! gb (gap-buffer-gap-start gb))
      (close-port in))
    gb))

;; TODO this isn't a good implementation.
(define (text-file->gap-buffer file :optional (transcoder (native-transcoder))
			       (gap-delta +default-gap-delta+))
  (let ((s (call-with-input-file file get-string-all :transcoder transcoder)))
    (string->gap-buffer s gap-delta)))

;; again a bit of memory waste
;; NB: we put string/character as UTF-32 big
(define (string->gap-buffer s :optional (gap-delta +default-gap-delta+))
  ;; this is less memory
  (let* ((size (string-length s))
	 (gb (make-gap-buffer (* size 4) gap-delta))
	 (buf (gb-buffer gb)))
    (let loop ((i 0))
      (unless (= i size)
	(bytevector-u32-set! buf (* i 4) (char->integer (string-ref s i)) 'big)
	(loop (+ i 1))))
    (gb-gap-start-set! gb (* size 4))
    (gb-point-set! gb (gap-buffer-gap-start gb))
    gb)
  #;
  (let* ((bv (string->utf32 s (endianness big)))
	 (size (bytevector-length bv))
	 (gb (make-gap-buffer size gap-delta)))
    (bytevector-copy! bv 0 (gb-buffer gb) 0 size)
    (gb-gap-start-set! gb size)
    gb))

(define (gap-buffer-raw-content-length gb)
  (let* ((bv (gb-buffer gb))
	 (len (bytevector-length bv))
	 (start (gap-buffer-gap-start gb))
	 (end (gap-buffer-gap-end gb)))
    (- len (- end start))))

(define-syntax define-ref
  (syntax-rules ()
    ((_ name ref offset)
     (define (name gb index :optional fallback)
       (define (oob)
	 (if (undefined? fallback)
	     (error 'gap-buffer-raw-ref "index out of range" index)
	     fallback))
       (define (ret i) (ref (gb-buffer gb) i))
       (cond ((< index 0) (oob))
	     ((< index (/ (gap-buffer-gap-start gb) offset))
	      (ret (* index offset)))
	     ((< index (/ (gap-buffer-raw-content-length gb) offset))
	      ;; the ref procedure should handle offset
	      (ret (* (+ index 
			 (- (/ (gap-buffer-gap-end gb) offset)
			    (/ (gap-buffer-gap-start gb) offset)))
		      offset)))
	     (else (oob)))))))

(define-ref gap-buffer-raw-ref bytevector-u8-ref 1)
(define-ref gap-buffer-ref (lambda (b i) 
			     (integer->char 
			      (bytevector-u32-ref b i (endianness big)))) 4)

(define (gap-buffer-content-length gb) (/ (gap-buffer-raw-content-length gb) 4))

;; content must be a bytevector
;; TODO maybe we should allow u8 when we expose raw procedures
(define (gap-buffer-raw-insert! gb content)
  (let ((p (gb-point gb))
	(s (gap-buffer-gap-start gb))
	(e (gap-buffer-gap-end gb)))
    (cond ((not (= p s))
	   (gap-buffer-raw-insert! (%move-gap-to-point! gb) content))
	  ((= s e) 
	   (gap-buffer-raw-insert! (%expand-gap! gb (bytevector-length content))
				   content))
	  (else
	   ;; insert it
	   (let ((buf (gb-buffer gb))
		 (len (bytevector-length content)))
	     (bytevector-copy! content 0 buf s len)
	     (gb-gap-start-set! gb (+ s len)))))))

;; content can be char/string
;; the content will be UTF32 big
(define (gap-buffer-insert! gb content)
  (if (string? content)
      (gap-buffer-raw-insert! gb (string->utf32 content (endianness big)))
      ;; assume character
      (gap-buffer-insert! gb (string content))))

(define (gap-buffer-raw-move! gb offset :optional (whence 'beginning))
  (let ((p (case whence
	     ((beginning) offset)
	     ;; is current point?
	     ((current) (+ (gb-point gb) offset))
	     ((end) (+ offset (gap-buffer-raw-content-length gb)))
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
			    (when (> r (bytevector-length (gb-buffer gb)))
			      (error 'gap-buffer-raw-move! "invalid offset" 
				     offset whence))
			    r)
			  p))))

(define (gap-buffer-move! gb offset :optional (whence 'beginning))
  (gap-buffer-raw-move! gb (* offset 4) whence))

;; delete
(define (gap-buffer-raw-delete! gb size)
  (unless (= (gb-point gb) (gap-buffer-gap-start gb))
    (%move-gap-to-point! gb))
  (gb-gap-end-set! gb (+ (gap-buffer-gap-end gb) size)))
(define (gap-buffer-delete! gb size) (gap-buffer-raw-delete! gb (* size 4)))

;; change! delete size and insert content
(define (gap-buffer-raw-change! gb size content)
  (gap-buffer-raw-delete! gb size)
  (gap-buffer-raw-insert! gb content))
(define (gap-buffer-change! gb size content)
  (if (string? content)
      (gap-buffer-raw-change! gb size (string->utf32 content (endianness big)))
      (gap-buffer-change! gb size (string content))))

;; clear
(define (gap-buffer-clear! gb) 
  (gb-gap-start-set! gb 0) 
  (gb-gap-end-set! gb (bytevector-length (gb-buffer gb))))

;; copy
;; we do rather awkwardly
(define (gap-buffer-copy gb)
  (let* ((buf (gb-buffer gb))
	 (len (bytevector-length buf))
	 (delta (gb-gap-delta gb))
	 (r (make-gap-buffer len delta)))
    (bytevector-copy! buf 0 (gb-buffer r) 0 len)
    (gb-point-set! r (gb-point gb))
    (gb-gap-start-set! r (gap-buffer-gap-start gb))
    (gb-gap-end-set! r (gap-buffer-gap-end gb))
    r))

;; converter
;; TODO do we want ->generator like Gauche?
(define (gap-buffer->string gb :optional (start 0) (end +inf.0))
  (let ((count (- (min (gap-buffer-content-length gb) end) start))
	(gap-start (gap-buffer-gap-start gb))
	(gap-skip (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb))))
    (when (< count 0) 
      (assertion-violation 'gap-buffer->string "start is too large" start))
    (let-values (((out extract) (open-string-output-port)))
      (let loop ((index start))
	(cond ((>= index count) (extract))
	      ((< index gap-start)
	       (put-char out (gap-buffer-ref gb index))
	       (loop (+ index 1)))
	      (else (loop (+ index gap-skip))))))))

;; should we expose this?
(define (gap-buffer-gap-size gb)
  (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))

;; internal
;; These internal procedure doesn't consider size of content (e.g. char = 4)
;; just uses byte
(define (%expand-gap! gb size)
  (when (> size (gap-buffer-gap-size gb))
    (let* ((n (+ size (gb-gap-delta gb)))
	   (buf (gb-buffer (%expand-gap-buffer! gb n)))
	   (e (gap-buffer-gap-end gb)))
      (bytevector-copy! buf (+ e n) buf e (- (bytevector-length buf) e))
      (gb-gap-end-set! gb (+ e n))))
  gb)
      
(define (%expand-gap-buffer! gb size)
  (let* ((bv (gb-buffer gb))
	 (len (bytevector-length bv)))
    (when (> (+ len size) (gap-buffer-raw-content-length gb))
      (let* ((new-size (get-delta (+ len size (gb-gap-delta gb))))
	     (new-buffer (make-bytevector new-size 0)))
	(bytevector-copy! new-buffer 0 bv 0 len)
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
	     (bytevector-copy! buf p buf (+ p (- e s)) (- s p))
	     (gb-gap-start-set! gb p)
	     (gb-gap-end-set! gb (- e (- s p)))))
	  (else
	   (let ((buf (gb-buffer gb)))
	     (bytevector-copy! buf e buf s (- p e))
	     (gb-gap-start-set! gb (+ s (- p e)))
	     (gb-gap-end-set! gb p)
	     (gb-point-set! gb (gap-buffer-gap-start gb)))))
    gb))
)
	    
	    
