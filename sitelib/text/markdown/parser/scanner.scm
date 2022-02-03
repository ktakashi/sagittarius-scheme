;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/scanner.scm - Scanner
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (text markdown parser scanner)
    (export scanner? scanner:of
	    scanner:peek scanner:peek-previous
	    scanner:has-next? scanner:next!
	    scanner:next-char? scanner:next-string?
	    scanner:match-char scanner:match-charset
	    scanner:whitespace
	    scanner:find-char scanner:find-charset
	    scanner:position scanner:position!
	    scanner:source

	    position?
	    )
    (import (rnrs)
	    (core misc)
	    (srfi :14 char-sets)
	    (text markdown parser source))

(define-vector-type scanner 
  (make-scanner lines line-index index current-line line-length line-count)
  scanner?
  (lines scanner-lines)
  (line-index scanner-line-index scanner-line-index-set!)
  (index scanner-index scanner-index-set!)
  (current-line scanner-current-line scanner-current-line-set!)
  (line-length scanner-line-length scanner-line-length-set!)
  (line-count scanner-line-count))

(define (scanner:of lines)
  (let* ((v (source-lines->vector lines))
	 (size (vector-length v)))
    (if (zero? size)
	(make-scanner v 0 0 (source-line:of "" #f) 0 0)
	(let ((l (vector-ref v 0)))
	  (make-scanner v 0 0 l (source-line:length l) size)))))

(define (scanner:peek s)
  (let ((index (scanner-index s))
	(line-length (scanner-line-length s)))
    (cond ((< index line-length)
	   (source-line:char-at (scanner-current-line s) index))
	  ((< (scanner-line-index s) (- (scanner-line-count s) 1)) #\newline)
	  (else #f))))

(define (scanner:peek-previous s)
  (let ((index (scanner-index s)))
    (cond ((> index 0)
	   (source-line:char-at (scanner-current-line s) (- index 1)))
	  ((> (scanner-line-index s) 0) #\newline)
	  (else #f))))

(define (scanner:has-next? s)
  (let ((index (scanner-index s))
	(line-length (scanner-line-length s)))
    (or (< index line-length)
	(< (scanner-line-index s) (- (scanner-line-count s) 1)))))

(define (scanner:next! s)
  (let* ((index (scanner-index s))
	 (line-index (scanner-line-index s))
	 (next-index (+ index 1)))
    (if (> next-index (scanner-line-length s))
	(let ((next-line-index (+ (scanner-line-index s) 1)))
	  (scanner-line-index-set! s next-line-index)
	  (if (< next-line-index (scanner-line-count s))
	      (scanner:set-line! s
				 (vector-ref (scanner-lines s) next-line-index))
	      (scanner:set-line! s (source-line:of "" #f)))
	  (scanner-index-set! s 0))
	(scanner-index-set! s next-index))
    ;; for convenience
    #t))

(define (scanner:next-char? s c)
  (and (eqv? (scanner:peek s) c)
       (scanner:next! s)))

(define (scanner:next-string? s str)
  (let ((index (scanner-index s))
	(line-length (scanner-line-length s))
	(len (string-length str)))
    (and (< index line-length)
	 (<= (+ index len) line-length)
	 (source-line:prefix? (scanner-current-line s) str index)
	 (scanner-index-set! s (+ (scanner-index s) len))
	 #t)))

(define (scanner:match-char s c)
  (do ((i 0 (+ i 1)))
      ((not (eqv? (scanner:peek s) c)) i)
    (scanner:next! s)))
(define (scanner:match-charset s c)
  (do ((i 0 (+ i 1)))
      ((not (char-set-contains? c (scanner:peek s))) i)
    (scanner:next! s)))
(define *markdown:whitespace*
  (char-set-intersection char-set:whitespace char-set:ascii))
(define (scanner:whitespace s)
  (scanner:match-charset s *markdown:whitespace*))

(define (scanner:find-char s c)
  (let loop ((i 0))
    (let ((pc (scanner:peek s)))
      (cond ((not pc) -1)
	    ((eqv? pc c) i)
	    (else (scanner:next! s) (loop (+ i 1)))))))
(define (scanner:find-charset s c)
  (let loop ((i 0))
    (let ((pc (scanner:peek s)))
      (cond ((not pc) -1)
	    ((char-set-contains? c pc) i)
	    (else (scanner:next! s) (loop (+ i 1)))))))

(define-vector-type posititon (make-position line-index index) position?
  (line-index position-line-index)
  (index position-index))

(define (scanner:position s)
  (make-position (scanner-line-index s) (scanner-index s)))
(define (scanner:position! s p)
  (define pindex (position-index p))
  (define pline-index (position-line-index p))
  (scanner:check-position s pline-index pindex)
  (scanner-index-set! s pindex)
  (scanner-line-index-set! s pline-index)
  (scanner:set-line! s (vector-ref (scanner-lines s) pline-index)))

(define (scanner:source s begin end)
  (define begin-index (position-index begin))
  (define end-index (position-index end))
  (define begin-line-index (position-line-index begin))
  (define end-line-index (position-line-index end))
  (define lines (scanner-lines s))
  (if (= begin-line-index end-line-index)
      (let* ((line (vector-ref lines begin-line-index))
	     (content (source-line:substring line begin-index end-index))
	     (loc (source-line-location line)))
	(source-lines:of
	 (source-line:of
	  (source-line-content content) ;; to add location
	  (and loc
	       (source-location:of
		(source-location-line loc)
		(+ (source-location-column loc) begin-index)
		(source-line:length content))))))
      (let ((source-lines (source-lines:empty))
	    (first-line (vector-ref lines begin-line-index)))
	(source-lines:add-line! source-lines
				(source-line:substring first-line begin-index))
	(do ((i (+ begin-line-index 1) (+ i 1)))
	    ((= i (- end-line-index 1))
	     (let ((last-line (vector-ref lines end-line-index)))
	       (source-lines:add-line! source-lines
		(source-line:substring last-line 0 end-index))))
	  (source-lines:add-line! source-lines (vector-ref lines i))))))

;; private
(define (scanner:set-line! s line)
  (scanner-current-line-set! s line)
  (scanner-line-length-set! s (source-line:length line)))

(define (scanner:check-position s line-index index)
  (when (or (negative? line-index)
	    (>= line-index (scanner-line-count s)))
    (assertion-violation 'scanner:check-position
			 "Line index out of range"
			 `((line-index ,line-index)
			   (lines ,(scanner-line-count s)))))
  (let ((line (vector-ref (scanner-lines s) line-index)))
    (when (or (negative? index)
	      (> index (source-line:length line)))
      (assertion-violation 'scanner:check-position
			 "Index out of range"
			 `((index ,index)
			   (length ,(source-line:length line)))))))
  
)
    
