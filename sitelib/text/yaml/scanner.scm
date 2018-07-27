;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/scanner.scm - YAML scanner
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; The implementation is based on PyYAML
;; Original Licence
;; Copyright (c) 2006 Kirill Simonov
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

#!nounbound
(library (text yaml scanner)
    (export port->yaml-scanner-lseq
	    &yaml-scanner yaml-scanner-error?
	    yaml-scanner-error-when yaml-scanner-error-mark)
    (import (rnrs)
	    (text yaml conditions)
	    (text yaml tokens)
	    (only (scheme base) vector-copy!)
	    (only (srfi :1 lists) reverse!)
	    (srfi :14 char-sets)
	    (srfi :117 list-queues)
	    (srfi :127 lseqs))
;;  Scanner produces tokens of the following types:
;;  STREAM-START
;;  STREAM-END
;;  DIRECTIVE(name, value)
;;  DOCUMENT-START
;;  DOCUMENT-END
;;  BLOCK-SEQUENCE-START
;;  BLOCK-MAPPING-START
;;  BLOCK-END
;;  FLOW-SEQUENCE-START
;;  FLOW-MAPPING-START
;;  FLOW-SEQUENCE-END
;;  FLOW-MAPPING-END
;;  BLOCK-ENTRY
;;  FLOW-ENTRY
;;  KEY
;;  VALUE
;;  ALIAS(value)
;;  ANCHOR(value)
;;  TAG(value)
;;  SCALAR(value, plain, style)
(define (port->yaml-scanner-lseq in)
  (generator->lseq (port->yaml-scanner-generator in)))

(define (make-simple-key tn r i l c m) (vector tn r i l c m))
(define (simple-key-token-number sk) (vector-ref sk 0))
(define (simple-key-required? sk)    (vector-ref sk 1))
(define (simple-key-position sk)     (vector-ref sk 2))
(define (simple-key-line sk)         (vector-ref sk 3))
(define (simple-key-column sk)       (vector-ref sk 4))
(define (simple-key-mark sk)         (vector-ref sk 5))

;; TODO move
(define-condition-type &yaml-scanner &yaml
  make-yaml-scanner-error yaml-scanner-error?
  (when yaml-scanner-error-when)
  (mark yaml-scanner-error-mark))

(define (scanner-error when msg mark . irr)
  (raise (condition
	  (make-yaml-scanner-error when mark)
	  (make-who-condition 'yaml-scanner)
	  (make-message-condition msg)
	  (make-irritants-condition irr))))

(define +directive-name-set+
  (char-set-intersection
   (char-set-union char-set:letter+digit (char-set #\- #\_))
   char-set:ascii))

(define +break-set+ (string->char-set "\r\n\x85;\x2028;\x2029;"))
(define +white-set+ (char-set-union (string->char-set "\x0; \t") +break-set+))
(define +non-plain-set+
  (char-set-union +white-set+ (string->char-set "-?:,[]{}#&*!|>'\"%@")))
(define +uri-char-set+
  (char-set-intersection
   (char-set-union (string->char-set "-/;?:@&=+$,_.!~*'()[]%^")
		   +directive-name-set+)
   char-set:ascii))

(define (yaml-delimitor? c)
  (or (eof-object? c) (char-set-contains? +white-set+ c)))
(define (yaml-directive-delimitor? c)
  (or (eof-object? c) (eqv? #\space c) (char-set-contains? +break-set+ c)))

(define (port->yaml-scanner-generator in)
  ;; info
  (define line 0)
  (define column 0)
  (define position 0)
  ;; buffer
  (define buffer-length 0)
  (define buffer (make-vector 1024 #\nul))

  (define done? #f)
  (define flow-level 0)
  (define indent -1)
  (define indents (list-queue))

  (define tokens (list-queue))
  (define tokens-taken 0)
  ;;; Variables related to simple keys treatment.
  ;; A simple key is a key that is not denoted by the '?' indicator.
  ;; Example of simple keys:
  ;;   ---
  ;;   block simple key: value
  ;;   ? not a simple key:
  ;;   { flow simple key: value }
  ;; We emit the KEY token before all keys, so when we find a potential
  ;; simple key, we try to locate the corresponding ':' indicator.
  ;; Simple keys should be limited to a single line and 1024 characters.
  
  ;; Can a simple key start at the current position? A simple key may
  ;; start:
  ;; - at the beginning of the line, not counting indentation spaces
  ;;       (in block context),
  ;; - after '{', '[', ',' (in the flow context),
  ;; - after '?', ':', '-' (in the block context).
  ;; In the block context, this flag also signifies if a block
  ;; collection may start at the current position.
  (define allow-simple-key #t)
  ;; Keep track of possible simple keys. This is a dictionary. The key
  ;; is `flow_level'; there can be no more than one possible simple key
  ;; for each level. The value is a SIMPLE-KEY record:
  ;;   (token-number, required, index, line, column, mark)
  ;; A simple key may start with ALIAS, ANCHOR, TAG, SCALAR(flow),
  ;; '[' or '{' tokens.
  (define possible-simple-keys (make-eqv-hashtable))
  
  ;; peek the next i-th character
  (define (peek in . maybe-index)
    (define index (if (null? maybe-index) 0 (car maybe-index)))
    ;; happens very rarely I think
    (when (>= index (vector-length buffer))
      (let ((new-buffer (make-vector (* (vector-length buffer) 2) #\nul)))
	(vector-copy! new-buffer 0 buffer)
	(set! buffer new-buffer)))
    (when (>= index buffer-length)
      (do ((j buffer-length (+ j 1)))
	  ((= j (+ buffer-length 1)) (set! buffer-length j))
	(vector-set! buffer j (get-char in))))
    (vector-ref buffer index))
  (define (prefix in . maybe-length)
    (define l (if (null? maybe-length) 1 (car maybe-length)))
    (do ((i 0 (+ i 1))
	 (r '() (cons (peek in i) r)))
	((or (eof-object? (peek in i)) (= i l)) (list->string (reverse! r)))))
  ;; read the next character and reduce buffer
  (define (consume in)
    (define (update! c)
      (when (char? c)
	(cond ((or (memv c '(#\newline #\x2028 #\x2029))
		   (and (eqv? c #\return)
			(not (eqv? (peek in 1) #\newline))))
	       (set! line (+ line 1))
	       (set! column 0))
	      ((not (eqv? #\xFEFF c))
	       (set! column (+ column 1))))
	(set! position (+ position 1)))
      c)
    (if (positive? buffer-length)
	(let ((c (vector-ref buffer 0)))
	  (set! buffer-length (- buffer-length 1))
	  (vector-copy! buffer 0 buffer 1)
	  (vector-set! buffer buffer-length #\null) ;; not really needed
	  (update! c))
	(update! (get-char in))))
  (define (read in len)
    (do ((i 0 (+ i 1)) (r '() (cons (consume in) r)))
	((= i len) (list->string (reverse! r)))))
  (define (read-while in cset)
    (do ((c (peek in) (peek in i)) (i 0 (+ i 1)))
	((or (eof-object? c) (not (char-set-contains? cset c)))
	 (and (not (zero? i)) (read in (- i 1))))))
  (define (skip in ignore)
    (do ((c (peek in) (peek in)))
	((not (eqv? ignore c)))
      (forward in)))
  (define (skip-comment in)
    (when (eqv? #\# (peek in))
      (do ((c (peek in) (peek in)))
	  ((or (eof-object? c) (char-set-contains? +break-set+ c)))
	(forward in))))
  ;; read the next l characters and discards
  (define (forward in . maybe-l)
    (define l (if (null? maybe-l) 1 (car maybe-l)))
    (do ((i 0 (+ i 1))) ((= i l)) (consume in)))

  (define (get-mark) (make-yaml-scanner-mark in position line column))
  (define (add-token! token) (list-queue-add-back! tokens token))

  ;;; Fetchers
  (define (fetch-directive in)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-directive in)))

  (define (fetch-document-start in)
    (fetch-document-indicator in make-document-start-token))
    
  (define (fetch-document-end in)
    (fetch-document-indicator in make-document-end-token))
  
  (define (fetch-document-indicator in token)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (let ((start-mark (get-mark)))
      (forward 3)
      (let ((end-mark (get-mark)))
	(add-token! (token start-mark end-mark)))))

  (define (fetch-block-entry in)
    (when (zero? flow-level)
      (unless allow-simple-key
	(scanner-error #f "Sequence entries are not allowed here" (get-mark)))
      (when (add-indent! column)
	(let ((mark (get-mark)))
	  (add-token! (make-block-sequence-start-token mark mark)))))
    (set! allow-simple-key #t)
    (remove-possible-simple-key!)
    (let ((start-mark (get-mark)))
      (forward in)
      (let ((end-mark (get-mark)))
	(add-token! (make-block-entry-token start-mark end-mark)))))

  (define (fetch-flow-sequence-start in)
    (fetch-flow-collection-start in make-flow-sequence-start-token))
  
  (define (fetch-flow-mapping-start in)
    (fetch-flow-collection-start in make-flow-mapping-start-token))
  
  (define (fetch-flow-collection-start in token)
    (save-possible-simple-key!)
    (set! flow-level (+ flow-level 1))
    (set! allow-simple-key #t)
    (let ((start-mark (get-mark)))
      (forward in)
      (let ((end-mark (get-mark)))
	(add-token! (token start-mark end-mark)))))
  
  (define (fetch-flow-sequence-end in)
    (fetch-flow-collection-end in make-flow-sequence-end-token))
  
  (define (fetch-flow-mapping-end in)
    (fetch-flow-collection-end in make-flow-mapping-end-token))
  
  (define (fetch-flow-collection-end in token)
    (remove-possible-simple-key!)
    (set! flow-level (- flow-level 1))
    (set! allow-simple-key #f)
    (let ((start-mark (get-mark)))
      (forward in)
      (let ((end-mark (get-mark)))
	(add-token! (token start-mark end-mark)))))

  (define (fetch-flow-entry in)
    (set! allow-simple-key #t)
    (remove-possible-simple-key!)
    (let ((start-mark (get-mark)))
      (forward in)
      (let ((end-mark (get-mark)))
	(add-token! (make-flow-entry-token start-mark end-mark)))))
  
  (define (fetch-stream-end in)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (hashtable-clear! possible-simple-keys)
    (let ((mark (get-mark)))
      (list-queue-add-back! tokens (make-stream-end-token mark mark))
      (set! done? #t)))
  
  (define (fetch-plain in)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-plain in)))
  
  ;;; Checkers
  ;; DIRECTIVE: ^ '%' ...
  ;; The '%' indicator is already checked.
  (define (check-directive? in) (zero? column))
  ;; DOCUMENT-START: ^ '---' (' '|'\n')
  (define (check-document-start? in)
    (and (zero? column)
	 (string=? "---" (prefix in 3))
	 (yaml-delimitor? (peek in 3))))
  ;; DOCUMENT-END: ^ '...' (' '|'\n')
  (define (check-document-end? in)
    (and (zero? column)
	 (string=? "..." (prefix in 3))
	 (yaml-delimitor? (peek in 3))))
  ;; BLOCK-ENTRY: '-' (' '|'\n')
  (define (check-block-entry? in) (yaml-delimitor? (peek in 1)))
  ;; A plain scalar may start with any non-space character except:
  ;;   '-', '?', ':', ',', '[', ']', '{', '}',
  ;;   '#', '&', '*', '!', '|', '>', '\', '\"',
  ;;   '%', '@', '`'.
  ;;
  ;; It may also start with
  ;;   '-', '?', ':'
  ;; if it is followed by a non-space character.
  ;;
  ;; Note that we limit the last rule to the block context (except the
  ;; '-' character) because we want the flow context to be space
  ;; independent.
  (define (check-plain? in)
    (define c (peek in))
    (and (not (eof-object? c))
	 (or (not (char-set-contains? +non-plain-set+ c))
	     (and (not (char-set-contains? +white-set+ c))
		  (or (eqv? #\- c)
		      (and (zero? flow-level)
			   (memv c '(#\: #\?))))))))

    ;;; Scanners
  ;; We ignore spaces, line breaks and comments.
  ;; If we find a line break in the block section, we set the flag
  ;; `allow_simple_key' on.
  (define (scan-to-next-token in)
    (when (and (zero? position) (eqv? (peek in) #\xFEFF)) (forward in))
    (let loop ()
      (skip in #\space)
      (skip-comment in)
      (when (> (string-length (scan-line-break in)) 0)
	(when (zero? flow-level) (set! allow-simple-key #t))
	(loop))))

  (define (scan-line-break in)
    (let ((c (peek in)))
      (case c
	((#\return #\newline #\x85)
	 (if (equal? "\r\n" (prefix in 2))
	     (forward in 2)
	     (forward in))
	 "\n")
	((#\x2028 #\x2029)
	 (forward in)
	 (string c))
	(else ""))))
  ;; See the specification for details.
  (define (scan-directive in)
    (define (scan-value in name)
      (cond ((string=? "YAML" name)
	     (let ((v (scan-yaml-directive-value in)))
	       (values v (get-mark))))
	    ((string=? "TAG" name)
	     (let ((v (scan-tag-directive-value in)))
	       (values v (get-mark))))
	    (else
	     (do ((m (get-mark))
		  (i 0 (+ i 1))
		  (c (peek in) (peek in i)))
		 ((or (eof-object? c) (char-set-contains? +break-set+ c))
		  (values (if (zero? i) #f (read in (- i 1))) m))))))
    (let ((start-mark (get-mark)))
      (forward in)
      (let ((name (scan-directive-name in)))
	(let-values (((value end-mark) (scan-value in name)))
	  (scan-directive-ignored-line in)
	  (make-directive-token start-mark end-mark name value)))))
  (define (scan-directive-ignored-line in)
    (skip in #\space)
    (skip-comment in)
    (let ((c (peek in)))
      (unless (or (eof-object? c) (char-set-contains? +break-set+ c))
	(scanner-error "While scanning a directive"
		       "Expected a comment or a line break"
		       (get-mark)
		       c)))
    (scan-line-break in))
      
  ;; See the specification for details.
  (define (scan-directive-name in)
    (define len
      (do ((i 0 (+ i 1)) (c (peek in) (peek in i)))
	  ((or (eof-object? c)
	       (not (char-set-contains? +directive-name-set+ c)))
	   (when (zero? i)
	     (scanner-error "While scanning a directive name"
			    "Expected alphanumeric characters"
			    (get-mark)
			    (peek in)))
	   (- i 1))))
    (let ((value (read in len)))
      (unless (yaml-directive-delimitor? (peek in))
	(scanner-error "While scanning a directive name"
		       "Expected whitespace"
		       (get-mark)
		       (peek in)))
      value))
  (define (scan-yaml-directive-value in)
    (skip in #\space)
    (let* ((major (scan-yaml-directive-number in))
	   (c (consume in)))
      (unless (eqv? #\. c)
	(scanner-error "While scanning a YAML directive"
		       "Expected digit or '.'"
		       (get-mark)
		       c))
      (let ((minor (scan-yaml-directive-number in)))
	(unless (yaml-directive-delimitor? (peek in))
	  (scanner-error "While scanning a YAML directive"
			 "Expected whitespace"
			 (get-mark)
			 (peek in)))
	(cons major minor))))
  (define (scan-yaml-directive-number in)
    (let ((c (peek in)))
      (unless (and (char? c) (char<=? #\0 c #\9))
	(scanner-error "While scanning a YAML directive"
		       "Expected digit"
		       (get-mark)
		       c)))
    (let ((len (do ((c (peek in) (peek in i)) (i 0 (+ i 1)))
		   ((or (eof-object? c) (not (char<=? #\0 c #\9))) (- i 1)))))
      (string->number (read in len))))

  (define (scan-tag-directive-value in)
    (skip in #\space)
    (let ((handle (scan-tag-directive-handle in)))
      (skip in #\space)
      (cons handle (scan-tag-directive-prefix in))))
  (define (scan-tag-directive-handle in)
    (let ((handle (scan-tag-handle in "directive")))
      (unless (eqv? #\space (peek in))
	(scanner-error "While scanning a TAG handle"
		       "Expected ' '"
		       (get-mark)
		       (peek in)))
      handle))
  (define (scan-tag-directive-prefix in)
    (let ((prefix (scan-tag-uri in "directive")))
      (unless (yaml-directive-delimitor? (peek in))
	(scanner-error "While scanning a TAG prefix"
		       "Expected whitespace"
		       (get-mark)
		       (peek in)))
      prefix))
  ;; See the specification for details.
  ;; For some strange reason, the specification does not allow '_' in
  ;; tag handles. I have allowed it anyway.
  (define (scan-tag-handle in name)
    (unless (eqv? #\! (peek in))
      (scanner-error (string-append "While scanning a " name)
		     "Expected '!'"
		     (get-mark)
		     (peek in)))
    (do ((i 1 (+ i 1)) (c (peek in 1) (peek in i)))
	((or (eof-object? c)
	     (not (char-set-contains? +directive-name-set+ c)))
	 (cond ((= i 1)
		;; handle secondary tag handle.
		(if (eqv? c #\!)
		    (read in 2)
		    (read in 1)))
	       ((not (eqv? #\! (peek in (- i 1))))
		(scanner-error (string-append "While scanning a " name)
			       "Expected '!'"
			       (get-mark)
			       (peek in i)))
	       (else (read in i))))))
  ;; we don't check if the uri is well-formed or not
  (define (scan-tag-uri in name)
    (define (unescape-uri uri)
      (define len (string-length uri))
      (define (err)
	(scanner-error (string-append "While scanning a " name)
		       "Invalid URI escape"
		       (get-mark)
		       uri))
      (let-values (((out extract) (open-string-output-port)))
	;; we use string-ref since it's O(1)
	(let loop ((i 0))
	  (if (= i len)
	      (extract)
	      (let ((c (string-ref uri i)))
		(case c
		  ((#\%)
		   (when (> (+ i 3) len) (err))
		   (let ((n (string->number
			     (substring uri (+ i 1) (+ i 3)) 16)))
		     (unless (number? n) (err))
		     (put-char out (integer->char n)))
		   (loop (+ i 3)))
		  (else (put-char out c) (loop (+ i 1)))))))))
    
    (let ((escaped-uri (read-while in +uri-char-set+)))
      (unless escaped-uri
	(scanner-error (string-append "While scanning a " name)
		       "Expected URI"
		       (get-mark)))
      (unescape-uri escaped-uri)))
  ;; See the specification for details.
  ;; We add an additional restriction for the flow context:
  ;;   plain scalars in the flow context cannot contain ',' ':' '?'.
  ;; We also keep track of the `allow-simple-key' flag here.
  (define (scan-plain in)
    (define (find-ch in len)
      (let ((ch (peek in len)))
	(if (or (eof-object? ch)
		(char-set-contains? +white-set+ ch)
		(and (zero? flow-level)
		     (eqv? ch #\:)
		     (char-set-contains? +white-set+ (peek (+ len 1))))
		(and (not (zero? flow-level))
		     (memv ch '(#\, #\: #\? #\[ #\] #\{ #\}))))
	    (values len ch)
	    (find-ch in (+ len 1)))))
    (define (finish chunks start-mark end-mark)
      (make-scalar-token chunks #t start-mark end-mark))
    (define start-mark (get-mark))
    (define current-indent (+ indent 1))
    (let-values (((chunks extract) (open-string-output-port)))
      (let loop ((start-mark start-mark)
		 (end-mark start-mark)
		 (spaces ""))
	(if (eqv? #\# (peek in))
	    (finish (extract) start-mark end-mark)
	    (let-values (((len ch) (find-ch in 0)))
	      (cond ((and (not (zero? flow-level)) (eqv? ch #\:)
			  (char? (peek in (+ len 1)))
			  (char-set-contains? +white-set+ (peek in (+ len 1))))
		     (forward len)
		     (scanner-error "While scanning a plain scalar"
				    "Found unexpected ':'"
				    (get-mark)))
		    ((zero? len) (finish (extract) start-mark end-mark))
		    (else
		     (set! allow-simple-key #f)
		     (put-string chunks spaces)
		     (do ((i 0 (+ i 1)))
			 ((= i len))
		       (put-char chunks (consume in)))
		     (let ((end-mark (get-mark))
			   (spaces (scan-plain-spaces in current-indent
						      start-mark)))
		       (if (or (zero? (string-length spaces))
			       (eqv? #\# (peek in))
			       (and (zero? flow-level)
				    (< column current-indent)))
			   (finish (extract) start-mark end-mark)
			   (loop start-mark end-mark spaces))))))))))

  (define (scan-plain-spaces in indent start-mark)
    ;; See the specification for details.
    ;; The specification is really confusing about tabs in plain scalars.
    ;; We just forbid them completely. Do not use tabs in YAML!
    (define (finish whitespaces line-break breaks)
      (let-values (((out extract) (open-string-output-port)))
	(cond (whitespaces (put-string out whitespaces))
	      ((equal? "\n" line-break) (put-string out line-break))
	      ((and line-break (null? breaks)) (put-char out #\space))
	      (line-break (for-each (lambda (lb) (put-string out lb)) breaks)))
	(extract)))
    (define (get-line-breaks in line-break prefix)
      (define (should-finish? prefix in)
	(and (or (string=? "---" prefix) (string=? "..." prefix))
	     (char-set-contains? +white-set+ (peek in 3))))
      (if (should-finish? prefix in)
	  (values #t '())
	  (let loop ((c (peek in))
		     (breaks '()))
	    (if (or (eqv? #\space c) (char-set-contains? +break-set+ c))
		(if (eqv? c #\space)
		    (begin
		      (forward in)
		      (loop (peek in) breaks))
		    (let* ((line-break (scan-line-break in))
			   (prefix (prefix in 3)))
		      (if (should-finish? prefix in)
			  (values #t '())
			  (loop (peek in) (cons line-break breaks)))))
		(values #f breaks)))))
    (define len (let loop ((i 0))
		  (let ((c (peek in i)))
		    (if (or (eof-object? c) (eqv? c #\space))
			i
			(loop (+ i 1))))))
    (let* ((whitespaces (read in len))
	   (ch (peek in)))
      (if (and (char? ch) (char-set-contains? +break-set+ ch))
	  (let ((line-break (scan-line-break in))
		(prefix (prefix 3)))
	    (set! allow-simple-key #t)
	    (let-values (((finish? breaks)
			  (get-line-breaks in line-break prefix)))
	      (if finish?
		  ""
		  (finish #f line-break breaks))))
	  (finish whitespaces #f '()))))
  

  ;;; Indentation functions
  ;; In the flow context, indentation is ignored. We make the scanner
  ;; less restrictive than specification requires.
  (define (unwind-indent! column)
    (when (zero? flow-level)
      (do ()
	  ((<= indent column))
	(let ((mark (get-mark)))
	  (set! indent (list-queue-remove-front! indents))
	  (add-token! (make-block-end-token mark mark))))))
  (define (add-indent! column)
    ;; Check if we need to increase indentation.
    (and (< indent column)
	 (begin
	   (list-queue-add-back! indents column)
	   (set! indent column)
	   #t)))
  ;;; Simple keys treatment
  ;; Return the number of the nearest possible simple key. Actually we
  ;; don't need to loop through the whole dictionary.
  (define (next-possible-simple-key)
    (define (next current level)
      (let ((key (hashtable-ref possible-simple-keys level #f)))
	(if (< (simple-key-token-number key) current)
	    (simple-key-token-number key)
	    current)))
    (do ((levels (hashtable-keys possible-simple-keys))
	 (min-token-number +inf.0 (next min-token-number (vector-ref levels i)))
	 (i 0 (+ i 1)))
	((= i (vector-length levels)) min-token-number)))
  ;; Remove entries that are no longer possible simple keys. According to
  ;; the YAML specification, simple keys
  ;; - should be limited to a single line,
  ;; - should be no longer than 1024 characters.
  ;; Disabling this procedure will allow simple keys of any length and
  ;; height (may cause problems if indentation is broken though).
  (define (stale-posible-simple-keys)
    (do ((levels (hashtable-keys possible-simple-keys))
	 (i 0 (+ i 1)))
	((= i (vector-length levels)))
      (let* ((level (vector-ref levels i))
	     (key (hashtable-ref possible-simple-keys level #f)))
	(when (or (= (simple-key-line key) line)
		  (> (- (simple-key-position key) position) 1024))
	  (when (simple-key-required? key)
	    (scanner-error "While scanning a simple key"
			   "Could not find expected ':'"
			   (get-mark)))
	  (hashtable-delete! possible-simple-keys level)))))

  ;; The next token may start a simple key. We check if it's possible
  ;; and save its position. This function is called for
  ;;   ALIAS, ANCHOR, TAG, SCALAR(flow), '[', and '{'.
  (define (save-possible-simple-key!)
    (define required? (and (zero? flow-level) (= indent column)))
    (unless (or allow-simple-key (not required?))
      (scanner-error #f "Required simple key not allowed" (get-mark)))
    (when allow-simple-key
      (remove-possible-simple-key!)
      (let ((token-number (+ tokens-taken (length (list-queue-list tokens)))))
	(hashtable-set! possible-simple-keys flow-level
			(make-simple-key token-number required?
					 position line column (get-mark))))))
  ;; Remove the saved possible key position at the current flow level
  (define (remove-possible-simple-key!)
    (cond ((hashtable-ref possible-simple-keys flow-level #f) =>
	   (lambda (key)
	     (when (simple-key-required? key)
	       (scanner-error "While scanning a simple key"
			      "Could not find expected ':'"
			      (get-mark)))
	     (hashtable-delete! possible-simple-keys flow-level)))))

  (define char-table
    `(
      (#\% . ((,check-directive? . ,fetch-directive)))
      (#\- . ((,check-document-start? . ,fetch-document-start)
	      (,check-block-entry? . ,fetch-block-entry)))
      (#\. . ((,check-document-end? . ,fetch-document-end)))
      (#\[ . ,fetch-flow-sequence-start)
      (#\{ . ,fetch-flow-mapping-start)
      (#\] . ,fetch-flow-sequence-end)
      (#\} . ,fetch-flow-mapping-end)
      (#\, . ,fetch-flow-entry)
      ))
  (define (fetch-more-tokens in)
    (define (check-ch? ch)
      (cond ((assv ch char-table) =>
	     (lambda (slot)
	       (let ((t (cdr slot)))
		 (if (procedure? t)
		     t
		     (let loop ((t t))
		       (and (not (null? t))
			    (if ((caar t) in)
				(cdar t)
				(loop (cdr t)))))))))
	    (else #f)))
    (scan-to-next-token in)
    (stale-posible-simple-keys)
    (unwind-indent! column)
    (let ((c (peek in)))
      (cond ((or (eof-object? c) (eqv? #\nul c))
	     (fetch-stream-end in))
	    ((check-ch? c) => (lambda (p) (p in)))
	    ((check-plain? c) (fetch-plain in))
	    (else (scanner-error
		   "While scanning for the next token"
		   "Found a character that cannot start any token"
		   (get-mark)
		   c)))))

  (define (need-more-tokens?)
    (cond (done? #f)
	  ((list-queue-empty? tokens))
	  (else (stale-posible-simple-keys)
		(= (next-possible-simple-key) tokens-taken))))
  
  (define (get-token in)
    (when (need-more-tokens?) (fetch-more-tokens in))
    (set! tokens-taken (+ tokens-taken))
    (if (list-queue-empty? tokens)
	(eof-object)
	(list-queue-remove-front! tokens)))
  (lambda () (get-token in)))
    
)