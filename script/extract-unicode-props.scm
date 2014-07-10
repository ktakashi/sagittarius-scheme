#!/bin/sh
#| -*- scheme -*-
exec sash $0 "$@"
|#

;; Originally from Chibi Scheme modified for Sagittarius.

;; Simple tool to extract Unicode properties as character-sets.
;;
;; Usage:
;;   extract-unicode-props.scm Lowercase Punctuation=P Blank=Zs,0009 >out
;;
;; Accepts a list of char-set names with optional definitions as
;; arguments, and writes their Scheme definitions to stdout.  A
;; char-set can be of the form:
;;
;;   Name: equivalent to Name=Name
;;   Name=value,...:
;;
;; A value can be any of:
;;
;;   Property_Name: all unicode characters with the given derived property
;;   Xx: all unicode characters with the given general category
;;   X: all unicode characters with any general category X*
;;   NNNN: a single unicode value in hex format
;;   NNNN-NNNN: an inclusive range of unicode values in hex format
;;
;; The char-set names generated are always lowercased, replacing _
;; with -, for convenicence when the char-set name coincides with a
;; Unicode property name.
;;
;; Assumes the files UnicodeData.txt and DerivedCoreProperties.txt are
;; in the unicode/data/ current directory, unless overridden with the
;; --data or --derived options.


#!read-macro=sagittarius/regex
(import (rnrs)
	(rnrs eval)
	(only (scheme base) string-map)
	(srfi :13) ;; for string trim
	(srfi :26)
	(getopt) 
	(sagittarius)
	(sagittarius regex)
	(pp))

(define (warn . args)
  (let ((err (current-error-port)))
    (for-each (lambda (x) (display x err)) args)
    (newline err)))

;; Parse UnicodeData.txt for characters matching a given class.
(define (extract-char-set-category cat data)
  (define (join-to-range n ls)
    (cond ((null? ls)
	   (list n))
	  ((eqv? (car ls) (- n 1))
	   (cons (cons (car ls) n) (cdr ls)))
	  ((and (pair? (car ls)) (eqv? (- n 1) (cdar ls)))
	   (cons (cons (caar ls) n) (cdr ls)))
	  (else
	   (cons n ls))))
  (call-with-input-file data
    (lambda (in)
      (let lp ((ranges '()))
        (let ((line (get-line in)))
          (cond
           ((eof-object? line)
            `(char-set-union
              ,@(map (lambda (x)
                       (if (pair? x)
                           `(ucs-range->char-set ,(car x) ,(+ 1 (cdr x)))
                           `(char-set ,(integer->char x))))
                     (reverse ranges))))
           ((or (string=? line "") (eqv? #\# (string-ref line 0)))
            (lp ranges))
           (else
            (let ((ls (string-split line #\;)))
              (cond
               ((< (length ls) 3)
                (warn "invalid UnicodeData line: " line)
                (lp ranges))
               (else
                (let ((ch (string->number (car ls) 16))
                      (name (cadr ls))
                      (ch-cat (car (cddr ls))))
                  (cond
                   ((or (not ch) (not (= 2 (string-length ch-cat))))
                    (warn "invalid UnicodeData line: " line))
                   ((if (char? cat)
                        (eqv? cat (string-ref ch-cat 0))
                        (equal? cat ch-cat))
                    (lp (join-to-range ch ranges)))
                   (else
                    (lp ranges))))))))))))))

;; Parse DerivedCoreProperties.txt for characters matching a given
;; property.
(define (extract-char-set-property prop derived)
  (define (string-trim-comment str comment-ch)
    (car (string-split str comment-ch)))
  (call-with-input-file derived
    (lambda (in)
      (let lp ((ranges '()))
        (let ((line (get-line in)))
          (cond
           ((eof-object? line)
            `(char-set-union ,@(reverse ranges)))
           ((or (string=? line "") (eqv? #\# (string-ref line 0))) (lp ranges))
           (else
            (let ((ls (string-split (string-trim-comment line #\#) #\;)))
              (cond
               ((< (length ls) 2)
                (warn "invalid DerivedCoreProperties line: " line)
                (lp ranges))
               ((string-ci=? prop (string-trim-both (cadr ls)))
                (cond
                 ((string-contains (car ls) "..") =>
		  (lambda (i)
		    (let* ((str (string-trim (car ls)))
			   (start (string->number (string-copy str 0 i) 16))
			   (end (string->number 
				 (string-trim-both (string-copy str (+ i 2)))
				 16)))
		      (if (and start end (<= 0 start end #x110000))
			  (lp (cons `(ucs-range->char-set ,start ,(+ end 1))
				    ranges))
			  (error 'extract-char-set-property
				 "invalid char range: " line)))))
                 ((string->number (cadr ls) 16) =>
		  (lambda (n)
		    (lp (cons `(char-set ,(integer->char n)) ranges))))
                 (else
                  (lp ranges))))
               (else
                (lp ranges)))))))))))

(define (extract-char-set-simple def data derived)
  (let ((ls (string-split def #\-)))
    (cond
     ((= 2 (length ls))
      (let ((start (string->number (car ls) 16))
            (end (string->number (cadr ls) 16)))
        (if (and start end (<= start end))
            `(ucs-range->char-set ,start ,(+ end 1))
            (error 'extract-char-set-simple
		   "invalid character range, expected NNNN-MMMM, got: " def))))
     ((string->number def 16)
      => (lambda (start) `(char-set ,(integer->char start))))
     ((and (= 1 (string-length def))
           (char-upper-case? (string-ref def 0)))
      (extract-char-set-category (string-ref def 0) data))
     ((and (= 2 (string-length def))
           (char-upper-case? (string-ref def 0))
           (char-lower-case? (string-ref def 1)))
      (extract-char-set-category def data))
     ;; derived properties
     ((and (> (string-length def) 1)
           (eqv? #\: (string-ref def 0)))
      (extract-char-set-property (string-copy def 1) derived))
     (else
      (extract-char-set-property def derived)))))

(define (extract-char-set def data derived)
  (let ((defs (string-split def #\,)))
    (cond
     ((= 1 (length defs))
      (extract-char-set-simple (car defs) data derived))
     (else
      `(char-set-union
        ,@(map (lambda (def) (extract-char-set-simple def data derived))
               defs))))))

(define (process-char-set name def data derived)
  (define (normalize-char-set-name str)
    (string-append
     (if (eqv? #\: (string-ref str 0)) "char-set" "char-set:")
     (string-map (lambda (ch) (if (eqv? ch #\_) #\- (char-downcase ch))) str)))
  (let ((name (string->symbol (normalize-char-set-name name))))
    (values name `(define ,name ,(extract-char-set def data derived)))))

(define (main args)
  (define (write-options out)
    (display ";; " out)
    (for-each (cut format out "~a " <>) args)
    (newline out))
  (define (finish lib defs out)
    (define (simplify def)
      (let ((name (cadr def))
	    ;; now we construct the charset here
	    (set  (eval (caddr def) (environment '(srfi :14)))))
	`(define ,name
	   (let ((base (char-set)))
	     ,@(map (lambda (range)
		      `(%char-set-add-range! base ,(car range) ,(cdr range)))
		    (%char-set-ranges set))))))
    (define (write-library defs)
      ;; to be able to handle this without srfi-14 we need to use
      ;; builtin procedures. so modify it a bit.
      (let ((names (map car defs))
	    (defs (map simplify (map cdr defs))))
	(pp
	 `(library ,(read (open-string-input-port lib))
	      (export ,@names)
	      (import (core) (sagittarius))
	    ,@defs)
	 out)))
    ;; write options
    (write-options out)
    (if lib
	(write-library defs)
	(for-each (cut pp <> out) (map cdr defs)))
    (close-output-port out))
  (with-args (cdr args)
      ((data (#\d "data") #t "unicode/data/UnicodeData.txt")
       (derived (#\e "derived") #t "data/DerivedCoreProperties.txt")
       (out  (#\o "output") #t (current-output-port))
       (lib  (#\l "library") #t #f)
       . rest)
    (let ((out (if (output-port? out) 
		   out
		   (open-file-output-port out (file-options no-fail)
					  (buffer-mode block)
					  (native-transcoder)))))
      (let loop ((ls rest) (r '()))
	(define (process name def) (process-char-set name def data derived))
	(if (null? ls)
	    (finish lib (reverse! r) out)
	    (let ((ls2 (string-split (car ls) #\=)))
	      (let ((name (car ls2))
		    (def  (if (null? (cdr ls2)) (car ls2) (cadr ls2))))
		(let-values (((name def) (process name def)))
		  (loop (cdr ls) (acons name def r))))))))))