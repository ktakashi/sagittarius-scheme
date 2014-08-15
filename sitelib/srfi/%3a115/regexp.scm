;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; regexp.scm - SRFI-115 Scheme Regular Expressions
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :115 regexp)
    (export regexp regexp? valid-sre? rx regexp->sre char-set->sre
	    regexp-matches regexp-matches? regexp-search
	    regexp-replace regexp-replace-all
	    regexp-fold regexp-extract regexp-split
	    regexp-match? regexp-match-count
	    regexp-match-submatch
	    regexp-match->list
	    regexp-match-submatch-start regexp-match-submatch-end)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius regex)
	    (text sre)
	    (srfi :14))

  (define regexp? regex-pattern?)
  (define (regexp sre) 
    (if (regexp? sre)
	sre
	(sre->regex sre)))

  (define (valid-sre? sre) 
    (guard (e (else #f)) (sre-parse sre)))

  (define regexp->sre regex->sre)
  (define (char-set->sre cs) (list (char-set->string cset)))

  (define (regexp-matches re str :optional (start 0) (end (string-length str)))
    (let ((m (regex-matcher (regexp re) str start end)))
      (and (regex-matches m) m)))

  (define (regexp-matches? re str :optional (start 0) (end (string-length str)))
    (regex-matches re str start end))

  (define (regexp-search re str :optional (start 0) (end (string-length str)))
    (let ((m (regex-matcher (regexp re) str start end)))
      (and (regex-looking-at m) m)))

  (define (regexp-fold rx kons knil str 
		       :optional (finish default-finish)
		       (start 0) (end (string-length str)))
    (regex-fold (regexp rx) kons knil str finish start end))

  (define (regexp-split rx str 
			:optional (start 0) (end (string-length text)))
    (string-split (regexp rx) str start end))
  
  (define (regexp-extract rx str :optional (start 0) (end (string-length str)))
    (regexp-fold rx 
		 (lambda (from md str a)
		   (let ((s (md 0)))
		     (if (string=? s "") a (cons s a))))
		 '()
		 str 
		 (lambda (from md str a) (reverse! a))
		 start end))
  
  (define (regexp-split rx str :optional (start 0) (end (string-length str)))
    (string-split str (regexp rx) start end))
  
  (define (compile-subst m subst)
    (define (resolve-integer i) (format "$~a" i))
    (define (resolve-symbol sym)
      (case sym
	((pre) "$p")
	((post) "$P")
	(else
	 (cond ((regex-group m sym) => resolve-integer)
	       (else (error 'compile-subst "invalid symbol" sym))))))
    (define (compile subst)
      ;; TODO 'pre and 'post
      (call-with-string-output-port
       (lambda (out)
	 (let loop ((subst subst))
	   (unless (null? subst)
	     (let ((item (car subst)))
	       (cond ((integer? item) (display (resolve-integer item) out))
		     ((string? item) (display item out))
		     ((symbol? item) (display (resolve-symbol item) out))
		     (else 
		      (error 'compile-subst "not supported" item subst)))
	       (loop (cdr subst))))))))
    (cond ((pair? subst) (compile subst))
	  ((integer? subst) (resolve-integer subst))
	  ;; symbol 'pre and 'post
	  ((symbol? subst)  (resolve-symbol subst))
	  (else subst)))
  (define (regexp-replace rx str subst :optional (start 0) (end #f) (count 0))
    (let ((m (regex-matcher (regexp rx) str start 
			    (or end (string-length str)))))
      ;; TODO we need to make subst a bit modified
      (regex-replace m (compile-subst m subst) count)))

  (define (regexp-replace-all rx str subst :optional (start 0) (end #f))
    (let ((m (regex-matcher (regexp rx) str start 
			    (or end (string-length str)))))
      ;; TODO we need to make subst a bit modified
      (regex-replace-all m (compile-subst m subst))))
		  
  (define regexp-match? regex-matcher?)
  (define (regexp-match-count m) (- (regex-capture-count m) 1))
  (define (regexp-match-submatch m i) (regex-group m i))
  ;; for compatibility...
  (define (regexp-match-submatch-start m i) 
    (let ((s (regex-group-start m i))
	  (e (regex-group-end m i)))
      (+ s (if (= s e) 1 0))))
  (define (regexp-match-submatch-end m i)
    (let ((s (regex-group-start m i))
	  (e (regex-group-end m i)))
      (+ e (if (= s e) 1 0))))
  (define (regexp-match->list m)
    (let ((count (regex-capture-count m)))
      (let loop ((i 0) (r '()))
	(if (= i count)
	    (reverse! r)
	    (loop (+ i 1) (cons (regex-group m i) r))))))

)
