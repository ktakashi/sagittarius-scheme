;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; binary/pack.aux.scm - helpers for pack library
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

(library (binary pack)
    (export pack pack! format-size
	    define-packer)
    (import (rnrs)
	    (sagittarius)
	    (srfi :13)
	    (prefix (binary pack-aux) aux:))

  ;; FIXME this is not very nice looking
  (define-syntax define-packer
    (syntax-rules (lambda)
      ((_ base e (lambda (v) body ...))
       (aux:add-extension base e (lambda (v) body ...)))))

  (define-syntax format-size
    (lambda (x)
      (syntax-case x ()
	((_ fmt vals ...)
	 (string? (syntax->datum #'fmt))
	 (apply aux:format-size (syntax->datum #'fmt) #'(vals ...)))
	((_ fmt vals ...)
	 #'(aux:format-size fmt vals ...))
	(var (identifier? #'var) #'aux:format-size))))

  (define-syntax pack!*
    (lambda (x)
      (define (drop vals n fmt)
	(cond ((zero? n) vals)
	      ((null? (syntax->datum vals))
	       (syntax-violation #f "Too few values for the format" fmt))
	      (else
	       (with-syntax (((val1 vals ...) vals))
		 (drop #'(vals ...) (- n 1) fmt)))))
      (define (zeroers start end)
	;; Return code which sets the bytes between start and end to
	;; zero.
	(cond ((and (integer? start) (integer? end))
	       (do ((i start (+ i 1))
		    (setters '() (cons #`(bytevector-u8-set! bv #,i 0)
				       setters)))
		   ((= i end) setters)))
	      ((eq? start end) '())
	      (else
	       (list #`(bytevector-fill! bv 0 #,start #,end)))))
      ;; add -native right before -set!
      (define (->nset set)
	(let* ((s (symbol->string set))
	       (i (string-index-right s #\-)))
	  (string->symbol
	   (string-append
	    (substring s 0 i)
	    "-native"
	    (string-copy s i)))))
      (define (get-setters fmt* offset vals)
	(let* ((fmt (syntax->datum fmt*))
	       (len (string-length fmt)))
	  (let lp ((i 0)
		   ;; If the offset is an integer, then the
		   ;; offsets for all fields can be computed
		   ;; directly.
		   (o (if (integer? (syntax->datum offset))
			  (syntax->datum offset)
			  #'off))
		   (rep #f)
		   (indefinite #f)
		   (endian #f)
		   (align #t)
		   (setters '())
		   (vals vals))
	    (cond ((= i len)
		   (unless (null? (syntax->datum vals))
		     (syntax-violation 'pack!
				       "Too many values for the format" fmt*))
		   (reverse! setters))
		  ((char-whitespace? (string-ref fmt i))
		   (lp (+ i 1) o rep indefinite endian align setters vals))
		  (else
		   (case (string-ref fmt i)
		     ;; repeat
		     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		      => (lambda (c)
			   (when indefinite
			     (error 'pack! "'*' and digit can't cooporate" fmt))
			   (lp (+ i 1) o
			       (+ (digit-value c) (* (if rep rep 0) 10)) #f
			       endian align setters vals)))
		     ((#\*)
		      (when rep
			(error 'pack! "'*' and digit can't cooporate" fmt))
		      (lp (+ i 1) o #f #t endian align setters vals))
		     ;; native endian
		     ((#\=) 
		      (lp (+ i 1) o #f #f #f align setters vals))
		     ;; little endian
		     ((#\<)
		      (lp (+ i 1) o #f #f #'(endianness little)
			  align setters vals))
		     ;; big endian
		     ((#\> #\!)
		      (lp (+ i 1) o #f #f #'(endianness big)
			  align setters vals))
		     ;; padding 0
		     ((#\x)
		      (when indefinite
			(error 'pack!
			       "indefinite padding is not supported" fmt))
		      (lp (+ i 1) (aux:add o (or rep 1)) #f #f endian align
			  (append (zeroers o (aux:add o (or rep 1))) setters)
			  vals))
		     ;; align
		     ((#\a)
		      (lp (+ i 1) o rep indefinite endian #t setters vals))
		     ((#\u)
		      (lp (+ i 1) o rep indefinite endian #f setters vals))
		     (else => 
		      (lambda (c)
			(let*-values (((set n conv) (aux:lookup-name c #t))
				      ((startoff) 
				       (if align (aux:roundb o n) o)))
			  (define (parse-vals vals)
			    (if conv
				(syntax-case vals ()
				  (() '())
				  ((a . d)
				   #`(((aux:lookup-converter #,c) a) . d)))
				vals))
			  (define (definite-rep rep)
			    (lp (+ i 1) (aux:add startoff (* n rep))
				#f #f
				endian align
				(let lp* ((o* startoff) (rep rep)
					  (vals vals)
					  (setters (append 
						    (zeroers o startoff)
						    setters)))
				  (if (zero? rep)
				      setters
				      (begin
					(when (null? (syntax->datum vals))
					  (syntax-violation 
					   'pack!
					   "Too few values for the format" 
					   #'fmt*))
					(with-syntax ((foff o*)
						      ((val1 vals ...) 
						       (parse-vals vals)))
					  (lp* (aux:add o* n) (- rep 1)
					       #'(vals ...)
					       (cons (cond
						      ((= n 1)
						       #`(#,set bv foff val1))
						      (endian
						       #`(#,set bv foff val1
								#,endian))
						      ((not align)
						       #`(#,set 
							  bv foff val1
							  (endianness native)))
						      (else
						       (let ((nset (->nset set)))
							 #`(#,nset bv foff val1))))
						     setters))))))
				(drop vals rep #'fmt*)))
			  (define (indefinite-rep)
			    (when (> (string-length fmt) (+ i 1))
			      (error 'pack! "'*' must be the last position" fmt))
			    (let lp* ((o* startoff) (vals vals)
				      (setters (append (zeroers o startoff)
						       setters)))
			      (if (null? vals)
				  (reverse! setters)
				  (with-syntax ((foff o*)
						((val1 vals ...)
						 (parse-vals vals)))
				    (lp* (aux:add o* n)
					 #'(vals ...)
					 (cons (cond
						((= n 1)
						 #`(#,set bv foff val1))
						(endian
						 #`(#,set bv foff val1
							  #,endian))
						((not align)
						 #`(#,set 
						    bv foff val1
						    (endianness native)))
						(else
						 (let ((nset (->nset set)))
						   #`(#,nset bv foff val1))))
					       setters))))))
			  (unless set 
			    (error 'pack! "Bad character in format string"
				   fmt c))
			  (if indefinite
			      (indefinite-rep)
			      (definite-rep (or rep 1))))))))))))
      (syntax-case x ()
	((_ fmt* bytevector offset vals ...)
	 (with-syntax (((setters ...) (get-setters #'fmt*
						   #'offset #'(vals ...))))
	   #'(let ((bv bytevector)
		   (off offset))
	       setters ...
	       bv))))))

  (define (pack!** fmt bv offset . vals)
    (define (zero! i n)
      (do ((i i (+ i 1))
	   (m (+ i n)))
	  ((= i m))
	(bytevector-u8-set! bv i 0)))
    (let lp ((i 0) (o offset) 
	     (rep #f) (indefinite #f)
	     (endian (native-endianness))
	     (align #t)
	     (vals vals))
      (cond ((= i (string-length fmt))
	     (unless (null? vals)
	       (error 'pack! "Too many values for the format" fmt))
	     bv)
	    ((char-whitespace? (string-ref fmt i))
	     ;; skip
	     (lp (+ i 1) o rep indefinite endian align vals))
	    (else
	     (case (string-ref fmt i)
	       ;; repeat
	       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		=> (lambda (c)
		     (when indefinite
		       (error 'pack! "'*' and digit can't cooporate" fmt))
		     (lp (+ i 1) o
			 (+ (digit-value c) (* (if rep rep 0) 10)) #f
			 endian align vals)))
	       ((#\*)
		(when rep
		  (error 'pack! "'*' and digit can't cooporate" fmt))
		(lp (+ i 1) o #f #t endian align vals))
	       ;; native endian
	       ((#\=) (lp (+ i 1) o #f #f (native-endianness) align vals))
	       ;; little endian
	       ((#\<) (lp (+ i 1) o #f #f (endianness little) align vals))
	       ;; big endian
	       ((#\> #\!) (lp (+ i 1) o #f #f (endianness big) align vals))
	       ;; padding 0
	       ((#\x)
		(when indefinite
		  (error 'pack! "indefinite padding is not supported" fmt))
		(zero! o (or rep 1))
		(lp (+ i 1) (+ o (or rep 1)) #f #f endian align vals))
	       ;; align
	       ((#\a) (lp (+ i 1) o rep indefinite endian #t vals))
	       ((#\u) (lp (+ i 1) o rep indefinite endian #f vals))
	       (else 
		=> (lambda (c)
		     (let*-values (((set n converter) (aux:lookup-proc c #t))
				   ((o*) (if align (aux:roundb o n) o)))
		       (define (set-value! v o)
			 (let ((v (if converter (converter v) v)))
			   (if (= n 1)
			       (set bv o v)
			       (set bv o v endian))))
		       (define (definite-rep)
			 (do ((rep (or rep 1) (- rep 1))
			      (o o* (+ o n))
			      (vals vals (cdr vals)))
			     ((zero? rep)
			      (lp (+ i 1) (+ o (* n rep))
				  #f #f endian align vals))
			   (when (null? vals)
			     (error 'pack! "Too few values for the format" fmt))
			   (set-value! (car vals) o)))
		       (define (indefinite-rep)
			 (when (> (string-length fmt) (+ i 1))
			   (error 'pack! "'*' must be the last position" fmt))
			 (do ((vals vals (cdr vals)) (o o (+ o n)))
			     ((null? vals) bv)
			   (set-value! (car vals) o)))
		       (unless set 
			 (error 'pack! "Bad character in format string" fmt c))
		       (zero! o (- o* o))
		       (if indefinite
			   (indefinite-rep)
			   (definite-rep))))))))))

  (define-syntax pack!
    (lambda (x)
      (syntax-case x ()
	((_ fmt bv offset vals ...)
	 (string? (syntax->datum #'fmt))
	 #'(pack!* fmt bv offset vals ...))
	((_ . rest) #'(pack!** . rest))
	(var (identifier? #'var) #'pack!**))))

  (define (pack** fmt . vals)
    (let ((bv (make-bytevector (apply format-size fmt vals))))
      (apply pack! fmt bv 0 vals)))

  (define-syntax pack
    (lambda (x)
      (syntax-case x ()
	((_ fmt vals ...)
	 ;; no infinite mark in format
	 (and (string? (syntax->datum #'fmt)) (format-size #'fmt))
	 #'(let ((bv (make-bytevector (format-size fmt))))
	     (pack! fmt bv 0 vals ...)))
	((_ fmt vals ...)
	 #'(let ((bv (make-bytevector (format-size fmt vals ...))))
	     (pack! fmt bv 0 vals ...)))
	(var (identifier? #'var) #'pack**))))

)