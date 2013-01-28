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
    (export pack pack! unpack get-unpack
	    format-size
	    define-s8-parcker 
	    define-u8-parcker 
	    define-s16-parcker
	    define-u16-parcker
	    define-s32-parcker
	    define-u32-parcker
	    define-s64-parcker
	    define-u64-parcker
	    define-f32-parcker
	    define-f64-parcker)
    (import (rnrs)
	    (sagittarius)
	    (srfi :13)
	    (prefix (binary pack-aux) aux:))

  ;; internal
  (define-syntax define-packer
    (syntax-rules ()
      ((_ name base)
       (define-syntax name
	 (syntax-rules (pack unpack)
	   ((_ (char v) 
	       (pack expr1 (... ...)) 
	       (unpack expr2 (... ...)))
	    (aux:add-extension base char
			       (lambda (v) expr1 (... ...))
			       (lambda (v) expr2 (... ...)))))))))

  (define-packer define-s8-parcker  #\c)
  (define-packer define-u8-parcker  #\C)
  (define-packer define-s16-parcker #\s)
  (define-packer define-u16-parcker #\S)
  (define-packer define-s32-parcker #\l)
  (define-packer define-u32-parcker #\L)
  (define-packer define-s64-parcker #\q)
  (define-packer define-u64-parcker #\Q)
  (define-packer define-f32-parcker #\f)
  (define-packer define-f64-parcker #\d)

  (define-syntax format-size
    (lambda (x)
      (syntax-case x ()
	((_ fmt vals ...)
	 (string? (syntax->datum #'fmt))
	 (apply aux:format-size (syntax->datum #'fmt) #'(vals ...)))
	((_ fmt vals ...)
	 #'(aux:format-size fmt vals ...))
	(var (identifier? #'var) #'aux:format-size))))

  (define-syntax unpack*
    (lambda (x)
      (define (get-refs fmt* offset)
	(let* ((fmt (syntax->datum fmt*))
	       (len (string-length fmt)))
	  (let lp ((i 0)
		   ;; If the offset is an integer,
		   ;; then the offsets for all fields
		   ;; can be computed directly.
		   ;; Otherwise, code is generated to
		   ;; compute the offsets.
		   (o (if (integer? (syntax->datum #'offset))
			  (syntax->datum #'offset)
			  #'off))
		   (rep #f)
		   (endian #f)
		   (align #t)
		   (refs '()))
	    (cond ((= i len) (reverse! refs))
		  ((char-whitespace? (string-ref fmt i))
		   (lp (+ i 1) o rep endian align refs))
		  (else
		   (case (string-ref fmt i)
		     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		      => (lambda (c)
			   (lp (+ i 1) o
			       (+ (digit-value c) (* (if rep rep 0) 10))
			       endian align refs)))
		     ((#\=)
		      (lp (+ i 1) o #f #f align refs))
		     ((#\<)
		      (lp (+ i 1) o #f #'(endianness little) align refs))
		     ((#\> #\!)
		      (lp (+ i 1) o #f #'(endianness big) align refs))
		     ((#\x)
		      (lp (+ i 1) (aux:add o (or rep 1)) #f endian align refs))
		     ((#\a)
		      (lp (+ i 1) o rep endian #t refs))
		     ((#\u)
		      (lp (+ i 1) o rep endian #f refs))
		     (else
		      => (lambda (c)
			   (let*-values (((ref n cn) (aux:lookup-name c #f))
					 ((o)   (if align (aux:roundb o n) o)))
			     (define (ref-value o)
			       (define (get-ref)
				 (cond ((= n 1) #`(#,ref bv #,o))
				       (endian #`(#,ref bv #,o #,endian))
				       (else
					(let ((nref (aux:->native ref #t)))
					  #`(#,nref bv #,o)))))
			       (with-syntax ((ref (get-ref)))
				 (if cn
				     #`((aux:lookup-converter #,c #f) ref)
				     #'ref)))
			     (let ((rep (or rep 1)))
			       (lp (+ i 1) (aux:add o (* n rep)) #f
				   endian align
				   (let lp* ((o o) (rep rep) (refs refs))
				     (if (zero? rep) refs
					 (lp* (aux:add o n) (- rep 1)
					      (with-syntax ((foff o))
						(cons (ref-value #'foff)
						      refs))))))))))))))))
      (syntax-case x ()
	((_ fmt bv)
	 #'(unpack* fmt bv 0))
	((_ fmt bytevector offset)
	 (with-syntax (((refs ...) (get-refs #'fmt #'offset)))
	   #'(let ((bv bytevector)
		   (off offset))
	       (values refs ...)))))))

  ;; non macro version
  (define (unpack** fmt bv :optional (offset 0))
    (define limit (string-length fmt))
    (let lp ((i 0) (o offset)
	     (rep #f) (indefinite #f)
	     (endian #f) (align #t)
	     (refs '()))
      (cond ((= i limit) (apply values (reverse! refs)))
	    ((char-whitespace? (string-ref fmt i))
	     (lp (+ i 1) o rep indefinite endian align refs))
	    (else
	     (case (string-ref fmt i)
	       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		=> (lambda (c)
		     (when indefinite
		       (error 'unpack "'*' and digits can't cooporate." fmt))
		     (lp (+ i 1) o
			 (+ (digit-value c) (* (if rep rep 0) 10)) #f
			 endian align refs)))
	       ((#\*)
		(when rep
		  (error 'unpack "'*' and digits can't cooporate." fmt))
		(lp (+ i 1) o #f #t endian align refs))
	       ;; endianness
	       ((#\=) (lp (+ i 1) o #f #f #f align refs))
	       ((#\<) (lp (+ i 1) o #f #f (endianness little) align refs))
	       ((#\> #\!) (lp (+ i 1) o #f #f (endianness big) align refs))
	       ;; padding
	       ((#\x) 
		(when indefinite
		  (error 'unpack
			 "indefinite padding is not supported" fmt))
		(lp (+ i 1) (+ o (or rep 1)) #f #f endian align refs))
	       ;; align
	       ((#\a) (lp (+ i 1) o rep indefinite endian #t refs))
	       ((#\u) (lp (+ i 1) o rep indefinite endian #f refs))
	       (else => 
		(lambda (c)
		  (let*-values (((ref n cn) (aux:lookup-proc c #f))
				((o)      (if align (aux:roundb o n) o)))
		    (define (ref-value o)
		      (define (ret v) (if cn (cn v) v))
		      (ret
		       (cond ((= n 1) (ref bv o))
			     (endian (ref bv o endian))
			     (else
			      (let* ((name (aux:lookup-name c #f))
				     (nref (->native name #f)))
				(nref bv o))))))
		    (if indefinite
			(begin
			  (when (> limit (+ i 1))
			    (error 'unpack "'*' must be the last position"
				   fmt))
			  (do ((limit (bytevector-length bv))
			       (o o (+ o n))
			       (refs refs (cons (ref-value o) refs)))
			      ((>= o limit) (apply values (reverse! refs)))))
			(let ((rep (or rep 1)))
			  (lp (+ i 1) (+ o (* n rep)) #f #f
			      endian align
			      (let lp* ((o o) (rep rep) (refs refs))
				(if (zero? rep)
				    refs
				    (lp* (+ o n) (- rep 1)
					 (cons (ref-value o) refs)))))))))))))))

  (define-syntax unpack
    (lambda (x)
      (syntax-case x ()
	((_ fmt bv) #'(unpack fmt bv 0))
	((_ fmt bv offset)
	 (and (string? (syntax->datum #'fmt))
	      ;; unpack* macro can't handle indefinite length
	      (format-size (syntax->datum #'fmt)))
	 #'(unpack* fmt bv offset))
	((_ . rest) #'(unpack** . rest))
	(var (identifier? #'var) #'unpack** ))))

  (define (get-unpack** port fmt)
    (let ((size (format-size fmt)))
      (unless size
	(error 'get-unpack "format string contains '*'" fmt))
      (unpack fmt (get-bytevector-n port size))))

  (define-syntax get-unpack
    (lambda (x)
      (syntax-case x ()
	((_ port fmt)
	 (format-size #'fmt)
	 #'(unpack fmt (get-bytevector-n port (format-size fmt))))
	(var (identifier? #'var) #'get-unpack**)
	;; make sure we have human understandable error message
	(_
	 (syntax-violation 'get-unpack
			   "format string contains '*'" #'fmt)))))

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
				   #`(((aux:lookup-converter #,c #t) a) . d)))
				vals))
			  (define (set-value foff val1)
			    (cond ((= n 1) #`(#,set bv #,foff #,val1))
				  (endian #`(#,set bv #,foff #,val1 #,endian))
				  (else
				   (let ((nset (aux:->native set #t)))
				     #`(#,nset bv #,foff #,val1)))))
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
					       (cons (set-value #'foff #'val1)
						     setters))))))
				(drop vals rep #'fmt*)))
			  (define (indefinite-rep)
			    (when (> (string-length fmt) (+ i 1))
			      (syntax-violation 'pack!
			       "'*' must be the last position" fmt))
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
					 (cons (set-value #'foff #'val1)
					       setters))))))
			  (unless set 
			    (syntax-violation
			     'pack! "Bad character in format string"
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
	     (endian #f)
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
	       ((#\=) (lp (+ i 1) o #f #f #f align vals))
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
			   (cond ((= n 1) (set bv o v))
				 (endian (set bv o v endian))
				 (else
				  (let* ((name (aux:lookup-name c #t))
					 (proc (aux:->native name #f)))
				    ;; To make R6RS compatible use below
				    ;;(eval `(,proc ,bv ,o ,v)
				    ;;      (environment '(rnrs)))
				    (proc bv o v))))))
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