;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/debug - Debugging support.
;;;  
;;;   Copyright (c) 2010-2023  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius debug)
    (export :export-reader-macro
	    unbound-variable
	    macroexpand macroexpand-1 macroexpand-n
	    make-remote-debugger
	    ;; just for convenience
	    (rename (connect-remote-repl connect-remote-debugger))
	    remote-debugger-terminate!
	    remote-debugger-port)
    (import (rnrs)
	    (rnrs eval)
	    (core errors)
	    (sagittarius)
	    (only (sagittarius clos) unbound-variable)
	    (sagittarius control)
	    (sagittarius reader)
	    (sagittarius remote-repl)
	    (sagittarius socket)
	    (sagittarius threads)
	    (sagittarius vm) ;; need this
	    (srfi :39 parameters)
	    (util port))

  ;; TODO find import
(define (load-expander library name)
  (import-library library '(sagittarius) `(only (sagittarius) ,name) #t))

;; this expands all
(define (macroexpand expr :optional (library (current-library)))
  ;; import %macroexpand into given library
  (load-expander library '%macroexpand)
  (eval `(%macroexpand ,expr) library))

;; these variants expand only top level macro
(define (macroexpand-1 expr . opt) (apply macroexpand-n expr 1 opt))

(define (macroexpand-n expr n :optional (library (current-library))
		       (strip? #t))
  (define (expand expr library) (eval `(%macroexpand-1 ,expr) library))
  (define (strip expr)
    (define seen (make-eq-hashtable))
    (define count 0)
    (let loop ((expr expr))
      (cond ((pair? expr)
	     (let ((a (loop (car expr)))
		   (d (loop (cdr expr))))
	       (if (and (eq? a (car expr)) (eq? d (cdr expr)))
		   expr
		   (cons a d))))
	    ((vector? expr) (list->vector (loop (vector->list expr))))
	    ((identifier? expr)
	     (cond ((hashtable-ref seen expr #f))
		   ((find-binding (id-library expr) (id-name expr) #f)
		    (syntax->datum expr))
		   (else
		    (let ((name (string->symbol
				 (format "~a.~a"
					 (syntax->datum expr) count))))
		      (set! count (+ count 1))
		      (hashtable-set! seen expr name)
		      name))))
	    (else expr))))
  (load-expander library '%macroexpand-1)
  (do ((i 0 (+ i 1)) (expr expr (expand expr library)))
      ((= i n) (if strip? (strip expr) expr))))


;; save the original reader
(define sh-bang-reader (get-dispatch-macro-character #\# #\!))

(define-dispatch-macro #\# #\! (debug-switch-reader port c param)
  ;; save original position
  (let* ((pos (port-position port))
	 ;; read the symbol and convert it to string
	 (v (format "~a" (read port))))
    ;; should we support reverting?
    (if (string=? "debug" v)
	(add-port-data! port 'debug #t)
	;; let the original handle
	(begin (set-port-position! port pos)
	       (sh-bang-reader port c param)))))

(define %debug-print #'debug-print)	; make identifier

;;(define debug-print-width (make-parameter 65))

(define-dispatch-macro #\# #\? (debug-reader port c param)
  (let1 c2 (read-char port)
    (cond ((eof-object? c2) c2)
	  ((char=? c2 #\=)
	   ;; #?=form
	   (if (get-port-data port 'debug)
	       (list %debug-print (read port))
	       (read port)))
	  (else
	   (raise-i/o-read-error 
	    'debug-reader
	    (format "unsupported #?-syntax #?~a" c2) port)))))

(define-syntax debug-print
  (syntax-rules ()
    ((_ form)
     (begin 
       (debug-print-pre 'form)
       (receive vals form
	 (debug-print-post vals))))))

(define (debug-print-pre form)
  (format/ss (current-error-port) "#?=~s\n" form))

(define (debug-print-post vals)
  (if (null? vals)
      (format (current-error-port) "#?-<void>\n")
      (begin
	(format/ss (current-error-port) "#?-    ~s\n" (car vals))
	(for-each (^(elt)
		    (format/ss (current-error-port)
			       "#?+    ~s\n"  elt))
		  (cdr vals))))
  (apply values vals))

;; Remote debug
(define-record-type (remote-debugger %make-remote-debugger remote-debugger?)
  (fields terminator socket)) 
(define (make-remote-debugger service . rest)
  (define env (environment '(except (rnrs) exit)
			   '(sagittarius debug remote)))
  (let-values (((p s)
		(apply make-remote-repl service :environment env :log #f rest)))
    (let ((t (thread-start!
	      (make-thread p (gensym "remote-debugger-thread-")))))
      (%make-remote-debugger (lambda () (thread-interrupt! t)) s))))

(define (remote-debugger-terminate! debugger)
  ((remote-debugger-terminator debugger)))

(define (remote-debugger-port debugger)
  (socket-info-port (socket-info (remote-debugger-socket debugger))))

)

;; Should we put this in (sagittarius debug)?
#!nounbound
(library (sagittarius debug remote)
    (export :export-reader-macro
	    sleeping-threads thread-backtrace->pretty-string
	    thread-current-procedure
	    (rename (kernel-managed-threads all-threads))
	    print)
    (import (rnrs)
	    (sagittarius debug)
	    (sagittarius kernel)
	    (sagittarius threads)
	    (sagittarius vm)
	    (srfi :1 lists))
(define (print . args) (for-each display args) (newline))

(define (sleeping-threads :optional (timeout 0.01))
  (define self (current-thread))
  (define to (or (and (real? timeout) timeout) 0.01)) ;; 10ms default
  (filter-map
   (lambda (t)
     (cond ((eq? self t) #f)
	   ((thread-suspend! t to #f) => (lambda (t) (thread-resume! t) #f))
	   (else t)))
   (kernel-managed-threads)))

(define (thread-backtrace->pretty-string t)
  (let-values (((out e) (open-string-output-port)))
    (display "Thread " out) (display (thread-name t) out) (newline out)
    (format-stack-trace (thread-backtrace t) out)
    (e)))

)
