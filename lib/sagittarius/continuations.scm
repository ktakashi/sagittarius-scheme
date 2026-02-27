;;; -*- Scheme -*-
;;;
;;; continuations.scm - Continuations
;;;  
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius continuations)
    (export call-with-continuation-prompt call/prompt
	    abort-current-continuation abort/cc
	    call-with-composable-continuation call/comp

	    call/delim-cc
	    call-with-delimited-current-continuation

	    &continuation make-continuation-violation
	    continuation-violation? continuation-violation-prompt-tag

	    call-with-continuation-barrier
	    
	    continuation? composable-continuation?
	    continuation-prompt-available?

	    with-continuation-mark with-continuation-marks
	    continuation-mark-set?
	    continuation-mark-set->list continuation-mark-set->list*
	    current-continuation-marks
	    continuation-mark-set-first
	    call-with-immediate-continuation-mark

	    make-continuation-mark-key continuation-mark-key?

	    default-continuation-prompt-tag
	    make-continuation-prompt-tag continuation-prompt-tag?

	    shift reset
	    prompt control)
    (import (except (core) call/cc call-with-current-continuation)
	    (core syntax)
	    (core record)
	    (core conditions)
	    (sagittarius))

(define-syntax with-continuation-mark
  (lambda (x)
    (syntax-case x ()
      ((_ k v expr ...)
       #'(call/cm (vector (cons k v)) (lambda () expr ...))))))
(define-syntax with-continuation-marks
  (lambda (x)
    (syntax-case x ()
      ((_ ((k v) ...) expr ...)
       #'(call/cm (vector (cons k v) ...) (lambda () expr ...))))))

(define (continuation-mark-set->list mark-set key 
	     :optional (prompt-tag (default-continuation-prompt-tag)))
  ;; If mark-set is #f, use current-continuation-marks
  (let* ((ms (or mark-set (current-continuation-marks prompt-tag)))
	 (frames (vector-ref ms 1)))
    (let loop ((frames frames) (result '()))
      (if (null? frames)
	  (reverse! result)
	  (let* ((frame (car frames))
		 (entry (assq key frame)))
	    (if entry
		(loop (cdr frames) (cons (cdr entry) result))
		(loop (cdr frames) result)))))))

(define (continuation-mark-set->list* mark-set keys 
	 :optional (default #f) (prompt-tag (default-continuation-prompt-tag)))
  ;; Helper function to check if any key in keys has an entry in frame
  (define (has-any-key? frame keys)
    (let loop ((ks keys))
      (cond ((null? ks) #f)
	    ((assq (car ks) frame) #t)
	    (else (loop (cdr ks))))))
  ;; If mark-set is #f, use current-continuation-marks
  (let* ((ms (or mark-set (current-continuation-marks prompt-tag)))
	 (frames (vector-ref ms 1))
	 (key-count (length keys)))
    (let loop ((frames frames) (result '()))
      (if (null? frames)
	  (reverse result)
	  (let* ((frame (car frames))
		 ;; Check if this frame has any of our keys
		 (has-key? (has-any-key? frame keys)))
	    (if has-key?
		(let ((vec (make-vector key-count default)))
		  ;; Fill in values for each key
		  (let key-loop ((ks keys) (i 0))
		    (if (null? ks)
			(loop (cdr frames) (cons vec result))
			(let ((entry (assq (car ks) frame)))
			  (when entry
			    (vector-set! vec i (cdr entry)))
			  (key-loop (cdr ks) (+ i 1))))))
		(loop (cdr frames) result)))))))

(define (continuation-mark-set-first mark-set key 
	 :optional (default #f) (prompt-tag (default-continuation-prompt-tag)))
  ;; If mark-set is #f, use current-continuation-marks
  (let* ((ms (or mark-set (current-continuation-marks prompt-tag)))
	 (frames (vector-ref ms 1)))
    (let loop ((frames frames))
      (if (null? frames)
	  default
	  (let* ((frame (car frames))
		 (entry (assq key frame)))
	    (if entry
		(cdr entry)
		(loop (cdr frames))))))))

(define (continuation-mark-set->iterator . arg*)
  (let f ((ls (apply continuation-mark-set->list* arg*)))
    (lambda ()
      (if (null? ls)
          (values #f
                  (lambda ()
                    (apply assertion-violation
                           'continuation-mark-set->iterator
                           "attempt to iterate past the end"
                           arg*)))
          (values (car ls) (f (cdr ls)))))))

(define-record-type continuation-mark-key
  (nongenerative) (sealed #t) (opaque #f)
  (fields (mutable name))
  (protocol
   (lambda (p)
     (lambda (:optional ((name (or symbol? #f)) #f)) (p name)))))

;;;; shift reset

;; (define (abort-current-continuation/keep-prompt tag thunk)
;;   ((call-with-continuation-prompt
;;     (lambda ()
;;       ((call-with-delimited-current-continuation
;;         (lambda (k) (lambda () k))
;;         tag)))
;;     tag)
;;    thunk))
;; (define (make-call-with-shift abort-cc inserted-handler)
;;   (letrec ((call-with-shift
;;             (lambda (f :optional (tag (default-continuation-prompt-tag)))
;;               (call-with-composable-continuation
;;                (lambda (k)
;;                  (abort-cc
;;                   tag
;;                   (lambda ()
;;                     (f (lambda vals
;;                          (call-with-continuation-prompt
;;                           (lambda () (apply k vals))
;;                           tag
;;                           inserted-handler))))))
;;                tag))))
;;     call-with-shift))

;; (define call-with-shift
;;   (make-call-with-shift abort-current-continuation/keep-prompt #f))

;; (define (make-call-with-control abort-cc)
;;   ;; Uses call/cc to always keep the enclosing prompt.
;;   (letrec ((call-with-control
;;             (lambda (f :optional (tag (default-continuation-prompt-tag)))
;;               (call-with-composable-continuation
;;                (lambda (k) (abort-cc tag (lambda () (f k))))
;;                tag))))
;;     call-with-control))

;; (define call-with-control
;;   (make-call-with-control abort-current-continuation/keep-prompt))

;; (define-syntax define-prompt-macros
;;   (syntax-rules ()
;;     ((_ prompt prompt-at call-with-prompt)
;;      (begin
;;        (define-syntax prompt
;;          (syntax-rules ()
;;            ((prompt expr0 expr (... ...))
;;             (call-with-prompt (lambda () expr0 expr (... ...))))))
;;        (define-syntax prompt-at
;;          (syntax-rules ()
;;            ((prompt-at tag expr0 expr (... ...))
;;             (call-with-prompt (lambda () expr0 expr (... ...)) tag))))))))

;; (define-syntax define-control-macros
;;   (syntax-rules ()
;;     ((_ control control-at call-with-control)
;;      (begin
;;        (define-syntax control
;; 	 (lambda (stx)
;;            (syntax-case stx ()
;;              ((control id expr0 expr (... ...))
;;               (identifier? #'id)
;;               #'(call-with-control
;; 		 (lambda (id) expr0 expr (... ...)))))))
;;        (define-syntax control-at
;; 	 (lambda (stx)
;;            (syntax-case stx ()
;;              ((control-at tag id expr0 expr (... ...))
;;               (identifier? #'id)
;;               #'(call-with-control
;; 		 (lambda (id) expr0 expr (... ...)) tag)))))))))

;; (define-prompt-macros prompt prompt-at call-with-continuation-prompt)
;; (define-control-macros control control-at call-with-control)

;; (define-control-macros shift shift-at call-with-shift)
;; (define-prompt-macros reset reset-at call-with-continuation-prompt)

;; ;; From SRFI-226 implementation
(define-syntax reset
  (lambda (x)
    (syntax-case x ()
      [(reset e1 e2 ...)
       #'(call-with-continuation-prompt
	  (lambda ()
	    e1 e2 ...))])))

(define-syntax shift
  (lambda (x)
    (syntax-case x ()
      [(shift k e1 e2 ...)
       #'(call-with-composable-continuation
	  (lambda (c)
            (define k (lambda args (reset (apply c args))))
            (abort-current-continuation (default-continuation-prompt-tag)
					(lambda () e1 e2 ...))))])))

(define-syntax prompt
  (lambda (x)
    (syntax-case x ()
      [(prompt e1 e2 ...)
       #'(call-with-continuation-prompt
	  (lambda () e1 e2 ...)
	  (default-continuation-prompt-tag)
	  (lambda (thunk) (thunk)))])))
  
(define-syntax control
  (lambda (x)
    (syntax-case x ()
      [(control k e1 e2 ...)
       #'(call-with-composable-continuation
	  (lambda (k)
	    (abort-current-continuation (default-continuation-prompt-tag)
					(lambda () e1 e2 ...))))])))

)
