;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; scheme-tools.scm - Library inspection and compiler pass diagnostics
;;;
;;;   Copyright (c) 2026 Sagittarius contributors
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

(import (rnrs)
        (sagittarius compiler))

(define (write-item label value)
  (display label)
  (write value)
  (newline))

(define (inspect-library file)
  (define (library-form? form)
    (and (list? form)
         (>= (length form) 2)
         (memq (car form) '(library define-library))))
  (define (print-declaration declaration)
    (when (and (pair? declaration)
               (memq (car declaration)
                     '(export import include include-ci
                       include-library-declarations cond-expand)))
      (case (car declaration)
        ((export import)
         (for-each (lambda (item)
                     (write-item (if (eq? (car declaration) 'export)
                                     "  export: " "  import: ")
                                 item))
                   (cdr declaration)))
        (else (write-item "  unexpanded declaration: " declaration)))))
  (call-with-input-file file
    (lambda (port)
      (let loop ((form (read port)))
        (cond ((eof-object? form)
               (assertion-violation 'inspect-library
                                    "No library or define-library form" file))
              ((library-form? form)
               (write-item "form: " (car form))
               (write-item "library: " (cadr form))
               (for-each print-declaration (cddr form)))
              (else (loop (read port))))))))

(define (print-compiler-pass pass expression)
  (let* ((port (open-string-input-port expression))
         (form (read port)))
    (when (or (eof-object? form) (not (eof-object? (read port))))
      (assertion-violation 'compiler-pass
                           "Expected exactly one Scheme expression" expression))
    ((case pass
       ((1) compile-p1)
       ((2) compile-p2)
       ((3) compile-p3)
       ((4) compile-p4)
       ((5) compile-p5)
       (else (assertion-violation 'compiler-pass "Pass must be 1..5" pass)))
     form)))

(define (main args)
  (let ((arguments (cdr args)))
    (cond ((and (= (length arguments) 2)
                (string=? (car arguments) "inspect"))
           (inspect-library (cadr arguments)))
          ((and (= (length arguments) 3)
                (string=? (car arguments) "compiler-pass"))
           (print-compiler-pass (string->number (cadr arguments))
                                (caddr arguments)))
          (else (assertion-violation 'scheme-tools
                                     "Expected inspect FILE or compiler-pass N EXPR"
                                     arguments)))))
