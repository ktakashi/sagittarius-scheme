;; SPDX-FileCopyrightText: 2026 Artyom Bologov
;; SPDX-License-Identifier: MIT

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(import (rnrs))
(import (srfi :64))
(import (srfi :253))
(import (srfi :273))

(define-check email? string?)
(define-check positive-integer?
  (lambda (x) (and (integer? x) (positive? x))))

(define-syntax check-arg-true
  (syntax-rules ()
    ((_ pred val)
     (begin
       (check-arg pred val)
       #t))))

(test-begin "define-check")
(test-equal 3 3)
(test-assert (check-arg-true email? "srfi-273@srfi.schemers.org"))
(test-assert (check-arg-true positive-integer? 3))
(test-error (check-arg-true positive-integer? 0))
(test-error (check-arg-true positive-integer? -8))
;; Domain error
;; (test-error (define-check invalid 3))
(test-end "define-check")

(test-begin "define-values-checked")
(define-values-checked (quot rem) (integer? integer?)
  (truncate/ 1 2))
(test-assert quot)
(test-assert rem)
(define-values-checked (a) (real?) 3)
(test-assert a)
;; FIXME: Impossible to test because define-* forms belong to top level only
;; (test-error (begin
;;               (define-values-checked (quot rem) (integer? string?)
;;                 (truncate/ 1 2))
;;               #t))
;; (test-error (define-values-checked (a) (string?) 3))
;; Ensure that symbols are not bound on type mismatch
(define-values-checked (x y) (integer? string?)
  (truncate/ 1 2))
(test-error (eval 'x (interaction-environment)))
(test-error (eval 'y (interaction-environment)))
;; Test modifications, on implementations supporting that
;; (test-error (set! quot "not an integer"))
(test-end "define-values-checked")

(define (hello x)
  (string-append "Hello, " x "!"))

(test-begin "declare-checked")
;; Post-declaration for standard symbols
(test-assert (begin
               (declare-checked (negative? (x real?)) => (boolean?))
               #t))
(test-assert (begin
               (declare-checked (call/cc (k procedure?)))
               #t))
;; Post-declaration for a new symbol.
;; Uncomment if supported
;; (test-assert (begin
;;                (declare-checked (hello (k string?)) => (string?))
;;                (hello "Stranger")))
;; (test-error (begin
;;                (declare-checked (hello (k string?)) => (integer?))
;;                (hello "Stranger")))
;; (test-error (begin
;;                (declare-checked (hello (k string?)) => (string?))
;;                (hello 5)))
(test-end "declare-checked")
