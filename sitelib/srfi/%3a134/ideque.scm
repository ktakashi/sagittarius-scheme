;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a134/ideque.scm - Immutable Deques
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :134 ideque)
  (export ideque ideque-tabulate ideque-unfold ideque-unfold-right 
          ideque? ideque-empty? ideque= ideque-any ideque-every

          ideque-front ideque-add-front ideque-remove-front
          ideque-back  ideque-add-back  ideque-remove-back

          ideque-ref
          ideque-take ideque-take-right ideque-drop ideque-drop-right
          ideque-split-at

          ideque-length ideque-append ideque-reverse
          ideque-count ideque-zip

          ideque-map ideque-filter-map
          ideque-for-each ideque-for-each-right
          ideque-fold ideque-fold-right
          ideque-append-map
          
          ideque-filter ideque-remove ideque-partition

          ideque-find ideque-find-right
          ideque-take-while ideque-take-while-right
          ideque-drop-while ideque-drop-while-right
          ideque-span ideque-break
          
          list->ideque ideque->list
          generator->ideque ideque->generator)
  (import (rnrs)
	  (srfi :1)
	  (srfi :41)
	  (only (srfi :158) generator->list))
;; modification notes
;; - Using R6RS record instread of SRFI-9
;; - Using div instead of quotient
;; - Passing who argument to type/length check
;; - Using assertion-violation to signal an error

;; From https://srfi-email.schemers.org/srfi-134/msg/20723580/

;;;  Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
;;;  Copyright © 2022 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation files
;;;  (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify, merge,
;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;  and to permit persons to whom the Software is furnished to do so,
;;;  subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.</p>
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;;  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;  SOFTWARE
;;;

;; This implements banker's deque as described in
;; Chris Okasaki's Purely Functional Data Structures.
;; It provides amortized O(1) basic operations.
;; Original two-list version written by Shiro Kawai.
;; Stream version by Wolfgang Corcoran-Mathe.

;;;; Stream utility

(define (stream=? elt= s1 s2)
  (if (stream-null? s1)
      (stream-null? s2)
      (and (stream-pair? s2)
           (elt= (stream-car s1) (stream-car s2))
           (stream=? elt= (stream-cdr s1) (stream-cdr s2)))))

(define (stream-count pred s)
  (stream-fold (lambda (n x) (if (pred x) (+ n 1) n))
               0
               s))

(define stream-filter-map
  (stream-lambda (proc s)
    (cond ((stream-null? s) stream-null)
          ((proc (stream-car s)) =>
           (lambda (x)
             (stream-cons x (stream-filter-map proc (stream-cdr s)))))
          (else (stream-filter-map proc (stream-cdr s))))))

;; From SRFI 41. Clever!
(define (stream-partition pred s)
  (stream-unfolds
   (lambda (s)
     (if (stream-null? s)
         (values s '() '())
         (let ((a (stream-car s)) (s* (stream-cdr s)))
           (if (pred a)
               (values s* (list a) #f)
               (values s* #f (list a))))))
   s))

;; Could be improved.
(define (stream-span pred s)
  (values (stream-take-while pred s) (stream-drop-while pred s)))

(define (stream-break pred s)
  (stream-span (lambda (x) (not (pred x))) s))

(define (stream-any pred s)
  (let lp ((s s))
    (cond ((stream-null? s) #f)
          ((pred (stream-car s)))
          (else (lp (stream-cdr s))))))

(define (stream-every pred s)
  (let lp ((s s) (last-val #t))
    (cond ((stream-null? s) last-val)
          ((pred (stream-car s)) =>
           (lambda (x) (lp (stream-cdr s) x)))
          (else #f))))

;; Compare two streams up to whichever shorter one.
;; Returns the compare result and the tails of uncompared streams.
(define (stream-prefix= elt= a b)
  (let loop ((a a) (b b))
    (cond ((or (stream-null? a) (stream-null? b)) (values #t a b))
          ((elt= (stream-car a) (stream-car b))
           (loop (stream-cdr a) (stream-cdr b)))
          (else (values #f a b)))))

;; Compare two streams for equality using 'elt=' to compare elements.
(define (stream=? elt= s1 s2)
  (if (stream-null? s1)
      (stream-null? s2)
      (and (stream-pair? s2)
           (elt= (stream-car s1) (stream-car s2))
           (stream=? elt= (stream-cdr s1) (stream-cdr s2)))))

;; some compatibility stuff
(define-syntax receive
  (syntax-rules ()
    ((_ binds mv-expr body ...)
     (let-values ((binds mv-expr)) body ...))))

;;;; ideque type

(define-record-type (dq %make-dq ideque?) ;; (rename (dq <ideque>))
  (fields lenf  ; length of front chain
	  f     ; front chain
	  lenr  ; length of rear chain
	  r))   ; rear chain

;; We use a singleton for empty deque
(define *empty* (%make-dq 0 stream-null 0 stream-null))

;; Common type checker
(define (%check-ideque who x)
  (unless (ideque? x)
    (assertion-violation who "ideque expected" x)))

;; Internal constructor.  Returns a new ideque, with balancing 'front' and
;; 'rear' chains.

;; Front/back stream length differential factor.
(define stream-length-factor 3)

(define (make-deque lenf f lenr r)
  (cond ((> lenf (+ (* lenr stream-length-factor) 1))
         (let* ((i (div (+ lenf lenr) 2))
                (j (- (+ lenf lenr) i))
                (f. (stream-take i f))
                (r. (stream-append
                     r
                     (stream-reverse (stream-drop i f)))))
           (%make-dq i f. j r.)))
        ((> lenr (+ (* lenf stream-length-factor) 1))
         (let* ((j (div (+ lenf lenr) 2))
                (i (- (+ lenf lenr) j))
                (r. (stream-take j r))
                (f. (stream-append
                     f
                     (stream-reverse (stream-drop j r)))))
           (%make-dq i f. j r.)))
        (else (%make-dq lenf f lenr r))))

;;;; Basic operations

(define (ideque-empty? dq)
  (%check-ideque 'ideque-empty? dq)
  (and (zero? (dq-lenf dq))
       (zero? (dq-lenr dq))))

(define (ideque-add-front dq x)
  (%check-ideque 'ideque-add-front dq)
  (make-deque (+ (dq-lenf dq) 1)
              (stream-cons x (dq-f dq))
              (dq-lenr dq)
              (dq-r dq)))

(define (ideque-front dq)
  (%check-ideque 'ideque-front dq)
  (if (zero? (dq-lenf dq))
      (if (zero? (dq-lenr dq))
          (error 'ideque-front "Empty deque:" dq)
          (stream-car (dq-r dq)))
      (stream-car (dq-f dq))))

(define (ideque-remove-front dq)
  (%check-ideque 'ideque-remove-front dq)
  (if (zero? (dq-lenf dq))
      (if (zero? (dq-lenr dq))
          (error 'ideque-remove-front "Empty deque:" dq)
          *empty*)
      (make-deque (- (dq-lenf dq) 1)
                  (stream-cdr (dq-f dq))
                  (dq-lenr dq)
                  (dq-r dq))))

(define (ideque-add-back dq x)
  (%check-ideque 'ideque-add-back dq)
  (make-deque (dq-lenf dq)
              (dq-f dq)
              (+ (dq-lenr dq) 1)
              (stream-cons x (dq-r dq))))

(define (ideque-back dq)
  (%check-ideque 'ideque-back dq)
  (if (zero? (dq-lenr dq))
      (if (zero? (dq-lenf dq))
          (error 'ideque-back "Empty deque:" dq)
          (stream-car (dq-f dq)))
      (stream-car (dq-r dq))))

(define (ideque-remove-back dq)
  (%check-ideque 'ideque-remove-back dq)
  (if (zero? (dq-lenr dq))
      (if (zero? (dq-lenf dq))
          (error 'ideque-remove-back "Empty deque:" dq)
          *empty*)
      (make-deque (dq-lenf dq)
                  (dq-f dq)
                  (- (dq-lenr dq) 1)
                  (stream-cdr (dq-r dq)))))

(define (ideque-reverse dq)
  (%check-ideque 'ideque-reverse dq)
  (if (ideque-empty? dq)
      *empty*
      (%make-dq (dq-lenr dq) (dq-r dq) (dq-lenf dq) (dq-f dq))))

;;; Exported constructors

(define (ideque . args)
  (if (null? args)
      *empty*
      (list->ideque args)))

(define (ideque-tabulate size init)
  (let ((lenf (div size 2))
        (lenr (div (+ size 1) 2)))
    (%make-dq lenf
              (stream-unfold init
                             (lambda (n) (< n lenf))
                             (lambda (n) (+ n 1))
                             0)
              lenr
              (stream-unfold (lambda (n) (init (- size n 1)))
                             (lambda (n) (< n lenr))
                             (lambda (n) (+ n 1))
                             0))))

(define (ideque-unfold p f g seed)
  (list->ideque (unfold p f g seed)))

(define (ideque-unfold-right p f g seed)
  (ideque-reverse (list->ideque (unfold p f g seed))))

;;;; Other operations

(define ideque=
  (case-lambda
    ((elt=) #t)
    ((elt= ideque) (%check-ideque 'ideque= ideque) #t)
    ((elt= dq1 dq2) (%ideque=-binary elt= dq1 dq2))
    ((elt= . dqs)
     ;; The comparison scheme is the same as srfi-1's list=.
     (apply list= elt= (map ideque->list dqs)))))

(define (%ideque-same-length dq1 dq2)
  (= (ideque-length dq1) (ideque-length dq2)))

;; we optimize two-arg case
(define (%ideque=-binary elt= dq1 dq2)
  (%check-ideque '%ideque=-binary dq1)
  (%check-ideque '%ideque=-binary dq2)
  (or (eq? dq1 dq2)
      (and (%ideque-same-length dq1 dq2)
           (receive (x t1 t2)
                    (stream-prefix= elt= (dq-f dq1) (dq-f dq2))
             (and x
                  (receive (y r1 r2)
                           (stream-prefix= elt= (dq-r dq1) (dq-r dq2))
                    (and y
                         (if (null? t1)
                             (stream=? elt= t2 (stream-reverse r1))
                             (stream=? elt= t1 (stream-reverse r2))))))))))


(define (ideque-ref dq n)
  (%check-ideque 'ideque-ref dq)
  (let ((len (+ (dq-lenf dq) (dq-lenr dq))))
    (cond ((or (< n 0) (>= n len)) (error 'ideque-ref "Index out of range:" n))
          ((< n (dq-lenf dq)) (stream-ref (dq-f dq) n))
          (else (stream-ref (dq-r dq) (- len n 1))))))

(define (%ideque-take dq n)             ; n is within the range
  (let ((lenf (dq-lenf dq))
        (f    (dq-f dq))
        (lenr (dq-lenr dq)))
    (if (<= n lenf)
        (make-deque n (stream-take n f) 0 stream-null)
        (let ((lenr. (- lenr (- n lenf))))
          (make-deque lenf f lenr. (stream-drop lenr. (dq-r dq)))))))

(define (%ideque-drop dq n)             ; n is within the range
  (let ((lenf (dq-lenf dq))
        (f    (dq-f dq))
        (lenr (dq-lenr dq))
        (r    (dq-r dq)))
    (if (<= n lenf)
        (make-deque (- lenf n) (stream-drop n f) lenr r)
        (let ((lenr. (- lenr (- n lenf))))
          (make-deque 0 stream-null lenr. (stream-take lenr. r))))))

(define (%check-length who dq n)
  (unless (<= 0 n (ideque-length dq))
    (assertion-violation who "argument is out of range" n)))

(define (ideque-take dq n)
  (%check-ideque 'ideque-take dq)
  (%check-length 'ideque-take dq n)
  (%ideque-take dq n))

(define (ideque-take-right dq n)
  (%check-ideque 'ideque-take-right dq)
  (%check-length 'ideque-take-right dq n)
  (%ideque-drop dq (- (ideque-length dq) n)))

(define (ideque-drop dq n)
  (%check-ideque 'ideque-drop dq)
  (%check-length 'ideque-drop dq n)
  (%ideque-drop dq n))

(define (ideque-drop-right dq n)
  (%check-ideque 'ideque-drop-right dq)
  (%check-length 'ideque-drop-right dq n)
  (%ideque-take dq (- (ideque-length dq) n)))

(define (ideque-split-at dq n)
  (%check-ideque 'ideque-split-at dq)
  (%check-length 'ideque-split-at dq n)
  (values (%ideque-take dq n)
          (%ideque-drop dq n)))

(define (ideque-length dq)
  (%check-ideque 'ideque-length dq)
  (+ (dq-lenf dq) (dq-lenr dq)))

(define (ideque-append . dqs)
  ;; We could save some list copying by carefully split dqs into front and
  ;; rear groups and append separately, but for now we don't bother...
  (list->ideque (concatenate (map ideque->list dqs))))

(define (ideque-count pred dq)
  (%check-ideque 'ideque-count dq)
  (+ (stream-count pred (dq-f dq)) (stream-count pred (dq-r dq))))

(define (ideque-zip dq . dqs)
  ;; An easy way.
  (let ((elts (apply zip (ideque->list dq) (map ideque->list dqs))))
    (make-deque (length elts) (list->stream elts) 0 stream-null)))

(define (ideque-map proc dq)
  (%check-ideque 'ideque-map dq)
  (%make-dq (dq-lenf dq) (stream-map proc (dq-f dq))
            (dq-lenr dq) (stream-map proc (dq-r dq))))

(define (ideque-filter-map proc dq)
  (%check-ideque 'ideque-filter-map dq)
  (let ((f (stream-filter-map proc (dq-f dq)))
        (r (stream-filter-map proc (dq-r dq))))
    (make-deque (stream-length f) f (stream-length r) r)))

(define (ideque-for-each proc dq)
  (%check-ideque 'ideque-for-each dq)
  (stream-for-each proc (dq-f dq))
  (stream-for-each proc (stream-reverse (dq-r dq))))

(define (ideque-for-each-right proc dq)
  (%check-ideque 'ideque-for-each-right dq)
  (stream-for-each proc (dq-r dq))
  (stream-for-each proc (stream-reverse (dq-f dq))))

(define (ideque-fold proc knil dq)
  (let ((proc* (lambda (acc x) (proc x acc))))  ; stream-fold compat
    (%check-ideque 'ideque-fold dq)
    (stream-fold proc*
                 (stream-fold proc* knil (dq-f dq))
                 (stream-reverse (dq-r dq)))))

;; There's no stream-fold-right, so just convert dq.
(define (ideque-fold-right proc knil dq)
  (%check-ideque 'ideque-fold-right dq)
  (fold-right proc knil (ideque->list dq)))

(define (ideque-append-map proc dq)
  ;; can be cleverer, but for now...
  (list->ideque (append-map proc (ideque->list dq))))

(define (%ideque-filter pred dq)
  (%check-ideque '%ideque-filter dq)
  (let ((f (stream-filter pred (dq-f dq)))
        (r (stream-filter pred (dq-r dq))))
    (make-deque (stream-length f) f (stream-length r) r)))

(define (ideque-filter pred dq) (%ideque-filter pred dq))
(define (ideque-remove pred dq)
  (%ideque-filter (lambda (x) (not (pred x))) dq))

(define (ideque-partition pred dq)
  (%check-ideque 'ideque-partition dq)
  (receive (f1 f2) (stream-partition pred (dq-f dq))
    (receive (r1 r2) (stream-partition pred (dq-r dq))
      (values (make-deque (stream-length f1) f1 (stream-length r1) r1)
              (make-deque (stream-length f2) f2 (stream-length r2) r2)))))

(define *not-found* (cons #f #f)) ; unique value

(define (%search pred seq1 seq2 failure)
  ;; We could write seek as CPS, but we employ *not-found* instead to avoid
  ;; closure allocation.
  (define (seek pred s)
    (cond ((stream-null? s) *not-found*)
          ((pred (stream-car s)) (stream-car s))
          (else (seek pred (stream-cdr s)))))
  (let ((r (seek pred seq1)))
    (if (not (eq? r *not-found*))
        r
        (let ((r (seek pred (stream-reverse seq2))))
          (if (not (eq? r *not-found*))
              r
              (failure))))))

(define (ideque-find pred dq . opts)
  (%check-ideque 'ideque-find dq)
  (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
    (%search pred (dq-f dq) (dq-r dq) failure)))

(define (ideque-find-right pred dq . opts)
  (%check-ideque 'ideque-find-right dq)
  (let ((failure (if (pair? opts) (car opts) (lambda () #f))))
    (%search pred (dq-r dq) (dq-f dq) failure)))

(define (ideque-take-while pred dq)
  (%check-ideque 'ideque-take-while dq)
  (receive (hd tl) (stream-span pred (dq-f dq))
    (if (stream-null? tl)
        (receive (hd. tl.) (stream-span pred (stream-reverse (dq-r dq)))
          (make-deque (dq-lenf dq)
                      (dq-f dq)
                      (stream-length hd.)
                      (stream-reverse hd.)))
        (make-deque (stream-length hd) hd 0 stream-null))))

(define (ideque-take-while-right pred dq)
  (%check-ideque 'ideque-take-while-right dq)
  (ideque-reverse (ideque-take-while pred (ideque-reverse dq))))

(define (ideque-drop-while pred dq)
  (%check-ideque 'ideque-drop-while dq)
  (receive (hd tl) (stream-span pred (dq-f dq))
    (if (stream-null? tl)
        (receive (hd. tl.) (stream-span pred (stream-reverse (dq-r dq)))
          (make-deque (stream-length tl.) tl. 0 stream-null))
        (make-deque (stream-length tl) tl (dq-lenr dq) (dq-r dq)))))

(define (ideque-drop-while-right pred dq)
  (%check-ideque 'ideque-drop-while-right dq)
  (ideque-reverse (ideque-drop-while pred (ideque-reverse dq))))

(define (%idq-span-break op pred dq)
  (%check-ideque '%idq-span-break dq)
  (receive (head tail) (op pred (dq-f dq))
    (if (null? tail)
        (receive (head. tail.) (op pred (stream-reverse (dq-r dq)))
          (values (make-deque (stream-length head)
                              head
                              (stream-length head.)
                              (stream-reverse head.))
                  (make-deque (stream-length tail.) tail. 0 stream-null)))
        (values
         (make-deque (stream-length head) head 0 stream-null)
         (make-deque (stream-length tail) tail (dq-lenr dq) (dq-r dq))))))

(define (ideque-span pred dq) (%idq-span-break stream-span pred dq))
(define (ideque-break pred dq) (%idq-span-break stream-break pred dq))

(define (ideque-any pred dq)
  (%check-ideque 'ideque-any dq)
  (if (stream-null? (dq-r dq))
      (stream-any pred (dq-f dq))
      (or (stream-any pred (dq-f dq))
          (stream-any pred (stream-reverse (dq-r dq))))))

(define (ideque-every pred dq)
  (%check-ideque 'ideque-every dq)
  (if (stream-null? (dq-r dq))
      (stream-every pred (dq-f dq))
      (and (stream-every pred (dq-f dq))
           (stream-every pred (stream-reverse (dq-r dq))))))

(define (ideque->list dq)
  (%check-ideque 'ideque->list dq)
  (append (stream->list (dq-f dq))
          (stream->list (stream-reverse (dq-r dq)))))

(define (list->ideque lis)
  (make-deque (length lis) (list->stream lis) 0 stream-null))

(define (ideque->generator dq)
  (%check-ideque 'ideque->generator dq)
  (lambda ()
    (if (ideque-empty? dq)
        (eof-object)
        (let ((v (ideque-front dq)))
          (set! dq (ideque-remove-front dq))
          v))))

(define (generator->ideque gen)
  (list->ideque (generator->list gen)))
)
