;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a99.scm/records/procedural.scm - ERR5RS records
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

;; Original copyright
;; Copyright (C) William D Clinger 2008. All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a 
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. REMEMBER, THERE IS
;; NO SCHEME UNDERGROUND. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
;; THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

(library (srfi :99 records procedural)

  (export make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator)

  (import (rnrs base)
          (rnrs lists)
          (rnrs records procedural)
          (srfi :99 records inspection))

  ; Note: the options are permitted by ERR5RS,
  ; but are not part of ERR5RS.

  (define (make-rtd name fieldspecs . rest)
    (let* ((parent (if (null? rest) #f (car rest)))
           (options (if (null? rest) '() (cdr rest)))
           (sealed? (and (memq 'sealed options) #t))
           (opaque? (and (memq 'opaque options) #t))
           (uid (let ((probe (memq 'uid options)))
                  (if (and probe (not (null? (cdr probe))))
                      (cadr probe)
                      #f))))
      (make-record-type-descriptor
       name
       parent
       uid
       sealed?
       opaque?
       (vector-map (lambda (fieldspec)
                     (if (symbol? fieldspec)
                         (list 'mutable fieldspec)
                         fieldspec))
                   fieldspecs))))
  
  (define rtd? record-type-descriptor?)
  
  (define (rtd-constructor rtd . rest)
  
    ; Computes permutation and allocates permutation buffer
    ; when the constructor is created, not when the constructor
    ; is called.  More error checking is recommended.
  
    (define (make-constructor fieldspecs allnames maker)
      (let* ((k (length fieldspecs))
             (n (length allnames))
             (buffer (make-vector n))
             (reverse-all-names (reverse allnames)))
  
        (define (position fieldname)
          (let ((names (memq fieldname reverse-all-names)))
            (assert names)
            (- (length names) 1)))
  
        (let ((indexes (map position fieldspecs)))
  
          ; The following can be made quite efficient by
          ; hand-coding it in some lower-level language,
          ; e.g. Larceny's mal.  Even case-lambda would
          ; be good enough in most systems.
  
          (lambda args
            (assert (= (length args) k))
            (for-each (lambda (arg posn)
                        (vector-set! buffer posn arg))
                      args indexes)
            (apply maker (vector->list buffer))))))
  
    (if (null? rest)
        (record-constructor
         (make-record-constructor-descriptor rtd #f #f))
        (begin (assert (null? (cdr rest)))
               (make-constructor
                (vector->list (car rest))
                (vector->list (rtd-all-field-names rtd))
                (record-constructor
                 (make-record-constructor-descriptor rtd #f #f))))))
  
  (define rtd-predicate record-predicate)
  
  (define (rtd-accessor rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-accessor rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-accessor
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))
  
  (define (rtd-mutator rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-mutator rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-mutator
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))

  )