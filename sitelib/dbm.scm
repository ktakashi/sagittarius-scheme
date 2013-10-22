;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; dbm.scm - generic DBM interface
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

;; the interface are mostly taken from Gauche
(library (dbm)
    (export <dbm> <dbm-meta>
	    dbm-open dbm-close dbm-closed?
	    dbm-get dbm-put! dbm-delete! dbm-exists?
	    dbm-fold dbm-for-each dbm-map
	    dbm-db-exists? dbm-db-remove dbm-db-copy dbm-db-move

	    dbm-type->class)
    (import (rnrs)
	    (rnrs eval)
	    (clos user)
	    (sagittarius)
	    (dbm private)
	    (srfi :26 cut))

  (define-method dbm-open ((class <dbm-meta>) . initargs)
    (dbm-open (apply make class initargs)))

  (define (write-to-string sexp)
    (call-with-string-output-port (cut write/ss sexp <>)))
  (define (read-from-string str)
    (read/ss (open-string-input-port str)))

  (define (identity x) x)
  (define-method dbm-open ((self <dbm>))
    (define (pick-proc slot default custom)
      (let ((spec (slot-ref self slot)))
	(cond ((eq? spec #f) identity)
	      ((eq? spec #t) default)
	      ((and (pair? spec)
		    (null? (cddr spec))
		    (procedure? (car spec))
		    (procedure? (cadr spec)))
	       (custom spec))
	      (else 
	       (error 'dbm-open
		      (format "bad value for ~s: \
                               has to be boolean or a list of two procedures, \
                               but got ~s" slot spec)
		      self)))))

    (slot-set! self 'k2s (pick-proc 'key-convert write-to-string car))
    (slot-set! self 's2k (pick-proc 'key-convert read-from-string cadr))
    (slot-set! self 'v2s (pick-proc 'value-convert write-to-string car))
    (slot-set! self 's2v (pick-proc 'value-convert read-from-string cadr))
    self)

  ;; pre checks
  (define-method dbm-put! :before ((dbm <dbm>) key value)
    (when (dbm-closed? dbm) 
      (error 'dbm-put! "dbm already closed" dbm))
    (when (eqv? (slot-ref dbm 'rw-mode) :read)
      (error 'dbm-put! "dbm is read only" dbm)))

  (define-method dbm-get :before ((dbm <dbm>) key . args)
    (when (dbm-closed? dbm) (error 'dbm-get "dbm already closed" dbm)))

  (define-method dbm-exists? :before ((dbm <dbm>) key)
    (when (dbm-closed? dbm) (error 'dbm-exists? "dbm already closed" dbm)))

  (define-method dbm-delete! :before ((dbm <dbm>) key)
    (when (dbm-closed? dbm) (error 'dbm-delete! "dbm already closed" dbm))
    (when (eqv? (slot-ref dbm 'rw-mode) :read)
      (error 'dbm-delete! "dbm is read only" dbm)))
  
  (define-method dbm-fold :before ((dbm <dbm>) proc knil)
    (when (dbm-closed? dbm) (error 'dbm-fold "dbm already closed" dbm)))
  (define-method dbm-for-each :before ((dbm <dbm>) key)
    (when (dbm-closed? dbm) (error 'dbm-for-each "dbm already closed" dbm)))
  (define-method dbm-map :before ((dbm <dbm>) key)
    (when (dbm-closed? dbm) (error 'dbm-map "dbm already closed" dbm)))

  ;; method
  (define-method dbm-fold    ((dbm <dbm>) proc knil) #f)
  (define-method dbm-close   ((dbm <dbm>)) #f)
  (define-method dbm-closed? ((dbm <dbm>)) #f)

  (define-method dbm-for-each ((dbm <dbm>) proc)
    (dbm-fold dbm (lambda (key value r) (proc key value)) #f))

  (define-method dbm-map ((dbm <dbm>) proc)
    (reverse!
     (dbm-fold dbm (lambda (key value r) (cons (proc key value) r)) '())))

  ;;
  ;; Meta-operations
  ;;  Subclass has to implement at least dbm-db-exists? and dbm-db-remove.
  ;;

  (define-method dbm-db-exists? ((class <dbm-meta>) name)
    (error 'dbm-db-exists? "not supported" class))

  (define-method dbm-db-remove ((class <dbm-meta>) name)
    (errorf 'dbm-db-remove "not supported" class))

  (define-method dbm-db-copy ((class <dbm-meta>) from to)
    ;; generic one - might be slow, and it may not copy meta info.
    ;; it also doesn't check if from and to is the same databases;
    ;; but it opens from-db first with read mode, so if the implementation
    ;; has sane locking, the to-db opening with create option would fail.
    ;; (That's why we're using let* here.)
    (let* ((from-db (dbm-open class :path from :rw-mode :read))
	   (to-db   (dbm-open class :path to   :rw-mode :create)))
      (dbm-for-each from-db (lambda (k v) (dbm-put! to-db k v)))
      (dbm-close to-db)
      (dbm-close from-db)))

  (define-method dbm-db-move ((class <dbm-meta>) from to)
    ;; generic one - see above.
    (let* ((from-db (dbm-open class :path from :rw-mode :read))
	   (to-db   (dbm-open class :path to   :rw-mode :create)))
      (dbm-for-each from-db (lambda (k v) (dbm-put! to-db k v)))
      (dbm-close to-db)
      (dbm-close from-db)
      (dbm-db-remove class from)))

  (define (dbm-type->class dbmtype)
    (let ((library `(dbm ,dbmtype))
	  (class-name (string->symbol (format "<~a>" dbmtype))))
      (guard (e (else #f))
	(eval class-name (environment library)))))
  
)