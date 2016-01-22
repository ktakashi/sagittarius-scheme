;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/generators.scm - Generators
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

;; base library for generators
;; SRFI-121 will be implemented with this

;; 
(library (sagittarius generators)
    (export ;; predefined generator
	    null-generator

	    ;; operations (from SRFI-121)
	    gcons* gappend gcombine gfilter gremove
	    gtake gdrop gtake-while gdrop-while
	    gdelete gdelete-neighbor-dups gindex gselect

	    ;; from Gauche
	    gconcatenate gflatten gmerge gmap gfilter-map
	    glet* glet1 do-generator generate
	    giota grange gunfold

	    ;; these are not generator operations
	    ;; using generators as its arguments
	    generator-for-each 
	    generator-fold generator-fold-right
	    generator-find 
	    generator-length generator-count 
	    generator-any generator-every
	    generator-unfold

	    ;; converter
	    generator->list generator->reverse-list
	    generator->vector generator->vector!  generator->string

	    list->generator 
	    vector->generator reverse-vector->generator
	    string->generator
	    bytevector->generator
	    port->char-generator port->byte-generator
	    
	    ->generator
	    )
    (import (rnrs)
	    (clos user)
	    (util list)
	    (srfi :26 cut)
	    (srfi :31 rec)
	    (sagittarius)
	    (sagittarius control))

  (define-generic ->generator) 
  (define-method ->generator ((x <list>)) (list->generator x))
  (define-method ->generator ((x <vector>)) (vector->generator x))
  (define-method ->generator ((x <string>)) (string->generator x))
  (define-method ->generator ((x <bytevector>)) (bytevector->generator x))
  (define-method ->generator ((x <port>)) 
    (if (binary-port? x)
	(port->byte-generator x)
	(port->char-generator x)))

  ;; convert given x to somewhat to generator
  (define (->gen x)
    (cond ((procedure? x) x)
	  ((pair? x) (list->generator x))
	  ((null? x) null-generator)
	  ((vector? x) (vector->generator x))
	  ((string? x) (string->generator x))
	  ((bytevector? x) (bytevector->generator x))
	  ((binary-port? x) (port->byte-generator x))
	  ((textual-port? x) (port->char-generator x))
	  ;; TODO should we call ->generator here?
	  (else x)))
  (define (->gens args) (map ->gen args))

  ;; From Gauche's document
  (define-syntax glet*
    (syntax-rules ()
      ((_ () body body2 ...) (let () body body2 ...))
      ((_ ((var gen-expr) more-bindings ...) . body)
       (let1 var gen-expr
	 (if (eof-object? var)
	     var
	     (glet* (more-bindings ...) . body))))
      ((_ (( gen-expr ) more-bindings ...) . body)
       (let1 var gen-expr
	 (if (eof-object? var)
	     var
	     (glet* (more-bindings ...) . body))))))

  (define-syntax glet1
    (syntax-rules ()
      ((_ var expr body body1 ...)
       (glet* ((var expr)) body body1 ...))))

  (define-syntax do-generator
    (syntax-rules ()
      ((_ (var gexpr) body ...)
       (let1 g gexpr
	 (let loop ()
	   (glet1 var (g) body ... (loop)))))))

  ;; for convenience
  (define-syntax inc!
    (syntax-rules ()
      ((_ v) (inc! v 1))
      ((_ v n) (set! v (+ v n)))))
  (define-syntax dec!
    (syntax-rules ()
      ((_ v) (dec! v 1))
      ((_ v n) (set! v (- v n)))))

  ;; constructors
  (define null-generator
    (let ((r (eof-object)))
      (lambda () r)))
  
  ;; converter
  (define (generator->reverse-list g . maybe-k) 
    (let ((k (and-let* (( (not (null? maybe-k)) ))
	       (rlet1 k (car maybe-k)
		 (unless (and (integer? k) (positive? k))
		   (assertion-violation 'generator->list
					"non negative integer required" k))))))
      (let loop ((i 0) (r '()))
	(if (eqv? i k)
	    r
	    (let1 e (g)
	      (if (eof-object? e)
		  r
		  (loop (+ i 1) (cons e r))))))))
  (define (generator->list g . maybe-k)
    (let1 r (apply generator->reverse-list g maybe-k)
      (reverse! r)))
  (define (generator->vector g . maybe-k)
    (list->vector (apply generator->list g maybe-k)))

  (define (generator->vector! vec at g)
    (define len (vector-length vec))
    (let loop ((off at) (count 0))
      (if (= len off) 
	  count
	  (let1 v (g)
	    (if (eof-object? v)
		count
		(begin
		  (vector-set! vec off v)
		  (loop (+ off 1) (+ count 1))))))))

  (define (generator->string g . maybe-k)
    ;; for my laziness
    (list->string (apply generator->list g maybe-k)))

  ;; gcons* :: (a, ..., Generator a) -> Generator a
  (define (gcons* . args)
    (lambda ()
      (cond ((null? args) null-generator)
	    ((null? (cdr args)) ((->gen (car args))))
	    (else (pop! args)))))

  ;; gappend :: [Generator a] -> Generator a
  (define (gappend . args)
    (let ((gs args) (g #f))
      (if (null? args)
	  null-generator
	  (rec (f)
	    (unless g (set! g (->gen (pop! gs))))
	    (let ((v (g)))
	      (cond ((not (eof-object? v)) v)
		    ((null? gs) v)
		    (else (set! g #f) (f))))))))

  ;; gcombine :: ((a,b) -> (c,b), b, Generator a) -> Generator c
  (define gcombine
    (case-lambda
     ((proc seed gen)
      (let1 g (->gen gen)
	(lambda ()
	  (glet1 v (g)
	    (let-values (((rv next-seed) (proc v seed)))
	      (set! seed next-seed)
	      rv)))))
     ((proc seed gen . more)
      (let1 gs (->gens (cons gen more))
	(lambda ()
	  (glet1 vs (fold-right (lambda (g s) (glet* ((s) (v (g))) (cons v s)))
				(list seed) gs)
	    (let-values (((rv next-seed) (apply proc vs)))
	      (set! seed next-seed)
	      rv)))))))
    
  ;; gfilter :: (a -> bool, Generator a) -> Generator a
  ;; gremove :: (a -> bool, Generator a) -> Generator a
  (define (gfilter pred gen)
    (let ((gen (->gen gen)))
      (lambda ()
	(let loop ((v (gen)))
	  (cond ((eof-object? v) v)
		((pred v) v)
		(else (loop (gen))))))))
  (define (gremove pred gen) (gfilter (lambda (v) (not (pred v))) gen))

  (define (gtake gen n :optional (padding (eof-object)))
    (let ((k 0) (gen (->gen gen)))
      (lambda ()
	(if (< k n)
	    (let1 v (gen)
	      (inc! k)
	      (if (eof-object? v) padding v))
	    (eof-object)))))
  (define (gdrop gen n)
    (let ((k n) (gen (->gen gen)))
      (lambda ()
	(unless (<= k 0) (dotimes (i n) (dec! k) (gen)))
	(gen))))

  ;; gtake-while :: (a -> bool, Generator a) -> Generator a
  (define (gtake-while pred gen)
    (let ((gen (->gen gen))
	  (end #f))
      (lambda ()
	(if end 
	    (eof-object)
	    (let1 v (gen)
	      (if (or (eof-object? v) (not (pred v)))
		  (begin (set! end #t) (eof-object))
		  v))))))
  (define (gdrop-while pred gen)
    (let ((gen (->gen gen))
	  (found #f))
      (lambda ()
	(if found
	    (gen)
	    (let loop ()
	      (glet1 v (gen)
		(if (pred v)
		    (loop)
		    (begin (set! found #t) v))))))))

  (define (gdelete item gen :optional (= equal?))
    (let1 gen (->gen gen)
      (lambda ()
	(let loop ()
	  (glet1 v (gen)
	    (if (= item v)
		(loop)
		v))))))

  (define (gdelete-neighbor-dups gen :optional (= equal?))
    (let ((gen (->gen gen))
	  (first #t)
	  (prev #f))
      (lambda ()
	(let loop ()
	  (let1 v (gen)
	    (cond (first (set! first #f) (set! prev v) (loop))
		  ((or (eof-object? v)
		       (not (= prev v)))
		   (rlet1 r prev (set! prev v)))
		  (else (loop))))))))
  
  (define (gindex value-gen index-gen)
    (let ((vgen (->gen value-gen))
	  (igen (->gen index-gen))
	  ;; to raise an error if index is not strictly increased
	  (prev -1))
      (define (skip n)
	(let loop ((i 0))
	  (glet1 v (vgen)
	    (if (= i n) v (loop (+ i 1))))))
      (lambda ()
	(let loop ()
	  (glet1 i (igen)
	    (when (< i prev)
	      (error 'gindex "index is not increased strictly" prev i))
	    (rlet1 r (skip (- i prev 1))
	      (set! prev i)
	      ;; TODO should we?
	      (when (eof-object? r)
		(error 'gindex "value generator is exhauseted"))))))))

  (define (gselect value-gen truth-gen)
    (let ((vgen (->gen value-gen))
	  (tgen (->gen truth-gen)))
      (lambda ()
	(let loop ()
	  (glet* ((v (vgen))
		  (t (tgen)))
	    (if t v (loop)))))))
  
  ;; gconcatenate :: Generator Generator a -> Generator a
  (define (gconcatenate gen)
    (let ((gen (->gen gen))
	  (g (gen)))
      (lambda ()
	(let loop ()
	  (if (eof-object? g)
	      g
	      (let ((v (g)))
		(if (eof-object? v)
		    (begin (set! g (->gen (gen))) (loop))
		    v)))))))

  ;; gflatten :: Generator [a] -> Generator a
  (define (gflatten gen)
    (let ((gen (->gen gen))
	  (c '()))
      (rec (g)
	(cond ((eof-object? c) c)
	      ((pair? c) (pop! c))
	      (else (set! c (gen)) (g))))))

  ;; gmerge :: ((a, a) -> Bool, Generator a, Generator a, ...) -> Generator a
  (define gmerge
    (case-lambda
     ((proc gen) (->gen gen))
     ((proc gen1 gen2)
      (let ((gen1 (->gen gen1))
	    (gen2 (->gen gen2)))
	(let ((e1 (gen1))
	      (e2 (gen2)))
	  (lambda ()
	    (if (eof-object? e1)
		(if (eof-object? e2)
		    e2
		    (rlet1 r e2 (set! e2 (gen2))))
		(if (eof-object? e2)
		    (rlet1 r e1 (set! e1 (gen1)))
		    (if (proc e1 e2)
			(rlet1 r e1 (set! e1 (gen1)))
			(rlet1 r e2 (set! e2 (gen2))))))))))
     ((proc . gens)
      (apply gmerge proc (map (cut apply gmerge proc <>) (slices gens 2))))))

  ;; gmap :: (a -> b, Generator a) -> Generator b
  (define gmap
    (case-lambda
     ((fn gen)
      (let ((gen (->gen gen)))
	(lambda ()
	  (glet1 v (gen) (fn v)))))
     ((fn gen . more)
      (let1 gs (->gens (cons gen more))
	(lambda ()
	  (let1 vs (map (lambda (f) (f)) gs)
	    (if (exists eof-object? vs) (eof-object) (apply fn vs))))))))


  ;; gfilter-map :: (a -> b, Generator a) -> Generator b
  (define gfilter-map
    (case-lambda
     ((fn gen)
      (let ((gen (->gen gen)))
	(lambda () 
	  (let loop ()
	    (glet1 v (gen)
	      (or (fn v) (loop)))))))
     ((fn gen . more)
      (let1 gs (->gens (cons gen more))
	(lambda ()
	  (let loop ()
	    (let1 vs (map (lambda (f) (f)) gs)
	      (cond ((exists eof-object? vs) (eof-object))
		    ((apply fn vs))
		    (else (loop))))))))))

  ;; Seems this would be nicer
  ;; follow Gauche
  (define (giota :optional (count +inf.0) (start 0) (step 1))
    (let ((next start)
	  (i 0)
	  (e (null-generator)))
      (lambda ()
	(if (= i count)
	    e
	    (rlet1 r next
	      (inc! next step)
	      (inc! i))))))

  (define (grange :optional (start 0) (end +inf.0) (step 1))
    (let1 val (if (exact? step) start (inexact start))
      (lambda ()
	(if (>= val end)
	    (eof-object)
	    (rlet1 r val (inc! val step))))))

  (define (gunfold stop? mapper successor seed :optional (tail-gen #f))
    (let ((seed seed) (end? #f) (tail #f))
      (lambda ()
	(cond (end? (tail))
	      ((stop? seed)
	       (set! end? #t)
	       (set! tail (if tail-gen (tail-gen seed) eof-object))
	       (tail))
	      (else (rlet1 r (mapper seed) (set! seed (successor seed))))))))

  (define generator-for-each
    (case-lambda
     ((fn gen)
      (let loop ((v (gen)))
	(unless (eof-object? v) (fn v) (loop (gen)))))
     ((fn gen . more)
      (let1 gs (cons gen more)
	(let loop ((vs (map (lambda (f) (f)) gs)))
	  (unless (exists eof-object? vs) 
	    (apply fn vs) (loop (map (lambda (f) (f)) gs))))))))

  (define generator-fold 
    (case-lambda
     ((fn knil gen)
      (let loop ((item (gen))
		 (knil knil))
	(if (eof-object? item)
	    knil
	    (let1 r (fn item knil)
	      (loop (gen) r)))))
     ((fn knil gen . more)
      (let1 gens (cons gen more)
	(let loop ((knil knil))
	  (let ((vs (fold-right (lambda (g knil)
				  (let1 v (g)
				    (if (eof-object? v)
					v
					(cons v knil)))) (list knil) gens)))
	    (if (eof-object? vs)
		knil
		(loop (apply fn knil)))))))))

  (define (generator-fold-right proc seed gen . gens)
    ;; I'm a bit lazy to do tail recursive call with generator
    ;; so first we make generators to list then apply it fold-right
    ;; [Generator a] -> [(a)]
    (define (generators->list gens)
      (let loop ((r '()))
	(let ((vs (map (lambda (f) (f)) gens)))
	  (if (exists eof-object? vs)
	      r
	      (loop (cons vs r))))))
    ;; now the lists are transposed so just apply until it's end
    (let loop ((knil seed) (lists (generators->list gens)))
      (if (null? lists)
	  knil
	  (loop (apply proc (append! (car lists) (list knil)))
		(cdr lists)))))

  (define (generator-find pred gen)
    (let loop ((v (gen)))
      (cond ((eof-object? v) #f)
	    ((pred v) v)
	    (else (loop (gen))))))
  
  (define (generator-count pred gen)
    (let loop ((count 0) (v (gen)))
      (cond ((eof-object? v) count)
	    ((pred v) (loop (+ count 1) (gen)))
	    (else (loop count (gen))))))

  (define (generator-length gen)
    (generator-count (lambda (v) #t) gen))

  (define (generator-any pred gen)
    (let loop ((v (gen)))
      (cond ((eof-object? v) #f)
	    ((pred v))
	    (else (loop (gen))))))

  (define (generator-every pred gen)
    (let loop ((last #t) (v (gen)))
      (cond ((eof-object? v) last)
	    ((pred v) => (lambda (v) (loop v (gen))))
	    (else #f))))

  (define (generator-unfold gen unfold . args)
    (apply unfold eof-object? (lambda (x) x) (lambda (x) (gen)) (gen) args))

  ;; list->generator :: [a ...] -> Generator a
  ;; API signature is taken from Gauche (start and end)
  (define (list->generator args :optional (start #f) (end #f))
    (let1 start (or start 0)
      (cond ((> start 0)
	     (list->generator (drop* args start) 0 (and end (- end start))))
	    (end
	     (lambda ()
	       (if (or (null? args) (<= end 0))
		   (eof-object)
		   (begin (dec! end) (pop! args)))))
	    (else
	     (lambda ()
	       (if (null? args)
		   (eof-object)
		   (pop! args)))))))

  ;; for my laziness
  (define-syntax define-sequential-generator
    (syntax-rules ()
      ((_ name length ref)
       (define (name vec :optional (start #f) (end #f))
	 (let ((start (or start 0))
	       (end (or end (length vec)))
	       (e (null-generator)))
	   (lambda ()
	     (if (= start end)
		 e
		 (rlet1 r (ref vec start) (inc! start)))))))))
  
  (define-sequential-generator vector->generator vector-length vector-ref)
  (define-sequential-generator string->generator string-length string-ref)
  (define-sequential-generator bytevector->generator bytevector-length 
    bytevector-u8-ref)

  ;; end to start access
  (define (reverse-vector->generator vec :optional (start #f) (end #f))
    (let ((start (or start 0))
	  (end (- (or end (vector-length vec)) 1))
	  (e (null-generator)))
      (lambda ()
	(if (< end start)
	    e
	    (rlet1 r (vector-ref vec end) (dec! end))))))

  (define (port->byte-generator p) (cut get-u8 p))
  (define (port->char-generator p) (cut get-char p))

  ;; generate
  (define (generate proc)
    (define resume #f)
    (define return #f)
    (define (yield . v) (call/cc (lambda (r) (set! resume r) (apply return v))))
    (lambda ()
      (call/cc 
       (lambda (c)
	 (set! return c)
	 (if resume 
	     (resume)
	     (begin 
	       (proc yield)
	       (set! resume null-generator)
	       (return (eof-object))))))))
)

	    
