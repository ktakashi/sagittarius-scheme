;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; random.scm math library
;;;

;; It's basically for backward compatiblity, though it's rather weird
;; to import (sagittarius crypto random) on non cryptographical library.
;; So it's not deprecated
#!nounbound
(library (math random)
    (export (rename (random-generator? prng?))
	    pseudo-random?
	    secure-random?
	    ;; random number generator
	    pseudo-random
	    secure-random
	    random-seed-set!
	    random
	    (rename (random-generator-read-random-bytes read-random-bytes))
	    read-random-bytes!

	    (rename (*prng:yarrow* Yarrow)
		    (*prng:fortuna* Fortuna)
		    (*prng:rc4* RC4)
		    (*prng:sober-128* SOBER-128)
		    (*prng:system* System)
		    (*prng:chacha20* ChaCha20))
	    register-prng
	    lookup-prng
	    (rename (<random-generator> <prng>)
		    (<builtin-random-generator> <builtin-prng>))
	    <user-prng>
	    <secure-random>
	    (rename (random-generator-state prng-state))
	    ;; utility
	    read-sys-random
	    read-sys-random!
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto random))

(define-class <secure-random> () ()) ;; interface
(define-class <user-prng> (<custom-random-generator>) ())
(define (secure-random? o)
  (or (secure-random-generator? o)
      (and (custom-random-generator? o) (is-a? o <secure-random>))))
(define (pseudo-random? o)
  (or (builtin-random-generator? o)
      (and (custom-random-generator? o) (not (secure-random? o)))))

(define-generic lookup-prng)
(define-method lookup-prng (o) #f)

;; sad...
(define-method :after initialize ((o <user-prng>) initargs)
  (let ((read-random! (slot-ref o 'read-random!)))
    (slot-set! o 'read-random! (lambda (prng buf s len)
				 (let ((tmp (make-bytevector len)))
				   (read-random! prng tmp len)
				   (bytevector-copy! tmp 0 buf s len)
				   buf)))
    o))

(define (pseudo-random type
		       :key ((seed (or #f bytevector? integer?)) #f)
		       :allow-other-keys :rest opts)
  (let ((prng (apply pseudo-random-generator type :seed seed opts)))
    (when (and seed (builtin-random-generator? prng))
      (random-generator-randomize! prng (if (bytevector? seed)
					    seed
					    (integer->bytevector seed))))
    prng))

;; system prng uses /dev/urandom or equivalent machine dependent
;; source, so no reason for us to use secure-random-generator
(define system-prng (pseudo-random-generator *prng:system*))
(define (read-sys-random bits)
  (let* ((size (div (+ bits 7) 8))
	 (buf (make-bytevector size)))
    (read-sys-random! buf)))
(define (read-sys-random! bv :optional (start 0)
				       (len (- (bytevector-length bv) start)))
  (random-generator-read-random-bytes! system-prng bv start len)
  bv)
  
(define (secure-random type
		       :key ((bits integer?) 128) :allow-other-keys
		       :rest opts)
  (let ((prng (apply secure-random-generator type (div bits 8) opts)))
    (when (and (custom-random-generator? prng)
	       (not (is-a? prng <secure-random>)))
      (error 'secure-random
	     "given type is not sub type of <secure-random>" type))
    prng))

(define (read-random-bytes! prng buf size)
  (random-generator-read-random-bytes! prng buf 0 size))

(define (random-seed-set! prng (seed (or integer? bytevector?)))
  (let ((seed (if (integer? seed) (integer->bytevector seed) seed)))
    (random-generator-randomize! prng seed)))

(define (random prng size :key (read-size #f))
  (random-generator-random-integer prng size))

(define-syntax register-prng
  (syntax-rules ()
    ((_ m class)
     (define-method make-custom-random-generator ((o (eql m)) . opts)
       (apply make class opts)))))
)
