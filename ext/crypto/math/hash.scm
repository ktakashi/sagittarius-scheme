;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; random.scm math library
;;;
#!deprecated
#!nounbound
(library (math hash)
    (export hash-algorithm?
	    hash-algorithm
	    hash-init!
	    hash-process!
	    hash-done!
	    hash-size
	    hash-block-size
	    hash-oid

	    (rename (*digest:whirlpool* WHIRLPOOL)
		    (*digest:ripemd-128* RIPEMD-128)
		    (*digest:ripemd-160* RIPEMD-160)
		    (*digest:ripemd-256* RIPEMD-256)
		    (*digest:ripemd-320* RIPEMD-320)
		    (*digest:sha-1* SHA-1)
		    (*digest:sha-224* SHA-224)
		    (*digest:sha-256* SHA-256)
		    (*digest:sha-384* SHA-384)
		    (*digest:sha-512* SHA-512)
		    (*digest:sha-512/224* SHA-512/224)
		    (*digest:sha-512/256* SHA-512/256)
		    (*digest:sha3-224* SHA-3-224)
		    (*digest:sha3-256* SHA-3-256)
		    (*digest:sha3-384* SHA-3-384)
		    (*digest:sha3-512* SHA-3-512)
		    (*digest:keccak-224* Keccak-224)
		    (*digest:keccak-256* Keccak-256)
		    (*digest:keccak-384* Keccak-384)
		    (*digest:keccak-512* Keccak-512)
		    (*digest:tiger-192* Tiger-192)
		    (*digest:md5* MD5)
		    (*digest:md4* MD4)
		    (*digest:md2* MD2)
		    (*digest:blake2s-128* BLAKE2s-128) 
		    (*digest:blake2s-160* BLAKE2s-160)
		    (*digest:blake2s-224* BLAKE2s-224)
		    (*digest:blake2s-256* BLAKE2s-256)
		    (*digest:blake2b-160* BLAKE2b-160)
		    (*digest:blake2b-256* BLAKE2b-256)
		    (*digest:blake2b-384* BLAKE2b-384)
		    (*digest:blake2b-512* BLAKE2b-512)
		    (*digest:shake-128* SHAKE128)
		    (*digest:shake-256* SHAKE256))

	    oid->hash-algorithm
	    
	    ;; for convenience
	    hash hash!
	    register-hash
	    lookup-hash
	    <hash-algorithm>
	    <user-hash-algorithm>
	    <builtin-hash-algorithm>

	    ;; for other libraries' backward compatibilities
	    hash-algorithm->digest-descriptor
	    )

    (import (rnrs)
	    (clos core)
	    (clos user)
	    (sagittarius crypto digests))

(define-class <hash-algorithm> () ())
(define (hash-algorithm? o) (is-a? o <hash-algorithm>))

(define-class <user-hash-algorithm> (<hash-algorithm>)
  ((hash :init-keyword :hash :reader user-hash-algorithm-hash)
   (init :init-keyword :init :reader user-hash-algorithm-init)
   (process :init-keyword :process :reader user-hash-algorithm-process)
   (done :init-keyword :done :reader user-hash-algorithm-done)
   (block-size :init-keyword :block-size :reader user-hash-algorithm-block-size)
   (hash-size :init-keyword :hash-size :reader user-hash-algorithm-hash-size)
   (oid :init-keyword :oid :reader user-hash-algorithm-oid)
   (state :init-keyword :state :reader user-hash-algorithm-state)
   ;; ...
   (digest :reader user-hash-algorithm-digest)
   ))
(define (user-hash-algorithm? o) (is-a? o <user-hash-algorithm>))
(define-method initialize :after ((o <user-hash-algorithm>) initargs)
  (define (safe-slot-ref o slot default)
    (if (slot-bound? o slot)
	(slot-ref o slot)
	default))
  (call-next-method)
  (let ((descriptor (make-digest-descriptor
		     (class-of o)
		     (let ((init (safe-slot-ref o 'init (lambda () o))))
		       (lambda () (init o)))
		     (safe-slot-ref o 'process (lambda (me bv . ignore) bv))
		     (safe-slot-ref o 'done (lambda (me out . ignore) out))
		     (safe-slot-ref o 'block-size #f)
		     (safe-slot-ref o 'hash-size #f)
		     (safe-slot-ref o 'oid #f))))
    (slot-set! o 'digest descriptor)
    o))

(define-class <builtin-hash-algorithm> (<hash-algorithm>)
  ((md :init-keyword :md :reader builtin-hash-algorithm-md)
   (digest :init-keyword :digest :reader builtin-hash-algorithm-digest)))
(define (builtin-hash-algorithm? o) (is-a? o <builtin-hash-algorithm>))

(define (hash-algorithm->digest-descriptor (hash hash-algorithm?))
  (if (builtin-hash-algorithm? hash)
      (builtin-hash-algorithm-digest hash)
      (user-hash-algorithm-digest hash)))

(define-generic lookup-hash)
(define-method lookup-hash (o) #f)
(define-syntax register-hash
  (syntax-rules ()
    ((_ name class)
     (let ()
       (define-method lookup-hash ((o (eql name))) class)))))

(define (hash-algorithm name . opts)
  (cond ((hash-algorithm? name) name) ;; for convenience
	((digest-descriptor? name)
	 (make <builtin-hash-algorithm> :md (make-message-digest name)
	       :digest name))
	((lookup-hash name) => (lambda (clazz) (apply make clazz opts)))
	(else
	 (assertion-violation 'hash-algorithm "unknown hash" name))))

(define (hash-init! (algorithm hash-algorithm?))
  (if (builtin-hash-algorithm? algorithm)
      (message-digest-init! (builtin-hash-algorithm-md algorithm))
      ((user-hash-algorithm-init algorithm) algorithm)))
(define (hash-process! (algorithm hash-algorithm?) (in bytevector?)
		       :optional (start 0) (end (bytevector-length in)))
  (if (builtin-hash-algorithm? algorithm)
      (message-digest-process! (builtin-hash-algorithm-md algorithm)
			       in start (- end start))
      ((user-hash-algorithm-process algorithm) algorithm in start end)))
(define (hash-done! (algorithm hash-algorithm?)
		    (out bytevector?)
		    :optional (start 0) (end (bytevector-length out)))
  (if (builtin-hash-algorithm? algorithm)
      (let ((size (hash-size algorithm)))
	(when (and size (< (- end start) size))
	  (assertion-violation 'hash-done!
			       "Output buffer doesn't have sufficient storage"
			       size (- end start)))
	(message-digest-done! (builtin-hash-algorithm-md algorithm) out
			    start (- end start)))
      ((user-hash-algorithm-done algorithm) algorithm out start end))
  out)

(define (hash-size algorithm)
  (if (hash-algorithm? algorithm)
      (if (builtin-hash-algorithm? algorithm)
	  (digest-descriptor-digest-size
	   (message-digest-descriptor (builtin-hash-algorithm-md algorithm)))
	  (user-hash-algorithm-hash-size algorithm))
      (hash-size (hash-algorithm algorithm))))

(define (hash-block-size algorithm)
  (if (hash-algorithm? algorithm)
      (if (builtin-hash-algorithm? algorithm)
	  (digest-descriptor-block-size
	   (message-digest-descriptor (builtin-hash-algorithm-md algorithm)))
	  (user-hash-algorithm-block-size algorithm))
      (hash-block-size (hash-algorithm algorithm))))

(define (hash-oid algorithm)
  (if (hash-algorithm? algorithm)
      (if (builtin-hash-algorithm? algorithm)
	  (digest-descriptor-oid
	   (message-digest-descriptor (builtin-hash-algorithm-md algorithm)))
	  (user-hash-algorithm-oid algorithm))
      (hash-oid (hash-algorithm algorithm))))

(define (get-size algo :key (size (hash-block-size algo)) :allow-other-keys)
  (define hsize (hash-size algo))
  (if (or (not hsize) (zero? hsize))
      size
      hsize))
  
(define (hash type bv . opts)
  (let* ((algo (if (hash-algorithm? type)
		   type
		   (apply hash-algorithm type opts)))
	 (out (make-bytevector (apply get-size algo opts))))
    (apply hash! algo out bv opts)))

(define (hash! type out bv . opts)
  (let* ((algo (if (hash-algorithm? type)
		   type
		   (apply hash-algorithm type opts)))
	 (size (hash-size algo)))
    (when (and size (> size 0) (< (bytevector-length out) size))
      (assertion-violation 'hash! "output buffer is too short"))
    (hash-init! algo)
    (hash-process! algo bv)
    (hash-done! algo out)
    out))

(define *oid/algorithm*
  (filter values
	  (map (lambda (name)
		 (let ((algo (hash-algorithm name)))
		   (cons (hash-oid algo) algo)))
	       (list *digest:whirlpool*
	    
		     *digest:ripemd-128* *digest:ripemd-160*
		     *digest:ripemd-256* *digest:ripemd-320*
		     
		     *digest:sha-1*
		     
		     *digest:sha-224* *digest:sha-256*
		     *digest:sha-384*
		     *digest:sha-512* *digest:sha-512/224* *digest:sha-512/256*
		     
		     *digest:sha3-224* *digest:sha3-256* *digest:sha3-384*
		     *digest:sha3-512*
		     
		     *digest:keccak-224* *digest:keccak-256* *digest:keccak-384*
		     *digest:keccak-512*
		     
		     *digest:tiger-192*
		     
		     *digest:md5* *digest:md4* *digest:md2*
		     
		     *digest:blake2s-128* *digest:blake2s-160*
		     *digest:blake2s-224* *digest:blake2s-256*
		     *digest:blake2b-160* *digest:blake2b-256*
		     *digest:blake2b-384* *digest:blake2b-512*
		     
		     *digest:shake-128* *digest:shake-256*))))
(define (oid->hash-algorithm oid)
  ;; we don't do user hash for now...
  (cond ((assoc oid *oid/algorithm*) => cdr)
	(else #f)))
  
)
