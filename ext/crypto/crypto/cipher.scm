;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; cipher.scm Cryptographic library
;;; 
#!core
(library (crypto cipher)
    (export crypto-object?
	    cipher-keysize
	    cipher-blocksize
	    cipher-iv
	    cipher-update-aad!
	    cipher-tag! cipher-tag
	    cipher-max-tag-size
	    cipher?
	    make-cipher
	    cipher-encrypt
	    cipher-decrypt
	    cipher-signature
	    cipher-verify
	    key-check-value

	    cipher-encrypt/tag
	    cipher-decrypt/tag
	    cipher-decrypt/verify

	    ;; parameters
	    define-mode-parameter
	    make-composite-parameter mode-parameter?
	    <mode-name-parameter> make-mode-name-parameter mode-name-parameter?
	    parameter-mode-name
	    <iv-parameter> make-iv-parameter iv-parameter?
	    parameter-iv
	    <ctr-parameter> make-ctr-parameter ctr-parameter?
	    parameter-ctr-mode
	    <rfc3686-parameter> make-rfc3686-parameter rfc3686-parameter?
	    <padding-parameter> make-padding-parameter padding-parameter?
	    parameter-padder
	    <round-parameter> make-round-parameter round-parameter?
	    parameter-rounds
	    ;; TODO more?

	    ;; signing
	    ;; supported algorithms
	    Blowfish
	    X-Tea
	    RC2
	    RC5-32/12/b
	    RC6-32/20/b
	    SAFER+
	    SAFER-K64
	    SAFER-SK64
	    SAFER-K128
	    SAFER-SK128
	    AES
	    Twofish
	    DES
	    DES3
	    DESede
	    CAST5
	    CAST-128
	    Noekeon
	    Skipjack
	    Khazad
	    SEED
	    KASUMI
	    Camellia
	    ;; supported modes
	    MODE_ECB
	    MODE_CBC
	    MODE_CFB
	    MODE_OFB
	    MODE_CTR
	    MODE_GCM
	    ;; ctr conter mode
	    CTR_COUNTER_LITTLE_ENDIAN
	    CTR_COUNTER_BIG_ENDIAN
	    LTC_CTR_RFC3686

	    <crypto>
	    <cipher>
	    <cipher-spi>
	    <key>
	    <symmetric-key>
	    <asymmetric-key>
	    ;; for backward compatibility
	    cipher
	    (rename (cipher-encrypt encrypt)
		    (cipher-decrypt decrypt)
		    (cipher-signature sign)
		    (cipher-verify verify))
	    )
    (import (core)
	    (core base)
	    (core syntax)
	    (core inline)
	    (core record)
	    (crypto key)
	    (crypto pkcs)
	    (sagittarius)
	    (clos core)
	    (sagittarius crypto))

  ;; OK, the same idea as aeolus :)
  ;; just an interface
  (define-record-type (<mode-parameter> make-mode-parameter mode-parameter?))
  
  (define-record-type (<composite-parameter> %make-composite-parameter
					     composite-parameter?)
    (parent <mode-parameter>)
    (fields (immutable parameters parameter-composite-parameters)))

  (define-inline (make-composite-parameter . params)
    (unless (for-all mode-parameter? params)
      (assertion-violation 'make-composite-parameter
			   "mode-parameter is required" params))
    (let loop ((params params) (r '()))
      (cond ((null? params)
	     (%make-composite-parameter (reverse! r)))
	    ((composite-parameter? (car params))
	     (loop (cdr params) 
		   `(,@(reverse (parameter-composite-parameters (car params)))
		     . r)))
	    (else (loop (cdr params) (cons (car params) r))))))

  (define (find-parameter p pred)
    (cond ((composite-parameter? p)
	   (let loop ((parameters (parameter-composite-parameters p)))
	     (cond ((null? parameters) #f)
		   ((pred (car parameters)) (car parameters))
		   (else (loop (cdr parameters))))))
	  ((pred p) p)
	  (else #f)))

  (define-syntax define-mode-parameter
    (lambda (x)
      (define (make-ctr&pred k name)
	(let* ((s (symbol->string (syntax->datum name)))
	       (c (string->symbol (format "make-~a" s)))
	       (p (string->symbol (format "~a?" s))))
	  (datum->syntax k (list c p))))
      (syntax-case x ()
	((k (name ctr pred) specs ...)
	 #'(define-mode-aux (name ctr pred) specs ...))
	((k name specs ...)
	 (identifier? #'name)
	 (with-syntax (((ctr pred) (make-ctr&pred #'k #'name)))
	   #'(define-mode-parameter (name ctr pred) specs ...))))))

  (define-syntax define-mode-aux
    (lambda (x)
      (define (parse-fields k record-name pred fields)
	(define rn (symbol->string (syntax->datum record-name)))
	(define immutable #'immutable)
	(define (gen-get k name)
	  (let* ((s (symbol->string (syntax->datum name)))
		 ;; what ever is fine
		 (n (string->symbol (format "~a-~a" rn s))))
	    (datum->syntax k n)))
	(define (gen-acc k name get)
	  (define (ensure n) (datum->syntax k (syntax->datum n)))
	  (with-syntax ((ref get) (acc name) (pred pred))
	    ;; find-parameter should be in this scope but 
	    ;; get and acc must be defined scope
	    #'(define (acc o :optional (fallback #f))
		(cond ((find-parameter o pred) => ref)
		      (else fallback)))))
	    
	(let loop ((fields fields) (field* '()) (acc '()))
	  (syntax-case fields ()
	    ((name rest ...)
	     (identifier? #'name)
	     (let ((get (gen-get k #'name)))
	       (with-syntax ((dummy (datum->syntax k 'dummy0)))
		 (loop #'(rest ...) 
		       (cons #'(immutable name dummy) field*)
		       (cons (gen-acc k get #'dummy) acc)))))
	    (((name get) rest ...)
	     (with-syntax ((dummy (datum->syntax k 'dummy1)))
	       (loop #'(rest ...) 
		     (cons #'(immutable name dummy) field*)
		     (cons (gen-acc k #'get #'dummy) acc))))
	    (() (datum->syntax k (list (reverse! field*) (reverse! acc)))))))

      ;; we need to find them
      (syntax-case x (fields parent protocol <mode-parameter>)
	((k (name ctr pred) specs ...)
	 #'(define-mode-aux "parse" (name ctr pred)
	     () (parent <mode-parameter>) (protocol #f) (specs ...)))
	;; fields
	((k "parse" (name ctr pred) () (parent* ...) (protocol* ...)
	    ((fields field* ...) rest ...))
	 (with-syntax ((((field* ...) (acc ...))
			(parse-fields #'k #'name #'pred #'(field* ...))))
	   #'(define-mode-aux "parse" (name ctr pred)
	       ((fields field* ...) acc ...) (parent* ...) (protocol* ...)
	       (rest ...))))
	;; parent
	((k "parse" (name ctr pred) (fields* ...)
	    (parent <mode-parameter>)
	    (protocol* ...)
	    ((parent p) rest ...))
	 #'(define-mode-aux "parse" (name ctr pred)
	     (fields* ...) (parent p) (protocol* ...)
	     (rest ...)))
	;; protocol
	((k "parse" (name ctr pred) (fields* ...) (parent* ...) (protocol #f)
	    ((protocol p) rest ...))
	 #'(define-mode-aux "parse" (name ctr pred)
	     (fields* ...) (parent* ...) (protocol p)
	     (rest ...)))
	;; done
	((k "parse" (name ctr pred) 
	    () ;; nofield
	    (parent* ...) (protocol* ...)
	    ())
	 #'(begin
	     (define-record-type (name ctr real-pred)
	       (parent* ...)
	       (protocol* ...))
	     (define (pred o)
	       (and (mode-parameter? o)
		    (or (real-pred o)
			(and (find-parameter o real-pred) #t))))))
	((k "parse" (name ctr pred) 
	    ((field* ...) acc ...)
	    (parent* ...) (protocol* ...)
	    ())
	 #'(begin
	     (define-record-type (name ctr %real-pred)
	       (field* ...)
	       (parent* ...)
	       (protocol* ...))
	     acc ...
	     (define (pred o)
	       (and (mode-parameter? o)
		    (or (%real-pred o)
			(and (find-parameter o %real-pred) #t)))))))))

  (define-mode-parameter (<mode-name-parameter> make-mode-name-parameter
						mode-name-parameter?)
    (fields (mode-name parameter-mode-name)))

  (define-mode-parameter (<iv-parameter> make-iv-parameter iv-parameter?)
    (fields (iv parameter-iv)))
  (define-mode-parameter (<padding-parameter> make-padding-parameter
					      padding-parameter?)
    (fields (padder parameter-padder)))
  (define-mode-parameter (<ctr-parameter> make-ctr-parameter ctr-parameter?)
    (fields (mode   parameter-ctr-mode))
    (parent <iv-parameter>)
    (protocol (lambda (p)
		(lambda (iv :optional (mode CTR_COUNTER_BIG_ENDIAN))
		  ((p iv) mode)))))
  (define-mode-parameter (<rfc3686-parameter> make-rfc3686-parameter 
					      rfc3686-parameter?)
    (parent <ctr-parameter>)
    (protocol (lambda (p)
		(lambda (iv nonce :optional (mode CTR_COUNTER_BIG_ENDIAN))
		  (let ((v (make-bytevector 16 0))
			(nlen (bytevector-length nonce))
			(ivlen (bytevector-length iv)))
		    (if (= mode CTR_COUNTER_BIG_ENDIAN)
			;; NONCE || IV || ONE = 16
			(begin
			  (bytevector-copy! nonce 0 v 0 nlen)
			  (bytevector-copy! iv 0 v nlen ivlen))
			;; ONE || IV || NONCE (i guess)
			(begin
			  (bytevector-copy! iv 0 v 4 ivlen)
			  (bytevector-copy! nonce 0 (+ 4 ivlen) nlen)))
		    ;; let it do libtomcrypt
		    ((p v (+ mode LTC_CTR_RFC3686))))))))

  (define-mode-parameter (<round-parameter> make-round-parameter
					    round-parameter?)
    (fields (rounds parameter-rounds)))

  (define (cipher-keysize cipher test)
    (unless (cipher? cipher)
      (assertion-violation 'cipher-keysize
			   (format "cipher required but got ~s" cipher)))
    (suggest-keysize cipher test))

  (define (cipher type key 
		  :key (mode MODE_ECB)
		  (iv #f)
		  (padder pkcs5-padder)
		  (rounds 0)
		  (ctr-mode CTR_COUNTER_BIG_ENDIAN)
		  :allow-other-keys
		  :rest rest)
    (define (rfc3686?)
      (not (zero? (bitwise-and ctr-mode LTC_CTR_RFC3686))))
    (apply make-cipher type key 
	   :mode-parameter (make-composite-parameter
			    (make-mode-name-parameter mode)
			    (make-padding-parameter padder)
			    (make-round-parameter rounds)
			    (if (rfc3686?)
				;; should we add nonce?
				(make-rfc3686-parameter iv #vu8(0 0 0 0)
							ctr-mode)
				(make-ctr-parameter iv  ctr-mode)))
	   rest))

  (define (make-cipher type key
		       :key (mode-parameter #f)
		       :allow-other-keys
		       :rest rest)
    (define parameter mode-parameter)
    ;; kinda silly but for now
    (let ((mode (or (and parameter (parameter-mode-name parameter MODE_ECB))
		     MODE_ECB))
	  (iv (and parameter (parameter-iv parameter)))
	  ;; these 2 doesn't have to be there but
	  ;; make-builtin-cipher-spi requires it.
	  ;; TODO better construction
	  (rounds (or (and parameter (parameter-rounds parameter 0)) 0))
	  (ctr-mode (or (and parameter 
			     (parameter-ctr-mode parameter
						 CTR_COUNTER_BIG_ENDIAN))
			CTR_COUNTER_BIG_ENDIAN))
	  (padder (and parameter (parameter-padder parameter))))
      (unless (or (= mode MODE_ECB) (bytevector? iv))
	(assertion-violation 'cipher
			     "on the given mode iv id required"))
      (let ((spi (cond ((lookup-cipher-spi type)
			=> (lambda (spi)
			     (if (boolean? spi)
				 (make-builtin-cipher-spi
				  type mode key iv rounds padder ctr-mode)
				 (apply make spi key 
					:mode-parameter parameter
					rest))))
		       ((cipher-spi? type) type) ;; reuse 
		       (else
			(assertion-violation 'cipher
					     "unknown cipher type" type)))))
	(create-cipher spi))))

  (define-constant +check-value+ (make-bytevector 8 0))
  (define (key-check-value type key :optional (size 3))
    (unless (<= 3 size 8)
      (assertion-violation 'key-check-value "size must be between 3 to 8"
			   size))
    (let ((c (make-cipher type key)))
      (bytevector-copy (cipher-encrypt c +check-value+) 0 size)))

  ;; with authentication
  (define (cipher-tag cipher :key (size (cipher-max-tag-size cipher)))
    (let ((tag (make-bytevector size)))
      (cipher-tag! cipher tag)
      tag))

  (define (cipher-encrypt/tag cipher data
			      :key (tag-size (cipher-max-tag-size cipher)))
    (let ((encrypted (cipher-encrypt cipher data)))
      (values encrypted (cipher-tag cipher :size tag-size))))
  (define (cipher-decrypt/tag cipher data
			      :key (tag-size (cipher-max-tag-size cipher)))
    (let ((pt (cipher-decrypt cipher data)))
      (values pt (cipher-tag cipher :size tag-size))))
  (define (cipher-decrypt/verify cipher encrypted tag)
    (let-values (((pt target)
		  (cipher-decrypt/tag cipher encrypted
				      :tag-size (bytevector-length tag))))
      (unless (bytevector=? tag target)
	(raise-decrypt-error (slot-ref cipher 'name)
			     'cipher-decrypt/verify
			     "invalid tag is given"))
      pt))

)
