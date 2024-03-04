;; Making test vectors from wycheproof
(import (rnrs)
	(text json)
	(text json pointer)
	(util file)
	(util bytevector)
	(asn.1) ;; old...
	(srfi :13)
	(pp)
	(getopt))

(define ((file-prefix? prefix*) path)
  (let-values (((dir file ext) (decompose-path path)))
    (exists (lambda (prefix) (string-prefix? prefix file)) prefix*)))
(define signature-vector?
  (file-prefix? '("dsa" "ecdsa"
		  "rsa_pss" "rsa_sig"
		  "ed448" "eddsa")))
(define prime-vector? (file-prefix? '("primality_test")))
(define hmac-vector? (file-prefix? '("hmac")))
(define cmac-vector? (file-prefix? '("aes_cmac")))
(define gmac-vector? (file-prefix? '("gmac")))
(define hkdf-vector? (file-prefix? '("hkdf")))
(define chacha20-poly1305-vector? (file-prefix? '("chacha20_poly1305")))

;;; Signature
(define algorithm-pointer (json-pointer "/algorithm"))
(define test-groups-pointer (json-pointer "/testGroups"))

(define sha-pointer (json-pointer "/sha"))
(define key-der-pointer (json-pointer "/keyDer"))
(define tests-pointer (json-pointer "/tests"))
(define mgf-pointer (json-pointer "/mgf"))
(define mgf-sha-pointer (json-pointer "/mgfSha"))
(define slen-pointer (json-pointer "/sLen"))
(define type-pointer (json-pointer "/type"))
(define (->signature-test-runner source algorithm json)
  (define (test->vector test)
    (list->vector
     (map (lambda (e)
	    (let ((k (car e)))
	      (cond ((string=? k "msg") (hex-string->bytevector (cdr e)))
		    ((string=? k "sig") (hex-string->bytevector (cdr e)))
		    ((string=? k "result")
		     (or (string=? "valid" (cdr e))
			 (string=? "acceptable" (cdr e))))
		    (else (cdr e)))))
	  (vector->list test))))
  (define (mgf json)
    (let ((mgf (mgf-pointer json))
	  (mgf-sha (mgf-sha-pointer json))
	  (slen (slen-pointer json)))
      `(:mgf ,mgf
	:mgf-digest ,mgf-sha
	:salt-length ,slen)))
  (define (p1363? type) (string-contains type "P1363"))

  (let ((sha (sha-pointer json))
	(key-der (key-der-pointer json))
	(tests (tests-pointer json))
	(type (type-pointer json)))
    `(test-signature/testvector ,source
      :algorithm ,algorithm
      ,@(if (json-pointer-not-found? sha)
	    '()
	    `(:digest ,sha))
      :public-key ,(hex-string->bytevector key-der)
      ,@(if (string=? algorithm "RSASSA-PSS")
	    (mgf json)
	    '())
      :der-encode ,(not (p1363? type))
      :tests  ',(map test->vector tests))))

(define (->prime-test-runner source algorithm json)
  (define (test->vector test)
    (list->vector
     (map (lambda (e)
	    (let ((k (car e)))
	      (cond ((string=? k "value") (string->number (cdr e) 16))
		    ((string=? k "result")
		     (or (string=? "valid" (cdr e))
			 (string=? "acceptable" (cdr e))))
		    (else (cdr e)))))
	  (vector->list test))))
  (let ((tests (tests-pointer json)))
    `(test-prime ,source ',(map test->vector tests))))

(define key-size-pointer (json-pointer "/keySize"))
(define tag-size-pointer (json-pointer "/tagSize"))
(define ((->mac-test-runner test-mac) source algorithm json)
  (define (test->vector test)
    (list->vector
     (map (lambda (e)
	    (let ((k (car e)))
	      (cond ((string=? k "result") (string=? "valid" (cdr e)))
		    ((string=? k "key") (hex-string->bytevector (cdr e)))
		    ((string=? k "tag") (hex-string->bytevector (cdr e)))
		    ((string=? k "msg") (hex-string->bytevector (cdr e)))
		    (else (cdr e)))))
	  (vector->list test))))

  (let ((tests (tests-pointer json))
	(key-size (key-size-pointer json))
	(tag-size (tag-size-pointer json)))
    `(,test-mac ,source
      :algorithm ,algorithm
      :key-size ,key-size
      :tag-size ,tag-size
      :tests ',(map test->vector tests))))

(define (->gmac-test-runner source algorithm json)
  (define (test->vector test)
    (list->vector
     (map (lambda (e)
	    (let ((k (car e)))
	      (cond ((string=? k "result") (string=? "valid" (cdr e)))
		    ((string=? k "key") (hex-string->bytevector (cdr e)))
		    ((string=? k "tag") (hex-string->bytevector (cdr e)))
		    ((string=? k "iv")  (hex-string->bytevector (cdr e)))
		    ((string=? k "msg") (hex-string->bytevector (cdr e)))
		    (else (cdr e)))))
	  (vector->list test))))

  (let ((tests (tests-pointer json))
	(key-size (key-size-pointer json))
	(tag-size (tag-size-pointer json)))
    `(test-gmac ,source
      :algorithm ,algorithm
      :key-size ,key-size
      :tag-size ,tag-size
      :tests ',(map test->vector tests))))

(define (->hkdf-test-runner source algorithm json)
  (define (test->vector test)
    (list->vector
     (map (lambda (e)
	    (let ((k (car e)))
	      (cond ((string=? k "result") (string=? "valid" (cdr e)))
		    ((string=? k "ikm") (hex-string->bytevector (cdr e)))
		    ((string=? k "salt") (hex-string->bytevector (cdr e)))
		    ((string=? k "info") (hex-string->bytevector (cdr e)))
		    ((string=? k "okm") (hex-string->bytevector (cdr e)))
		    (else (cdr e)))))
	  (vector->list test))))

  (let ((tests (tests-pointer json))
	(key-size (key-size-pointer json))
	(tag-size (tag-size-pointer json)))
    `(test-hkdf ,source
      :algorithm ,algorithm
      :key-size ,key-size
      :tests ',(map test->vector tests))))

(define (->chacha20-poly-test-runner source algorithm json)
  (define (test->vector test)
    (list->vector
     (map (lambda (e)
	    (let ((k (car e)))
	      (cond ((string=? k "result") (string=? "valid" (cdr e)))
		    ((string=? k "key") (hex-string->bytevector (cdr e)))
		    ((string=? k "iv") (hex-string->bytevector (cdr e)))
		    ((string=? k "aad") (hex-string->bytevector (cdr e)))
		    ((string=? k "msg") (hex-string->bytevector (cdr e)))
		    ((string=? k "ct") (hex-string->bytevector (cdr e)))
		    ((string=? k "tag") (hex-string->bytevector (cdr e)))
		    (else (cdr e)))))
	  (vector->list test))))
  (let ((tests (tests-pointer json))
	(key-size (key-size-pointer json))
	(tag-size (tag-size-pointer json)))
    `(test-chacha20-poly1305 ,source
      :algorithm ,algorithm
      :key-size ,key-size
      :tests ',(map test->vector tests))))

(define ((test-vector->test-runner ->test-runner) json source)
  (define (filename source)
    (let-values (((dir f e) (decompose-path source)))
      f))
  (define source-name (filename source))
  (define ((make-test-runner source algorithm) json)
    (->test-runner source-name algorithm json))
  (print "Making testvector from " source)
  (let ((algorithm (algorithm-pointer json))
	(test-groups (test-groups-pointer json)))
    (map (make-test-runner source algorithm) test-groups)))

(define ((write-in base dir type) file&json*)
  (define outdir (build-path* base dir type))
  (unless (file-exists? outdir) (create-directory* outdir))
  (let-values (((d name e) (decompose-path (car file&json*))))
    (let ((file (build-path outdir (string-append name ".scm"))))
      (when (file-exists? file) (delete-file file))
      (call-with-output-file file
	(lambda (out)
	  (for-each (lambda (json) (pp json out)) (cdr file&json*))))
      (build-path* "." type (string-append name ".scm")))))

(define (write-includer outdir type files)
  (let ((file (build-path outdir (string-append type ".scm"))))
    (when (file-exists? file) (delete-file file))
    (call-with-output-file file
      (lambda (out)
	(for-each (lambda (f) (write `(include ,f) out) (newline out))
		  files)))))

(define ((file->json conv) file)
  (cons file (conv (call-with-input-file file json-read) file)))
(define ((write-test-vectors outdir) in-dir)
  (let ((files (find-files in-dir :recursive #f :pattern "\\.json$")))
    (write-includer outdir (build-path "testvectors" "signature")
     (map (write-in outdir "testvectors" "signature")
	  (map (file->json (test-vector->test-runner ->signature-test-runner))
	       (filter signature-vector? files))))
    (write-includer outdir (build-path "testvectors" "prime")
     (map (write-in outdir "testvectors" "prime")
	  (map (file->json (test-vector->test-runner ->prime-test-runner))
	       (filter prime-vector? files))))
    (write-includer outdir (build-path "testvectors" "hmac")
     (map (write-in outdir "testvectors" "hmac")
	  (map (file->json (test-vector->test-runner
			    (->mac-test-runner 'test-hmac)))
	       (filter hmac-vector? files))))
    (write-includer outdir (build-path "testvectors" "cmac")
     (map (write-in outdir "testvectors" "cmac")
	  (map (file->json (test-vector->test-runner
			    (->mac-test-runner 'test-cmac)))
	       (filter cmac-vector? files))))
    (write-includer outdir (build-path "testvectors" "gmac")
     (map (write-in outdir "testvectors" "gmac")
	  (map (file->json (test-vector->test-runner ->gmac-test-runner))
	       (filter gmac-vector? files))))
    (write-includer outdir (build-path "testvectors" "hkdf")
     (map (write-in outdir "testvectors" "hkdf")
	  (map (file->json (test-vector->test-runner ->hkdf-test-runner))
	       (filter hkdf-vector? files))))
    (write-includer outdir (build-path "testvectors" "chacha20-poly1305")
     (map (write-in outdir "testvectors" "chacha20-poly1305")
	  (map (file->json (test-vector->test-runner ->chacha20-poly-test-runner))
	       (filter chacha20-poly1305-vector? files))))))

(define (usage me)
  (print me "[OPTIONS] dir ...")
  (print "OPTIONS")
  (print " -h, --help")
  (print "  show this message and quit")
  (print " -o $OUTPUT, --output=$OUTPUT")
  (print "  specifying output directory [required]")
  (exit -1))

(define (main args)
  (with-args (cdr args)
      ((verbose (#\v "verbose") #f #f)
       (help?   (#\h "help") #f #f)
       (output  (#\o "output") #t #f)
       . dir)
    (when (or help? (null? dir) (not output)) (usage (car args)))
    (for-each (write-test-vectors output) dir)))
    

