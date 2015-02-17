(import (rnrs)
	(security keystore)
	(rfc base64)
	(rfc x.509)
	(crypto)
	(srfi :19)
	(srfi :64))

;; the same as PKCS#12 test ...
(define test-p12
  (base64-decode-string
   (string-append
    "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"
    "HLhcYQcQ1R+RucctAgIEAAAA")
   :transcoder #f))

(test-begin "Generic keystore APIs")

(let ((in (open-bytevector-input-port test-p12)))
  (test-assert "keystore? (1)" (make-keystore 'pkcs12))
  (test-assert "keystore? (2)" (keystore? (load-keystore 'pkcs12 in "test"))))

(let* ((in (open-bytevector-input-port test-p12))
       (keystore (load-keystore 'pkcs12 in "test")))
  (test-assert "keystore-get-key (pkcs12)"
	       (private-key? (keystore-get-key keystore "ca" "test")))
  (test-assert "keystore-get-certificate (pkcs12)"
	       (x509-certificate?
		(keystore-get-certificate keystore "ca")))
  
  (let ((file "test.p12"))
    (when (file-exists? file) (delete-file file))
    ;; test storing, we can put different password now
    (store-keystore-to-file keystore file "test2")
    (let ((ks (load-keystore-file 'pkcs12 file "test2")))
      (test-assert "keystore-get-key"
		   (private-key? (keystore-get-key ks "ca" "test")))
      (test-assert "keystore-get-certificate"
		   (x509-certificate?
		    (keystore-get-certificate ks "ca"))))
    (delete-file file)
    ))

(define (test-generic type file)
  (let* ((ks (make-keystore type))
	 (keypair (generate-key-pair RSA))
	 (keypair2 (generate-key-pair RSA)))
    (define cert (make-x509-basic-certificate keypair 0
		   (make-x509-issuer 
		    '((C . "foo")
		      (O . "bar")))
		   (make-validity (current-date)
				  (current-date))
		   (make-x509-issuer '((DN . "buzz")))))
    ;; chain certs
    (define cert2 (make-x509-basic-certificate keypair2 0
		    (make-x509-issuer 
		     '((C . "foo2")
		       (O . "bar2")))
		    (make-validity (current-date)
				   (current-date))
		    (make-x509-issuer '((DN . "buzz2")))))
    (test-assert (format "store key ~a" type)
		 (keystore-set-key! ks "key" 
				    (keypair-private keypair)
				    "pass"
				    (list cert cert2)))

    (test-error (format "store key without cert ~a" type)
		condition?
		(keystore-set-key! ks "key" (keypair-private keypair) 
				   "pass" '()))
    
    (test-assert (format "store cert ~a" type)
		 (keystore-set-certificate! ks "cert" cert))

    (test-assert (format "chain ~a" type)
		 (not (null? (keystore-get-certificate-chain ks "cert"))))

    (when (file-exists? file) (delete-file file))
    ;; test storing, we can put different password now
    (store-keystore-to-file ks file "test3")
    (let ((ks (load-keystore-file type file "test3")))
      (test-assert (format "keystore-get-key ~a" type)
		   (private-key? (keystore-get-key ks "key" "pass")))
      (test-assert (format "keystore-get-certificate ~a" type)
		   (x509-certificate?
		    (keystore-get-certificate ks "cert"))))
    (delete-file file)
    ))
(test-generic 'pkcs12 "test.p12")
(test-generic 'jks "test.jks")
(test-generic 'jceks "test.jceks")

;; direct keystore APIs
(import (security keystore jks)
	(security keystore jceks))

(let ()
  (define ks
    (call-with-input-file (string-append (current-directory)
					 "/test/data/test.jks")
      (lambda (in) (load-jks-keystore in "test"))
      :transcoder #f))

  (test-assert "JKS get key"
	       (private-key? (jks-keystore-get-key ks "priv-key" "pass")))

  (let* ((keypair (generate-key-pair RSA))
	 (keypair2 (generate-key-pair RSA)))
    (define cert (make-x509-basic-certificate keypair 0
		   (make-x509-issuer 
		    '((C . "foo")
		      (O . "bar")))
		   (make-validity (current-date)
				  (current-date))
		   (make-x509-issuer '((DN . "buzz")))))
    ;; chain certs
    (define cert2 (make-x509-basic-certificate keypair2 0
		    (make-x509-issuer 
		     '((C . "foo2")
		       (O . "bar2")))
		    (make-validity (current-date)
				   (current-date))
		    (make-x509-issuer '((DN . "buzz2")))))
    (test-assert "JKS set key"
		 (jks-keystore-set-key! ks "key" 
					(keypair-private keypair)
					"pass"
					(list cert cert2)))

    (test-assert "JKS get key(2)" 
		 (private-key? (jks-keystore-get-key ks "key" "pass")))

    (test-assert "JKS get certificate"
		 (x509-certificate? (jks-keystore-get-certificate ks "cert")))
    
    (test-assert "JKS get chain" 
		 (not (null? 
		       (jks-keystore-get-certificate-chain ks "priv-key"))))

    (let ((file "test.jks"))
      (when (file-exists? file) (delete-file file))
      ;; test storing, we can put different password now
      (store-jks-keystore-to-file ks file "test3")
      (let ((ks (load-jks-keystore-file file "test3")))
	(test-assert "keystore-get-key"
		     (private-key? (jks-keystore-get-key ks "key" "pass")))
	(test-assert "keystore-get-certificate"
		     (x509-certificate?
		      (jks-keystore-get-certificate ks "cert"))))
      (delete-file file)))
)

(define (test-jceks file)
  (define ks
    (call-with-input-file (string-append (current-directory) "/test/data/" file)
      (lambda (in) (load-jceks-keystore in "test"))
      :transcoder #f))

  (test-assert "JCEKS get key"
	       (private-key? (jceks-keystore-get-key ks "priv-key" "pass")))

  (let* ((keypair (generate-key-pair RSA))
	 (keypair2 (generate-key-pair RSA)))
    (define cert (make-x509-basic-certificate keypair 0
		   (make-x509-issuer 
		    '((C . "foo")
		      (O . "bar")))
		   (make-validity (current-date)
				  (current-date))
		   (make-x509-issuer '((DN . "buzz")))))
    ;; chain certs
    (define cert2 (make-x509-basic-certificate keypair2 0
		    (make-x509-issuer 
		     '((C . "foo2")
		       (O . "bar2")))
		    (make-validity (current-date)
				   (current-date))
		    (make-x509-issuer '((DN . "buzz2")))))

    (test-assert "JCEKS set key"
		 (jceks-keystore-set-key! ks "key" 
					(keypair-private keypair)
					"pass"
					(list cert cert2)))

    (test-assert "JCEKS get key(2)" 
		 (private-key? (jceks-keystore-get-key ks "key" "pass")))

    (test-assert "JCEKS get certificate"
		 (x509-certificate? (jceks-keystore-get-certificate ks "cert")))
    
    (test-assert "JCEKS get chain" 
		 (not (null? 
		       (jceks-keystore-get-certificate-chain ks "priv-key"))))

    (when (file-exists? file) (delete-file file))
    ;; test storing, we can put different password now
    (store-jceks-keystore-to-file ks file "test3")
    (let ((ks (load-jceks-keystore-file file "test3")))
      (test-assert "keystore-get-key"
		   (private-key? (jceks-keystore-get-key ks "key" "pass")))
      (test-assert "keystore-get-certificate"
		   (x509-certificate?
		    (jceks-keystore-get-certificate ks "cert"))))
    (delete-file file)))

(test-jceks "test.jceks")
;; can read this as well
(test-jceks "test.jks")

(test-end)
