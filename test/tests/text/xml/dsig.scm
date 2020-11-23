(import (rnrs)
	(srfi :64 testing)
	(rfc base64)
	(rsa pkcs :10)
	(text xml dom)
	(text xml dom writer)
	(text xml dsig)
	(text xml xpath dm)
	(util file)
	(crypto)
	(math))

(define private-key
  "MIIBPAIBAAJBAM7xaDmTsYZj1ZxJOVpAkCXKp/2SmprG1IA90cGs4wr1fiCRWHQ+\
   sdJwiX2j932CW7DpjOg4GEn2CrPwWIQLfdkCAwEAAQJBAInnc5YS5xVwiBPq8+5B
   4g1dHE+tl5uW7ls7VwGijXZp6Mi7D+GJJ57w6wo1vzjGNIFUAs07+17XBRpPeqaW
   MVECIQDz2t+jH7zB/wSbf3titZtyRIaYGCiV20sb9Xc/56QWHQIhANk/6Ncem83E
   wJpJTS3r+QFgkPVhQF0VEZJ0bI7fDAntAiEAuStZqH/AELu6Xu2V3uWyjTl1zuaB
   YxHrXeauT8tw8Q0CIQDVjbMuM1JodO33O/L4HywIpIoaC10fouRBGNzVnH/TCQIg
   ZoOzTnUmv2X4DaxbH4kfBg5/9e/mwK8wLZy2gn+a2A0=")
(define public-key
  "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAM7xaDmTsYZj1ZxJOVpAkCXKp/2SmprG\
   1IA90cGs4wr1fiCRWHQ+sdJwiX2j932CW7DpjOg4GEn2CrPwWIQLfdkCAwEAAQ==")
(define signing-key
  (import-private-key RSA
   (base64-decode-string private-key :transcoder #f)))
(define verify-key
  (subject-public-key-info->public-key
   (import-public-key PKCS10
    (base64-decode-string public-key :transcoder #f))))

(define keypair (make-keypair signing-key verify-key))

(define (test-xml-signature file expected-sig . algos)
  (define dom (xml-file->dom-tree (build-path* (current-directory)
					       "test/data"
					      file)))
  (define sc (apply ds:make-signing-context "" algos))
  (define si (xmldsig:make-signed-info sc))

  (test-equal "Result DOM" dom (xmldsig:sign! dom si keypair))
  (let ((sig (document:get-elements-by-tag-name dom "SignatureValue")))
    (test-assert (node-list? sig))
    (test-equal 1 (node-list-length sig))
    (test-equal (base64-decode-string expected-sig)
		(base64-decode-string
		 (xpath-dm:string-value (node-list:item sig 0))))

    (test-assert "Self verify"
		 (xmldsig:verify dom (lambda (ki) verify-key)))))

(test-begin "XML signature")

(test-xml-signature
 "test1.xml"
 "KrNNBtxw4ppGVOCWWndW6INDexdXs5Ei1/GqiUFwofjwrGmKmEw4hrCLG7p86StJ5kfGHYncezvrexggfQSVZw=="
 *xmldsig:canonicalization-exc-c14n*
 *xmldsig:digest-sha256*
 *xmldsig:rsa-sha256*)

(test-xml-signature
 "test1.xml"
 "HVnC9BZBPDdlBBAsnAmgQ9Xkqs2c/PfWYqbVrxTp6/zDMUvQysASNK3qvv6MmQKk9y5HZ4aJVaBaCkasquxFGQ=="
 *xmldsig:canonicalization-c14n*
 *xmldsig:digest-sha256*
 *xmldsig:rsa-sha256*)

(test-xml-signature
 "test1.xml"
 "gtO//R5NtfeFDp5+U0z5gYEDLHElJqTOX/I8cfqMoatc2jj7suYkFdyGHBK4zFmSC039tV8biclvgxeNn67phQ=="
 *xmldsig:canonicalization-c14n*
 *xmldsig:digest-sha512*
 *xmldsig:rsa-sha256*)

;;; TODO test the rest of the algorithms...
(test-end)
