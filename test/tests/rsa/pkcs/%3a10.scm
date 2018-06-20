(import (rnrs)
	(crypto)
	(rsa pkcs :10)
	(clos user)
	(rfc pem)
	(asn.1)
	(srfi :64))

(test-begin "PKCS 10")

(let ()
  (define key-pem
    "-----BEGIN PUBLIC KEY-----
MIIBHzANBgkqhkiG9w0BAQEFAAOCAQwAMIIBBwKBgQMwO3kPsUnaNAbUlaubn7ip
4pNEXjvUOxjvLwUhtybr6Ng4undLtSQPCPf7ygoUKh1KYeqXMpTmhKjRos3xioTy
23CZuOl3WIsLiRKSVYyqBc9d8rxjNMXuUIOiNO38ealcR4p44zfHI66INPuKmTG3
RQP/6p5hv1PYcWmErEeDewKBgGEXxgRIsTlFGrW2C2JXoSvakMCWD60eAH0W2PpD
qlqqOFD8JA5UFK0roQkOjhLWSVu8c6DLpWJQQlXHPqP702qIg/gx2o0bm4EzrCEJ
4gYo6Ax+U7q6TOWhQpiBHnC0ojE8kUoqMhfALpUaruTJ6zmj8IA1e1M6bMqVF8sr
lb/N
-----END PUBLIC KEY-----")
  (let-values (((param content) (parse-pem-string key-pem)))
    (let* ((spki (import-public-key PKCS10 content))
	   (bv (asn.1-encode (asn.1-encodable->asn.1-object spki))))
      (test-assert (subject-public-key-info? spki))
      (test-assert (public-key? (subject-public-key-info->public-key spki)))
      (test-assert (is-a? (subject-public-key-info->public-key spki)
			  <rsa-public-key>))
      (test-equal bv (export-public-key spki)))))

(test-end)
