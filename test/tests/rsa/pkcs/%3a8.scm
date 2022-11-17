(import (rnrs)
	(srfi :64)
	(rfc base64)
	(crypto)
	(clos user)
	(asn.1)
	(rsa pkcs :8))

(test-begin "PKCS 8")

(define (bv->asn.1 bv) (read-asn.1-object (open-bytevector-input-port bv)))

(test-group "EC Private key"
  (define key
    (base64-decode-string
     "MIIBMAIBADCB0wYHKoZIzj0CATCBxwIBATAkBgcqhkjOPQEBAhkA////////////
      /////////v//////////MEsEGP////////////////////7//////////AQYIhI9
      wjlaBcqnQj2uzMlHYKfUYiVr1WkWAxUAxGloRDXes3jEtlypWR4qV2MFmi4EMQR9
      KXeBAMZaHaF4NxZYjc4ri0rujiKPGJY4qQ8iY3M3M0tJ3LZqbcj5l4rKdkipQ7AC
      GQD///////////////96YtAxyD9ClPZA7BMCAQEEVTBTAgEBBBiKtwssqrxHY/gu
      KDD4QgmyLDKaqBv2wEWhNAMyAAT5j6o+ojeB6jaFAfx4rtGf5hYbT1N6NnlAWiP1
      +bEWtTJiEVqnpeZN0m0SLybIGZY="
     :transcoder #f))
  (let ((pki (import-private-key PKCS8 key)))
    (test-assert (private-key-info? pki))
    (test-assert (private-key? (private-key-info->private-key pki)))
    (test-assert (is-a? (private-key-info->private-key pki)
			<ecdsa-private-key>))
    (test-equal key (export-private-key pki))
    #;(test-equal key
		(export-private-key
		 (make-private-key-info (private-key-info->private-key pki))))))

(test-group "EdDSA Private key"
  (define key
    (base64-decode-string
     "MHICAQEwBQYDK2VwBCIEINTuctv5E1hK1bbY8fdp+K06/nwoy/HU++CXqI9EdVhC\
      oB8wHQYKKoZIhvcNAQkJFDEPDA1DdXJkbGUgQ2hhaXJzgSEAGb9ECWmEzf6FQbrB\
      Z9w7lshQhqowtrbLDFw4rXAxZuE="
     :transcoder #f))

  (let ((pki (import-private-key PKCS8 key)))
    (test-assert "EdDSA private-key-info?" (one-asymmetric-key? pki))
    (test-assert "EdDSA private-key?"
		 (private-key? (private-key-info->private-key pki)))
    (test-assert "EdDSA is-a?" (is-a? (private-key-info->private-key pki)
				      <eddsa-private-key>))
    (test-equal "EdDSA export" key (export-private-key pki))
    ;; the below can't be achieved as we don't hold attributes
    ;; and the test data from RFC contains attributes...
    #;(test-equal "EdDSA export(2)" key
		(export-private-key
		 (make-private-key-info (private-key-info->private-key pki))))))

(test-end)
