(import (rnrs)
	(rfc oauth signature)
	(util bytevector)
	(rsa pkcs :8)
	(rfc base64)
	(rfc x.509)
	(srfi :64))

(test-begin "OAuth 1.0")

(test-group "HMAC-SHA1 Signature"
  ;; from https://dev.twitter.com/oauth/overview/creating-signatures
  (define secret
    "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw&LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE")
  (define base-string
    "POST&https%3A%2F%2Fapi.twitter.com%2F1%2Fstatuses%2Fupdate.json&include_entities%3Dtrue%26oauth_consumer_key%3Dxvz1evFS4wEEPTGEFPHBog%26oauth_nonce%3DkYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1318622958%26oauth_token%3D370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb%26oauth_version%3D1.0%26status%3DHello%2520Ladies%2520%252B%2520Gentlemen%252C%2520a%2520signed%2520OAuth%2520request%2521")
  (let-values (((signature-generator extract)
		(make-oauth-hmac-sha1-signer #f (string->utf8 secret))))
    (signature-generator (string->utf8 base-string))
    (test-equal "tnnArxj06cWHq44gCs1OSKk/jLY=" (extract))
    (signature-generator (string->utf8 base-string))
    (test-equal "tnnArxj06cWHq44gCs1OSKk/jLY=" (extract))
    
    (let ((in (open-bytevector-input-port (string->utf8 base-string))))
      (let loop ()
	(let ((bv (get-bytevector-n in 64)))
	  (unless (eof-object? bv) (signature-generator bv) (loop))))
      (test-equal "tnnArxj06cWHq44gCs1OSKk/jLY=" (extract)))

    (let ((verifier (make-oauth-hmac-sha1-verifier (string->utf8 secret))))
      (test-assert (verifier (string->utf8 base-string)
			     "tnnArxj06cWHq44gCs1OSKk/jLY=")))))

(test-group "RSA-SHA1 Signature"
  ;; from http://wiki.oauth.net/w/page/12238556/TestCases
  (define private-key-info
    (make-private-key-info (base64-decode-string
    "MIICdgIBADANBgkqhkiG9w0BAQEFAASCAmAwggJcAgEAAoGBALRiMLAh9iimur8V
     A7qVvdqxevEuUkW4K+2KdMXmnQbG9Aa7k7eBjK1S+0LYmVjPKlJGNXHDGuy5Fw/d
     7rjVJ0BLB+ubPK8iA/Tw3hLQgXMRRGRXXCn8ikfuQfjUS1uZSatdLB81mydBETlJ
     hI6GH4twrbDJCR2Bwy/XWXgqgGRzAgMBAAECgYBYWVtleUzavkbrPjy0T5FMou8H
     X9u2AC2ry8vD/l7cqedtwMPp9k7TubgNFo+NGvKsl2ynyprOZR1xjQ7WgrgVB+mm
     uScOM/5HVceFuGRDhYTCObE+y1kxRloNYXnx3ei1zbeYLPCHdhxRYW7T0qcynNmw
     rn05/KO2RLjgQNalsQJBANeA3Q4Nugqy4QBUCEC09SqylT2K9FrrItqL2QKc9v0Z
     zO2uwllCbg0dwpVuYPYXYvikNHHg+aCWF+VXsb9rpPsCQQDWR9TT4ORdzoj+Nccn
     qkMsDmzt0EfNaAOwHOmVJ2RVBspPcxt5iN4HI7HNeG6U5YsFBb+/GZbgfBT3kpNG
     WPTpAkBI+gFhjfJvRw38n3g/+UeAkwMI2TJQS4n8+hid0uus3/zOjDySH3XHCUno
     cn1xOJAyZODBo47E+67R4jV1/gzbAkEAklJaspRPXP877NssM5nAZMU0/O/NGCZ+
     3jPgDUno6WbJn5cqm8MqWhW1xGkImgRk+fkDBquiq4gPiT898jusgQJAd5Zrr6Q8
     AO/0isr/3aa6O6NLQxISLKcPDk2NOccAfS/xOtfOz4sJYM3+Bs4Io9+dZGSDCA54
     Lw03eHTNQghS0A=="
    :transcoder #f)))
  (define x509-certificate
    (make-x509-certificate
     (base64-decode-string
      "MIIBpjCCAQ+gAwIBAgIBATANBgkqhkiG9w0BAQUFADAZMRcwFQYDVQQDDA5UZXN0
       IFByaW5jaXBhbDAeFw03MDAxMDEwODAwMDBaFw0zODEyMzEwODAwMDBaMBkxFzAV
       BgNVBAMMDlRlc3QgUHJpbmNpcGFsMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKB
       gQC0YjCwIfYoprq/FQO6lb3asXrxLlJFuCvtinTF5p0GxvQGu5O3gYytUvtC2JlY
       zypSRjVxwxrsuRcP3e641SdASwfrmzyvIgP08N4S0IFzEURkV1wp/IpH7kH41Etb
       mUmrXSwfNZsnQRE5SYSOhh+LcK2wyQkdgcMv11l4KoBkcwIDAQABMA0GCSqGSIb3
       DQEBBQUAA4GBAGZLPEuJ5SiJ2ryq+CmEGOXfvlTtEL2nuGtr9PewxkgnOjZpUy+d
       4TvuXJbNQc8f4AMWL/tO9w0Fk80rWKp9ea8/df4qMq5qlFWlx6yOLQxumNOmECKb
       WpkUQDIDJEoFUzKMVuJf4KO/FJ345+BNLGgbJ6WujreoM1X/gYfdnJ/J"
      :transcoder #f)))
  (define private-key (pki->private-key private-key-info))
  (define public-key (x509-certificate-get-public-key x509-certificate))
  (define base-string
    "GET&http%3A%2F%2Fphotos.example.net%2Fphotos&file%3Dvacaction.jpg%26oauth_consumer_key%3Ddpf43f3p2l4k3l03%26oauth_nonce%3D13917289812797014437%26oauth_signature_method%3DRSA-SHA1%26oauth_timestamp%3D1196666512%26oauth_version%3D1.0%26size%3Doriginal")
  (define signature "jvTp/wX1TYtByB1m+Pbyo0lnCOLIsyGCH7wke8AUs3BpnwZJtAuEJkvQL2/9n4s5wUmUl4aCI4BwpraNx4RtEXMe5qg5T1LVTGliMRpKasKsW//e+RinhejgCuzoH26dyF8iY2ZZ/5D1ilgeijhV/vBka5twt399mXwaYdCwFYE=")
  (let-values (((signature-generator extract)
		(make-oauth-rsa-sha1-signer #f private-key)))
    (signature-generator (string->utf8 base-string))
    (test-equal signature (extract))
    (signature-generator (string->utf8 base-string))
    (test-equal signature (extract))
    
    (let ((in (open-bytevector-input-port (string->utf8 base-string))))
      (let loop ()
	(let ((bv (get-bytevector-n in 64)))
	  (unless (eof-object? bv) (signature-generator bv) (loop))))
      (test-equal signature (extract)))
    
    (let ((verifier (make-oauth-rsa-sha1-verifier public-key)))
      (test-assert (verifier (string->utf8 base-string) signature)))))

(test-end)
