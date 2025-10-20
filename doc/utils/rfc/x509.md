[§2] (rfc x509) - X.509 certificate utility library {#rfc.x509}
-------------

This library does not support whole feature of X.509, it just parse and verify
a message and signature. So it can not verify certificate itself.

If you need more functionality, check 
[`(sagittarius crypto x509)`](#sagittarius.crypto.x509) library.

###### [!Library] `(rfc x509)` 

Exports X.509 utility procedures.

###### [!Generic] `make-x509-certificate`  _(in_ _<port>)_
###### [!Generic] `make-x509-certificate`  _(sequence_ _<asn.1-sequence>)_

Creates an X.509 certificate object from given binary input port or ASN.1
sequence object (second form).


###### [!Function] `x509-certificate?`  _o_

Return #t if the _o_ is X.509 certificate object, otherwise #f.

###### [!Function] `x509-certificate-get-version`  _x509_

Return version of given X.509 certificate object.

###### [!Function] `x509-certificate-get-serial-number`  _x509_

Return serial number of given X.509 certificate object.

###### [!Function] `x509-certificate-get-issuer-dn`  _x509_

Return issuer DN of given X.509 certificate object as X.509 principal.

###### [!Function] `x509-certificate-get-subject-dn`  _x509_

Return subject DN of given X.509 certificate object as X.509 principal.

NOTE: These Issuer DN and Subject DN getters return \<x.509-principal> object,
however I did not implement any utility for this class, so it's just useless
for now.


###### [!Function] `x509-certificate-get-not-before`  _x509_

Return start date of given X.509 certificate object.

###### [!Function] `x509-certificate-get-not-after`  _x509_

Return end date of given X.509 certificate object.

###### [!Function] `x509-certificate-get-signature`  _x509_

Return signature of given X.509 certificate object. 

NOTE: This signature is not for `verify` described below.


###### [!Function] `x509-certificate-get-signature-algorithm`  _x509_

Return signature algorithm of given X.509 certificate object as an OID
string.

###### [!Function] `x509-certificate-get-public-key`  _x509_

Return public key of given X.509 certificate object. The return value is
`<public-key>` described in the section
[Key library - (sagittarius crypto keys)](#sagittarius.crypto.keys).


###### [!Function] `verify`  _x509_ _message_ _signature_ . ignore  **[@deprecated]**

_message_ and _signature_ must be bytevector.

Verify given _message_ with _signature_ and _x509_ certificate.


###### [!Function] `check-validity`  _x509_ _:optional_ _(date_ _(current-date))_

Validate if the given certificate is valid in given _date_. Return #t
if it's valid, otherwise raises `&assertion`.


