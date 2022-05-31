[§2] (security keystore) - Generic Keystore Access {#security.keystore}
-------------

###### [!Library] `(security keystore)` 

This library provides generic access for variety of keystores.

The following example shows how to use this library:

``````````scheme
(import (security keystore))

(define keystore (load-keystore 'jks "keystore.jks" "storepass"))

(keystore-get-key keystore "key-name" "key-pass")
;; -> <private-key>

(keystore-get-certificate keystore "cert-name")
;; -> <x509-certificate>

;; certs must be a list of certificates
(keystore-set-key! keystore "key-name2" private-key "key-pass2" certs)

(keystore-set-certificate! keystore "cert-name2" cert)

(store-keystore-to-file keystore "keystore2.jks" "storepass2")
``````````

### [§3] Generic APIs

###### [!Class] `<keystore>` 

Base class of keystores.

###### [!Function] `keystore?`  _obj_

Returns #t if given _obj_ is a keystore object. Otherwise #f.

###### [!Function] `load-keystore`  _type_ _input-port_ _storepass_
###### [!Function] `load-keystore-file`  _type_ _file_ _storepass_

Loads _type_ keystore from _input-port_ or _file_.

_storepass_ must be a string and may or may not be used to decrypt 
keystore content.

`load-keystore` loads from given binary input port _input-port_.

`load-keystore-file` loads from given file _file_.


###### [!Function] `make-keystore`  _type_

Returns newly created _type_ keystore object.

Currently `pkcs12`, `jks` and `jceks` are supported.


###### [!Generic] `keystore-get-key`  _keystore_ _alias_ _keypass_

The method shall return a private key associated with _alias_from _keystore_. If there is no key entry associated with _alias_ 
then #f shall be returned.

_alias_ shall be a string.

_keypass_ shall be a string. It may or may not be used to decrypt 
the private key.


###### [!Generic] `keystore-get-certificate`  _keystore_ _alias_

The method shall return an X509 certificate associated with _alias_from _keystore_. If there is no certificate entry associated with 
_alias_ then #f shall be returned.

_alias_ shall be a string.


###### [!Generic] `keystore-get-certificate-chain`  _keystore_ _alias_

The method shall return a list of key certificates associated 
with _alias_ from _keystore_. If there is no certificate entries
associated with _alias_ then #f shall be returned.

_alias_ shall be a string.


###### [!Generic] `keystore-get-creation-date`  _keystore_ _alias_

The method shall return a date object of _alias_ associated entry.
If there is no entry associated with _alias_ then #f shall be returned.

_alias_ shall be a string.


###### [!Generic] `keystore-contains-alias?`  _keystore_ _alias_

The method shall return #t if _keystore_ contains an entry associated
with _alias_. Otherwise #f shall be returend.


###### [!Generic] `store-keystore`  _keystore_ _output-port_ _storepass_
###### [!Generic] `store-keystore-to-file`  _keystore_ _file_ _storepass_

The methods shall write _keystore_ to _output-port_ or 
_file_.

The `store-keystore` shall write to given binary output port 
_output-port_.

The `store-keystore-to-file` shall write to given file _file_.

_storepass_ shall be a string and may or may not be used to encrypt 
whole contents.


###### [!Generic] `keystore-set-key!`  _keystore_ _alias_ _private-key_ _keypass_ _certs_

_alias_ shall be a string represents the name of _private-key_ in
the _keystore_.

_private-key_ shall be an RSA private key.

_key-pass_ shall be a string and may or may not be used to encrypt
given _private-key_.

_certs_ shall be a list of X509 certificates which associated with
_private-key_.

The method shall store given _private-key_ to _keystore_.


###### [!Generic] `keystore-set-certificate!`  _keystore_ _alias_ _cert_

_alias_ shall be a string represents the name of _cert_ in
the _keystore_.

_cert_ shall be an X509 certificate.

The method shall store given _cert_ to _keystore_.


###### [!Generic] `keystore-delete-entry!`  _keystore_ _alias_

The method shall remove the entry associated with _alias_ from 
_keystore_.


