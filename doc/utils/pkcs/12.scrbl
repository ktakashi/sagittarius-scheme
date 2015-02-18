@; -*- coding: utf-8 -*-
@subsection[:tag "pkcs.12"]{(rsa pkcs :12) - PKCS#12}

This section describes the implementation of PKCS#12 specification library.
However we do not describe PKCS#12 itself and I don't think it is necessary to
know it if you just want to use it.

@define[Library]{@name{rsa pkcs :12}}
@desc{This library provides procedures for PKCS#12 operations.}

@subsubsection{Keystore APIs}

The example shows how to use keystore APIs:

@codeblock{
(import (rnrs) (rsa pkcs :12))

(define keystore (load-pkcs12-keystore-file "keystore.p12" "pass"))

(pkcs12-keystore-get-key keystore "key-name" "key-pass")
;; -> <private-key>

(pkcs12-keystore-get-certificate keystore "cert-name")
;; -> <x509-certificate>

;; certs are list of X509 certificate associated with given private-key
(pkcs12-keystore-set-key! keystore "key-name2" private-key "key-pass2" certs)

;; cert must be an X509 certificate
(pkcs12-keystore-set-certificate! keystore "cert-name2" cert)

(store-pkcs12-keystore-to-file keystore "keystore2.p12" "pass2")
}

@define[Class]{@name{<pkcs12-keystore>}}
@desc{PKCS#12 keystore class.}

@define[Function]{@name{pkcs12-keystore?} @args{obj}}
@desc{Returns #t if given @var{obj} is PKCS#12 keystore object, otherwise #f.}

@define[Function]{@name{make-pkcs12-keystore}}
@desc{Returns newly created PKCS#12 keystore object.}

@define[Function]{@name{load-pkcs12-keystore} @args{input-port storepass}}
@define[Function]{@name{load-pkcs12-keystore-file} @args{file storepass}}
@desc{Loads PKCS#12 keystore from given @var{input-port} or @var{file} and
returns newly created keystore object.

The @code{load-pkcs12-keystore} loads from given binary input port.

The @code{load-pkcs12-keystore-file} loads from given file.

@var{storepass} must be a string and a password for given keystore.
}

@define[Function]{@name{pkcs12-keystore-get-key} 
 @args{keystore alias keypass}}
@desc{Retrives a private key associated with @var{alias} from @var{keystore}.
If there is no key entry associated with @var{alias} then #f is returned.

@var{alias} must be a string.

@var{keypass} must be a string. It is used to decrypt the private key and
it is @b{not} allowed to pass empty password.}

@define[Function]{@name{pkcs12-keystore-get-certificate} @args{keystore alias}}
@desc{Retrives a certificate associated with @var{alias} from @var{keystore}.
If there is no certificate entry associated with @var{alias} then #f is 
returned.

@var{alias} must be a string.
}

@define[Function]{@name{pkcs12-keystore-get-certificate-chain}
 @args{keystore alias}}
@desc{Retrives certificate chain associated with given key alias @var{alias}
from @var{keystore}. If there is no certificate chain then @code{'()} is
returned.

@var{alias} must be a string.
}

@define[Function]{@name{pkcs12-keystore-contains-alias?} @args{keystore alias}}
@desc{Returns #t if @var{keystore} contains @var{alias}. Otherwise #f.}

@define[Function]{@name{store-pkcs12-keystore}
 @args{keystore output-port storepass}}
@define[Function]{@name{store-pkcs12-keystore-to-file}
 @args{keystore file storepass}}
@desc{Writes given @var{keystore} to @var{output-port} or @var{file}.

The @code{store-pkcs12-keystore} writes to given binary output port
@var{output-port}.

The @code{store-pkcs12-keystore-to-file} writes to given file @var{file}.

@var{storepass} must be a string and is used to encrypt whole contents.
}

@define[Function]{@name{pkcs12-keystore-set-key!}
 @args{keystore alias private-key key-pass certs}}
@desc{@var{alias} must be a string represents the name of @var{private-key} in
the @var{keystore}.

@var{private-key} must be an RSA private key.

@var{key-pass} must be a string and is used to encrypt given @var{private-key}.

@var{certs} must be a list of X509 certificates which associated with
@var{private-key}.

Stores given @var{private-key} to @var{keystore}.

The implementation allows users to set separate password from storepass.
Be aware that current implementation of Bouncy Castle JCE uses the same
password as storepass to encrypt a private key. Thus if you use different
password, then it is not compatible with Bouncy Castle.
}

@define[Function]{@name{pkcs12-keystore-set-certificate!}
 @args{keystore alias cert}}
@desc{@var{alias} must be a string represents the name of @var{cert} in
@var{keystore}.

@var{cert} must be an X509 certificate.

Stores given @var{cert} to @var{keystore}.
}

@define[Function]{@name{pkcs12-keystore-delete-entry!} @args{keystore alias}}
@desc{@var{alias} must be a string.

Removes the entry associated with @var{alias} in @var{keystore}.
}

@define[Function]{@name{pkcs12-keystore-aliases} @args{keystore}}
@desc{Returns all defined names in @var{keystore}.}

@; TBD PKCS#12 cipher
