@; -*- coding: utf-8 -*-
@subsection[:tag "security.keystore"]{(security keystore) - Generic Keystore Access}

@define[Library]{@name{(security keystore)}}
@desc{This library provides generic access for variety of keystores.}

The following example shows how to use this library:
@codeblock{
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
}

@subsubsection{Generic APIs}

@define[Class]{@name{<keystore>}}
@desc{Base class of keystores.}

@define[Function]{@name{keystore?} @args{obj}}
@desc{Returns #t if given @var{obj} is a keystore object. Otherwise #f.}

@define[Function]{@name{load-keystore} @args{type input-port storepass}}
@define[Function]{@name{load-keystore-file} @args{type file storepass}}
@desc{Loads @var{type} keystore from @var{input-port} or @var{file}.

@var{storepass} must be a string and may or may not be used to decrypt 
keystore content.

@code{load-keystore} loads from given binary input port @var{input-port}.

@code{load-keystore-file} loads from given file @var{file}.
}
@define[Function]{@name{make-keystore} @args{type}}
@desc{Returns newly created @var{type} keystore object.

Currently @code{pkcs12}, @code{jks} and @code{jceks} are supported.
}

@define[Generic]{@name{keystore-get-key} @args{keystore alias keypass}}
@desc{The method shall return a private key associated with @var{alias}
from @var{keystore}. If there is no key entry associated with @var{alias} 
then #f shall be returned.

@var{alias} shall be a string.

@var{keypass} shall be a string. It may or may not be used to decrypt 
the private key.
}

@define[Generic]{@name{keystore-get-certificate} @args{keystore alias}}
@desc{The method shall return an X509 certificate associated with @var{alias}
from @var{keystore}. If there is no certificate entry associated with 
@var{alias} then #f shall be returned.

@var{alias} shall be a string.
}

@define[Generic]{@name{keystore-get-certificate-chain} @args{keystore alias}}
@desc{The method shall return a list of key certificates associated 
with @var{alias} from @var{keystore}. If there is no certificate entries
associated with @var{alias} then #f shall be returned.

@var{alias} shall be a string.
}

@define[Generic]{@name{keystore-get-creation-date} @args{keystore alias}}
@desc{The method shall return a date object of @var{alias} associated entry.
If there is no entry associated with @var{alias} then #f shall be returned.

@var{alias} shall be a string.
}

@define[Generic]{@name{keystore-contains-alias?} @args{keystore alias}}
@desc{The method shall return #t if @var{keystore} contains an entry associated
with @var{alias}. Otherwise #f shall be returend.
}

@define[Generic]{@name{store-keystore} @args{keystore output-port storepass}}
@define[Generic]{@name{store-keystore-to-file}  @args{keystore file storepass}}
@desc{The methods shall write @var{keystore} to @var{output-port} or 
@var{file}.

The @code{store-keystore} shall write to given binary output port 
@var{output-port}.

The @code{store-keystore-to-file} shall write to given file @var{file}.

@var{storepass} shall be a string and may or may not be used to encrypt 
whole contents.
}

@define[Generic]{@name{keystore-set-key!}
 @args{keystore alias private-key keypass certs}}
@desc{@var{alias} shall be a string represents the name of @var{private-key} in
the @var{keystore}.

@var{private-key} shall be an RSA private key.

@var{key-pass} shall be a string and may or may not be used to encrypt
given @var{private-key}.

@var{certs} shall be a list of X509 certificates which associated with
@var{private-key}.

The method shall store given @var{private-key} to @var{keystore}.
}

@define[Generic]{@name{keystore-set-certificate!} @args{keystore alias cert}}
@desc{@var{alias} shall be a string represents the name of @var{cert} in
the @var{keystore}.

@var{cert} shall be an X509 certificate.

The method shall store given @var{cert} to @var{keystore}.
}

@define[Generic]{@name{keystore-delete-entry!} @args{keystore alias}}
@desc{The method shall remove the entry associated with @var{alias} from 
@var{keystore}.
}

@; TBD
@; @subsubsection{JKS}
@; @subsubsection{JCEKS}
@; @subsubsection{Library name convension}
