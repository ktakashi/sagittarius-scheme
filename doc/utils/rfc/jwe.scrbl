@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.jwe"]{(rfc jwe) - Json Web Encryption}

@define[Library]{@name{(rfc jwe)}}
@desc{This library provides Json Web Encryption (JWE) APIs. JWE is defined in
@hyperlink[:href "https://datatracker.ietf.org/doc/html/rfc7516"]{RFC 7516}.
}

The following example shows how to exchange secret key.
@codeblock{
(import (rnrs)
        (rfc jwe)
        (rfc jwk))

(define jwk-bob
  (json-string->jwk
   "{\"kty\":\"EC\",
     \"crv\":\"P-256\",
     \"x\":\"weNJy2HscCSM6AEDTDg04biOvhFhyyWvOHQfeF_PxMQ\",
     \"y\":\"e8lnCO-AlStT-NJVX-crhB7QRYhiix03illJOVAOyck\",
     \"d\":\"VEmDZpDXXK8p8N0Cndsxs924q6nS1RXFASRl6BfUqdw\"}"))

(define jwe-header
  (jwe-header-builder
   (alg 'ECDH-ES+A128KW)
   (enc 'A128GCM)
   (apu "QWxpY2U")
   (apv "Qm9i")))

;; Alice wants to encrypt with Bob's public key
(define alice-encryptor (make-ecdh-jwe-encryptor (jwk->public-key jwk-bob)))

;; Bob needs to decrypt Alice's message with his private key
(define bob-decryptor (make-ecdh-jwe-decryptor jwk-bob))

(define secret-key (string->utf8 "down the rabbit hole"))

(let ((jwe-object (jwe:encrypt alice-encryptor jwe-header secret-key)))
  (jwe:serialize jwe-object) ;; -> compact JWE string
  (let ((secret-key (jwe:decrypt bob-decryptor jwe-object)))
    (utf8->string secret-key))) ;; -> "down the rabbit hole"
}

The above is just a taste of how to share a secret key without shared
secret. In the real world application, you should implement your
application more carefully.

@subsubsection{JWE Object}

@define["Record Type"]{@name{<jwe-object>}}
@desc{The record type of JWE objects.
JWS object has 5 fields, @code{header}, @code{encrypted-key}, @code{iv},
@code{cipher-text} and @code{authentication-tag}.
}
@define[Function]{@name{jwe-object?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JWE object otherwise #f.}

@define[Function]{@name{make-jws-object}
 @args{jwe-header encrypted-key iv cipher-text authentication-tag}}
@desc{Construct a newly allocated JWE object.

This constructor doesn't validate if the @var{cipher-text} can be decrypted
by the @var{encrypted-key} as that's not possible.

NOTE: This constructor is not meant be used by users.
}

@define[Function]{@name{jwe-object-header} @args{jwe-object}}
@desc{Returns the value of @code{header} field of given @var{jwe-object}}
@define[Function]{@name{jwe-object-encrypted-key} @args{jwe-object}}
@desc{Returns the value of @code{encrypted-key} field of given @var{jwe-object}}
@define[Function]{@name{jwe-object-iv} @args{jwe-object}}
@desc{Returns the value of @code{iv} field of given @var{jwe-object}}
@define[Function]{@name{jwe-object-cipher-text} @args{jwe-object}}
@desc{Returns the value of @code{cipher-text} field of given @var{jwe-object}}
@define[Function]{@name{jwe-object-authentication-tag} @args{jwe-object}}
@desc{Returns the value of @code{authentication-tag} field of given
 @var{jwe-object}}

@subsubsection{JWE Header}

@define["Record Type"]{@name{<jwe-header>}}
@desc{The record type of JWE header.

This record type has the below fields:
@itemlist[
@item{@code{typ}: JWE type, must be a symbol}
@item{@code{cty}: JWE content type}
@item{@code{alg}: JWE algorithm, must be a symbol}
@item{@code{enc}: JWE encryption algorithm, must be a symbol}
@item{@code{jku}: JWK Set URL}
@item{@code{jwk}: JWK, must be a JWK object}
@item{@code{kid}: Key ID}
@item{@code{x5u}: X.509 certificate URL}
@item{@code{x5c}: X.509 certiticate chain, must be a list of X.509 certificate}
@item{@code{x5t}: X.509 certificate SHA-1 Thumbprint, must be a bytevector}
@item{@code{x5t-s256}: X.509 certificate SHA-256 Thumbprint, must be a bytevector}
@item{@code{crit}: Critical header parameter, must be a list of string}
@item{@code{zip}: Compression algorithm}
@item{@code{p2s}: Salt, must be a bytevector}
@item{@code{p2c}: Iteration count, must be an integer}
@item{@code{iv}: Initial vector, must be a bytevector}
@item{@code{tag}: Authentication tag, must be a bytevector}
@item{@code{apu}: }
@item{@code{apv}: }
@item{@code{epk}: }
@item{@code{custom-parameters}}
]
The above fields have accessors prefixed @var{jwe-header-}. For example,
to read @code{typ} field, you can use @code{jwe-header-typ} procedure.
}

@define[Function]{@name{jwe-header?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JWE header, otherwise #f.}

@define[Macro]{@name{jwe-header-builder} @args{(field value) @dots{}}}
@desc{A builder macro of JWE header. The macro is generated by
@code{(record builder)}. see @secref["record.builder"]{(record builder)}
for more details.
}

@define[Function]{@name{jwe-header->json} @args{jwe-header}}
@define[Function]{@name{write-jwe-header} @args{jwe-header}}
@define[Function]{@name{write-jwe-header} @args{jwe-header port}}
@define[Function]{@name{jwe-header->json-string} @args{jwe-header}}
@desc{Serialize the given @var{json-header} to a S-exp representaion,
to @var{port} or string.

If first form of @code{write-jwe-header} is used, then it writes the
serialized JWE header to current output port.
}

@define[Function]{@name{json->jwe-header} @args{obj}}
@define[Function]{@name{read-jwe-header} @args{}}
@define[Function]{@name{read-jwe-header} @args{:optional port}}
@define[Function]{@name{json-string->jwe-header} @args{string}}
@desc{Construct JWE header from S-exp JSON representation of @var{obj},
from input port @var{port} or a string @var{string}.

If the first form of @code{read-jwe-header} is used, then it reads from
current input port.
}

@subsubsection{JWE Operations}

@define[Function]{@name{jwe:parse} @args{string}}
@desc{Parse the given compact JWE of @var{string} and return JWE object.

If the format of the given @var{string} is invalid, then an error is signaled.
}

@define[Function]{@name{jwe:serialize} @args{jwe-object}}
@desc{Serialize the given @var{jwe-object} to compact JWE form.}

@define[Function]{@name{jwe:encrypt} @args{jwe-encryptor jwe-header plain-text}}
@desc{Returns a JWE object whose @code{cipher-text} is the encrypted
@var{payload}.

The @var{jwe-encryptor} must be one of the JWE encryptors described below
section.

The @var{jwe-header} must be a JWE header object.

The @var{plain-text} must be a bytevector.
}

@define[Function]{@name{jwe:encrypt} @args{jwe-decryptor jwe-object}}
@define[Function]{@name{jwe:encrypt}
 @args{jwe-decryptor jwe-object critical-headers}}
@desc{Returns decrypted @code{cipher-text} as a bytevector.

The @var{jwe-decryptor} must be one of the JWE decryptors described below
section.

The @var{jwe-object} must be a JWE object.

If the second form is used, then the @code{crit} paramteters of the
@code{header} will be checked.
}

@subsubsection{JWE Encryptors}

JWE encryptor is a procedure takes two arguments, JWE header and plain text.

@define[Function]{@name{make-ecdh-jwe-encryptor} @args{key}}
@desc{@var{key} must be a EC JWK, OKP JWK, EcDSA public key, X25519 public key
or X448 public key.

Creates a ECDH JWE encryptor.

This encryptor supports @code{ECDH-ES}, @code{ECDH-ES+A128KW},
@code{ECDH-ES+A198KW}, and @code{ECDH-ES+A256KW} algorithms.
}

@define[Function]{@name{make-rsa-jwe-encryptor} @args{key}}
@desc{@var{key} must be a RSA JWK or RSA public key.

Creates a RSA JWE encryptor.

This encryptor supports @code{RSA1_5}, @code{RSA-OAEP} and
@code{RSA-OAEP-256} algorithms.
}

@define[Function]{@name{make-aeskw-jwe-encryptor} @args{key}}
@desc{@var{key} must be a OCT JWK or AES secret key.

Creates a AESKW JWE encryptor.

This encryptor supports @code{A128KW}, @code{A192KW}, @code{A256KW},
@code{A128GCMKW}, @code{A192GCMKW}, and @code{A256GCMKW}, algorithms.
}


@define[Function]{@name{make-pbes2-jwe-encryptor} @args{password}}
@desc{@var{password} must be a string or a bytevector.

Creates a PBES2 JWE encryptor.
}

@define[Function]{@name{make-direct-jwe-encryptor} @args{key}}
@desc{@var{key} must be an AES secret key.

Creates a direct JWE encryptor.

This encryptor uses given @var{key} as CEK.
}


@subsubsection{JWE Decryptors}

JWE decryptor is a procedure takes 5 arguments, JWE header, encrypted key,
IV, cipher text and authentication tag.

@define[Function]{@name{make-ecdh-jwe-decryptor} @args{key}}
@desc{@var{key} must be a EC JWK, OKP JWK, EcDSA private key, X25519 private key
or X448 private key.

Creates a ECDH JWE decryptor.

This decryptor supports @code{ECDH-ES}, @code{ECDH-ES+A128KW},
@code{ECDH-ES+A198KW}, and @code{ECDH-ES+A256KW} algorithms.
}

@define[Function]{@name{make-rsa-jwe-decryptor} @args{key}}
@desc{@var{key} must be a RSA JWK or RSA private key.

Creates a RSA JWE decryptor.

This decryptor supports @code{RSA1_5}, @code{RSA-OAEP} and
@code{RSA-OAEP-256} algorithms.
}

@define[Function]{@name{make-aeskw-jwe-decryptor} @args{key}}
@desc{@var{key} must be a OCT JWK or AES secret key.

Creates a AESKW JWE decryptor.

This decryptor supports @code{A128KW}, @code{A192KW}, @code{A256KW},
@code{A128GCMKW}, @code{A192GCMKW}, and @code{A256GCMKW}, algorithms.
}


@define[Function]{@name{make-pbes2-jwe-decryptor} @args{password}}
@desc{@var{password} must be a string or a bytevector.

Creates a PBES2 JWE decryptor.
}

@define[Function]{@name{make-direct-jwe-decryptor} @args{key}}
@desc{@var{key} must be an AES secret key.

Creates a direct JWE decryptor.

This decryptor uses given @var{key} as CEK.
}