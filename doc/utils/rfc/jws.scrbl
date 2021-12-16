@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.jws"]{(rfc jws) - Json Web Signature}

@define[Library]{@name{(rfc jws)}}
@desc{This library provides Json Web Signature (JWS) APIs. JWS is defined
in @hyperlink[:href "https://tools.ietf.org/html/rfc7515"]{RFC 7515}.

The library supports @code{ES256K} defined in
@hyperlink[:href "https://datatracker.ietf.org/doc/html/rfc8812"]{RFC 8812}
section 3.1.

It also supports EdDSA of both Ed25519 and Ed448 defined in
@hyperlink[:href "https://datatracker.ietf.org/doc/html/rfc8037"]{RFC 8037}.
}

The following examples show how to sign and verify JWS token
@codeblock{
;; Signing
(import (rnrs)
        (rfc jws)
        (rfc jwk) ;; to convert key pair to JWK
        (crypto))

;; Generate Ed25519 key pair
(define keypair (generate-key-pair Ed25519))

;; JWS header with crit
(define jws-header
  (jws-header-builder
   (alg 'EdDSA)
   (crit '("sagittarius:iss" "sagittarius:sub"))
   (custom-parameters '(("sagittarius:iss" "Sagittarius Scheme")
                        ("sagittarius:sub" "JWS test")))))

(define payload
  (string->utf8 "Payload can be anything as long as a bytevector"))

;; Get signer
(define signer (private-key->jws-signer (keypair-private keypair)))

;; The JWS object is not signed yet
(define unsecure-jws-object (make-jws-object jws-header payload))

(let ((jws-object (jws:sign unsecure-jws-object signer)))
  (jws:serialize jws-object)
  ;; -> eyJzYWdpdHRhcml1czpzdWIiOiJKV1MgdGVzdCIsInNhZ2l0dGFyaXVzOmlzcyI6IlNhZ2l0dGFyaXVzIFNjaGVtZSIsImFsZyI6IkVkRFNBIiwiY3JpdCI6WyJzYWdpdHRhcml1czppc3MiLCJzYWdpdHRhcml1czpzdWIiXX0.UGF5bG9hZCBjYW4gYmUgYW55dGhpbmcgYXMgbG9uZyBhcyBhIGJ5dGV2ZWN0b3I.5Aj_AJh4DW01kV80XtFbxRRMw2ktxIrQ5-UXoCwKVWI0Ke0q0t3vpcFnESL39zYDwi3Ps8eLxfmEb-TvhkQGBg
  (jwk->json-string (public-key->jwk (keypair-public keypair)))
  ;; -> {"kty":"OKP","crv":"Ed25519","x":"o_t1R4fWf7obqTZWlXxrgPG09BMU-zuhqHvb9_ayOew"}
  )
}

@codeblock{
;; Verify
(import (rnrs)
        (rfc jws)
        (rfc jwk) ;; to convert JWK to public key
        (crypto))

;; JWS string
(define jws-string
  "eyJzYWdpdHRhcml1czpzdWIiOiJKV1MgdGVzdCIsInNhZ2l0dGFyaXVzOmlzcyI6IlNhZ2l0dGFyaXVzIFNjaGVtZSIsImFsZyI6IkVkRFNBIiwiY3JpdCI6WyJzYWdpdHRhcml1czppc3MiLCJzYWdpdHRhcml1czpzdWIiXX0.UGF5bG9hZCBjYW4gYmUgYW55dGhpbmcgYXMgbG9uZyBhcyBhIGJ5dGV2ZWN0b3I.5Aj_AJh4DW01kV80XtFbxRRMw2ktxIrQ5-UXoCwKVWI0Ke0q0t3vpcFnESL39zYDwi3Ps8eLxfmEb-TvhkQGBg")

;; Ed25519 public key
(define jwk
  (json-string->jwk
   "{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"o_t1R4fWf7obqTZWlXxrgPG09BMU-zuhqHvb9_ayOew\"}"))

;; Parse the string
(define jws-object (jws:parse jws-string))

;; Make a verifier from the JWK
(define verifier (public-key->jws-verifier jwk))

(when (jws:verify jws-object verifier '("sagittarius:iss" "sagittarius:sub"))
  (utf8->string (jws-object-payload jws-object))) ;; -> payload
}

The above examples only show the flavour of the APIs. In real world
applications, the users of the library must consider a lot more to
make the application secure.

@subsubsection{JWS Object}

@define["Record Type"]{@name{<jws-object>}}
@desc{The record type of JWS objects.
JWS object has 2 fields, @code{header} and @code{payload}.
}
@define[Function]{@name{jws-object?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JWS object otherwise #f.}

@define[Function]{@name{make-jws-object} @args{jws-header payload}}
@desc{Construct a newly allocated JWS object.

@var{Jws-header} must be a JWS header object.

@var{payload} must be a bytevector.
}
@define[Function]{@name{jws-object-header} @args{jws-object}}
@desc{Returns the value of @code{header} field of given @var{jws-object}.}
@define[Function]{@name{jws-object-payload} @args{jws-object}}
@desc{Returns the value of @code{payload} field of given @var{jws-object}.}

@define["Record Type"]{@name{<jws-signed-object>}}
@desc{The record type of signed JWS objects.

Signed JWS object is a sub record of JWS object, which has @code{signature}
field.
}
@define[Function]{@name{jws-signed-object?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a signed JWS object otherwise #f.

This object can be obtained by @code{jws:sign} or @code{jws:parse}.
}
@define[Function]{@name{jws-signed-object-signature} @args{signed-jws-object}}
@desc{Returns the value of @code{signature} field of
given @var{signed-jws-object}.}

@subsubsection{JWS Header}

@define["Record Type"]{@name{<jws-header>}}
@desc{The record type of JWS header.

This record type has the below fields:
@itemlist[
@item{@code{typ}: JWS type, must be a symbol}
@item{@code{cty}: JWS content type}
@item{@code{alg}: JWS algorithm, must be a symbol}
@item{@code{jku}: JWK Set URL}
@item{@code{jwk}: JWK, must be a JWK object}
@item{@code{kid}: Key ID}
@item{@code{x5u}: X.509 certificate URL}
@item{@code{x5c}: X.509 certiticate chain, must be a list of X.509 certificate}
@item{@code{x5t}: X.509 certificate SHA-1 Thumbprint, must be a bytevector}
@item{@code{x5t-s256}: X.509 certificate SHA-256 Thumbprint, must be a bytevector}
@item{@code{crit}: Critical header parameter, must be a list of string}
@item{@code{custom-parameters}}
]
The above fields have accessors prefixed @var{jws-header-}. For example,
to read @code{typ} field, you can use @code{jws-header-typ} procedure.
}

@define[Function]{@name{jws-header?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JWS header, otherwise #f.}

@define[Macro]{@name{jws-header-builder} @args{(field value) @dots{}}}
@desc{A builder macro of JWS header. The macro is generated by
@code{(record builder)}. see @secref["record.builder"]{(record builder)}
for more details.
}

@define[Function]{@name{jws-header->json} @args{jws-header}}
@define[Function]{@name{write-jws-header} @args{jws-header}}
@define[Function]{@name{write-jws-header} @args{jws-header port}}
@define[Function]{@name{jws-header->json-string} @args{jws-header}}
@desc{Serialize the given @var{json-header} to a S-exp representaion,
to @var{port} or string.

If first form of @code{write-jws-header} is used, then it writes the
serialized JWS header to current output port.
}

@define[Function]{@name{json->jws-header} @args{obj}}
@define[Function]{@name{read-jws-header} @args{}}
@define[Function]{@name{read-jws-header} @args{:optional port}}
@define[Function]{@name{json-string->jws-header} @args{string}}
@desc{Construct JWS header from S-exp JSON representation of @var{obj},
from input port @var{port} or a string @var{string}.

If the first form of @code{read-jws-header} is used, then it reads from
current input port.
}

@subsubsection{JWS Operations}

@define[Function]{@name{jws:parse} @args{string}}
@desc{Parse the given compact JWS of @var{string} and return signed JWS object.

If the format of the given @var{string} is invalid, then an error is signaled.
}

@define[Function]{@name{jws:serialize} @args{jws-object}}
@define[Function]{@name{jws:serialize} @args{jws-object detach-payload?}}
@desc{Serialize the given @var{jws-object} to compact JWS form.

If the second form is used, then the payload is omitted. (Detached form)
}

@define[Function]{@name{jws:sign} @args{jws-object signer}}
@desc{Sign the given @var{jws-object} with the given @var{signer} and returns
signed JWS object.
}

@define[Function]{@name{jws:verify} @args{jws-object verifier}}
@define[Function]{@name{jws:verify} @args{jws-object verifier critical-headers}}
@desc{Verify the given @var{jws-object} with the given @var{verifier}.
If the verification is success, then it returns #t.

Otherwise, it may return #f or signals an error, depending on the
underlying verifier.

If the second form of @code{jws:verify} is used, then it uses the given
@var{critical-headers} list to check @code{crit} header value.
}

@subsubsection{JWS Verifiers}

JWS verifier is a procedure takes 3 arguments, JWS header, signing content
and signature.

This library doesn't support @code{none} algorithm verifier. It is obvious
that if you want to support it, you just don't have to verify it.

@define[Function]{@name{make-mac-jws-verifier} @args{key}}
@desc{@var{key} must be a JWK:oct or a bytevector.

Creates a MAC JWS verifier.
}

@define[Function]{@name{make-rsa-jws-verifier} @args{key}}
@desc{@var{key} must be a JWK:RSA or a RSA public key.

Creates a RSA JWS verifier.
}

@define[Function]{@name{make-ecdsa-jws-verifier} @args{key}}
@desc{@var{key} must be a JWK:EC or a ECDSA public key.

Creates a ECDSA JWS verifier.
}

@define[Function]{@name{make-eddsa-jws-verifier} @args{key}}
@desc{@var{key} must be a JWK:OKP or a EdDSA public key.

Creates a EdDSA JWS verifier.
}

@subsubsection{JWS Signers}

JWS signer is a procedure takes 2 arguments, JWS header, signing content.

This library doesn't support @code{none} algorithm signer. If you want to
support it, you need to create a JWS object with @code{alg} header with
value of @code{none}.

@define[Function]{@name{make-mac-jws-signer} @args{key}}
@desc{@var{key} must be a JWK:oct or a bytevector.

Creates a MAC JWS signer.
}

@define[Function]{@name{make-rsa-jws-signer} @args{key}}
@desc{@var{key} must be a JWK:RSA or a RSA private key.

Creates a RSA JWS signer.
}

@define[Function]{@name{make-ecdsa-jws-signer} @args{key}}
@desc{@var{key} must be a JWK:EC or a ECDSA private key.

Creates a ECDSA JWS signer.
}

@define[Function]{@name{make-eddsa-jws-signer} @args{key}}
@desc{@var{key} must be a JWK:OKP or a EdDSA private key.

Creates a EdDSA JWS signer.
}
