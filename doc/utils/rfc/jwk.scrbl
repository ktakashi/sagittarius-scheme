@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.jwk"]{(rfc jwk) - Json Web Key}

@define[Library][@name{(rfc jwk)}]
@desc{This library provides Json Web Key (JWK) APIs. JWS is defined in
@hyperlink[:href "https://datatracker.ietf.org/doc/html/rfc7517"]{RFC 7517}.

This library also supports OKP defined in
@hyperlink[:href "https://datatracker.ietf.org/doc/html/rfc8037"]{RFC 8037}.
}

The following examples show how to interact with @code{(crypto)} keys.
@codeblock{
;; (crypto) keys to JWK/JWKS
(import (rnrs)
        (crypto)
        (rfc jwk))

(define keypair (generate-key-pair Ed25519))

(define private-key (keypair-private keypair))

(define jwk-config (jwk-config-builder (kid "my key id")))

(let ((jwks (make-jwk-set (list (key->jwk private-key jwk-config)))))
  (jwk-set->json-string jwks) ;; -> {"keys":[{"kid":"my key id",...}]}
  (jwk-set:find-key jwks (jwk-matcher:kid "my key id")) ;; -> #<jwk>
  (jwk-set->public-jwk-set jwks)) ;; -> #<jwk-set> contains only public key
}

@codeblock{
;; JWK/JWKS to (crypto) key
(import (rnrs)
        (crypto)
        (rfc jwk))

;; JWKS with EC private key
(define jwks-json
  #(("keys"
     #(("kty" . "EC")
       ("crv" . "P-256")
       ("x"   . "MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4")
       ("y"   . "4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM")
       ("d"   . "870MB6gfuTJ4HtUnUvYMyJpr5eUZNP4Bk43bVdj3eAE")
       ("use" . "enc")
       ("kid" . "1")))))
(define jwks (json->jwk-set jwks-json))
(define kid-matcher (jwk-matcher:kid "1"))

(define jwk (jwk-set:find-key jwks kid-matcher))

(jwk->public-key jwk)   ;; -> ECDSA public key
(jwk->private-key jwk)  ;; -> ECDSA private key
}

@subsubsection{JWK Set}

JWK Set (JWKS) is an object represents a set of JWKs.

@define[Function]{@name{jwk-set?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JWKS object otherwise #f.}

@define[Function]{@name{make-jwk-set} @args{keys}}
@desc{Construct a newly allocated JWKS object whose keys are @var{keys}.

@var{keys} must be a list of JWK objects.
}

@define[Function]{@name{jwk-set-keys} @args{jwk-set}}
@desc{Retrieves a list of JWKs from the given @var{jwk-set}.}


@define[Function]{@name{json->jwk-set} @args{obj}}
@define[Function]{@name{read-jwk-set}}
@define[Function]{@name{read-jwk-set} @args{port}}
@define[Function]{@name{json-string->jwk-set} @args{string}}
@desc{Construct JWKS from S-exp JSON representation of @var{obj},
from input port @var{port} or a string @var{string}.

If the first form of @code{read-jwk-set} is used, then it reads from
current input port.
}

@define[Function]{@name{jwk-set->json} @args{jwk-set}}
@define[Function]{@name{write-jwk-set} @args{jwk-set}}
@define[Function]{@name{write-jwk-set} @args{jwk-set port}}
@define[Function]{@name{jwk-set->json-string} @args{jwk-set}}
@desc{Serialize the given @var{jwk-set} to a S-exp representaion,
to @var{port} or string.

If first form of @code{write-jwk-set} is used, then it writes the
serialized JWK set to current output port.
}

@define[Function]{@name{jwk-set:find-key} @args{jwk-set jwk-matcher}}
@desc{Finds a key which matches to @var{jwk-matcher} from given @var{jwk-set}.}

@subsubsection{JWK Matcher}

A JWK matcher is a procedure takes one argument, @var{jwk}, and returns
the given @var{jwk} if it matches the condition otherwise returns #f.

The matchers provided by this library complies to the above so that
users can compose matchers like this:

@codeblock{
(import (rnrs)
        (rfc jwk)
        (sagittarius combinators))

(define kid/alg-matcher
 (compose (jwk-matcher:kid "kid") (jwk-matcher:alg 'EdDSA)))
}

@define[Function]{@name{jwk-matcher:kty} @args{obj}}
@define[Function]{@name{jwk-matcher:use} @args{obj}}
@define[Function]{@name{jwk-matcher:alg} @args{obj}}
@define[Function]{@name{jwk-matcher:kid} @args{obj}}
@define[Function]{@name{jwk-matcher:x5t} @args{obj}}
@define[Function]{@name{jwk-matcher:x5t-s256} @args{obj}}
@define[Function]{@name{jwk-matcher:crv} @args{obj}}
@desc{Creates a JWK matcher which checks @code{kty}, @code{use}, @code{alg},
@code{kid}, @code{x5t}, @code{x5t-s256} or @code{crv} field of the
target JWK is equal to @var{obj}, respectively.}

@define[Function]{@name{jwk-matcher:key-ops} @args{obj}}
@desc{Creates a JWK matcher which checks @code{key-ops} field of the
target JWK contains given @var{obj}.}

@define[Function]{@name{jwk-matcher:rsa} @args{jwk}}
@define[Function]{@name{jwk-matcher:ec} @args{jwk}}
@define[Function]{@name{jwk-matcher:oct} @args{jwk}}
@define[Function]{@name{jwk-matcher:okp} @args{jwk}}
@desc{Convenient JWK matchers which check @code{kty} to be @code{RSA},
@code{EC}, @code{oct} or @code{OKP}, respectively.}

@subsubsection{JWK}

JWK is an object which contains key information. The object contains
the following fields:
@itemlist{
  @item{@code{kty}: key type, symbol}
  @item{@code{use}: key usage, symbol, must be either @code{sig} or @code{enc}}
  @item{@code{key-ops}: key operation, a list of symbols}
  @item{@code{alg}: key algorithm, symbol}
  @item{@code{kid}: key ID}
  @item{@code{x5u}: URL of certificate}
  @item{@code{x5c}: Certificate chain, list of x509 certificate}
  @item{@code{x5t}: SHA-1 certificate finger print, bytevector}
  @item{@code{x5t-s256}: SHA-256 certificate finger print, bytevector}
}

@define[Function]{@name{jwk?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JWK object otherwise #f.}

@define[Function]{@name{jwk-kty} @args{jwk}}
@define[Function]{@name{jwk-use} @args{jwk}}
@define[Function]{@name{jwk-key-ops} @args{jwk}}
@define[Function]{@name{jwk-alg} @args{jwk}}
@define[Function]{@name{jwk-kid} @args{jwk}}
@define[Function]{@name{jwk-x5u} @args{jwk}}
@define[Function]{@name{jwk-x5c} @args{jwk}}
@define[Function]{@name{jwk-x5t} @args{jwk}}
@define[Function]{@name{jwk-x5t-s256} @args{jwk}}
@desc{Retrieves the field value of @var{jwk}.}

@define[Function]{@name{json->jwk} @args{obj}}
@define[Function]{@name{read-jwk} @args{}}
@define[Function]{@name{read-jwk} @args{:optional port}}
@define[Function]{@name{json-string->jwk} @args{string}}
@desc{Construct JWK from S-exp JSON representation of @var{obj},
from input port @var{port} or a string @var{string}.

If the first form of @code{read-jwk} is used, then it reads from
current input port.
}

@define[Function]{@name{jwk->json} @args{jwk}}
@define[Function]{@name{write-jwk} @args{jwk}}
@define[Function]{@name{write-jwk} @args{jwk port}}
@define[Function]{@name{jwk->json-string} @args{jwk}}
@desc{Serialize the given @var{jwk} to a S-exp representaion,
to @var{port} or string.

If first form of @code{write-jwk} is used, then it writes the
serialized JWK to current output port.
}

@sub*section{From JWK Conversion}

The below conversion procedures raise an error if the conversion is
not possible. For example, key type @code{oct} can't be public key.

@define[Function]{@name{jwk->public-key} @args{jwk}}
@define[Function]{@name{jwk->private-key} @args{jwk}}
@desc{Convert given @var{jwk} to @code{(crypto)} public key and private key,
respectively.}

@define[Function]{@name{jwk->octet-key} @args{jwk}}
@desc{Convert given @var{jwk} to octet key bytevector.}

@define[Function]{@name{jwk->public-jwk} @args{jwk}}
@desc{Convert given @var{jwk} to JWK which only contains public key
information.}


@sub*section{To JWK Conversion}


@define[Function]{@name{jwk:config?} @args{obj}}
@desc{Returns #t if the given @var{obj} is a JWK config object otherwise #f.

JWK may contain meta data, such as @code{kid}, to provide the information,
users can use JWK config object. The object has the below fields:
@itemlist{
  @item{@code{use}: key usage, symbol, must be either @code{sig} or @code{enc}}
  @item{@code{kid}: key ID}
  @item{@code{key-ops}: key operation, a list of symbols}
  @item{@code{alg}: key algorithm, symbol}
  @item{@code{x5u}: URL of certificate}
  @item{@code{x5c}: Certificate chain, list of x509 certificate}
  @item{@code{e}: RSA public key exponent}
}

@code{e} is provided due to the historical reason of not to have public
exponent in non CRT RSA private key. By default, the value is 65537.
}

@define[Macro]{@name{jwk-config-builder}}
@desc{A builder macro of JWK config. The macro is generated by
@code{(record builder)}. see @secref["record.builder"]{(record builder)}
for more details.
}

@define[Function]{@name{jwk->jwk-config} @args{jwk}}
@desc{Construct JWK config from given @var{jwk}.}

@define[Function]{@name{key->jwk} @args{key}}
@define[Function]{@name{key->jwk} @args{key jwk-config}}
@desc{Converts given @var{key} to JWK object.

If the second form is used, then the returning JWK contains the configured
information.

The @var{key} must be one of public key, private key, or secret key of
@code{(crypto)}.
}

@; TODO maybe we should document all different type of keys?
