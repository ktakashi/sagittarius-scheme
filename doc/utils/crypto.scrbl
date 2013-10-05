@; -*- coding: utf-8 -*-
@subsection[:tag "crypto"]{(crypto) - Cryptographic library}

This documentation does not describe cryptography itself. For example, it does
not describe what initial vector is and how long it must be. So users must know
about cryptography's this library supports.

This library uses @code{libtomcrypt}'s functionalities. The library is public
domain. Thank you for the great library.

Note: the @code{libtomcrypt} is a huge cryptographic library and I am not so 
good with cryptographics, so the @code{(crypto)} library is not totally tested.
Just the functionalities which I usually use are tested. If you find a bug or
wrong documentation, pleas report it.

@define[Library]{@name{(crypto)}}
@desc{This library is the top most library, it exports all the other libraries
procedures. Users must import only this and not to use the others.

@code{(crypto)} library supports both symmetric cryptography and public/private
key mechanism. For public/private key, it only supports RSA for now.
}

@define[Function]{@name{crypto-object?} @args{obj}}
@desc{Returns #t if @var{obj} is @code{crypto-object}.

@code{crypto-object} can be either @code{cipher} or @code{key}.
}

@subsubsection{Cipher operations}

@define[Function]{@name{cipher}
 @args{type key
 :key (mode MODE_ECB) (iv #f) (padder pkcs5-padder)
      (rounds 0) (ctr-mode CTR_COUNTER_LITTLE_ENDIAN)
 :rest options}}
@desc{Creates a cipher object.

@var{type} must be known cryptographic algorithm. Currently, @code{(crypto)}
library exports the algorithm below.

The symmetric key algorithms.
@define[Constant]{@name{Blowfish}}
@define[Constant]{@name{X-Tea}}
@define[Constant]{@name{RC2}}
@define[Constant]{@name{RC5-32/12/b}}
@define[Constant]{@name{RC6-32/20/b}}
@define[Constant]{@name{SAFER+}}
@define[Constant]{@name{SAFER-K64}}
@define[Constant]{@name{SAFER-SK64}}
@define[Constant]{@name{SAFER-K128}}
@define[Constant]{@name{SAFER-SK128}}
@define[Constant]{@name{AES}}
@define[Constant]{@name{Twofish}}
@define[Constant]{@name{DES}}
@define[Constant]{@name{DES3}}
@define[Constant]{@name{DESede}}
@define[Constant]{@name{CAST5}}
@define[Constant]{@name{CAST-128}}
@define[Constant]{@name{Noekeon}}
@define[Constant]{@name{Skipjack}}
@define[Constant]{@name{Khazad}}
@define[Constant]{@name{SEED}}
@define[Constant]{@name{KASUMI}}

The public key algorithm
@define[Constant]{@name{RSA}}

@var{key} must be a key object which will be created by key generate procedures
described below.

@var{mode} specifies the @bold{symmetric cipher}'s encryption and description
mode. If the cipher type is public key cipher, it will be ignored. Some modes
require initial vector @var{iv}. The possible mods are below.
@define[Constant]{@name{MODE_ECB}}
@define[Constant]{@name{MODE_CBC}}
@define[Constant]{@name{MODE_CFB}}
@define[Constant]{@name{MODE_OFB}}
@define[Constant]{@name{MODE_CTR}}

@var{iv} must be a bytevector or #f. This is an initial vector which some modes
require. cf) @code{MODE_CBC}.

@var{rounds} specify how many times the cipher rounds the key.

@var{ctr-mode} specifies counter mode. The possible mode is blow.
@define[Constant]{@name{CTR_COUNTER_LITTLE_ENDIAN}}
@define[Constant]{@name{CTR_COUNTER_BIG_ENDIAN}}
@define[Constant]{@name{LTC_CTR_RFC3686}}
}

@define[Function]{@name{cipher-keysize} @args{cipher test}}
@desc{Returns given cipher type's recommended keysize.

@var{cipher} must cipher object created by @code{cipher} procedure.

@var{test} must be fixnum.

If @var{test} is too small for the cipher, it will raise an error.

Note: this procedure is for helper. It is designed to use check keysize you want
to use.
}

@define[Function]{@name{cipher?} @args{obj}}
@desc{Returns #t if the @var{obj} is cipher object.}

@define[Function]{@name{encrypt} @args{cipher pt}}
@desc{@var{cipher} must be a cipher object.

@var{pt} must be a bytevector.

@code{encrypt} encrypts given plain text @var{pt} according to the given
@var{cipher}.
}

@define[Function]{@name{decrypt} @args{cipher ct}}
@desc{@var{cipher} must be a cipher object.

@var{ct} must be a bytevector.

@code{decrypt} decrypts given encrypted text @var{ct} according to the given
@var{cipher}.
}

@define[Function]{@name{sign} @args{public-cipher data :optional opt}}
@desc{@var{public-cipher} must be a cipher object created with public/private
key algorithm.

@var{data} must be a bytevector.

Signs given @var{data}. This procedure is just a wrapper for the real
implementations. Currently Sagittarius supports only RSA sign.

@var{opt} can specify the signer behaviour. Default supported RSA cipher can
accept keyword argument @var{encode}.

@var{encode} specifies the encoder. The default encoder is
@code{pkcs1-emsa-pss-encode}. And the rest keyword arguments will be passed to
encoder. Supported encoders are described below.
}

@define[Function]{@name{verify} @args{public-cipher M S :optional opt}}
@desc{@var{public-cipher} must be a cipher object created with public/private
key algorithm.

@var{M} and @var{S} must be bytevectors.

@var{M} is master message which will be compared with encoded message.

@var{S} is signed message.

The @cod{verity} procedure verifies two messages.

@var{opt} can specify the verifier behaviour. Default supported RSA cipher can
accept keyword argument @var{verify}.

@var{verify} specifies the verifier. The defaule verifier is
@code{pkcs1-emsa-pss-verify}. And the rest keyword arguments will be passed to
verifier. Supported verifiers are described below.
}

@subsubsection{Key operations}


@define[Generic]{@name{generate-secret-key}
 @args{(type <string>) (key <bytevector>)}}
@desc{@var{type} must be one of the supported symmetric algorithm.

@var{key} must be a bytevector and its length must satisfy the keysize which
the given algorithm requires.

Returns a sercret key object.
}
@define[Generic]{@name{generate-key-pair}
 @args{(type <top>) . options}}
@desc{@var{type} is for despatch. For default implementation, it must be
@code{RSA}.

Generates a key pair object.

Default implementation supports RSA key geneartion and @var{options} can be
keyword arguments described below.

@var{size} keyword argument is decides key length. Default value is 1024.

@var{prng} keyword argument is given, it will be passed to @code{random-prime}.
For more detail, see @secref["math.random"]{(math random)} library. Default
value is @code{(secure-random RC4)}.

@var{e} keyword argument is an exponent. Usually it does not have to be
specified with other number. Default value is 65537.
}

@define[Generic]{@name{generate-private-key}
 @args{type . options}}
@desc{@var{type} is for despatch. For default implementation, it must be
@code{RSA}.

Returns private key object.

Default RSA implementation @var{options} can be these arguments.
@dl-list[]{
@dl-item["modulus"]{The private key's modulus}
@dl-item["private-exponent"]{The private key's exponent}
@dl-item["public-exponent"]{keyword argument. Used for CRT private key object.}
@dl-item["p"]{keyword argument. Used for CRT private key object.}
@dl-item["q"]{keyword argument. Used for CRT private key object.}
}

}
@define[Function]{@name{generate-public-key} @args{type :optional opt}}
@desc{@var{type} is for despatch. For default implementation, it must be
@code{RSA}.

Returns public key object.

Default RSA implementation @var{opt} can be these arguments.
@dl-list{
@dl-item["modulus"]{The public key's modulus}
@dl-item["exponent"]{The public key's exponent}
}
}

@define[Function]{@name{keypair?} @args{obj}}
@desc{Returns #t if given @var{obj} is keypair object, otherwise #f}
@define[Function]{@name{keypair-private} @args{keypair}}
@desc{Returns private key from @var{keypair}}
@define[Function]{@name{keypair-public} @args{keypair}}
@desc{Returns public key from @var{keypair}}

@define[Function]{@name{key?} @args{obj}}
@desc{Returns #t if given @var{obj} is key object, otherwise #f}
@define[Class]{@name{<private-key>}}
@desc{CLOS class of private key object.}
@define[Class]{@name{<public-key>}}
@desc{CLOS class of public key object.}
@define[Function]{@name{private-key?} @args{obj}}
@desc{Returns #t if given @var{obj} is private key object, otherwise #f}
@define[Function]{@name{public-key?} @args{obj}}
@desc{Returns #t if given @var{obj} is public key object, otherwise #f}

@define[Function]{@name{split-key}
 @args{key :optional (count 3) (prng (secure-random RC4))}}
@desc{@var{key} must be a bytevector and plain key.

Splits the given @var{key} to @var{count} components and returns @var{count}
values as key components.

The return values might be different each time.
}

@define[Function]{@name{combine-key-components}
 @args{component1 components @dots{}}}
@define[Function]{@name{combine-key-components!}
 @args{result component1 components @dots{}}}
@desc{Renaming export of @code{bytevector-xor} and @code{bytevector-xor!}
respectively.

For more detail, see @secref["util.bytevector"]{(util bytevector)}.
}

@subsubsection{PKCS operations}

The procedures described in this section is implemented according to PKCS#1. I
don't have any intend to describe functionality. If you need to know what
exactly these procedures do, please see the PKCS#1 document.

@define[Function]{@name{pkcs5-padder} @args{bv block-size padding?}}
@desc{@var{bv} must be a bytevector.

@var{block-size} must be a non negative exact integer.

@var{padding?} must be a boolean.

Pads or Unpads paddings from @var{bv} according to PKCS#5.

If @var{padding?} is #t, the procedure will pad. otherwise it will unpad.
}

@define[Function]{@name{pkcs-v1.5-padding} @args{prng key block-type}}
@desc{@var{prng} must be prng object. See @secref["math.random"]{(math random)}.

@var{key} must be either private or public key object.

@var{block-type} must be one of these.
@define[Constant]{@name{PKCS-1-EME}}
@define[Constant]{@name{PKCS-1-EMSA}}

Returns a padding procedure. The procedure signature is the same as
@var{pkcs5-padder}.
}

@define[Function]{@name{pkcs1-emsa-pss-encode}
 @args{m em-bits
      :key (hash @code{(hash-algorithm SHA-1)}) (mgf mgf-1) (salt-length #f)
           (prng @code{(secure-random RC4)})}}
@desc{@var{m} must be a bytevector.

@var{em-bits} must be non negative exact integer.

Encodes given message @var{m} according to the PKCS#1 section 9.1.1.

The keyword arguments specified some behaviour.

@var{hash} specifies the hash algorithm. For more detail, see
@secref["math.hash"]{(math hash)} library.

@var{mgf} specifies mask generation procedure. 

Note: PKCS#1 only specifies MGF-1.

@var{salt-length} specifies salt's length. If it's #f encoder does not use salt.

@var{prng} is a pseudo random see @secref["math.random"]{(math random)}.
}
@define[Function]{@name{pkcs1-emsa-pss-verify}
 @args{m em-bits
      :key (hash @code{(hash-algorithm SHA-1)}) (mgf mgf-1)
           (prng @code{(secure-random RC4)})}}
@desc{@var{m} must be a bytevector.

@var{em-bits} must be non negative exact integer.

Verify given message @var{m} according to the PKCS#1 section 9.1.1.

Other keyword arguments are the same as @code{pkcs1-emsa-pss-encode}.
}
@define[Function]{@name{mgf-1} @args{mgf-seed mask-length hasher}}
@desc{@var{mgf-seed} must be a bytevector.

@var{mask-length} must be a non negative exact integer.

@var{hasher} must be a hash algorithm. See
@secref["math.random"]{(math random)}.

Creates a mask bytevector, according to PKCS#1 MGF-1.
}
@define[Function]{@name{pkcs1-emsa-v1.5-encode}
 @args{m em-bits :key (hash @code{(hash-algorithm SHA-1)})}}
@desc{@var{m} must be a bytevector.

@var{em-bits} must be non negative exact integer.

Encodes given message @var{m} according to the PKCS#1 section 9.2.

Other keyword arguments are the same as @code{pkcs1-emsa-pss-encode}.
}
@define[Function]{@name{pkcs1-emsa-v1.5-verify}
 @args{m em-bits :key (hash @code{(hash-algorithm SHA-1)})}}
@desc{@var{m} must be a bytevector.

@var{em-bits} must be non negative exact integer.

Verify given message @var{m} according to the PKCS#1 section 9.2.
Other keyword arguments are the same as @code{pkcs1-emsa-pss-encode}.
}

@subsubsection{Cryptographic conditions}
@define["Condition Type"]{@name{&crypto-error}}
@define[Function]{@name{crypto-error?} @args{obj}}
@desc{Subcondition of @code{&error}.

Base condition type of all cryptographic conditions.}

@define["Condition Type"]{@name{&encrypt-error}}
@define[Function]{@name{encrypt-error?} @args{obj}}
@define[Function]{@name{condition-encrypt-mechanism} @args{encrypt-error}}
@desc{This condition will be raised when encrypt operation is failed.}

@define["Condition Type"]{@name{&decrypt-error}}
@define[Function]{@name{decrypt-error?} @args{obj}}
@define[Function]{@name{condition-decrypt-mechanism} @args{decrypt-error}}
@desc{This condition will be raised when decrypt operation is failed.}

@define["Condition Type"]{@name{&encode-error}}
@define[Function]{@name{encode-error?} @args{obj}}
@desc{This condition will be raised when encoding operation is failed.}

@define["Condition Type"]{@name{&decode-error}}
@define[Function]{@name{decode-error?} @args{obj}}
@desc{This condition will be raised when decoding operation is failed.}

@define[Function]{@name{raise-encrypt-error}
 @args{who message mechanism :optional irritants}}
@desc{@var{who}, @var{message} and @var{irritants} are the same as
@code{assertion-violation}.

@var{mechanism} should be a name of cryptographic algorithm.

Raises @code{&encrypt-error}.
}
@define[Function]{@name{raise-decrypt-error}
 @args{who message mechanism :optional irritants}}
@desc{@var{who}, @var{message} and @var{irritants} are the same as
@code{assertion-violation}.

@var{mechanism} should be a name of cryptographic algorithm.

Raises @code{&decrypt-error}.
}
@define[Function]{@name{raise-encode-error}
 @args{who message :optional irritants}}
@desc{@var{who}, @var{message} and @var{irritants} are the same as
@code{assertion-violation}.

Raises @code{&encode-error}.
}
@define[Function]{@name{raise-decode-error}
 @args{who message :optional irritants}}
@desc{@var{who}, @var{message} and @var{irritants} are the same as
@code{assertion-violation}.

Raises @code{&decode-error}.
}

@subsubsection[:tag "custom.cipher"]{Creating own cipher}

If Sagittarius does not support sufficient cipher algorithm for you, then you
can write own cipher such as DSA. For this purpose, you might need to know how
this library works. It will be described the bottom of this section. If you just
want to create a new cipher, you just need to follow the example.

@codeblock{
(import (rnrs) (crypto) (clos user) (sagittarius))

(define (sample-encrypt pt key) pt)
(define (sample-decrypt ct key) ct)

(define-class <sample-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <sample-cipher-spi>) initargs)
  (slot-set! o 'name 'sample)
  (slot-set! o 'key #f)
  (slot-set! o 'encrypt sample-encrypt)
  (slot-set! o 'decrypt sample-decrypt)
  (slot-set! o 'padder #f)
  (slot-set! o 'signer (lambda _ #vu8()))
  (slot-set! o 'verifier (lambda _ #t))
  (slot-set! o 'keysize (lambda _ 0)))
(define-class <sample> () ())
(define sample (make <sample>))
(register-spi sample <sample-cipher-spi>)
;; test sample-cipher
(define sample-cipher (cipher sample #f))
(define message (string->utf8 "sample message"))
(let ((encrypted-message (encrypt sample-cipher message)))
  (decrypt sample-cipher encrypted-message))
;; -> #vu8(115 97 109 112 108 101 32 109 101 115 115 97 103 101)
}

The sample code actually does nothing. If you want to see real working code,
@code{ext/crypto/crypto/key/rsa.scm} might be a good example for you.

The basic idea of creating a new cipher is that you need to define own subclass
of @code{<cipher-spi>} and register it. 

@define[Class]{@name{<cipher-spi>}}
@desc{The base class for all SPI (Service Provider Interface).

Subclass must set these slots.

@dl-list{
  @dl-item["encrypt"]{The value must be a procedure which takes 2 arguments.}
  @dl-item["decrypt"]{The value must be a procedure which takes 2 arguments.}
  @dl-item["padder"]{The value must be #f or a procedure which takes 2
    arguments.
  }
}

NOTE: Default symmetric key ciphers use @code{pkcs5-padder} which takes 3
arguments, bytevector, block-size and padding flag. This is because the
procedure can be used by multi ciphers. And custom cipher must know its own
block size.

These slots are optional.

@dl-list{
  @dl-item["name"]{Describe the cipher.}
  @dl-item["key"]{The value will be passed to @code{encrypt}, @code{decrypt},
    @code{sign} and @code{verify} to be used.}
  @dl-item["signer"]{A procedure for signing. The given procedure must accept at
    least 2 arguments.
  }
  @dl-item["verifier"]{A procedure for verifying. The given procedure must
    accept at least 3 arguments.
  }
  @dl-item["keysize"]{A procedure to get recommended keysize of this cipher. The
    given procedure must accept 1 argument.
  }
}

NOTE: Even required slots, Sagittarius does not check if it's set or not.
}

@define[Function]{@name{register-spi} @args{mark spi}}
@desc{Register custom cipher spi.

@var{mark} can be any thing which returns #t then compared by @code{equal?}

@var{spi} must be subclass of @code{<cipher-spi>}

NOTE: We recommend to make @var{mark} the same as example code does and export
the registered @var{mark}.
}

The concept of this SPI is influenced by Java's JCE. The toplevel of cipher is
just a wrapper for real implementaion (SPI). When a cipher is created, the
@code{cipher} procedure actually creates an instance of SPI class and set it to
the cipher object. So users need not to know about the implementation and if the
implementation supply default parameter then users even can use it by default.

This is the class hierarchy of these crypto objects.

@codeblock{
+ <top>
  + <crypto>
      + <cipher>
      + <cipher-spi>
          + <builtin-cipher-spi> <- default implementations of symmetric keys.
          + <rsa-cipher-spi>     <- default RSA implementation
      + <key>
          + <symmetric-key>
              + <builtin-symmetric-key> <- default symmetric key. ex. DES
          + <asymmetric-key>
              + <private-key>
                  + <rsa-private-key>
                      + <rsa-private-crt-key>
              + <public-key>
                  + <rsa-public-key>
}

The @code{<cipher>} and @code{builtin-} prefixed classes can not have any
subclass.
