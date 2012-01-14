@; -*- coding: utf-8 -*-
@subsection[:tag "crypto"]{(crypto) - Cryptographic library}

This documentation does not describe cryptographic itself. For example, it does
not describe what initial vector is and how long it must be. So users must know
aboud cryptographics this library supports.

This library uses @code{libtomcrypt}'s functionalities. The library is public
domain. Thank you for the great library.

Note: the @code{libtomcrypt} is a huge cryptographic library and I am not so 
good with cryptographics, so the @code{(crypto)} library is not totally tested.
Just the functionalities which I usually use are tested. If you find a bug or
wrong documentation, pleas report it.

@define[Library]{@name{(crypto)}}
@desc{This library is the top most library, it exports all the other libraries
procedures. Users must import only this and not to use the others.

@code{(crypto)} library supports both symmetric cryptographic and public/private
key mechanism. For public/private key, it only supports RSA for now.
}

@define[Function]{@name{crypto-object?} @args{obj}}
@desc{Returns #t if @var{obj} is @code{crypto-object}.

@code{crypto-object} can be either @code{cipher} or @code{key}.
}

@subsubsection{Cipher operations}

@define[Function]{@name{cipher}
 @args{type key :key (mode MODE_ECB) (iv #f) (padder pkcs5-padder)
       (rounds 0) (ctr-mode CTR_COUNTER_LITTLE_ENDIAN)}}
@desc{Creates a cipher object.

@var{type} must be known cryptographic altorithm. Currently, @code{(crypto)}
library exports the alogorithm below.

The simmetric alogorithms.
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

The public key alogorithm
@define[Constant]{@name{RSA}}

@var{key} must be a bytevector and must be the exact size(key size) whith the
given algorithm requires. You can check it with the procedure
@code{cipher-keysize}.

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

@define[Function]{@name{cipher-keysize} @args{type test}}
@desc{Returns given cipher type's recommended keysize.

@var{type} must be one of the supported algorithms.

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

@var{pt} must be a bytevector.

@code{decrypt} decrypts given encripted text @var{ct} according to the given
@var{cipher}.
}

@define[Function]{@name{sign} @args{public-cipher data :optional opt}}
@desc{@var{public-cipher} must be a cipher object created with public/private
key algorithm.

@var{data} must be a bytevector.

Signs given @var{data}. This procedure is just a wrapper for the real
implementations. Currently Sagittarius supports only RSA sign.

@var{opt} can specify the signer behaviour. Currently it can be @var{encode}.
@var{encode} specifies the encoder. The defaule encoder is
@code{pkcs1-emsa-pss-encode}.
And the rest keyword arguments will be passed to encoder. Supported encoders are
described below.
}

@define[Function]{@name{verify} @args{public-cipher M S :optional opt}}
@desc{@var{public-cipher} must be a cipher object created with public/private
key algorithm.

@var{M} and @var{S} must be bytevectors.

@var{M} is master message which will be compared with encoded message.

@var{S} is signed message.

The @cod{verity} procedure verifies two messages.

@var{opt} can specify the verifier behaviour. Currently it can be @var{verify}.
@var{verify} specifies the verifier. The defaule verifier is
@code{pkcs1-emsa-pss-verify}.
And the rest keyword arguments will be passed to verifier. Supported verifiers
are described below.
}

@subsubsection{Key operations}


@define[Function]{@name{generate-secret-key} @args{type key}}
@desc{@var{type} must be one of the supported symmetric algorithm.

@var{key} must be a bytevector and its length must satisfy the keysize which
the given algorithm requires.

Returns a sercret key object.
}
@define[Function]{@name{generate-key-pair}
 @args{type :key (size 1024) (prng @code{(secure-random RC4)}) (e 65537)}}
@desc{@var{type} must be one of the supported public key algorithm.

Generates a key pair object.

@var{size} keyword argument is decides key length.

@var{prng} keyword argument is given, it will be passed to @code{random-prime}.
For more detail, see @secref["math.random"]{(math random)} library.

@var{e} keyword argument is an exponent. Usually it does not have to be
specified with other number.
}
@define[Function]{@name{generate-private-key} @args{type :optional opt}}
@desc{@var{type} must be one of the supported public key algorithm.

This is a wrapper procedure for real implementations. Currently Sagittarius
supports only RSA.

Returns private key object.

Currently @var{opt} can be these arguments.
@dl-list[]{
@dl-item["modulus"]{The private key's modulus}
@dl-item["private-exponent"]{The private key's exponent}
@dl-item["public-exponent"]{keyword argument. Used for CRT private key object.}
@dl-item["p"]{keyword argument. Used for CRT private key object.}
@dl-item["q"]{keyword argument. Used for CRT private key object.}
}

}
@define[Function]{@name{generate-public-key} @args{type :optional opt}}
@desc{@var{type} must be one of the supported public key algorithm.

This is a wrapper procedure for real implementations. Currently Sagittarius
supports only RSA.

Returns public key object.

Currently @var{opt} can be these arguments.
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
@define["Record Type"]{@name{private-key}}
@desc{Record type of private key object.}
@define["Record Type"]{@name{public-key}}
@desc{Record type of public key object.}
@define[Function]{@name{private-key?} @args{obj}}
@desc{Returns #t if given @var{obj} is private key object, otherwise #f}
@define[Function]{@name{public-key?} @args{obj}}
@desc{Returns #t if given @var{obj} is public key object, otherwise #f}

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
