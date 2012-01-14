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
@define[Function]{@name{cipher}
 @args{type key :key (mode MODE_ECB) (iv #f) (padder pkcs5-padder)
       (rounds 0) (ctr-mode CTR_COUNTER_LITTLE_ENDIAN)}
@desc{Creates a cipher object.

@var{type} must be known cryptographic altorithm. Currently, @code{(crypto)}
library exports the alogorithm below.
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

encrypt
decrypt
sign
verify
key?
generate-secret-key
generate-key-pair
generate-private-key
generate-public-key
RSA
keypair-private
keypair-public
private-key
private-key?
public-key
public-key?
private-key
private-key?
public-key
public-key?
pkcs5-padder
pkcs-v1.5-padding
PKCS-1-EME
PKCS-1-EMSA
pkcs1-emsa-pss-encode
pkcs1-emsa-pss-verify
mgf-1
pkcs1-emsa-v1.5-encode
pkcs1-emsa-v1.5-verify


&crypto-error crypto-error?
&encrypt-error encrypt-error?
&decrypt-error decrypt-error?
&encode-error encode-error?
&decode-error decode-error?
raise-encrypt-error
raise-decrypt-error
raise-encode-error
raise-decode-error
