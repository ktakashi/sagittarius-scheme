@; -*- coding: utf-8 -*-
@subsection[:tag "pkcs.5"]{(rsa pkcs :5) - Password Based Cryptography library}

This section describes the implementation of PKCS#5 specification library.
However we do not describe PKCS#5 itself and I don't think it is necessary to
know it if you just want to use it.

This example is the simplest way to use.

@codeblock{
(import (rnrs) (crypto) (rsa pkcs :5))
(define salt (string->utf8 "salt"))
(define iteration-count 1024)
(define pbe-parameter (make-pbe-parameter salt iteration-count))
(define pbe-key (generate-secret-key pbe-with-sha1-and-des "password"))
(define pbe-cipher (cipher pbe-with-sha1-and-des pbe-key 
                           :parameter pbe-parameter))
(encrypt pbe-cipher (string->utf8 "This is an example."))
;; -> #vu8(254 229 155 168 167 192 249 43 33 192 161 215 28 117
;;         169 129 147 60 16 52 235 79 90 23)
(decrypt pbe-cipher 
	 #vu8(254 229 155 168 167 192 249 43 33 192 161 215
              28 117 169 129 147 60 16 52 235 79 90 23))
;; -> #vu8(84 104 105 115 32 105 115 32 97 110 32 101 120 97
;;         109 112 108 101 46)
}

The library itself defines simply its cipher and key generate methods. Hence
user can use it with (crypto) library (see 
@secref["crypto"]{(crypto) - Cryptographic library})

NOTE: Currently the library only supports encryption and decryption, not MAC
generation nor verification.

NOTE: When you create cipher object with PBE related algorithms, the you need 
to pass @code{:parameter} keyword to @code{cipher} procedure, otherwise raises
an error.

@define[Library]{@name{(rsa pkcs :5)}}
@desc{This library exports PKCS#5 related cryptographic procedures.}

@subsubsection{User level APIs}

@define[Variable]{@name{pbe-with-md5-and-des}}
@define[Variable]{@name{pbe-with-md5-and-rc2}}
@define[Variable]{@name{pbe-with-sha1-and-des}}
@define[Variable]{@name{pbe-with-sha1-and-rc2}}
@desc{The algorithms used in PBES1 encryption and decryption and key generation.

The names describe the using hash functions and cryptographic schemes. For
example, @code{pbe-with-md5-and-des} uses MD5 hash and DES algorithm.
}

@define[Function]{@name{make-pbe-parameter} @args{salt iteration-count}}
@desc{@var{salt} must be a bytevector.

@var{iteration-count} must be a non negative exact integer.

Creates a parameter for PBE key generation. @var{salt} is salt and
@var{iteration-count} is the iteration count  for key derivation.
}

@define[Generic]{@name{generate-secret-key}
 @args{algorithm (password <string>)}}
@desc{@var{algorithm} must be one of the algorithms describes above.

Creates PBE secret key based on given @var{password} and @var{algorithm}.
}

@subsubsection{Low level APIs}

These APIs are for users who want to create own PBE mechanism such as PKCS#12.

@define[Function]{@name{pbkdf-1}
 @args{P S c dk-len :key (hash (hash-algorithm SHA-1)}}
@desc{Implementation of PBKDF1 describes in PKCS#5 specification.

The arguments are plain text (bytevector), salt (bytevector), iteration count
(non negative exact integer) and key length (non negative exact integer)
respectively.

The keyword argument @var{hash} specifies which hash algorithm will be used. The
default is SHA-1. 
}

@define[Function]{@name{pbkdf-2}
 @args{P S c dk-len :key (hash (hash-algorithm SHA-1)
 (prf (hash-algorithm HMAC :key P :hash hash))}}
@desc{Implementation of PBKDF2 describes in PKCS#5 specification.

The arguments are plain text (bytevector), salt (bytevector), iteration count
(non negative exact integer) and key length (non negative exact integer)
respectively.

The keyword argument @var{hash} specifies which hash algorithm will be used in
PRF function. The default is SHA-1 and if you don't have any reasonable reason,
this must not be changed.

The keyword argument @var{prf} specifies underlying pseudo random function which
must be hash object implemented with @code{<user-hash-algorithm>} describes in
@secref["custom.hash"]{Custom hash algorithm}. The default is HMAC and if you
don't have any reasonable reason, this must not be changed.
}

@define[Function]{@name{derive-key}
 @args{P S c dk-len :key (kdf pbkdf-2) :allow-other-keys}}
@desc{The implementation of key derive function. The required arguments are
the same as above @code{pbkdf-1} and @code{pbkdf-2}.

This procedure just calls given @var{kdf} with given arguments and returns
derived key bytevector.
}

@define[Generic]{@name{derive-key&iv}}
@desc{The PKCS#5 encryption and decryption procedures require to derive both
key and initial vector from given password and parameter (salt and iteration
count). This method is used in PBE cipher to derive key and initial vector.

The purpose of this method is to re-use @code{<pbe-cipher-spi>}. For example,
PKCS#12 can use this cipher, however it requires different key derivation
mechanism.
}

@subsubsection{Supporting PBES2 functionality}

Since I could not find any standard implementation, Sagittarius actually does
not support PBES2 encryption and decryption. However supporting it is not so
difficult. This is the sample code to support it.

@codeblock{
(import (rnrs) (clos user) (rfc hmac) (math))
(define-class <pbkef2-with-hmac-sha1-des3> () ())
(define pbkdf2-with-hmac-sha1-des3 (make <pbkef2-with-hmac-sha1-des3>))
(define-method generate-secret-key ((mark <pbkef2-with-hmac-sha1-des3>)
				    (password <string>))
  (make <pbe-secret-key> :password  password :hash (hash-algorithm HMAC)
	:scheme DES3 :iv-size 8 :length 24
	:type PKCS5-S2))
(register-spi pbkdf2-with-hmac-sha1-des3 <pbe-cipher-spi>)
}

And using this supported PBES2 cipher is like this;

@codeblock{
(let* ((param (make-pbe-parameter (string->utf8 "saltsalt") 1024))
       (key (generate-secret-key pbkdf2-with-hmac-sha1-des3 "password"))
       (pbe-cipher (cipher pbkdf2-with-hmac-sha1-des3 key :parameter param))
       (ciphertext (encrypt pbe-cipher (string->utf8 "This is an example."))))
  (utf8->string (decrypt pbe-cipher ciphertext)))
}

I suppose this is correct, but I could not find any implementation which
supports PBES2. I usually test with JCE, so if you have some recommendation,
please let me know.
