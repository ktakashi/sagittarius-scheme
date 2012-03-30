@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.x.509"]{(rfc x.509) - X.509 certificate utility library}

This library does not support whole feature of X.509, it just parse and verify
a message and signature. So it can not verify certificate itself.

@define[Library]{@name{(rfc x.509)}}
@desc{Exports X.509 utility procedures.}

@define[Generic]{@name{make-x509-certificate} @args{(in <port>)}}
@define[Generic]{@name{make-x509-certificate}
 @args{(sequence <asn.1-sequence>)}}
@desc{Creates an X.509 certificate object from given binary input port or ASN.1
sequence object (second form).
}

@define[Function]{@name{x509-certificate?} @args{o}}
@desc{Return #t if the @var{o} is X.509 certificate object, otherwise #f.}

@define[Function]{@name{x509-certificate-get-version} @args{x509}}
@desc{Return version of given X.509 certificate object.}

@define[Function]{@name{x509-certificate-get-serial-number} @args{x509}}
@desc{Return serial number of given X.509 certificate object.}

@define[Function]{@name{x509-certificate-get-issuer-dn} @args{x509}}
@desc{Return issuer DN of given X.509 certificate object as X.509 principal.}

@define[Function]{@name{x509-certificate-get-subject-dn} @args{x509}}
@desc{Return subject DN of given X.509 certificate object as X.509 principal.

NOTE: These Issuer DN and Subject DN getters return <x.509-principal> object,
however I did not implement any utility for this class, so it's just useless
for now.
}

@define[Function]{@name{x509-certificate-get-not-before} @args{x509}}
@desc{Return start date of given X.509 certificate object.}

@define[Function]{@name{x509-certificate-get-not-after} @args{x509}}
@desc{Return end date of given X.509 certificate object.}

@define[Function]{@name{x509-certificate-get-signature} @args{x509}}
@desc{Return signature of given X.509 certificate object. 

NOTE: This signature is not for @code{verify} described below.
}

@define[Function]{@name{x509-certificate-get-signature-algorithm} @args{x509}}
@desc{Return signature algorithm of given X.509 certificate object as an OID
string.}

@define[Function]{@name{x509-certificate-get-public-key} @args{x509}}
@desc{Return public key of given X.509 certificate object. The return value is
@code{<public-key>} described in the section
@secref["crypto"]{(crypto) - Cryptographic library}.
}

@define[Function]{@name{verify} @args{x509 message signature
  :key (verify pkcs1-emsa-v1.5-verify) (hash SHA-1)}}
@desc{@var{message} and @var{signature} must be bytevector.

Verify given @var{message} with @var{signature} and @var{x509} certificate.

This procedure uses the @code{verify} procedure in @code{(crypto)} library. The
keyword arguments will be passed to it. For more detail, see
@secref["crypto"]{(crypto) - Cryptographic library}.
}

@define[Function]{@name{check-validity}
 @args{x509 :optional (date (current-date))}}
@desc{Validate if the given certificate is valid in given @var{date}. Return #t
if it's valid, otherwise raises @code{&assertion}.
}
