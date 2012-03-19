@; -*- coding: utf-8 -*-
@subsection[:tag "asn.1"]{(asn.1) - Abstract Syntas Notation One library}

This section describes @code{(asn.1)} library. The library supports DER and BER
formats. We do not describe DER or BER format here.

@define[Library]{@name{(asn.1)}}
@desc{Top most library of asn.1 procedure collection libraries. There are
multiple of related libraries, however we do not mention those because it might
be changed in future. So do not use the child libraries directly, use this.}

@subsubsection{High level user APIs}

@define[Function]{@name{encode} @args{obj :key (encoder der-encode)}}
@desc{@var{obj} must be asn.1-object which described below sections.

If you specify keyword argument @var{encoder}, it must be generic function
which accepts an object and a binary output port respectively.

Encodes given @var{obj} to bytevector. If @var{obj} contains BER object, the
procedure encodes it to BER. This might be changed in future to configure which
format this should encode.

If you want to encode to other format such as CER or XER, then you need to
implement an encoder. This procedure does not check given arguments type, just
pass @var{obj} to @var{encoder} with binary output port.
}

@define[Function]{@name{read-asn.1-object} @args{in}}
@desc{@var{in} must be binary input port.

Reads asn.1-object from given port. The port must be DER or BER encoded stream
otherwise it raises @code{&assertion}
}

@subsubsection{Middle level user APIs}

This section is incompleted.

@define[Generic]{@name{make-der-application-specific}
 @args{<boolean> <integer> <bytevector>}}
@define[Generic]{@name{make-der-application-specific}
 @args{<integer> <bytevector>}}
@define[Generic]{@name{make-der-application-specific}
 @args{<boolean> <integer> <der-encodable>}}
@define[Generic]{@name{make-der-application-specific}
 @args{<integer> <der-encodable>}}
@desc{Creates a DER application specific object.}

@define[Generic]{@name{make-der-bit-string} @args{<bytevector> <integer>}}
@define[Generic]{@name{make-der-bit-string} @args{<bytevector>}}
@desc{Creates a DER bit string object.}

@define[Generic]{@name{make-der-bmp-string} @args{<bytevector>}}
@define[Generic]{@name{make-der-bmp-string} @args{<string>}}
@desc{Creates a DER bmp string object.}

@define[Generic]{@name{make-der-octet-string} @args{<bytevector>}}
@define[Generic]{@name{make-der-octet-string} @args{<der-encodable>}}
@desc{Creates a DER octet string object.}

@define[Generic]{@name{make-der-general-string} @args{<string>}}
@define[Generic]{@name{make-der-general-string} @args{<bytevector>}}
@desc{Creates a DER general string object.}

@define[Generic]{@name{make-der-ia5-string} @args{<string>}}
@define[Generic]{@name{make-der-ia5-string} @args{<string> <boolean>}}
@define[Generic]{@name{make-der-ia5-string} @args{<bytevector>}}
@desc{Creates a DER IA5 string object.}

@define[Generic]{@name{make-der-numeric-string} @args{<string>}}
@define[Generic]{@name{make-der-numeric-string} @args{<string> <boolean>}}
@define[Generic]{@name{make-der-numeric-string} @args{<bytevector>}}
@desc{Creates a DER numeric string object.}

@define[Generic]{@name{make-der-printable-string} @args{<string>}}
@define[Generic]{@name{make-der-printable-string} @args{<string> <boolean>}}
@define[Generic]{@name{make-der-printable-string} @args{<bytevector>}}
@desc{Creates a DER printable string object.}

@define[Generic]{@name{make-der-t61-string} @args{<string>}}
@define[Generic]{@name{make-der-t61-string} @args{<bytevector>}}
@desc{Creates a DER T61 string object.}

@define[Generic]{@name{make-der-universal-string} @args{<bytevector>}}
@desc{Creates a DER universal string object.}

@define[Generic]{@name{make-der-utf8-string} @args{<string>}}
@define[Generic]{@name{make-der-utf8-string} @args{<bytevector>}}
@desc{Creates a DER UTF8 string object.}

@define[Generic]{@name{make-der-visible-string} @args{<string>}}
@define[Generic]{@name{make-der-visible-string} @args{<bytevector>}}
@desc{Creates a DER visible string object.}

@define[Generic]{@name{make-der-boolean} @args{<boolean>}}
@define[Generic]{@name{make-der-boolean} @args{<bytevector>}}
@desc{Creates a DER boolean object.}

@define[Generic]{@name{make-der-enumerated} @args{<bytevector>}}
@define[Generic]{@name{make-der-enumerated} @args{<integer>}}
@desc{Creates a DER enumerated object.}

@define[Generic]{@name{make-der-integer} @args{<integer>}}
@define[Generic]{@name{make-der-integer} @args{<bytevector>}}
@desc{Creates a DER integer object.}

@define[Generic]{@name{make-der-object-identifier} @args{<string>}}
@define[Generic]{@name{make-der-object-identifier} @args{<bytevector>}}
@desc{Creates a DER OID object.}

@define[Generic]{@name{make-der-sequence}}
@define[Generic]{@name{make-der-sequence} @args{<der-encodable>}}
@define[Generic]{@name{make-der-sequence} @args{o @dots{}}}
@desc{Creates a DER sequence object.

If the third form is used, @var{o}s must be list of @code{<der-encodable>}.
}

@define[Generic]{@name{make-der-set}}
@define[Generic]{@name{make-der-set} @args{<der-encodable>}}
@define[Generic]{@name{make-der-set} @args{o @dots{}}}
@desc{Creates a DER set object.

If the third form is used, @var{o}s must be list of @code{<der-encodable>}.
}

@define[Generic]{@name{make-der-null}}
@desc{Creates a DER null object.}

@define[Generic]{@name{make-der-generalized-time} @args{<string>}}
@define[Generic]{@name{make-der-generalized-time} @args{<bytevector>}}
@define[Generic]{@name{make-der-generalized-time} @args{<date>}}
@desc{Creates a DER generalized time object.}

@define[Generic]{@name{make-der-utc-time} @args{<string>}}
@define[Generic]{@name{make-der-utc-time} @args{<bytevector>}}
@define[Generic]{@name{make-der-utc-time} @args{<date>}}
@desc{Creates a DER UTC time object.}

@define[Generic]{@name{make-der-tagged-object}
 @args{<boolean> <integer> <der-encodable>}}
@define[Generic]{@name{make-der-tagged-object}
 @args{<integer> <der-encodable>}}
@define[Generic]{@name{make-der-tagged-object} @args{<integer>}}
@desc{Creates a DER tagged object.}

@define[Generic]{@name{make-der-external}
 @args{<der-object-identifier> <der-integer>
       <asn.1-object> <der-tagged-object>}}
@define[Generic]{@name{make-der-external}
 @args{<der-object-identifier> <der-integer>
       <asn.1-object> <integer> <der-object>}}
@desc{Creates a DER external object.}

@define[Generic]{@name{make-ber-constructed-octet-string} @args{<bytevector>}}
@define[Generic]{@name{make-ber-constructed-octet-string} @args{l @dots{}}}
@desc{Creates a BER constructed object.}

@define[Generic]{@name{make-ber-application-specific}
 @args{<integer> l @dots{}}}
@desc{Creates a BER application specific object.}

@define[Generic]{@name{make-ber-tagged-object}
 @args{<boolean> <integer> <der-encodable>}}
@desc{Creates a BER tagged object.}

@define[Generic]{@name{make-ber-sequence} @args{l @dots{}}}
@desc{Creates a BER sequence object.

@var{l}s must be list of @code{<der-encodable>}.
}

@define[Generic]{@name{make-ber-set} @args{l @dots{}}}
@desc{Creates a BER set object.

@var{l}s must be list of @code{<der-encodable>}.
}

@define[Generic]{@name{make-ber-null}}
@desc{Creates a BER null object.}

@subsubsection{Low level User APIs}

This section is incompleted. Here must desribe classes defined in @code{(asn.1)}
library.