@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.quoted-printable"]{(rfc quoted-printable) - Base 64 encode and decode library}

@define[Library]{@name{(rfc quoted-printable)}}
@desc{This library provides quoted printable encoding and decoding procedures.}

@subsubsection{Encoding procedures}

@define[Function]{@name{quoted-printable-encode}
 @args{bv :key (line-width 76) (binary? #f)}}
@desc{@var{bv} must be a bytevector.

Encodes given bytevector to quoted printable encoded bytevector.

The keyword argument @var{line-width} specifies where the encode procedure
should put linefeed. If this is less than 1 or #f, encoder does not put
linefeed.

If the keyword argument @var{binary?} is not #f, then the procedure encodes
#x0a and #x0b to @code{=0A} and @code{=0D} respectively.
}

@define[Function]{@name{quoted-printable-encode-string string}
 @args{:key (line-width 76) transcoder (binary? #f)}}
@desc{Convenient procedure for string.

Encodes given @var{string} to quoted printable encoded string.

The keyword argument @var{transcoder} is used to convert given string to
bytevector. The converted bytevector will be passed to the
@code{quoted-printable-encode} procedure. The default is utf-8 codec with
NONE eol style.
}

@subsubsection{Decoding procedures}

@define[Function]{@name{quoted-printable-decode} @args{bv}}
@desc{@var{bv} must be a bytevector.

Decode quoted printable encoded bytevector to original bytevector.
}

@define[Function]{@name{quoted-printable-decode-string}
 @args{string :key (transcoder (native-transcoder))}}
@desc{Convenient procedure.

Decode quoted printable encoded string to original string. The procedure is
using @code{quoted-printable-decode}.

The keyword argument specifies how to convert the decoded bytevector to string.
If this is #f, the procedure returns raw bytevector.
}