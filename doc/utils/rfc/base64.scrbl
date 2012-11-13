@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.base64"]{(rfc base64) - Base 64 encode and decode library}

@define[Library]{@name{(rfc base64)}}
@desc{This library provides Base 64 encoding and decoding procedures.}

@subsubsection{Encoding procedures}

@define[Function]{@name{base64-encode} @args{in :key (line-width 76)}}
@desc{@var{in} must be a bytevector or binary input port.

Encodes given input @var{in} to Base 64 encoded bytevector.

The keyword argument @var{line-width} specifies where the encode procedure
should put linefeed. If this is less than 1 or #f, encoder does not put
linefeed.
}

@define[Function]{@name{base64-encode-string}
 @args{string :key (line-width 76) (transcoder (native-transcoder))}}
@desc{Convenient procedure for string.

Encodes given @var{string} to Base 64 encoded string.

The keyword argument @var{transcoder} is used to convert given string to
bytevector. The converted bytevector will be passed to the @code{base64-encode}
procedure.
}

@subsubsection{Decoding procedures}

@define[Function]{@name{base64-decode} @args{in}}
@desc{@var{in} must be a bytevector or binary input port.

Decode Base 64 encoded input @var{in} to original bytevector.
}

@define[Function]{@name{base64-decode-string}
 @args{string :key (transcoder (native-transcoder))}}
@desc{Convenient procedure.

Decode Base 64 encoded string to original string. The procedure is using
@code{base64-decode}.

The keyword argument specifies how to convert the decoded bytevector to string.
If this is #f, the procedure returns raw bytevector.
}