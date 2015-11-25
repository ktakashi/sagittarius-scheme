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

@define[Function]{@name{open-base64-encode-input-port}
 @args{source :key (owner? #f) (line-width #f)}}
@define[Function]{@name{open-base64-encode-output-port}
 @args{sink :key (owner? #f) (line-width #f)}}
@desc{Creates binary Base64 encode input and output port, respectively.

@var{source} must be binary inpurt port.

The input port reads bytes from @var{source} and returns Base64 encoded
result.

@var{sink} must be binary inpurt port.

The output port puts encoded bytes to @var{sink}. The port must be closed
to finish the encoding process properly.
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

@define[Function]{@name{open-base64-decode-input-port}
 @args{source :key (owner? #f)}}
@define[Function]{@name{open-base64-decode-output-port}
 @args{sink :key (owner? #f)}}
@desc{Creates binary Base64 decode input and output port, respectively.

@var{source} must be binary inpurt port.

The input port reads Base64 encoded bytes from @var{source} and returns 
decoded results.

@var{sink} must be binary inpurt port.

The output port puts decoded bytes to @var{sink}. The port must be closed
to finish the encoding process properly.
}

