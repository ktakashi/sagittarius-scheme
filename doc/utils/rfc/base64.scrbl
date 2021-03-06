@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.base64"]{(rfc base64) - Base 64 encode and decode library}

@define[Library]{@name{(rfc base64)}}
@desc{This library provides Base 64 encoding and decoding procedures.}

@subsubsection{Encoding procedures}

@define[Function]{@name{base64-encode}
 @args{in :key (line-width 76) (padding? #t)}}
@define[Function]{@name{base64url-encode}
 @args{in :key (line-width #f) (padding? #f)}}
@desc{@var{in} must be either a bytevector or binary input port.

Encodes given input @var{in} to Base 64 encoded bytevector.

The keyword argument @var{line-width} specifies where the encode procedure
should put linefeed. If this is less than 1 or #f, encoder does not put
linefeed.

The keyword argument @var{padding?} controls if the result encoded value
contains padding character @code{#\=} or not. If this is #f, then the result
value won't contain padding character.

The @code{base64url-encode} encodes the given input to Base 64 URL safe
encoded bytevector. Which doesn't use @code{+} and @code{/}.
}

@define[Function]{@name{base64-encode-string}
 @args{string :key (line-width 76) transcoder (padding? #t)}}
@define[Function]{@name{base64url-encode-string}
 @args{string :key (line-width #f) transcoder (padding? #f)}}
@desc{Convenient procedure for string.

Encodes given @var{string} to Base 64 encoded string.

The keyword argument @var{transcoder} is used to convert given string to
bytevector. The converted bytevector will be passed to the @code{base64-encode}
procedure. The default value is a transcoder with UTF-8 codec with EOL
style none.

The keyword argument @var{padding?} is the same as @code{base64-encode}.

The @code{base64url-encode-string} encodes the given input to Base 64 URL safe
encoded bytevector. Which doesn't use @code{+} and @code{/}.
}

@define[Function]{@name{open-base64-encode-input-port}
 @args{source :key (owner? #f) (line-width #f) (padding? #t)}}
@define[Function]{@name{open-base64-encode-output-port}
 @args{sink :key (owner? #f) (line-width #f) (padding? #t)}}
@define[Function]{@name{open-base64url-encode-input-port}
 @args{source :key (owner? #f) (line-width #f) (padding? #f)}}
@define[Function]{@name{open-base64url-encode-output-port}
 @args{sink :key (owner? #f) (line-width #f) (padding? #f)}}
@desc{Creates binary Base64 encode input and output port, respectively.

@var{source} must be binary inpurt port.

The input port reads bytes from @var{source} and returns Base64 encoded
result.

@var{sink} must be binary inpurt port.

The output port puts encoded bytes to @var{sink}. The port must be closed
to finish the encoding process properly.

The keyword argument @var{padding?} is the same as @code{base64-encode}.

The @code{open-base64url-encode-input-port} and
@code{open-base64url-encode-output-port} encode to Base64 URL safe encode.
}

@subsubsection{Decoding procedures}

@define[Function]{@name{base64-decode} @args{in}}
@define[Function]{@name{base64url-decode} @args{in}}
@desc{@var{in} must be a bytevector or binary input port.

Decode Base 64 encoded input @var{in} to original bytevector.

The @code{base64url-decode} decodes Base64 URL safe encoded value.
}

@define[Function]{@name{base64-decode-string}
 @args{string :key (transcoder (native-transcoder))}}
@define[Function]{@name{base64url-decode-string}
 @args{string :key (transcoder (native-transcoder))}}
@desc{Convenient procedure.

Decode Base 64 encoded string to original string. The procedure is using
@code{base64-decode}.

The keyword argument specifies how to convert the decoded bytevector to string.
If this is #f, the procedure returns raw bytevector.

The @code{base64url-decode-string} decodes Base64 URL safe encoded value.
}

@define[Function]{@name{open-base64-decode-input-port}
 @args{source :key (owner? #f)}}
@define[Function]{@name{open-base64-decode-output-port}
 @args{sink :key (owner? #f)}}
@define[Function]{@name{open-base64url-decode-input-port}
 @args{source :key (owner? #f)}}
@define[Function]{@name{open-base64url-decode-output-port}
 @args{sink :key (owner? #f)}}
@desc{Creates binary Base64 decode input and output port, respectively.

@var{source} must be binary inpurt port.

The input port reads Base64 encoded bytes from @var{source} and returns 
decoded results.

@var{sink} must be binary inpurt port.

The output port puts decoded bytes to @var{sink}. The port must be closed
to finish the encoding process properly.

The @code{open-base64url-decode-input-port} and
@code{open-base64url-decode-output-port} decode Base64 URL safe encoded
value.
}

@subsubsection{Low level APIs}

Both encode and decode procedures are using encoder and decoder. Both
encoder and decoder are just a procedure which takes 2 arguments
@var{get} and @var{put} and not reentrant as they have own internal
buffer to process the input.

@define[Function]{@name{make-base64-encoder}
 @args{:key (encode-table *base64-encode-table*) (line-width 76) (padding? #t)}}
@desc{
Creates a Base64 encoder. An encoder is a procedure which takes 2
arguments @var{get} and @var{put}.

@var{get} must be a procedure takes 0 argument and it must return one
of the followings; a fixnum of range 0 to 255, EOF object, or negative
integer. The fixnum value is treated as an input of the encoding
value. The negative integer value is treated as a continue
marker. When the encoder receives this value, then it won't encode
until the next value is available.

@var{put} must be a procedure takes 1 argument which is either a
fixnum of range 0 to 255 or @code{#f}. The fixnum is the encoded value
of the input. @code{#f} indicates line break point, so user can
determine which line break this encoder should use.

The following shows how to make Base64 encoder with line break of CRLF.

@codeblock{
(define (base64-encode-w/crlf bv)
  (let-values (((out e) (open-bytevector-output-port)))
    (define (put v)
      (if v
	  (put-u8 out v)
	  (begin (put-u8 out #x0d) (put-u8 out #x0a))))
    (define inp (open-bytevector-input-port bv))
    (define (in) (get-u8 inp))
    (define encoder (make-base64-encoder))
    (do () ((encoder in put)))
    (e)))
}

}

@define[Function]{@name{make-base64-decoder}
 @args{:key (decode-table *base64-decode-table*)}}
@desc{
Creates a Base64 decoder. A decoder is a procedure which takes 2
arguments @var{get} and @var{put}.

@var{get} is the same as the one from encoder.

@var{put} must be a procedure takes 1 arguments which is a fixnm of
range 0 to 255. The value is always a decoded byte.

}

@define[Constant]{@name{*base64-encode-table*}}
@define[Constant]{@name{*base64-decode-table*}}
@define[Constant]{@name{*base64-encode-url-table*}}
@define[Constant]{@name{*base64-decode-url-table*}}
@desc{

Default encode or decode tables. If the name contains @code{url}, then it
is suitable for "base64url".

}
