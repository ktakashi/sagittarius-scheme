@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.pem"]{(rfc pem) - PEM format  library}

@define[Library]{@name{(rfc pem)}}
@desc{This library provides PEM format file parser.

Currently only supports RFC 1421 format.
}

@subsubsection{Conditions}

This library defines these conditions.

@define["Condition Type"]{@name{&pem-error}}
@desc{Super condition of all PEM file process related conditions.}
@define["Condition Type"]{@name{&invalid-pem-format}}
@desc{This condition indicates, given PEM file contains invalid format.}

@subsubsection{Operations}

@define[Function]{@name{parse-pem}
 @args{in :key (multiple #f) (builder #f) (asn1 #f) decoder}}
@desc{@var{in} must be textual input port.

Parses given input port @var{in} and returns 2 values, parameter alist and
decoded bytevector.

Keyword arguments
@dl-list{
  @dl-item[@code{multiple}]{
    When this keyword argument is #t, then the procedure returns a list which
    contains alist of parameter and content.

    This parameter is useful for the PEM files which contains multiple contents.
  }
  @dl-item[@code{builder}]{
    This keyword argument must take a procedure which accept one argument or
    #f. If @var{builder} is specified then the given procedure will be called
    to build then contents of the PEM.

    This argument is not correspond with @var{asn1} keyword argument and has
    higher priority. So if both arguments are specified, then @var{builder}
    will be used.
  }
  @dl-item[@code{asn1}]{
    When this keyword argument is #t, then the procedure converts BASE64
    bytevector to ASN.1 object defined in @code{(asn.1)} library.
  }
  @dl-item[@code{decorder}]{
    When this keyword argument is specified, it must be a procedure which
    accepts one argument, then the @code{parse-pem} uses the specified
    procedure to convert body of the PEM content.

    If it's not specified, the procedure uses BASE64 decoding.
  }
}

The procedure may raise following conditions:
@dl-list{
  @dl-item[@code{&invalid-pem-format}]{
    When given @var{in} contains invalid PEM format.
  }
  @dl-item[@code{&pem-error}]{
    When given @var{in} contains Pre-Encapsulation Boundary as the end of
    Encapsulated Message and @code{:multiple} is #f.

    For example:
    @codeblock{
-----BEGIN FOO-----
... foo value ...
-----BEGIN BAR-----
... bar value...
-----END BAR-----
    }
    parsing PEM like above must specify @code{:multiple} with true value.
    Otherwise, @code{&pem-error} is signaled.
  }
}
}

@define[Function]{@name{parse-pem-file} @args{file :rest options}}
@define[Function]{@name{parse-pem-string} @args{pem-string :rest options}}
@desc{Convenient procedures.

Parse given file and PEM string, respectively.

@var{option} will be passed to the @code{parse-pem}.
}