@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.uri"]{(rfc uri) - Parse and construct URIs}

@define[Library]{@name{(rfc uri)}}
@desc{This library provides RFC3986 'URI Generic Syntax' procedures.}

@define[Function]{@name{uri-parse} @args{uri}}
@desc{@var{uri} must be string.

Parses given @var{uri} and returns following 7 values;
@itemlist{
@item{@var{scheme}}
@item{@var{user-info}}
@item{@var{host}}
@item{@var{port}}
@item{@var{path}}
@item{@var{query}}
@item{@var{fragment}}
}

Following examples are from RFC3986 text;
@codeblock{
   foo://example.com:8042/over/there?name=ferret#nose
   \_/   \______________/\_________/ \_________/ \__/
    |           |            |            |        |
 scheme     authority       path        query   fragment
    |   _____________________|__
   / \ /                        \
   urn:example:animal:ferret:nose
}

authority = [ @var{user-info} "@atmark{}" ] @var{host} [ ":" @var{port} ]

If given @var{uri} does not contain the part described above, it will be #f.
ex)
@snipet[=> (values http #f localhost #f #f #f #f)]{(uri-parse "http://localhost")}

}

@define[Function]{@name{uri-scheme&specific} @args{uri}}
@desc{@var{uri} must be string.

Parse given @var{uri} into scheme and rest. Returns the 2 values.
}

@define[Function]{@name{uri-decompose-hierarchical} @args{specific}}
@desc{@var{specific} must be string.

@var{specific} is a URI without scheme. For example, the specific of following 
URI 'http://localhost/foo.html' if '//localhost/foo.html'.

Parse given @var{specific} into 4 values @var{authority}, @var{path},
@var{query} and @var{fragment}.

If the @var{specific} does not contain the part, it will be #f.
}

@define[Function]{@name{uri-decompose-authority} @args{authority}}
@desc{@var{authority} must be string.

Parse given @var{authority} into  3 values, @var{user-info}, @var{host} and
@var{post}.

If the @var{authority} does not contain the part, it will be #f.
}

@define[Function]{@name{uri-decode} @args{in out :key (cgi-decode #f)}}
@desc{@var{in} must be binary input port.

@var{out} must binary output port.

Reads and decodes given @var{in} and put the result into @var{out}.

If the keyword argument @var{cgi-decode} is #t, the procedure decodes
@code{#x2b}('+') to @code{#x20}('#\space').
}

@define[Function]{@name{uri-decode-string}
 @args{string :key (encoding 'utf-8) (cgi-decode #f)}}
@desc{Decodes given @var{string} and returns decoded string.}

@define[Function]{@name{uri-encode}
 @args{in out :key (noescape *rfc3986-unreserved-char-set*)}}
@desc{@var{in} must be binary input port.

@var{out} must binary output port.

Reads and encodes given @var{in} and put the result into @var{out}.

The keyword argument @var{noescape} specifies which character must be escaped.
}

@define[Function]{@name{uri-encode-string}
 @args{string :key (encoding 'utf-8) :allow-other-keys}}
@desc{Encodes given @var{string} and returns encoded string.}

@define[Function]{@name{uri-compose}
 @args{:key (scheme #f) (userinfo #f) (host #f) (port #f) (authority #f)
            (path #f) (path* #f) (query #f) (fragment #f) (specific #f)}}
@desc{Composes URI from given arguments.

If all keyword arguments are #f, the procedure returns empty string.

The procedure put priority on bigger chunk of URI part. For example, if keyword
argument @var{specific} is specified, the procedure uses only @var{scheme} and
@var{specific}. Following describes the priority hierarchy;
@codeblock{
@var{scheme}
@var{specific}
  +- @var{authority}
       +- @var{userinfo}
       +- @var{host}
       +- @var{port}
  +- @var{path*}
       +- @var{path}
       +- @var{query}
       +- @var{fragment}
}
}

@define[Function]{@name{uri-merge}
 @args{base-uri relative-uri1 relative-uri2 ...}}
@desc{Merges given @var{relative-uris} to @var{base-uri} according to RFC 3986
section 5.
}

@define[Variable]{@name{*rfc3986-unreserved-char-set*}}
@define[Variable]{@name{*rfc2396-unreserved-char-set*}}
@desc{Charsets which contains no escape needed characters.

There is slight difference between RFC2396 and RFC3986. This library uses
RFC3986 charset by default to encode.
}
