@; -*- mode:scribble; coding: utf-8 -*-

@section[:tag "srfi"]{Supporting SRFIs}

SRFI is a great libraries, so there is no reason not to support. Without
exception Sagittarius also supports several SRFIs. The following list is the
supported SRFI. Documents are not written for now. So if you need to refer the
functions, please look for SRFI's site. I might write it later.

For now, I just put pointer to @hyperlink[:href "http://srfi.schemers.org/"]{the SRFI's web site}

@table[]{
@tr{@th{SRFI number} @th{Library name}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-0/srfi-0.html"]{SRFI-0}}
    @td{(srfi :0 cond-expand)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-1/srfi-1.html"]{SRFI-1}}
    @td{(srfi :1 lists)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-2/srfi-2.html"]{SRFI-2}}
    @td{(srfi :2 and-let*)}}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-4/srfi-4.html"]{SRFI-4}}
    @td{(srfi :4)

        This SRFI also contains reader macro described below this section.}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-6/srfi-6.html"]{SRFI-6}}
    @td{(srfi :6 basic-string-ports)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-8/srfi-8.html"]{SRFI-8}}
    @td{(srfi :8 receive)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-13/srfi-13.html"]{SRFI-13}}
    @td{(srfi :13 strings)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-14/srfi-14.html"]{SRFI-14}}
    @td{(srfi :14 char-set)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-17/srfi-17.html"]{SRFI-17}}
    @td{(srfi :17 generalized-set!)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-18/srfi-18.html"]{SRFI-18}}
    @td{(srfi :18 multithreading)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-19/srfi-19.html"]{SRFI-19}}
    @td{(srfi :19 time)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-22/srfi-22.html"]{SRFI-22}}
    @td{This SRFI does not provide any library.}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-23/srfi-23.html"]{SRFI-23}}
    @td{(srfi :23 error)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-25/srfi-25.html"]{SRFI-25}}
    @td{(srfi :25 multi-dimensional-arrays)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-26/srfi-26.html"]{SRFI-26}}
    @td{(srfi :26 cut)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-27/srfi-27.html"]{SRFI-27}}
    @td{(srfi :27 random-bits)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-29/srfi-29.html"]{SRFI-29}}
    @td{(srfi :27 localization)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-31/srfi-31.html"]{SRFI-31}}
    @td{(srfi :31 rec)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-37/srfi-37.html"]{SRFI-37}}
    @td{(srfi :37 args-fold)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-38/srfi-38.html"]{SRFI-38}}
    @td{(srfi :38 with-shared-structure)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-39/srfi-39.html"]{SRFI-39}}
    @td{(srfi :39 parameters)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-41/srfi-41.html"]{SRFI-41}}
    @td{(srfi :41 streams)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-42/srfi-42.html"]{SRFI-42}}
    @td{(srfi :42 eager-comprehensions)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-43/srfi-43.html"]{SRFI-43}}
    @td{(srfi :43 vectors)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-45/srfi-45.html"]{SRFI-45}}
    @td{(srfi :45 lazy)}}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-49/srfi-49.html"]{SRFI-49}}
    @td{(srfi :49)

        The library exports @code{srfi-49-read}, @code{srfi-49-load} procedures.
        And also be able to replace reader, For more detail, see
@secref["lib.sagittarius.reader"]{(sagittarius reader) - reader macro library}.}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-60/srfi-60.html"]{SRFI-60}}
    @td{(srfi :60 integer-bits)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-61/srfi-61.html"]{SRFI-61}}
    @td{This SRFI is supported by builtin @code{cond}}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-64/srfi-64.html"]{SRFI-64}}
    @td{(srfi :64 testing)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-69/srfi-69.html"]{SRFI-69}}
    @td{(srfi :69 basic-hash-tables)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-78/srfi-78.html"]{SRFI-78}}
    @td{(srfi :78 lightweight-testing)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-86/srfi-86.html"]{SRFI-86}}
    @td{(srfi :86 mu-and-nu)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-98/srfi-98.html"]{SRFI-98}}
    @td{(srfi :98 os-environment-variables)}}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-105/srfi-105.html"]{SRFI-105}}
    @td{(srfi :105)

    The library exports @code{curly-infix-read} and @code{neoteric-read}
    procedures. These procedures read SRFI-105 the infix style code that
    SRFI-105 specifying. And this also exports reader macros, you can
    activate it with @code{#!read-macro=srfi/:105} or 
    @code{#!read-macro=curly-infix}.

    Even though the specification said it MUST support @code{#!curly-infix},
    however the library just ignore and not activate the reader macros. So
    you need to explicitly write the one mentioned above. To keep your code
    portable between implementations that support this SRFI, you need to write
    both style as following;
@codeblock{
;; write both
#!read-macro=curly-infix
#!curly-infix
}
    The order doesn't matter, Sagittarius just ignores the latter style.
    }}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-106/srfi-106.html"]{SRFI-106}}
    @td{(srfi :106 socket)}}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-110/srfi-110.html"]{SRFI-110}}
    @td{(srfi :110)

    The library exports a replacible reader. To use it write following
    hash-bang directives.
@codeblock{
;; write both for compatibility
#!reader=sweet
#!sweet
}
    The order doesn't matter, Sagittarius just ignores the latter style.
    }}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-111/srfi-111.html"]{SRFI-111}}
    @td{(srfi :111 boxes)

    The long name is Sagittarius specific and the specified library name 
    is @code{(srfi :111)}. So for the portability it's better to use the
    @code{(srfi :111)}.

    The name @code{boxes} is taken from R7RS-large library name.
    }}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-112/srfi-112.html"]{SRFI-112}}
    @td{(srfi :112 inquery)

    The long name is Sagittarius specific and the specified library name 
    is @code{(srfi :112)}. So for the portability it's better to use the
    @code{(srfi :112)}.

    The name @code{inquery} is taken from R7RS-large library name.
    }}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-113/srfi-113.html"]{SRFI-113}}
    @td{(srfi :113 sets)

    The long name is Sagittarius specific and the specified library name 
    is @code{(srfi :113)}. So for the portability it's better to use the
    @code{(srfi :113)}.

    The name @code{sets} is taken from its name.
    }}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-114/srfi-114.html"]{SRFI-114}}
    @td{(srfi :114 comparators)

    The long name is Sagittarius specific and the specified library name 
    is @code{(srfi :114)}. So for the portability it's better to use the
    @code{(srfi :114)}.

    The name @code{comparators} is taken from its name.
    }}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-115/srfi-115.html"]{SRFI-115}}
    @td{(srfi :115 regexp)

    The long name is Sagittarius specific and the specified library name 
    is @code{(srfi :115)}. So for the portability it's better to use the
    @code{(srfi :115)}.

    The name @code{regexp} is taken from reference implementation provided
    on Chibi Scheme.
    }}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-116/srfi-116.html"]{SRFI-116}}
    @td{(srfi :116 ilists)

    The long name is Sagittarius specific and the specified library name 
    is @code{(srfi :116)}. So for the portability it's better to use the
    @code{(srfi :116)}.

    The name @code{ilists} is taken from sample implementation.
    }}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-117/srfi-117.html"]{SRFI-117}}
    @td{(srfi :117 list-queue)

    The long name is Sagittarius specific and the specified library name 
    is @code{(srfi :117)}. So for the portability it's better to use the
    @code{(srfi :117)}.

    The name @code{list-queue} is taken from sample implementation.
    }}
@tr{@td[:style "vertical-align: top;"]{@hyperlink[:href "http://srfi.schemers.org/srfi-120/srfi-120.html"]{SRFI-120}}
    @td{(srfi :120 timer)

    The long name is Sagittarius specific and the specified library name 
    is @code{(srfi :120)}. So for the portability it's better to use the
    @code{(srfi :120)}.

    The name @code{timer} is taken from sample implementation.
    }}

}

Each library can be imported like this: 
@snipet{(import (srfi :1))}
So you don't have to type the long name.

All libraries have R7RS style library name as well. So SRFI-1 can be imported
like this:
@snipet{(import (srfi 1))}

@subsection[:tag "srfi.reader.macro"]{Reader macros for SRFIs}

@subsubsection{SRFI-4}

The SRFI-4 also defines its reader macro. Sagittarius also suppots these. It
defines tagged vector and the tags can be @code{s8}, @code{u8}, @code{s16},
@code{u16}, @code{s32}, @code{u32}, @code{s64}, @code{u64}, @code{f32} or
@code{f64}. For each value of tags, the external representation of instances of
the vector is @code{#@var{tag}(... elements ...)}

On Sagittarius, these reader macros are not automatically enabled. You need to
explicitly import it. For more detail, see
@secref["lib.sagittarius.reader"]{(sagittarius reader) - reader macro library}.

