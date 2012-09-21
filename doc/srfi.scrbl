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
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-26/srfi-26.html"]{SRFI-26}}
    @td{(srfi :26 cut)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-27/srfi-27.html"]{SRFI-27}}
    @td{(srfi :27 random-bits)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-29/srfi-29.html"]{SRFI-29}}
    @td{(srfi :27 localization)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-31/srfi-31.html"]{SRFI-31}}
    @td{(srfi :31 rec)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-37/srfi-27.html"]{SRFI-37}}
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
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-49/srfi-49.html"]{SRFI-49}}
    @td{(srfi :49)

        The library exports @code{srfi-49-read}, @code{srfi-49-load} procedures.
        And also be able to replace reader, For more detail, see
@secref["lib.sagittarius.reader"]{(sagittarius reader) - reader macro library}.}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-61/srfi-61.html"]{SRFI-61}}
    @td{This SRFI is supported by builtin @code{cond}}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-64/srfi-64.html"]{SRFI-64}}
    @td{(srfi :64 testing)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-86/srfi-86.html"]{SRFI-86}}
    @td{(srfi :86 mu-and-nu)}}
@tr{@td{@hyperlink[:href "http://srfi.schemers.org/srfi-98/srfi-98.html"]{SRFI-98}}
    @td{(srfi :98 os-environment-variables)}}
}

Each library can be imported like this:
@snipet{(import (srfi :1))}
So you don't have to type the long name.

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

