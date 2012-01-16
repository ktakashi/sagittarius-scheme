@; -*- mode:scribble; coding: utf-8 -*-
@subsection[:tag "lib.sagittarius.regex"]{(sagittarius regex) - regular expression library}

As most of script language have own regular expression mechanism, Sagittarius
also has own regular expression library. It is influenced by Java's regular
expression, so there are a lot of differences between the most famous Perl
regular expression(perlre).

This feature may be changed, if R7RS large requires Perl like regular
expression.

Following examples show how to use Sagittarius's regular expression.
@codeblock[=> “world!”]{
;; For Perl like
(cond ((looking-at (regex “^hello\\s*(.+)”) “hello world!”)
            => (lambda (m) (m 1))))
}

@codeblock{
;; For Java like
(cond ((matches (regex “(\\w+?)\\s*(.+)”) “123hello world!”)) ;; this won't match
          (else “incovenient eh?”))
}

The @code{matches} procedure is total match, so it ignores boundary matcher
@code{'^'} and @code{'$'}. The @code{looking-at} procedure is partial match, so
it works as if perlre. 

@define[Library]{@name{(sagittarius regex)}}
@desc{This library provides Sagittarius regular expression procedures.}

@subsubsection{User level APIs for regular expression}

@define[Function]{@name{regex} @args{string :optional flags}}
@desc{@var{String} must be regular expression. Returns compiled regular
expression. @var{Flags}' descriptions are the end of this section. The following
table is the supported regular expression constructs.

@table[:title "Regular expression constructors"]{
@tr{@th{Construct} @th{Matches}}
@tr{@th[:colspan 2]{Characters}}
@tr{@td{@code{x}} @td{The character x}}
@tr{@td{@code{\\}} @td{The backslash character}}
@tr{@td{@code{\0n}} @td{The character with octal value 0n (0 <= n <= 7)}}
@tr{@td{@code{\0nn}} @td{The character with octal value 0nn (0 <= n <= 7)}}
@tr{@td{@code{\0mnn}} @td{The character with octal value 0mnn (0 <= m <= 3, 0 <= n <= 7)}}
@tr{@td{@code{\xhh}} @td{The character with hexadecimal value 0xhh}}
@tr{@td{@code{\uhhhh}} @td{The character with hexadecimal value 0xhhhh}}
@tr{@td{@code{\Uhhhhhhhh}} @td{The character with hexadecimal value
0xhhhhhhhh. If the value exceed the maxinum fixnum value it rases an error.}}
@tr{@td{@code{\t}} @td{The tab character ('\u0009')}}
@tr{@td{@code{\n}} @td{The newline (line feed) character ('\u000A')}}
@tr{@td{@code{\r}} @td{The carriage-return character ('\u000D')}}
@tr{@td{@code{\f}} @td{The form-feed character ('\u000C')}}
@tr{@td{@code{\a}} @td{The alert (bell) character ('\u0007')}}
@tr{@td{@code{\e}} @td{The escape character ('\u001B')}}
@tr{@td{@code{\cx}} @td{The control character corresponding to x}}
@tr{@th[:colspan 2]{Character classes}}
@tr{@td{@code{[abc]}} @td{a, b, or c (simple class)}}
@tr{@td{@code{[^abc]}} @td{Any character except a, b, or c (negation)}}
@tr{@td{@code{[a-zA-Z]}} @td{a through z or A through Z, inclusive (range)}}
@tr{@td{@code{[a-d[m-p]]}} @td{a through d, or m through p: [a-dm-p] (union)}}
@tr{@td{@code{[a-z&&[def]]}} @td{d, e, or f (intersection)}}
@tr{@td{@code{[a-z&&[^bc]]}} @td{a through z, except for b and c: [ad-z] (subtraction)}}
@tr{@td{@code{[a-z&&[^m-p]]}} @td{a through z, and not m through p: [a-lq-z](subtraction)}}
@tr{@th[:colspan 2]{Predefined character classes}}
@tr{@td{@code{.}} @td{Any character (may or may not match line terminators)}}
@tr{@td{@code{\d}} @td{A digit: [0-9]}}
@tr{@td{@code{\D}} @td{A non-digit: [^0-9]}}
@tr{@td{@code{\s}} @td{A whitespace character: [ \t\n\x0B\f\r]}}
@tr{@td{@code{\S}} @td{A non-whitespace character: [^\s]}}
@tr{@td{@code{\w}} @td{A word character: [a-zA-Z_0-9]}}
@tr{@td{@code{\W}} @td{A non-word character: [^\w]}}
@tr{@th[:colspan 2]{Boundary matchers}}
@tr{@td{@code{^}} @td{The beginning of a line}}
@tr{@td{@code{$}} @td{The end of a line}}
@tr{@td{@code{\b}} @td{A word boundary}}
@tr{@td{@code{\B}} @td{A non-word boundary}}
@tr{@td{@code{\A}} @td{The beginning of the input}}
@tr{@td{@code{\G}} @td{The end of the previous match}}
@tr{@td{@code{\Z}} @td{The end of the input but for the final terminator, if any}}
@tr{@td{@code{\z}} @td{The end of the input}}
@tr{@th[:colspan 2]{Greedy quantifiers}}
@tr{@td{@code{X?}} @td{X, once or not at all}}
@tr{@td{@code{X*}} @td{X, zero or more times}}
@tr{@td{@code{X+}} @td{X, one or more times}}
@tr{@td{@code{X{n}}} @td{X, exactly n times}}
@tr{@td{@code{X{n,}}} @td{X, at least n times}}
@tr{@td{@code{X{n,m}}} @td{X, at least n but not more than m times}}
@tr{@th[:colspan 2]{Reluctant quantifiers}}
@tr{@td{@code{X??}} @td{X, once or not at all}}
@tr{@td{@code{X*?}} @td{X, zero or more times}}
@tr{@td{@code{X+?}} @td{X, one or more times}}
@tr{@td{@code{X{n}?}} @td{X, exactly n times}}
@tr{@td{@code{X{n,}?}} @td{X, at least n times}}
@tr{@td{@code{X{n,m}?}} @td{X, at least n but not more than m times}}
@tr{@th[:colspan 2]{Possessive quantifiers}}
@tr{@td{@code{X?+}} @td{X, once or not at all}}
@tr{@td{@code{X*+}} @td{X, zero or more times}}
@tr{@td{@code{X++}} @td{X, one or more times}}
@tr{@td{@code{X{n}+}} @td{X, exactly n times}}
@tr{@td{@code{X{n,}+}} @td{X, at least n times}}
@tr{@td{@code{X{n,m}+}} @td{X, at least n but not more than m times}}
@tr{@th[:colspan 2]{Logical operators}}
@tr{@td{@code{XY}} @td{X followed by Y}}
@tr{@td{@code{X|Y}} @td{Either X or Y}}
@tr{@td{@code{(X)}} @td{X, as a capturing group}}
@tr{@th[:colspan 2]{Back references}}
@tr{@td{@code{\n}} @td{Whatever the nth capturing group matched}}
@tr{@th[:colspan 2]{Quotation}}
@tr{@td{@code{\}} @td{Nothing, but quotes the following character}}
@tr{@td{@code{\Q}} @td{Nothing, but quotes all characters until \E}}
@tr{@td{@code{\E}} @td{Nothing, but ends quoting started by \Q}}
@tr{@th[:colspan 2]{Special constructs (non-capturing)}}
@tr{@td{@code{(?:X)}} @td{X, as a non-capturing group}}
@tr{@td{@code{(?imsux-imsux)}} @td{Nothing, but turns match flags on - off}}
@tr{@td{@code{(?imsux-imsux:X)}}
 @td{X, as a non-capturing group with the given flags on - off}}
@tr{@td{@code{(?=X)}} @td{X, via zero-width positive lookahead}}
@tr{@td{@code{(?!X)}} @td{X, via zero-width negative lookahead}}
@tr{@td{@code{(?<=X)}} @td{X, via zero-width positive lookbehind}}
@tr{@td{@code{(?<!X)}} @td{X, via zero-width negative lookbehind}}
@tr{@td{@code{(?>X)}} @td{X, as an independent, non-capturing group}}
}
Since version 0.2.3, @code{\p} and @code{\P} are supported. It is cooporated
with SRFI-14 charset. However it is kind of tricky. For example regex parser
can reads @code{\p{InAscii}} or @code{\p{IsAscii}} and search charset named
@code{char-set:ascii} from current library. It must have @code{In} or @code{Is}
as its prefix.
}

@define["Reader Macro"]{@name{#/-reader}}
@desc{This reader macro provides Perl like regular expression syntax.
It allows you to write regular expression like this @code{#/\w+?/i} instead of
like this @code{(regex "\\w+?" CASE-INSENSITIVE)}.
}

@define[Function]{@name{looking-at} @args{regex string}}
@desc{@var{Regex} must be regular expression object. Returns closure if
@var{regex} matches input @var{string}.

The @code{matches} procedure attempts to match the entire input string against
the pattern of @var{regex}.

The @code{looking-at} procedure attempts to match the input string against the
pattern of @var{regex}.
}

@define[Function]{@name{regex-replace-first} @args{regex string1 string2}}
@define[Function]{@name{regex-replace-all} @args{regex string1 string2}}
@desc{@var{Regex} must be regular expression object. Replaces part of @var{string1}
where @var{regex} matches to @var{string2}.

The @code{regex-replace-first} procedure replaces the first match.

The @code{regex-replace-all} procedure replaces all strings which matches
@var{regex}.
}

@define[Function]{@name{string-split} @args{text pattern}}
@desc{@var{text} must be a string.

@var{pattern} must be a string or regex-pattern object.

Split @var{text} accoding to @var{pattern}.
}

@subsubsection{Low level APIs for regular expression}

The above procedures are wrapped User level API. However, you might want to use
low level API directory when you re-use matcher and recursively find pattern from
input. For that purpose, you need to use low level APIs directly.

NOTE: This API might be changed in future depending on R7RS large.

@define[Function]{@name{regex-pattern?} @args{obj}}
@desc{Returns #f if @var{obj} is regular expression object, otherwise #f.}

@define[Function]{@name{regex-matcher?} @args{obj}}
@desc{Returns #f if @var{obj} is matcher object, otherwise #f.}

@define[Function]{@name{compile-regex} @args{string :optional flags}}
@desc{The same as @code{regex} procedure.}

@define[Function]{@name{regex-matcher} @args{regex string}}
@desc{@var{Regex} must be regular expression object. Returns matcher object.}

@define[Function]{@name{regex-matches} @args{matcher}}
@desc{@var{Matcher} must be matcher object. Returns #t if @var{matcher} matches
the entire input string against input pattern, otherwise #f.
}

@define[Function]{@name{regex-looking-at} @args{matcher}}
@desc{@var{Matcher} must be matcher object. Returns #t if @var{matcher} matches
the input string against input pattern, otherwise #f.
}

@define[Function]{@name{regex-find} @args{matcher :optional start}}
@desc{@var{Matcher} must be matcher object. Resets @var{matcher} and then attempts
to find the next subsequence of the input string that matches the pattern, starting
at the specified index if optional argument is given otherwise from the beginning.
}

@subsubsection{Regular expression flags}

Regular expression compiler can take following flags.

@dl-list[
@dl-item["CASE-INSENSITIVE"]{Enables case-insensitive matching.}
@dl-item["COMMENTS"]{Permits whitespace and comments in pattern.}
@dl-item["MULTILINE"]{Enables multiline mode.}
@dl-item["LITERAL"]{Enables literal parsing of the pattern.}
@dl-item["DOTAIL"]{Enables dotall mode.}
@dl-item["UNICODE-CASE"]{Enables Unicode-aware case folding.}
]