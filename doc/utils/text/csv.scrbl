@; -*- coding: utf-8 -*-
@subsection[:tag "text.csv"]{(text csv) - Comma separated values parser library}

@define[Library]{@name{(text csv)}}
@desc{This library provides comma separated values parser and write procedures.
The implementation conforms RFC 4180.
}

@subsubsection{High level APIs}

The high level APIs are implemented as object oriented style. So we have CSV
object and its accessors.

@define[Function]{@name{csv?} @args{object}}
@desc{Returns #t if @var{object} is csv object, otherwise #f.}

@define[Generic]{@name{csv-header} @args{(csv <csv>)}}
@desc{Retrieves CSV header from given CSV object @var{csv} if it has, otherwise
'().
}

@define[Generic]{@name{csv-records} @args{(csv <csv>)}}
@desc{Retrieves CSV records from given CSV object @var{csv} if it has, otherwise
'().
}

@define[Generic]{@name{csv-read} @args{(port <port>) . option}
@define[Generic]{@name{csv-read} @args{(string <string>) . option}
@desc{Reads CSV data from given @var{port} and returns csv object.

If the second form is called, the procedure opens string input port from given
@var{string} and passes it to the first form.

The @var{option} will be passed to @code{csv->list} described below.
}

@define[Generic]{@name{csv-write}
 @args{(csv <csv>) :optional (out (current-output-port))}}
@desc{Writes given CSV object @var{csv} to the output port @var{port}.}

@subsubsection{Middle level APIs}

@define[Function]{@name{csv->list}
 @args{port :optional (first-line-is-header? #f)}}
@desc{Reads CSV data from given input port @var{port} and returns alist
representing CSV data.

If optional argument @var{first-line-is-header?} is #t, then the procedure reads
the first line as header line.

The returning alist is following format;

@codeblock{
alist  := (header{0,1} record*)
header := (:header value*)
record := (:record value*)
value  := string
}

Note: the value returning from @code{csv-records} or @code{csv-header} do not
have meta values @code{:record} and @code{:header}.
}

@subsubsection{Low level APIs}

@define[Class]{@name{<csv>}}
@desc{The class representing CSV. If you need to create empty CSV object, you
can do like following code;

@snipet{(make <csv>)}.

Make sure, you import @code{(clos user)} library.
}

@define[Generic]{@name{add-record!} @args{(csv <csv>) (list <list>)}}
@desc{Adds a CSV representing record @var{list} to given CSV object @var{csv}.

The record must have @code{:record} keyword in the first element of the list.}

@define[Generic]{@name{set-header!} @args{(csv <csv>) (list <list>)}}
@define[Generic]{@name{set-header!} @args{(csv <csv>) (string <string>)}}
@desc{Sets a CSV header @var{list} to given CSV object @var{csv}.

If the second form is called, then first parse the string to CSV header and
calls the first form with parsed CSV header.

The @var{list} must have @code{:header} keyword in the first element of the
list.}

@define[Generic]{@name{add-records!} @args{(csv <csv>) (list <list>)}}
@define[Generic]{@name{add-records!} @args{(csv <csv>) (string <string>)}}
@desc{Convenient procedures.

Adds given CSV records @var{list} to given CSV object @var{csv}.

If the second form is called, then first parse the given string to CSV
representing list and call the first form.
}

