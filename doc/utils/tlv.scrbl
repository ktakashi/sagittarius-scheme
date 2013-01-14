@; -*- mode:scribble; coding: utf-8; -*-

@subsection[:tag "util.tlv"]{(tlv) - TLV library}

@define[Library]{@name{(tlv)}}
@desc{This library provides TLV (tag length value) data operation procedures.
}
@; not yet
@;@define[Function]{@name{make-tlv-parser} @args{format :rest options}}
@;@desc{Creates an TLV parser of given @args{format} type.}

@subsubsection{High level APIs}

@define[Function]{@name{make-emv-tlv-parser}
 @args{:key (object-builder tlv-builder)}}
@desc{Creates EMV type TLV parser.

This procedure returns a procedure which accepts a binary port as its argument.

The keyword argument @code{object-builder} specifies how to construct a TLV
object. The default value is @code{tlv-builder}.
}

@define[Function]{@name{tlv-object?} @args{o}}
@desc{Returns #t if the given object is TLV object otherwise #f.}

@define[Function]{@name{tlv-tag} @args{tlv}}
@desc{Returns TLV tag of given TLV object @var{tlv}.}

@define[Function]{@name{tlv-data} @args{tlv}}
@desc{Returns TLV binary data if the given TLV object @var{tlv} is not
constructed, otherwise #f.}

@define[Function]{@name{tlv-components} @args{tlv}}
@desc{Returns TLV components of given TLV object @var{tlv}. If the @var{tlv}
is not constructed, this returns @code{()}.}

@define[Function]{@name{tlv->bytevector} @agrs{tlv}}
@desc{Converts given TLV object @var{tlv} to bytevector.}

@define[Function]{@name{write-tlv}
 @agrs{tlv :optional (out (current-output-port))}}
@desc{@var{out} must be binary output port.

Writes given TLV object @var{tlv} to @var{out} as TLV data.
}

@define[Function]{@name{dump-tlv} @args{:optional (out (current-output-port))}}
@desc{Dump given TLV object @var{tlv} as human readable form.}

@subsubsection{Custom object builder}

Sometimes default TLV object is not convenient to use. Then users can create
own object from TLV data passing own object builder to the
@code{make-emv-tlv-parser} procedure.

@define[Function]{@name{tlv-builder} @args{first-byte tag data constructed?}}
@desc{Default TLV object builder. User should not use this procedure directly.

@var{first-byte} is the first byte of the leading TLV object.

@var{tag} is the read tag of the TLV object.

@var{data} is either a list of TLV object or bytevector. User can check it with
@var{constructed?} argument.

@var{constructed?} indicates if the TLV object is constructed or not. If this is
#t then @var{data} is a list of TLV objects.
}