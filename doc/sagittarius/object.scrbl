@; -*- coding: utf-8; -*-
@subsection[:tag "lib.sagittarius.object"]{(sagittarius object) - Convenient refs and coercion procedures}

@define[Library]{@name{(sagittarius object)}}
@desc{This library provides convenient procedures.
}

@define[Generic]{@name{ref} @args{object key args @dots{}}}
@define[Generic]{@name{(setter ref)} @args{object key value}}
@desc{Returns given @var{object}'s values associated with @var{key}. The default
implementation uses @code{slot-ref}.

Following classes are specialised by default.
@itemlist{
 @item{@code{<hashtable>} uses @code{hashtable-ref}}
 @item{@code{<list>} uses @code{list-ref}}
 @item{@code{<string>} uses @code{string-ref}}
 @item{@code{<vector>} uses @code{vector-ref}}
}

}

@define[Generic]{@name{->string} @args{object}}
@desc{Returns string represented @var{object}.}

@define[Generic]{@name{->integer} @args{object}}
@desc{Returns integer represented @var{object}.}

@define[Generic]{@name{->number} @args{object}}
@desc{Returns number represented @var{object}.

The default value is 0.

If the given @var{object} is number, it returns given object itself. 

If the given @var{object} is string, it uses @code{string->number} as a
conversion procedure.

If the given @var{object} is character, it uses @code{char->integer} as a
conversion procedure.
}
