@; -*- coding: utf-8 -*-
@subsection[:tag "text.json.validator"]{(text json validator) - JSON validator}

@define[Library]{@name{(text json validator)}}
@desc{This library provides abstraction layer of JSON validation.}

@define["Record Type"]{@name{<json-validator>}}
@desc{Base class of JSON validators.

The record have @code{validator} field.
}

@define[Function]{@name{json-validator?} @args{obj}}
@desc{Return @code{#t} if the given @var{obj} is a JSON validator,
otherwise @code{#f}.}

@define[Function]{@name{make-json-validator} @args{validator}}
@desc{Creates a JSON validator.

The @var{validator} must be a procedure accepts one argument.
}

@define[Function]{@name{json-validator-validator} @args{json-validator}}
@desc{Returns the @code{validator} field value of the given
@var{json-validator}.
}

@define[Function]{@name{validate-json} @args{json-validator json}}
@desc{Calls value of @code{validator} field of given JSON validator with
parameter of @var{json}.
}
