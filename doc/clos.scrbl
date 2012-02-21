@; -*- mode:scribble; coding:utf-8; -*-

@section[:tag "clos"]{CLOS}

Since Sagittarius version 0.3.0, we supports CLOS so that all Scheme objects
have its class. For example @code{1} is instance of @code{<integer>} class.

However CLOS has huge features and I don't have intension to implement all of
it. Currently it does not support MOP. It might come later release.

This section does not describe CLOS itself.

@subsection[:tag "lib.clos.user"]{(clos user) -CLOS user APIs}

@define[Library]{@name{(clos user)}}
@desc{User level CLOS API collection library.}

@define[Macro]{@name{define-class} @args{name supers slots}}
@desc{@var{Name} must be symbol or identifier.

@var{Supers} must be list of class.

@var{Slots} must be following structure:

@var{slots} ::= (@var{slot} ...)
@var{slot}  ::= (@var{slot-name} @var{specifiers}*)
@var{specifiers} ::= @code{:init-keyword} @var{keyword} 
		 | @code{:init-value} @var{value}

Defines a new class.

NOTE: Current implementation does not support @code{:allocation} keyword like
CL. It might be implemented in future.
}

@define[Macro]{@name{define-generic} @args{name}}
@desc{@var{Name} must be symbol.

Creates a new generic function.
}

@define[Macro]{@name{define-method} @args{name specifiers body @dots{}}}
@desc{@var{Name} must be symbol.

@var{Specifiers} must be following structure:

@var{specifiers} ::= (@var{spec} ... @var{rest})
@var{spec} ::= (@var{argument-name} @var{class}) | (@var{argument-name})
@var{rest} ::= '() | symbol

Adds defined method to @var{name} generic. If the generic does not exist, this
will create a new generic function implicitly.
}

@define[Function]{@name{slot-ref} @args{obj slot-name}}
@desc{Returns the slot value specified @var{slot-name}.}

@define[Function]{@name{slot-set!} @args{obj slot-name value}}
@desc{Sets the slot value @var{value} with specified @var{slot-name}.}

@define[Generic]{@name{make} @args{class args @dots{}}}
@desc{Creates a new instance of @var{class}}

@subsection[:tag "lib.clos.core"]{(clos core) - CLOS core library}

@define[Library]{@name{(clos core)}}
@desc{Low level CLOS API collection library.}


@define[Generic]{@name{add-method} @args{generic method}}
@desc{@var{Generic} must be generic function. @var{method} must be method
object.

Adds @var{method} to @var{generic}.
}

