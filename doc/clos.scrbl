@; -*- mode:scribble; coding:utf-8; -*-

@section[:tag "clos"]{CLOS}

Since Sagittarius version 0.3.0, we supports CLOS so that all Scheme objects
have its class. For example @code{1} is instance of @code{<integer>} class.

However CLOS has huge features and I don't have intension to implement all of
it.

This section does not describe CLOS itself.

@subsection[:tag "lib.clos.user"]{(clos user) -CLOS user APIs}

@define[Library]{@name{(clos user)}}
@desc{User level CLOS API collection library.}

@define[Macro]{@name{define-class} @args{name supers slots . options}}
@desc{@var{Name} must be symbol or identifier.

@var{Supers} must be list of class.

@var{Slots} must be following structure:

@codeblock{
@var{slots} ::= (@var{slot} ...)
@var{slot}  ::= (@var{slot-name} @var{specifiers}*)
@var{specifiers} ::= @code{:init-keyword} @var{keyword} 
		 | @code{:init-value} @var{value}
                 | @code{:init-form} @var{form}
                 | @code{:reader} @var{reader-function}
                 | @code{:writer} @var{writer-function}
}

Defines a new class.

Slot specifiers:
@dl-list[]{
  @dl-item[@code{:init-keyword}]{
    This keyword specifies initialisation keyword argument used by the
    @code{make} procedure. Following code describes how to use:
    @snipet{(make <a-class> :slot-a 'slot-a-value)}
    @code{<a-class>} has a slot which slot definition contains the keyword
    @code{:init-keyword} with the keyword @var{:slot-a}. The code initialises
    an instance of the slot with given value @var{slot-a-value}.
  }
  @dl-item[@code{:init-value}]{
    This keyword specifies an initial value of target slot.
  }
  @dl-item[@code{:init-form}]{
    Similar with @code{:init-keyword} but this keyword takes expression which
    will be evaluated at initialiation time.
  }
  @dl-item[@code{:reader}]{
    This keyword creates a procedure takes 1 argument an instance of the class
    to access the slot, so users can read the slot without using @code{slot-ref}
    procedure.
  }
  @dl-item[@code{:writer}]{
    This keyword creates a procedure takes 2 argument an instance of the class
    and object to set the slot value with given object, so users can set the
    slot without using @code{slot-set!} procedure.
  }
}

@var{opttions} can specify the metaclass of this class with keyword 
@code{:metaclass}. 

NOTE: Current implementation does not support @code{:allocation} keyword
by default. If you need it, see 
@secref["sagittarius.mop.allocation"]{(sagittarius mop allocation)}.
}

@define[Macro]{@name{define-generic} @args{name}}
@desc{@var{Name} must be symbol.

Creates a new generic function.
}

@define[Macro]{@name{define-method} @args{name specifiers body @dots{}}}
@desc{@var{Name} must be symbol.

@var{Specifiers} must be following structure:

@codeblock{
@var{specifiers} ::= (@var{spec} ... @var{rest})
@var{spec} ::= (@var{argument-name} @var{class}) | (@var{argument-name})
@var{rest} ::= '() | symbol
}

Adds defined method to @var{name} generic. If the generic does not exist, this
will create a new generic function implicitly.
}

@define[Function]{@name{slot-ref} @args{obj slot-name}}
@desc{Returns the slot value specified @var{slot-name}.}

@define[Function]{@name{slot-set!} @args{obj slot-name value}}
@desc{Sets the slot value @var{value} with specified @var{slot-name}.}

@define[Generic]{@name{make} @args{class args @dots{}}}
@desc{Creates a new instance of @var{class}}

@define[Function]{@name{is-a?} @args{object class}}
@desc{Returns #t if @var{object} is an instance of @var{class}, otherwise #f.}

@define[Function]{@name{slot-ref-using-accessor} @args{object accessor}}
@desc{This procedure is for MOP.

Returns the slot value got by @var{accessor}.
}

@define[Function]{@name{slot-set-using-accessor!} @args{object accessor value}}
@desc{This procedure is for MOP.

Sets the slot value @var{value} to @var{object} using @var{accessor}.
}

@define[Generic]{@name{write-object} @args{object (out <port>)}}
@desc{This method will be called when writing the given @var{object}.

Defines how user defined class should be written.
}

@define[Generic]{@name{object-equal?} @args{object1 object2}}
@desc{This method will be called when @code{equal?} is called.

Defines how user defined class should be compared.
}

@subsection[:tag "lib.clos.core"]{(clos core) - CLOS core library}

@define[Library]{@name{(clos core)}}
@desc{Low level CLOS API collection library.}


@define[Generic]{@name{add-method} @args{generic method}}
@desc{@var{Generic} must be generic function. @var{method} must be method
object.

Adds @var{method} to @var{generic}.
}

@define[Generic]{@name{compute-getters-and-setters}
 @args{class slots}}
@desc{Returns all getters and setters for the given @var{class}'s slots.

Unlikely the other MOP such as Gauche, this procedure returns all of the slot
accessors. So user must modify the returned definitions and return the modified
list.

For the example code, see @secref["sagittarius.mop"]{Sagittarius MOP}.
}

@define[Function]{@name{slot-definition-name} @args{slot}}
@desc{Returns slot name of given @var{slot}.}
@define[Function]{@name{slot-definition-options} @args{slot}}
@desc{Returns slot options of given @var{slot}.}
@define[Function]{@name{slot-definition-option} @args{slot keyword . default}}
@desc{Returns slot option's value of given @var{slot} if it has the 
@var{keyword}.

If @var{default} is given, then it will be the fallback value when @var{keyword}
is not found. Otherwise this procedure raises an error.
}
