@; -*- mode:scribble; coding:utf-8; -*-
@subsection[:tag "sagittarius.mop"]{Sagittarius MOP}

MOP is "meta object protocol". As far as I know, there is no standard
specification even the name is really famous and most of CLOS is implemented
on MOP.

Then we decided to take the APIs and its behaviour from Tiny CLOS. The following
libraries are implemented with the APIs and can be examples for Sagittarius'
MOP.

@subsubsection{(sagittarius mop allocation)}

@define[Library]{@name{(sagittarius mop allocation)}}
@desc{Supporting @code{:allocation} option for @code{define-class}.}

@define[Class]{@name{<allocation-meta>}}
@define[Class]{@name{<allocation-mixin>}}
@desc{Meta class and mixin class to support @code{:allocation} option for
class slot definition, respectively.

The meta class must be used with @code{:metaclass} option of
@code{define-class}.

The mixin class must be a parent class.

Currently, we only support @code{:instance} and @code{:class} keywords.

The following code is the whole definition of this classes.

@codeblock{
(define-class <allocation-meta> (<class>) ())
(define-method compute-getters-and-setters ((class <allocation-meta>) slots)
  (let ((r (call-next-method)))
    (for-each
     (lambda (acc slot)
	(cond ((slot-definition-option slot :allocation :instance)
		=> (lambda (type)
		     (case type
		       ((:instance))
		       ((:class)
			(let* ((init-value (slot-definition-option
					    slot :init-value #f))
			       (init-thunk (slot-definition-option 
					    slot :init-thunk #f))
			       (def (if init-thunk (init-thunk) init-value)))
			  (slot-set! acc 'setter (lambda (o v) (set! def v)))
			  (slot-set! acc 'getter (lambda (o) def))))
		       (else
			(assertion-violation '<allocation-meta>
					     "unknown :allocation type"
					     type)))))))
		r slots)
    r))

(define-class <allocation-mixin> () () :metaclass <allocation-meta>)
}
}

@subsubsection{(sagittarius mop validator)}

@define[Library]{@name{(sagittarius mop validator)}}
@desc{Supporting @code{:validator} and @code{observer} options for
@code{define-class}.}

@define[Class]{@name{<validator-meta>}}
@define[Class]{@name{<validator-mixin>}}
@desc{Make user be able to add own validation mechanism to slots.

@code{:validator} is for before set the value to the slot so that user can check
the value if it's correct or not.

@code{:observer} is for after set the value to the slot so that user can check
which value is set to the slot.
}