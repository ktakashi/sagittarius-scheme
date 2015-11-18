@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "lib.sagittarius.generators"]{(sagittarius generators) - Generators}

@define[Library]{@name{(sagittarius generators)}}
@desc{This library provides procedures for generator.

@; From SRFI-121 Abstract
A generator is simply a procedure with no arguments that works as a source
of a series of values. Every time it is called, it yields a value. Generators
may be finite or infinite; a finite generator returns an end-of-file object
to indicate that it is exhausted. For example, read-char is a generator that
generates characters from the current input port. Generators provide 
lightweight laziness. 
}

@; TBD
