@; -*- mode:scribble; coding: utf-8 -*-

@subsubsection[:tag "rnrs.arithmetic.bitwise.6"]{Exact bitwise arithmetic}

This section describes the @code{(rnrs arithmetic bitwise (6))}library. The exact
bitwise arithmetic provides generic operations on exact integer objects. This
section uses @var{ei}, @var{ei1}, @var{ei2}, etc., as parameter names that must
be exact integer objects.

@define[Library]{@name{(rnrs arithmetic bitwise (6))}}
@desc{[R6RS] This library exports procedures for exact bitwise arithmetic
operations.}

@define[Function]{@name{bitwise-not} @args{ei}}
@desc{[R6RS] Returns the exact integer object whose two's complement representation
is the one's complement of the two's complement representation of @var{ei}.
}

@define[Function]{@name{bitwise-and} @args{ei1 @dots{}}}
@define[Function]{@name{bitwise-ior} @args{ei1 @dots{}}}
@define[Function]{@name{bitwise-xor} @args{ei1 @dots{}}}
@desc{[R6RS] These procedures return the exact integer object that is the bit-wise
"and", "inclusive or", or "exclusive or" of the two's complement representations
of their arguments. If they are passed only one argument, they return that argument.
If they are passed no arguments, they return the integer object (either - 1 or 0)
that acts as identity for the operation. 
}

@define[Function]{@name{bitwise-if} @args{ei1 ei2 ei3}}
@desc{[R6RS] Returns the exact integer object that is the bit-wise "if" of the
two's complement representations of its arguments, i.e. for each bit, if it is 1
in @var{ei1}, the corresponding bit in @var{ei2} becomes the value of the
corresponding bit in the result, and if it is 0, the corresponding bit in @var{ei3}
becomes the corresponding bit in the value of the result. This is the result of
the following computation:
@codeblock{
(bitwise-ior (bitwise-and @var{ei1} @var{ei2})
             (bitwise-and (bitwise-not @var{ei1}) @var{ei3}))
}
}

@define[Function]{@name{bitwise-bit-count} @args{ei}}
@desc{[R6RS] If @var{ei} is non-negative, this procedure returns the number of 1
bits in the two's complement representation of @var{ei}. Otherwise it returns the
result of the following computation:

@snipet{(bitwise-not (bitwise-bit-count (bitwise-not @var{ei})))}
}

@define[Function]{@name{bitwise-length} @args{ei}}
@desc{[R6RS] Returns the number of bits needed to represent @var{ei} if it is
positive, and the number of bits needed to represent @code{(bitwise-not @var{ei})}
if it is negative, which is the exact integer object that is the result of the
following computation:

@codeblock{
(do ((result 0 (+ result 1))
     (bits (if (negative? @var{ei})
               (bitwise-not @var{ei})
               @var{ei})
           (bitwise-arithmetic-shift bits -1)))
    ((zero? bits)
     result))
}
}

@define[Function]{@name{bitwise-first-bit-set} @args{ei}}
@desc{[R6RS] Returns the index of the least significant 1 bit in the two's
complement representation of @var{ei}. If ei is 0, then - 1 is returned.}

@define[Function]{@name{bitwise-bit-set?} @args{ei1 ei2}}
@desc{[R6RS] @var{Ei2} must be non-negative. The @code{bitwise-bit-set?} procedure
returns #t if the @var{ei2}th bit is 1 in the two's complement representation
of @var{ei1}, and #f otherwise. This is the result of the following computation:

@codeblock{
(not (zero?
       (bitwise-and
         (bitwise-arithmetic-shift-left 1 @var{ei2})
         @var{ei1})))
}
}

@define[Function]{@name{bitwise-copy-bit} @args{ei1 ei2 ei3}}
@desc{[R6RS] @var{Ei2} must be non-negative, and @var{ei3} must be either 0 or 1.
The @code{bitwise-copy-bit} procedure returns the result of replacing the
@var{ei2}th bit of @var{ei1} by the @var{ei2}th bit of @var{ei3}, which is the
result of the following computation:

@codeblock{
(let* ((mask (bitwise-arithmetic-shift-left 1 @var{ei2})))
  (bitwise-if mask
            (bitwise-arithmetic-shift-left @var{ei3} @var{ei2})
            @var{ei1}))
}
}

@define[Function]{@name{bitwise-bit-field} @args{ei1 ei2 ei3}}
@desc{[R6RS] @var{Ei2} and @var{ei3} must be non-negative, and @var{ei2} must be
less than or equal to @var{ei3}. The @code{bitwise-bit-field} procedure returns 
he number represented by the bits at the positions from @var{ei2} (inclusive) to
@var{ei3} (exclusive), which is the result of the following computation:

@codeblock{
(let ((mask
       (bitwise-not
        (bitwise-arithmetic-shift-left -1 @var{ei3}))))
  (bitwise-arithmetic-shift-right
    (bitwise-and @var{ei1} mask)
    @var{ei2}))
}
}

@define[Function]{@name{bitwise-copy-bit-field} @args{ei1 ei2 ei3 ei4}}
@desc{[R6RS] @var{Ei2} and @var{ei3} must be non-negative, and @var{ei2} must be
less than or equal to @var{ei3}. The @code{bitwise-copy-bit-field} procedure
returns the result of replacing in @var{ei1} the bits at positions from @var{ei2}
(inclusive) to @var{ei3} (exclusive) by the corresponding bits in @var{ei4}, which
is the fixnum result of the following computation:

@codeblock{
(let* ((to    @var{ei1})
       (start @var{ei2})
       (end   @var{ei3})
       (from  @var{ei4})
       (mask1
         (bitwise-arithmetic-shift-left -1 start))
       (mask2
         (bitwise-not
           (bitwise-arithmetic-shift-left -1 end)))
       (mask (bitwise-and mask1 mask2)))
  (bitwise-if mask
              (bitwise-arithmetic-shift-left from
                                             start)
              to))
}
}

@define[Function]{@name{bitwise-arithmetic-shift} @args{ei1 ei2}}
@desc{[R6RS] Returns the result of the following computation:

@snipet{(floor (* @var{ei1} (expt 2 @var{ei2})))}
}

@define[Function]{@name{bitwise-arithmetic-shift-left} @args{ei1 ei2}}
@define[Function]{@name{bitwise-arithmetic-shift-right} @args{ei1 ei2}}
@desc{[R6RS] @var{Ei2} must be non-negative. The @code{bitwise-arithmetic-shift-left}
procedure returns the same result as @code{bitwise-arithmetic-shift}, and

@snipet{(bitwise-arithmetic-shift-right @var{ei1} @var{ei2})}

returns the same result as

@snipet{(bitwise-arithmetic-shift @var{ei1} (- @var{ei2}))}.
}

@define[Function]{@name{bitwise-rotate-bit-field} @args{ei1 ei2 ei3 ei4}}
@desc{[R6RS] @var{Ei2}, @var{ei3}, @var{ei4} must be non-negative, @var{ei2}
must be less than or equal to @var{ei3}, and @var{ei4} must be non-negative.
The @code{bitwise-rotate-bit-field} procedure returns the result of cyclically
permuting in @var{ei1} the bits at positions from @var{ei2} (inclusive) to
@var{ei3} (exclusive) by @var{ei4} bits towards the more significant bits,
which is the result of the following computation:

@codeblock{
(let* ((n     @var{ei1})
       (start @var{ei2})
       (end   @var{ei3})
       (count @var{ei4})
       (width (- end start)))
  (if (positive? width)
      (let* ((count (mod count width))
             (field0
               (bitwise-bit-field n start end))
             (field1 (bitwise-arithmetic-shift-left
                       field0 count))
             (field2 (bitwise-arithmetic-shift-right
                       field0
                       (- width count)))
             (field (bitwise-ior field1 field2)))
        (bitwise-copy-bit-field n start end field))
      n))
}
}

@define[Function]{@name{bitwise-reverse-bit-field} @args{ei1 ei2 ei3}}
@desc{[R6RS] @var{Ei2} and @var{ei3} must be non-negative, and @var{ei2} must be
less than or equal to @var{ei3}. The @code{bitwise-reverse-bit-field} procedure
returns the result obtained from @var{ei1} by reversing the order of the bits at
positions from @var{ei2} (inclusive) to @var{ei3} (exclusive). 
}