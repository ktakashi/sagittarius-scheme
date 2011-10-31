@; -*- mode:scribble; coding: utf-8 -*-

@subsubsection[:tag "rnrs.arithmetic.fixnums.6"]{Fixnums}

@define[Library]{@name{(rnrs arithmetic fixnums (6))}}
@desc{On Sagittarius Scheme, fixnum is 30 bits or 62 bits depending on platform.
On 32 bits platform it fixnum is 30 bits, and 64 bits platform it is 62 bits.
However, non 32 bits platform is not well tested so if you find a bug please send
a report.

This section uses @var{fx}, @var{fx1} @var{fx2}, etc., as parameter names for
arguments that must be fixnums.
}

@define[Function]{@name{fixnum?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is an exact integer object within the fixnum
range, #f otherwise.}

@define[Function]{@name{fixnum-width}}
@define[Function]{@name{least-fixnum}}
@define[Function]{@name{greatest-fixnum}}
@desc{[R6RS] These procedures returns bit size of fixnum, minimum and maximum
value of the fixnum range, respectively.
}

@define[Function]{@name{fx=?} @args{fx1 fx2 fx3 @dots{}}}
@define[Function]{@name{fx>?} @args{fx1 fx2 fx3 @dots{}}}
@define[Function]{@name{fx<?} @args{fx1 fx2 fx3 @dots{}}}
@define[Function]{@name{fx>=?} @args{fx1 fx2 fx3 @dots{}}}
@define[Function]{@name{fx<=?} @args{fx1 fx2 fx3 @dots{}}}
@desc{[R6RS] These procedures return #t if their arguments are: equal,
monotonically increasing, monotonically decreasing, monotonically nondecreasing,
or monotonically nonincreasing, #f otherwise.}

@define[Function]{@name{fxzero?} @args{fx}}
@define[Function]{@name{fxpositive?} @args{fx}}
@define[Function]{@name{fxnegative?} @args{fx}}
@define[Function]{@name{fxodd?} @args{fx}}
@define[Function]{@name{fxeven?} @args{fx}}
@desc{[R6RS] These numerical predicates test a fixnum for a particular property,
returning #t or #f. The five properties tested by these procedures are: whether
the number object is zero, greater than zero, less than zero, odd, or even.}

@define[Function]{@name{fxmax} @args{fx1 fx2 @dots{}}}
@define[Function]{@name{fxmin} @args{fx1 fx2 @dots{}}}
@desc{[R6RS] These procedures return the maximum or minimum of their arguments.}

@define[Function]{@name{fx+} @args{fx1 fx2}}
@define[Function]{@name{fx*} @args{fx1 fx2}}
@desc{[R6RS] These procedures return the sum or product of their arguments,
provided that sum or product is a fixnum. An exception with condition type 
@code{&implementation-restriction} is raised if that sum or product is not a
fixnum.
}

@define[Function]{@name{fx-} @args{fx1 :optional fx2}}
@desc{[R6RS] With two arguments, this procedure returns the difference @var{fx1}
- @var{fx2}, provided that difference is a fixnum.

With one argument, this procedure returns the additive inverse of its argument,
provided that integer object is a fixnum.

An exception with condition type @code{&implementation-restriction} is raised if
the mathematically correct result of this procedure is not a fixnum.

NOTE: R6RS says it raises @code{&assertion} if the result is not fixnum, however
Sagittarius raises @code{&implementation-restriction} for consistency with
@code{fx+} and @code{fx*}.
}

@define[Function]{@name{fxdiv-and-mod} @args{fx1 fx2}}
@define[Function]{@name{fxdiv} @args{fx1 fx2}}
@define[Function]{@name{fxmod} @args{fx1 fx2}}
@define[Function]{@name{fxdiv0-and-mod0} @args{fx1 fx2}}
@define[Function]{@name{fxdiv0} @args{fx1 fx2}}
@define[Function]{@name{fxmod0} @args{fx1 fx2}}
@desc{[R6RS] @var{Fx2} must be nonzero. These procedures implement number-theoretic
integer division and return the results of the corresponding mathematical operations
specified in @secref["rnrs.base.6"]{@code{(rnrs base (6))}} section.
}

@define[Function]{@name{fx+/carry} @args{fx1 fx2 fx3}}
@desc{[R6RS] Returns the two fixnum results of the following computation:
@codeblock{
(let* ((s (+ @var{fx1} @var{fx2} @var{fx3}))
       (s0 (mod0 s (expt 2 (fixnum-width))))
       (s1 (div0 s (expt 2 (fixnum-width)))))
  (values s0 s1))
}
}

@define[Function]{@name{fx-/carry} @args{fx1 fx2 fx3}}
@desc{[R6RS] Returns the two fixnum results of the following computation:
@codeblock{
(let* ((d (- @var{fx1} @var{fx2} @var{fx3}))
       (d0 (mod0 d (expt 2 (fixnum-width))))
       (d1 (div0 d (expt 2 (fixnum-width)))))
  (values d0 d1))
}
}

@define[Function]{@name{fx*/carry} @args{fx1 fx2 fx3}}
@desc{[R6RS] Returns the two fixnum results of the following computation:
@codeblock{
(let* ((s (+ (* @var{fx1} @var{fx2}) @var{fx3}))
       (s0 (mod0 s (expt 2 (fixnum-width))))
       (s1 (div0 s (expt 2 (fixnum-width)))))
  (values s0 s1))
}
}

@define[Function]{@name{fxnot} @args{fx}}
@desc{[R6RS] Returns bitwise not of fixnum @var{fx}.}

@define[Function]{@name{fxand} @args{fx1 @dots{}}}
@define[Function]{@name{fxior} @args{fx1 @dots{}}}
@define[Function]{@name{fxxor} @args{fx1 @dots{}}}
@desc{[R6RS] These procedures return the fixnum that is the bit-wise "and",
"inclusive or", or "exclusive or" of the two's complement representations of
their arguments. If they are passed only one argument, they return that
argument. If they are passed no arguments, they return the fixnum 
(either - 1 or 0) that acts as identity for the operation.
}

@define[Function]{@name{fxif} @args{fx1 fx2 fx3}}
@desc{[R6RS] Returns the fixnum that is the bit-wise "if" of the two's
complement representations of its arguments, i.e. for each bit, if it is 1 in
@var{fx1}, the corresponding bit in @var{fx2} becomes the value of the
corresponding bit in the result, and if it is 0, the corresponding bit in
@var{fx3} becomes the corresponding bit in the value of the result. This is the
fixnum result of the following computation:

@codeblock{
(fxior (fxand @var{fx1} @var{fx2})
       (fxand (fxnot @var{fx1}) @var{fx3}))
}
}

@define[Function]{@name{fxbit-count} @args{fx}}
@desc{[R6RS] If @var{fx} is non-negative, this procedure returns the number of 1
bits in the two's complement representation of @var{fx}. Otherwise it returns the
result of the following computation:

@snipet{(fxnot (fxbit-count (fxnot @var{ei})))}
}

@define[Function]{@name{fxlength} @args{fx}}
@desc{[R6RS] Returns the number of bits needed to represent @var{fx} if it is
positive, and the number of bits needed to represent @code{(fxnot @var{fx})} if
it is negative, which is the fixnum result of the following computation:

@codeblock{
(do ((result 0 (+ result 1))
     (bits (if (fxnegative? @var{fx})
               (fxnot @var{fx})
               @var{fx})
           (fxarithmetic-shift-right bits 1)))
    ((fxzero? bits)
     result))
}
}

@define[Function]{@name{fxfirst-bit-set} @args{fx}}
@desc{[R6RS] Returns the index of the least significant 1 bit in the two's
complement representation of @var{fx}. If fx is 0, then - 1 is returned.}

@define[Function]{@name{fxbit-set?} @args{fx1 fx2}}
@desc{[R6RS] @var{Fx2} must be non-negative and less than @code{(fixnum-width)}.
The @code{fxbit-set?} procedure returns #t if the @var{fx2}th bit is 1 in the
two's complement representation of @var{fx1}, and #f otherwise. This is the
fixnum result of the following computation:
@codeblock{
(not
  (fxzero?
    (fxand @var{fx1}
           (fxarithmetic-shift-left 1 @var{fx2}))))
}
}

@define[Function]{@name{fxcopy-bit} @args{fx1 fx2 fx3}}
@desc{[R6RS] @var{Fx2} must be non-negative and less than @code{(fixnum-width)}.
@var{Fx3} must be 0 or 1. The @code{fxcopy-bit} procedure returns the result of
replacing the @var{fx2}th bit of @var{fx1} by @var{fx3}, which is the result of
the following computation:
@codeblock{
(let* ((mask (fxarithmetic-shift-left 1 @var{fx2})))
  (fxif mask
        (fxarithmetic-shift-left @var{fx3} @var{fx2})
        @var{fx1}))
}
}

@define[Function]{@name{fxbit-field} @args{fx1 fx2 fx3}}
@desc{[R6RS] @var{Fx2} and @var{fx3} must be non-negative and less than
@code{(fixnum-width)}. Moreover, @var{fx2} must be less than or equal to
@var{fx3}. The @code{fxbit-field} procedure returns the number represented by
the bits at the positions from @var{fx2} (inclusive) to @var{fx3} (exclusive),
which is the fixnum result of the following computation:
@codeblock{
(let* ((mask (fxnot
              (fxarithmetic-shift-left -1 @var{fx3}))))
  (fxarithmetic-shift-right (fxand @var{fx1} mask)
                            @var{fx2}))
}
}

@define[Function]{@name{fxcopy-bit-field} @args{fx1 fx2 fx3 fx4}}
@desc{[R6RS] @var{Fx2} and @var{fx3} must be non-negative and less than
@code{(fixnum-width)}. Moreover, @var{fx2} must be less than or equal to
@var{fx3}. The @code{fxcopy-bit-field} procedure returns the result of replacing
in @var{fx1} the bits at positions from @var{fx2} (inclusive) to @var{fx3}
(exclusive) by the corresponding bits in @var{fx4}, which is the fixnum result
of the following computation:
@codeblock{
(let* ((to    @var{fx1})
       (start @var{fx2})
       (end   @var{fx3})
       (from  @var{fx4})
       (mask1 (fxarithmetic-shift-left -1 start))
       (mask2 (fxnot
               (fxarithmetic-shift-left -1 end)))
       (mask (fxand mask1 mask2)))
  (fxif mask
        (fxarithmetic-shift-left from start)
        to))
}
}

@define[Function]{@name{fxarithmetic-shift} @args{fx1 fx2}}
@desc{[R6RS] The absolute value of @var{fx2} must be less than
@code{(fixnum-width)}. If 

@snipet{(floor (* @var{fx1} (expt 2 @var{fx2})))}

is a fixnum, then that fixnum is returned. Otherwise an exception with condition
type @code{&implementation-restriction} is raised. 
}

@define[Function]{@name{fxarithmetic-shift-left} @args{fx1 fx2}}
@define[Function]{@name{fxarithmetic-shift-right} @args{fx1 fx2}}
@desc{[R6RS] @var{Fx2} must be non-negative, and less than @code{(fixnum-width)}.
The @code{fxarithmetic-shift-left} procedure behaves the same as 
@code{fxarithmetic-shift}, and @code{(fxarithmetic-shift-right @var{fx1} @var{fx2})}
behaves the same as @code{(fxarithmetic-shift @var{fx1} (fx- @var{fx2}))}.
}

@define[Function]{@name{fxrotate-bit-field} @args{fx1 fx2 fx3 fx4}}
@desc{[R6RS] @var{Fx2}, @var{fx3}, and @var{fx4} must be non-negative and less
than @code{(fixnum-width)}. @var{Fx2} must be less than or equal to @var{fx3}.
@var{Fx4} must be less than the difference between @var{fx3} and @var{fx2}. The
@code{fxrotate-bit-field} procedure returns the result of cyclically permuting
in @var{fx1} the bits at positions from @var{fx2} (inclusive) to @var{fx3}
(exclusive) by @var{fx4} bits towards the more significant bits, which is the
result of the following computation:
@codeblock{
(let* ((n     @var{fx1})
       (start @var{fx2})
       (end   @var{fx3})
       (count @var{fx4})
       (width (fx- end start)))
  (if (fxpositive? width)
      (let* ((count (fxmod count width))
             (field0
               (fxbit-field n start end))
             (field1
               (fxarithmetic-shift-left
                 field0 count))
             (field2
               (fxarithmetic-shift-right
                 field0 (fx- width count)))
             (field (fxior field1 field2)))
        (fxcopy-bit-field n start end field))
      n))
}
}

@define[Function]{@name{fxreverse-bit-field} @args{fx1 fx2 fx3}}
@desc{[R6RS] @var{Fx2} and @var{fx3} must be non-negative and less than
@code{(fixnum-width)}. Moreover, @var{fx2} must be less than or equal to @var{fx3}.
The @code{fxreverse-bit-field} procedure returns the fixnum obtained from
@var{fx1} by reversing the order of the bits at positions from @var{fx2}
(inclusive) to @var{fx3} (exclusive). 
}
