@; -*- mode:scribble; coding: utf-8 -*-

@subsubsection[:tag "rnrs.arithmetic.flonums.6"]{Flonums}

This section describes the @code{(rnrs arithmetic flonums (6))}library.

This section uses @var{fl}, @var{fl1}, @var{fl2}, etc., as parameter names for
arguments that must be flonums, and @var{ifl} as a name for arguments that must
be integer-valued flonums, i.e., flonums for which the @code{integer-valued?}
predicate returns true.

@define[Library]{@name{(rnrs arithmetic flonums (6))}}
@desc{[R6RS] This library exports procedures for flonum operations.}

@define[Function]{@name{flonum?} @args{obj}}
@desc{[R6RS] Returns #t if @var{obj} is a flonum, #f otherwise.}

@define[Function]{@name{fl=?} @args{fl1 fl2 fl3 @dots{}}}
@define[Function]{@name{fl>?} @args{fl1 fl2 fl3 @dots{}}}
@define[Function]{@name{fl<?} @args{fl1 fl2 fl3 @dots{}}}
@define[Function]{@name{fl>=?} @args{fl1 fl2 fl3 @dots{}}}
@define[Function]{@name{fl<=?} @args{fl1 fl2 fl3 @dots{}}}
@desc{[R6RS] These procedures return #t if their arguments are (respectively):
equal, monotonically increasing, monotonically decreasing, monotonically
nondecreasing, or monotonically nonincreasing, #f otherwise.}

@define[Function]{@name{flinteger?} @args{fl}}
@define[Function]{@name{flzero?} @args{fl}}
@define[Function]{@name{flpositive?} @args{fl}}
@define[Function]{@name{flnegative?} @args{fl}}
@define[Function]{@name{flodd?} @args{fl}}
@define[Function]{@name{fleven?} @args{fl}}
@define[Function]{@name{flfinite?} @args{fl}}
@define[Function]{@name{flinfinite?} @args{fl}}
@define[Function]{@name{flnan?} @args{fl}}
@desc{[R6RS] These numerical predicates test a flonum for a particular property,
returning #t or #f. The @code{flinteger?} procedure tests whether the number
object is an integer, @code{flzero?} tests whether it is @code{fl=?} to zero,
@code{flpositive?} tests whether it is greater than zero, @code{flnegative?}
tests whether it is less than @code{zero}, @code{flodd?} tests whether it is
odd, @code{fleven?} tests whether it is even, @code{flfinite?} tests whether
it is not an infinity and not a NaN, @code{flinfinite?} tests whether it is
an infinity, and @code{flnan?} tests whether it is a NaN.
}

@define[Function]{@name{flmax} @args{fl1 fl2 @dots{}}}
@define[Function]{@name{flmin} @args{fl1 fl2 @dots{}}}
@desc{[R6RS] These procedures return the maximum or minimum of their arguments.
They always return a NaN when one or more of the arguments is a NaN.}

@define[Function]{@name{fl+} @args{fl1 @dots{}}}
@define[Function]{@name{fl*} @args{fl1 @dots{}}}
@desc{[R6RS] These procedures return the flonum sum or product of their flonum
arguments. In general, they should return the flonum that best approximates the
mathematical sum or product.}

@define[Function]{@name{flabs} @args{fl}}
@desc{[R6RS] Returns the absolute value of @var{fl.}}

@define[Function]{@name{fldiv-and-mod} @args{fl1 fl2}}
@define[Function]{@name{fldiv} @args{fl1 fl2}}
@define[Function]{@name{flmod} @args{fl1 fl2}}
@define[Function]{@name{fldiv0-and-mod0} @args{fl1 fl2}}
@define[Function]{@name{fldiv0} @args{fl1 fl2}}
@define[Function]{@name{flmod0} @args{fl1 fl2}}
@desc{[R6RS] These procedures implement number-theoretic integer division and
return the results of the corresponding mathematical operations (see
@secref["rnrs.base.6"]{@code{(rnrs base (6))}}. For zero divisors, these
procedures may return a NaN or some unspecified flonum.
}

@define[Function]{@name{flnumerator} @args{fl}}
@define[Function]{@name{fldenominator} @args{fl}}
@desc{[R6RS] These procedures return the numerator or denominator of @var{fl} as
a flonum; the result is computed as if @var{fl} was represented as a fraction in
lowest terms. The denominator is always positive. The denominator of 0.0 is
defined to be 1.0.}

@define[Function]{@name{flfloor} @args{fl}}
@define[Function]{@name{flceiling} @args{fl}}
@define[Function]{@name{fltruncate} @args{fl}}
@define[Function]{@name{flround} @args{fl}}
@desc{[R6RS] These procedures return integral flonums for flonum arguments that
are not infinities or NaNs. For such arguments, @code{flfloor} returns the largest
integral flonum not larger than @var{fl}. The @code{flceiling} procedure returns
the smallest integral flonum not smaller than @var{fl}. The @code{fltruncate}
procedure returns the integral flonum closest to @var{fl} whose absolute value
is not larger than the absolute value of @var{fl}. The @code{flround} procedure
returns the closest integral flonum to @var{fl}, rounding to even when @var{fl}
represents a number halfway between two integers.

Although infinities and NaNs are not integer objects, these procedures return an
infinity when given an infinity as an argument, and a NaN when given a NaN.
}

@define[Function]{@name{flexp} @args{fl}}
@define[Function]{@name{fllog} @args{fl1 :optional fl2}}
@define[Function]{@name{flsin} @args{fl}}
@define[Function]{@name{flcos} @args{fl}}
@define[Function]{@name{fltan} @args{fl}}
@define[Function]{@name{flasin} @args{fl}}
@define[Function]{@name{flacos} @args{fl}}
@define[Function]{@name{flatan} @args{fl1 :optional fl2}}
@desc{[R6RS] These procedures compute the usual transcendental functions. The @code{flexp}
procedure computes the base-e exponential of @var{fl}. The @code{fllog} procedure
with a single argument computes the natural logarithm of @code{fl1} (not the base
ten logarithm); @code{(fllog @var{fl1} @var{fl2})} computes the base-@var{fl2}
logarithm of @var{fl1}. The @code{flasin}, @code{flacos}, and @code{flatan}
procedures compute arcsine, arccosine, and arctangent, respectively.
@code{(flatan @var{fl1} @var{fl2})} computes the arc tangent of @var{fl1}/@var{fl2}.
}

@define[Function]{@name{flsqrt} @args{fl}}
@desc{[R6RS] Returns the principal square root of @var{fl}. For - 0.0, @code{flsqrt}
returns 0.0; for other negative arguments, the result unspecified flonum.
}

@define[Function]{@name{flexpt} @args{fl1 fl2}}
@desc{[R6RS] Either @var{fl1} should be non-negative, or, if @var{fl1} is negative,
@var{fl2} should be an integer object. The @code{flexpt} procedure returns @var{fl1}
raised to the power @var{fl2}. If @var{fl1} is negative and @var{fl2} is not an
integer object, the result is a NaN. If @var{fl1} is zero, then the result is zero. 
}

@define["Condition Type"]{@name{&no-infinities}}
@define[Function]{@name{make-no-infinities-violation} @args{obj}}
@define[Function]{@name{no-invinities-violation?} @args{obj}}
@define["Condition Type"]{@name{&no-nans}}
@define[Function]{@name{make-no-nans-violation} @args{obj}}
@define[Function]{@name{no-nans-violation?} @args{obj}}
@desc{[R6RS] These types describe that a program has executed an arithmetic operations
that is specified to return an infinity or a NaN, respectively.

Here is the hierarchy of these conditions.
@codeblock{
+ &implementation-restriction (see @secref["rnrs.conditions.6"]{"Conditions"})
    + &no-infinities
    + &no-nans
}
}

@define[Function]{@name{fixnum->flonum} @args{fx}}
@desc{[R6RS] Returns a flonum that is numerically closest to @var{fx}.}