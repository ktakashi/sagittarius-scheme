### [§3] Flonums {#rnrs.arithmetic.flonums.6}

This section describes the `(rnrs arithmetic flonums (6))`library.

This section uses _fl_, _fl1_, _fl2_, etc., as parameter names for
arguments that must be flonums, and _ifl_ as a name for arguments that must
be integer-valued flonums, i.e., flonums for which the `integer-valued?`predicate returns true.

###### [!Library] `(rnrs arithmetic flonums (6))` 

[R6RS] This library exports procedures for flonum operations.

###### [!Function] `flonum?`  _obj_

[R6RS] Returns #t if _obj_ is a flonum, #f otherwise.

###### [!Function] `fl=?`  _fl1_ _fl2_ _fl3_ _..._
###### [!Function] `fl>?`  _fl1_ _fl2_ _fl3_ _..._
###### [!Function] `fl<?`  _fl1_ _fl2_ _fl3_ _..._
###### [!Function] `fl>=?`  _fl1_ _fl2_ _fl3_ _..._
###### [!Function] `fl<=?`  _fl1_ _fl2_ _fl3_ _..._

[R6RS] These procedures return #t if their arguments are (respectively):
equal, monotonically increasing, monotonically decreasing, monotonically
nondecreasing, or monotonically nonincreasing, #f otherwise.

###### [!Function] `flinteger?`  _fl_
###### [!Function] `flzero?`  _fl_
###### [!Function] `flpositive?`  _fl_
###### [!Function] `flnegative?`  _fl_
###### [!Function] `flodd?`  _fl_
###### [!Function] `fleven?`  _fl_
###### [!Function] `flfinite?`  _fl_
###### [!Function] `flinfinite?`  _fl_
###### [!Function] `flnan?`  _fl_

[R6RS] These numerical predicates test a flonum for a particular property,
returning #t or #f. The `flinteger?` procedure tests whether the number
object is an integer, `flzero?` tests whether it is `fl=?` to zero,
`flpositive?` tests whether it is greater than zero, `flnegative?`tests whether it is less than `zero`, `flodd?` tests whether it is
odd, `fleven?` tests whether it is even, `flfinite?` tests whether
it is not an infinity and not a NaN, `flinfinite?` tests whether it is
an infinity, and `flnan?` tests whether it is a NaN.


###### [!Function] `flmax`  _fl1_ _fl2_ _..._
###### [!Function] `flmin`  _fl1_ _fl2_ _..._

[R6RS] These procedures return the maximum or minimum of their arguments.
They always return a NaN when one or more of the arguments is a NaN.

###### [!Function] `fl+`  _fl1_ _..._
###### [!Function] `fl*`  _fl1_ _..._

[R6RS] These procedures return the flonum sum or product of their flonum
arguments. In general, they should return the flonum that best approximates the
mathematical sum or product.

###### [!Function] `flabs`  _fl_

[R6RS] Returns the absolute value of _fl._

###### [!Function] `fldiv-and-mod`  _fl1_ _fl2_
###### [!Function] `fldiv`  _fl1_ _fl2_
###### [!Function] `flmod`  _fl1_ _fl2_
###### [!Function] `fldiv0-and-mod0`  _fl1_ _fl2_
###### [!Function] `fldiv0`  _fl1_ _fl2_
###### [!Function] `flmod0`  _fl1_ _fl2_

[R6RS] These procedures implement number-theoretic integer division and
return the results of the corresponding mathematical operations (see
[`(rnrs base (6))`](#rnrs.base.6). For zero divisors, these
procedures may return a NaN or some unspecified flonum.


###### [!Function] `flnumerator`  _fl_
###### [!Function] `fldenominator`  _fl_

[R6RS] These procedures return the numerator or denominator of _fl_ as
a flonum; the result is computed as if _fl_ was represented as a fraction in
lowest terms. The denominator is always positive. The denominator of 0.0 is
defined to be 1.0.

###### [!Function] `flfloor`  _fl_
###### [!Function] `flceiling`  _fl_
###### [!Function] `fltruncate`  _fl_
###### [!Function] `flround`  _fl_

[R6RS] These procedures return integral flonums for flonum arguments that
are not infinities or NaNs. For such arguments, `flfloor` returns the largest
integral flonum not larger than _fl_. The `flceiling` procedure returns
the smallest integral flonum not smaller than _fl_. The `fltruncate`procedure returns the integral flonum closest to _fl_ whose absolute value
is not larger than the absolute value of _fl_. The `flround` procedure
returns the closest integral flonum to _fl_, rounding to even when _fl_represents a number halfway between two integers.

Although infinities and NaNs are not integer objects, these procedures return an
infinity when given an infinity as an argument, and a NaN when given a NaN.


###### [!Function] `flexp`  _fl_
###### [!Function] `fllog`  _fl1_ _:optional_ _fl2_
###### [!Function] `flsin`  _fl_
###### [!Function] `flcos`  _fl_
###### [!Function] `fltan`  _fl_
###### [!Function] `flasin`  _fl_
###### [!Function] `flacos`  _fl_
###### [!Function] `flatan`  _fl1_ _:optional_ _fl2_

[R6RS] These procedures compute the usual transcendental functions. The `flexp`procedure computes the base-e exponential of _fl_. The `fllog` procedure
with a single argument computes the natural logarithm of `fl1` (not the base
ten logarithm); `(fllog _fl1_ _fl2_)` computes the base-_fl2_logarithm of _fl1_. The `flasin`, `flacos`, and `flatan`procedures compute arcsine, arccosine, and arctangent, respectively.
`(flatan _fl1_ _fl2_)` computes the arc tangent of _fl1_/_fl2_.


###### [!Function] `flsqrt`  _fl_

[R6RS] Returns the principal square root of _fl_. For - 0.0, `flsqrt`returns 0.0; for other negative arguments, the result unspecified flonum.


###### [!Function] `flexpt`  _fl1_ _fl2_

[R6RS] Either _fl1_ should be non-negative, or, if _fl1_ is negative,
_fl2_ should be an integer object. The `flexpt` procedure returns _fl1_raised to the power _fl2_. If _fl1_ is negative and _fl2_ is not an
integer object, the result is a NaN. If _fl1_ is zero, then the result is zero. 


###### [!Condition Type] `&no-infinities` 
###### [!Function] `make-no-infinities-violation`  _obj_
###### [!Function] `no-invinities-violation?`  _obj_
###### [!Condition Type] `&no-nans` 
###### [!Function] `make-no-nans-violation`  _obj_
###### [!Function] `no-nans-violation?`  _obj_

[R6RS] These types describe that a program has executed an arithmetic operations
that is specified to return an infinity or a NaN, respectively.

Here is the hierarchy of these conditions.

``````````scheme
+ &implementation-restriction (see ["Conditions"](#rnrs.conditions.6))
    + &no-infinities
    + &no-nans
``````````



###### [!Function] `fixnum->flonum`  _fx_

[R6RS] Returns a flonum that is numerically closest to _fx_.

