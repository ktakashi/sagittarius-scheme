### [§3] Fixnums {#rnrs.arithmetic.fixnums.6}

###### [!Library] `(rnrs arithmetic fixnums (6))` 

On Sagittarius Scheme, fixnum is 30 bits or 62 bits depending on platform.
On 32 bits platform it fixnum is 30 bits, and 64 bits platform it is 62 bits.
However, non 32 bits platform is not well tested so if you find a bug please send
a report.

This section uses _fx_, _fx1_ _fx2_, etc., as parameter names for
arguments that must be fixnums.


###### [!Function] `fixnum?`  _obj_

[R6RS] Returns #t if _obj_ is an exact integer object within the fixnum
range, #f otherwise.

###### [!Function] `fixnum-width` 
###### [!Function] `least-fixnum` 
###### [!Function] `greatest-fixnum` 

[R6RS] These procedures returns bit size of fixnum, minimum and maximum
value of the fixnum range, respectively.


###### [!Function] `fx=?`  _fx1_ _fx2_ _fx3_ _..._
###### [!Function] `fx>?`  _fx1_ _fx2_ _fx3_ _..._
###### [!Function] `fx<?`  _fx1_ _fx2_ _fx3_ _..._
###### [!Function] `fx>=?`  _fx1_ _fx2_ _fx3_ _..._
###### [!Function] `fx<=?`  _fx1_ _fx2_ _fx3_ _..._

[R6RS] These procedures return #t if their arguments are: equal,
monotonically increasing, monotonically decreasing, monotonically nondecreasing,
or monotonically nonincreasing, #f otherwise.

###### [!Function] `fxzero?`  _fx_
###### [!Function] `fxpositive?`  _fx_
###### [!Function] `fxnegative?`  _fx_
###### [!Function] `fxodd?`  _fx_
###### [!Function] `fxeven?`  _fx_

[R6RS] These numerical predicates test a fixnum for a particular property,
returning #t or #f. The five properties tested by these procedures are: whether
the number object is zero, greater than zero, less than zero, odd, or even.

###### [!Function] `fxmax`  _fx1_ _fx2_ _..._
###### [!Function] `fxmin`  _fx1_ _fx2_ _..._

[R6RS] These procedures return the maximum or minimum of their arguments.

###### [!Function] `fx+`  _fx1_ _fx2_
###### [!Function] `fx*`  _fx1_ _fx2_

[R6RS] These procedures return the sum or product of their arguments,
provided that sum or product is a fixnum. An exception with condition type 
`&implementation-restriction` is raised if that sum or product is not a
fixnum.


###### [!Function] `fx-`  _fx1_ _:optional_ _fx2_

[R6RS] With two arguments, this procedure returns the difference _fx1_- _fx2_, provided that difference is a fixnum.

With one argument, this procedure returns the additive inverse of its argument,
provided that integer object is a fixnum.

An exception with condition type `&implementation-restriction` is raised if
the mathematically correct result of this procedure is not a fixnum.

NOTE: R6RS says it raises `&assertion` if the result is not fixnum, however
Sagittarius raises `&implementation-restriction` for consistency with
`fx+` and `fx*`.


###### [!Function] `fxdiv-and-mod`  _fx1_ _fx2_
###### [!Function] `fxdiv`  _fx1_ _fx2_
###### [!Function] `fxmod`  _fx1_ _fx2_
###### [!Function] `fxdiv0-and-mod0`  _fx1_ _fx2_
###### [!Function] `fxdiv0`  _fx1_ _fx2_
###### [!Function] `fxmod0`  _fx1_ _fx2_

[R6RS] _Fx2_ must be nonzero. These procedures implement number-theoretic
integer division and return the results of the corresponding mathematical operations
specified in [`(rnrs base (6))`](#rnrs.base.6) section.


###### [!Function] `fx+/carry`  _fx1_ _fx2_ _fx3_

[R6RS] Returns the two fixnum results of the following computation:

``````````scheme
(let* ((s (+ _fx1_ _fx2_ _fx3_))
       (s0 (mod0 s (expt 2 (fixnum-width))))
       (s1 (div0 s (expt 2 (fixnum-width)))))
  (values s0 s1))
``````````



###### [!Function] `fx-/carry`  _fx1_ _fx2_ _fx3_

[R6RS] Returns the two fixnum results of the following computation:

``````````scheme
(let* ((d (- _fx1_ _fx2_ _fx3_))
       (d0 (mod0 d (expt 2 (fixnum-width))))
       (d1 (div0 d (expt 2 (fixnum-width)))))
  (values d0 d1))
``````````



###### [!Function] `fx*/carry`  _fx1_ _fx2_ _fx3_

[R6RS] Returns the two fixnum results of the following computation:

``````````scheme
(let* ((s (+ (* _fx1_ _fx2_) _fx3_))
       (s0 (mod0 s (expt 2 (fixnum-width))))
       (s1 (div0 s (expt 2 (fixnum-width)))))
  (values s0 s1))
``````````



###### [!Function] `fxnot`  _fx_

[R6RS] Returns bitwise not of fixnum _fx_.

###### [!Function] `fxand`  _fx1_ _..._
###### [!Function] `fxior`  _fx1_ _..._
###### [!Function] `fxxor`  _fx1_ _..._

[R6RS] These procedures return the fixnum that is the bit-wise "and",
"inclusive or", or "exclusive or" of the two's complement representations of
their arguments. If they are passed only one argument, they return that
argument. If they are passed no arguments, they return the fixnum 
(either - 1 or 0) that acts as identity for the operation.


###### [!Function] `fxif`  _fx1_ _fx2_ _fx3_

[R6RS] Returns the fixnum that is the bit-wise "if" of the two's
complement representations of its arguments, i.e. for each bit, if it is 1 in
_fx1_, the corresponding bit in _fx2_ becomes the value of the
corresponding bit in the result, and if it is 0, the corresponding bit in
_fx3_ becomes the corresponding bit in the value of the result. This is the
fixnum result of the following computation:

``````````scheme
(fxior (fxand _fx1_ _fx2_)
       (fxand (fxnot _fx1_) _fx3_))
``````````



###### [!Function] `fxbit-count`  _fx_

[R6RS] If _fx_ is non-negative, this procedure returns the number of 1
bits in the two's complement representation of _fx_. Otherwise it returns the
result of the following computation:

``(fxnot (fxbit-count (fxnot _ei_)))``



###### [!Function] `fxlength`  _fx_

[R6RS] Returns the number of bits needed to represent _fx_ if it is
positive, and the number of bits needed to represent `(fxnot _fx_)` if
it is negative, which is the fixnum result of the following computation:

``````````scheme
(do ((result 0 (+ result 1))
     (bits (if (fxnegative? _fx_)
               (fxnot _fx_)
               _fx_)
           (fxarithmetic-shift-right bits 1)))
    ((fxzero? bits)
     result))
``````````



###### [!Function] `fxfirst-bit-set`  _fx_

[R6RS] Returns the index of the least significant 1 bit in the two's
complement representation of _fx_. If fx is 0, then - 1 is returned.

###### [!Function] `fxbit-set?`  _fx1_ _fx2_

[R6RS] _Fx2_ must be non-negative and less than `(fixnum-width)`.
The `fxbit-set?` procedure returns #t if the _fx2_th bit is 1 in the
two's complement representation of _fx1_, and #f otherwise. This is the
fixnum result of the following computation:

``````````scheme
(not
  (fxzero?
    (fxand _fx1_           (fxarithmetic-shift-left 1 _fx2_))))
``````````



###### [!Function] `fxcopy-bit`  _fx1_ _fx2_ _fx3_

[R6RS] _Fx2_ must be non-negative and less than `(fixnum-width)`.
_Fx3_ must be 0 or 1. The `fxcopy-bit` procedure returns the result of
replacing the _fx2_th bit of _fx1_ by _fx3_, which is the result of
the following computation:

``````````scheme
(let* ((mask (fxarithmetic-shift-left 1 _fx2_)))
  (fxif mask
        (fxarithmetic-shift-left _fx3_ _fx2_)
        _fx1_))
``````````



###### [!Function] `fxbit-field`  _fx1_ _fx2_ _fx3_

[R6RS] _Fx2_ and _fx3_ must be non-negative and less than
`(fixnum-width)`. Moreover, _fx2_ must be less than or equal to
_fx3_. The `fxbit-field` procedure returns the number represented by
the bits at the positions from _fx2_ (inclusive) to _fx3_ (exclusive),
which is the fixnum result of the following computation:

``````````scheme
(let* ((mask (fxnot
              (fxarithmetic-shift-left -1 _fx3_))))
  (fxarithmetic-shift-right (fxand _fx1_ mask)
                            _fx2_))
``````````



###### [!Function] `fxcopy-bit-field`  _fx1_ _fx2_ _fx3_ _fx4_

[R6RS] _Fx2_ and _fx3_ must be non-negative and less than
`(fixnum-width)`. Moreover, _fx2_ must be less than or equal to
_fx3_. The `fxcopy-bit-field` procedure returns the result of replacing
in _fx1_ the bits at positions from _fx2_ (inclusive) to _fx3_(exclusive) by the corresponding bits in _fx4_, which is the fixnum result
of the following computation:

``````````scheme
(let* ((to    _fx1_)
       (start _fx2_)
       (end   _fx3_)
       (from  _fx4_)
       (mask1 (fxarithmetic-shift-left -1 start))
       (mask2 (fxnot
               (fxarithmetic-shift-left -1 end)))
       (mask (fxand mask1 mask2)))
  (fxif mask
        (fxarithmetic-shift-left from start)
        to))
``````````



###### [!Function] `fxarithmetic-shift`  _fx1_ _fx2_

[R6RS] The absolute value of _fx2_ must be less than
`(fixnum-width)`. If 

``(floor (* _fx1_ (expt 2 _fx2_)))``

is a fixnum, then that fixnum is returned. Otherwise an exception with condition
type `&implementation-restriction` is raised. 


###### [!Function] `fxarithmetic-shift-left`  _fx1_ _fx2_
###### [!Function] `fxarithmetic-shift-right`  _fx1_ _fx2_

[R6RS] _Fx2_ must be non-negative, and less than `(fixnum-width)`.
The `fxarithmetic-shift-left` procedure behaves the same as 
`fxarithmetic-shift`, and `(fxarithmetic-shift-right _fx1_ _fx2_)`behaves the same as `(fxarithmetic-shift _fx1_ (fx- _fx2_))`.


###### [!Function] `fxrotate-bit-field`  _fx1_ _fx2_ _fx3_ _fx4_

[R6RS] _Fx2_, _fx3_, and _fx4_ must be non-negative and less
than `(fixnum-width)`. _Fx2_ must be less than or equal to _fx3_.
_Fx4_ must be less than the difference between _fx3_ and _fx2_. The
`fxrotate-bit-field` procedure returns the result of cyclically permuting
in _fx1_ the bits at positions from _fx2_ (inclusive) to _fx3_(exclusive) by _fx4_ bits towards the more significant bits, which is the
result of the following computation:

``````````scheme
(let* ((n     _fx1_)
       (start _fx2_)
       (end   _fx3_)
       (count _fx4_)
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
``````````



###### [!Function] `fxreverse-bit-field`  _fx1_ _fx2_ _fx3_

[R6RS] _Fx2_ and _fx3_ must be non-negative and less than
`(fixnum-width)`. Moreover, _fx2_ must be less than or equal to _fx3_.
The `fxreverse-bit-field` procedure returns the fixnum obtained from
_fx1_ by reversing the order of the bits at positions from _fx2_(inclusive) to _fx3_ (exclusive). 


