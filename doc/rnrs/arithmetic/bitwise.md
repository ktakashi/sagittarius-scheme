### [§3] Exact bitwise arithmetic {#rnrs.arithmetic.bitwise.6}

This section describes the `(rnrs arithmetic bitwise (6))`library. 
The exact bitwise arithmetic provides generic operations on exact integer
objects. This section uses _ei_, _ei1_, _ei2_, etc., as parameter
names that must be exact integer objects.

###### [!Library] `(rnrs arithmetic bitwise (6))` 

[R6RS] This library exports procedures for exact bitwise arithmetic
operations.

###### [!Function] `bitwise-not`  _ei_

[R6RS] Returns the exact integer object whose two's complement 
representation is the one's complement of the two's complement representation
of _ei_.


###### [!Function] `bitwise-and`  _ei1_ _..._
###### [!Function] `bitwise-ior`  _ei1_ _..._
###### [!Function] `bitwise-xor`  _ei1_ _..._

[R6RS] These procedures return the exact integer object that is the
bit-wise "and", "inclusive or", or "exclusive or" of the two's complement
representations of their arguments. If they are passed only one argument,
they return that argument. If they are passed no arguments, they return the
integer object (either - 1 or 0) that acts as identity for the operation. 


###### [!Function] `bitwise-if`  _ei1_ _ei2_ _ei3_

[R6RS] Returns the exact integer object that is the bit-wise "if" of the
two's complement representations of its arguments, i.e. for each bit, if it is 1
in _ei1_, the corresponding bit in _ei2_ becomes the value of the
corresponding bit in the result, and if it is 0, the corresponding bit in
_ei3_ becomes the corresponding bit in the value of the result. This is
the result of the following computation:

``````````scheme
(bitwise-ior (bitwise-and _ei1_ _ei2_)
             (bitwise-and (bitwise-not _ei1_) _ei3_))
``````````



###### [!Function] `bitwise-bit-count`  _ei_

[R6RS] If _ei_ is non-negative, this procedure returns the number of 1
bits in the two's complement representation of _ei_. Otherwise it returns
the result of the following computation:

``(bitwise-not (bitwise-bit-count (bitwise-not _ei_)))``



###### [!Function] `bitwise-length`  _ei_

[R6RS] Returns the number of bits needed to represent _ei_ if it is
positive, and the number of bits needed to represent
`(bitwise-not _ei_)` if it is negative, which is the exact integer
object that is the result of the following computation:

``````````scheme
(do ((result 0 (+ result 1))
     (bits (if (negative? _ei_)
               (bitwise-not _ei_)
               _ei_)
           (bitwise-arithmetic-shift bits -1)))
    ((zero? bits)
     result))
``````````



###### [!Function] `bitwise-first-bit-set`  _ei_

[R6RS] Returns the index of the least significant 1 bit in the two's
complement representation of _ei_. If ei is 0, then - 1 is returned.

###### [!Function] `bitwise-bit-set?`  _ei1_ _ei2_

[R6RS] _Ei2_ must be non-negative. The `bitwise-bit-set?`procedure returns #t if the _ei2_th bit is 1 in the two's complement
representation of _ei1_, and #f otherwise. This is the result of the
following computation:

``````````scheme
(not (zero?
       (bitwise-and
         (bitwise-arithmetic-shift-left 1 _ei2_)
         _ei1_)))
``````````



###### [!Function] `bitwise-copy-bit`  _ei1_ _ei2_ _ei3_

[R6RS] _Ei2_ must be non-negative, and _ei3_ must be either 0
or 1. The `bitwise-copy-bit` procedure returns the result of replacing the
_ei2_th bit of _ei1_ by the _ei2_th bit of _ei3_, which is the
result of the following computation:

``````````scheme
(let* ((mask (bitwise-arithmetic-shift-left 1 _ei2_)))
  (bitwise-if mask
            (bitwise-arithmetic-shift-left _ei3_ _ei2_)
            _ei1_))
``````````



###### [!Function] `bitwise-bit-field`  _ei1_ _ei2_ _ei3_

[R6RS] _Ei2_ and _ei3_ must be non-negative, and _ei2_ must be
less than or equal to _ei3_. The `bitwise-bit-field` procedure returns 
he number represented by the bits at the positions from _ei2_ (inclusive) to
_ei3_ (exclusive), which is the result of the following computation:

``````````scheme
(let ((mask
       (bitwise-not
        (bitwise-arithmetic-shift-left -1 _ei3_))))
  (bitwise-arithmetic-shift-right
    (bitwise-and _ei1_ mask)
    _ei2_))
``````````



###### [!Function] `bitwise-copy-bit-field`  _ei1_ _ei2_ _ei3_ _ei4_

[R6RS] _Ei2_ and _ei3_ must be non-negative, and _ei2_ must
be less than or equal to _ei3_. The `bitwise-copy-bit-field` procedure
returns the result of replacing in _ei1_ the bits at positions from 
var{ei2} (inclusive) to _ei3_ (exclusive) by the corresponding bits in
_ei4_, which is the fixnum result of the following computation:

``````````scheme
(let* ((to    _ei1_)
       (start _ei2_)
       (end   _ei3_)
       (from  _ei4_)
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
``````````



###### [!Function] `bitwise-arithmetic-shift`  _ei1_ _ei2_

[R6RS] Returns the result of the following computation:

``(floor (* _ei1_ (expt 2 _ei2_)))``

_ei2_ must be a fixnum. This is implementation restriction.


###### [!Function] `bitwise-arithmetic-shift-left`  _ei1_ _ei2_
###### [!Function] `bitwise-arithmetic-shift-right`  _ei1_ _ei2_

[R6RS] _Ei2_ must be non-negative. The
`bitwise-arithmetic-shift-left` procedure returns the same result as
`bitwise-arithmetic-shift`, and

``(bitwise-arithmetic-shift-right _ei1_ _ei2_)``

returns the same result as

``(bitwise-arithmetic-shift _ei1_ (- _ei2_))``

.

_ei2_ must be a fixnum. This is implementation restriction.


###### [!Function] `bitwise-rotate-bit-field`  _ei1_ _ei2_ _ei3_ _ei4_

[R6RS] _Ei2_, _ei3_, _ei4_ must be non-negative, _ei2_must be less than or equal to _ei3_, and _ei4_ must be non-negative.
The `bitwise-rotate-bit-field` procedure returns the result of cyclically
permuting in _ei1_ the bits at positions from _ei2_ (inclusive) to
_ei3_ (exclusive) by _ei4_ bits towards the more significant bits,
which is the result of the following computation:

``````````scheme
(let* ((n     _ei1_)
       (start _ei2_)
       (end   _ei3_)
       (count _ei4_)
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
``````````

_ei4_ must be a fixnum. This is implementation restriction.


###### [!Function] `bitwise-reverse-bit-field`  _ei1_ _ei2_ _ei3_

[R6RS] _Ei2_ and _ei3_ must be non-negative, and _ei2_ must be
less than or equal to _ei3_. The `bitwise-reverse-bit-field` procedure
returns the result obtained from _ei1_ by reversing the order of the bits at
positions from _ei2_ (inclusive) to _ei3_ (exclusive). 


