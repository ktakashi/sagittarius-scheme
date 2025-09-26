[§2] Arithmetic libraries
-------------

This section describes Scheme's libraries for more specialized numerical
operations: fixnum and flonum arithmetic, as well as bitwise operations on exact
integer objects. 

### [§3] Bitwise operations

A number of procedures operate on the binary two's-complement representations of
exact integer objects: Bit positions within an exact integer object are counted
from the right, i.e. bit 0 is the least significant bit. Some procedures allow
extracting bit fields, i.e., number objects representing subsequences of the
binary representation of an exact integer object. Bit fields are always positive,
and always defined using a finite number of bits.

* @[[rnrs/arithmetic/fixnums.md](arithmetic/fixnums.md)]
* @[[rnrs/arithmetic/flonums.md](arithmetic/flonums.md)]
* @[[rnrs/arithmetic/bitwise.md](arithmetic/bitwise.md)]
