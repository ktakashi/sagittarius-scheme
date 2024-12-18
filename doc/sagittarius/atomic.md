[§2] (sagittarius atomic) - Atomic operation {#lib.sagittarius.atomic}
-------------

###### [!Library] `(sagittarius atomic)` **[@since] `0.9.13`**

This library provides atomic operation. The underlying implementation
of this library can be either `stdatomic` from C11 or `libatomic_ops`.
If the platform supports the first one, then that'd be chosen, otherwise
the latter one.

###### [!Function] `atomic?` _obj_

Returns `#t` if the given _obj_ is an atomic object, otherwise `#f`.

###### [!Function] `make-atomic` _obj_

Creates a fresh atomic object whoes initial value is _obj_.

###### [!Function] `atomic-fixnum?` _obj_

Returns `#t` if the given _obj_ is an atomic fixnum object, otherwise `#f`.

###### [!Function] `make-atomic-fixnum` (_n_ `fixnum?`)

Creates a fresh atomic fixnum object whoes initial value is _n_.

###### [!Function] `memory-order?` _obj_

Returns `#t` if the given _obj_ is a fixnum and value of memory order.

###### [!Constant] `*memory-order:relaxed*`
###### [!Constant] `*memory-order:consume*`
###### [!Constant] `*memory-order:acquire*`
###### [!Constant] `*memory-order:release*`
###### [!Constant] `*memory-order:acq-rel*`
###### [!Constant] `*memory-order:seq-cst*`

The memory order values. The meanings are the same as C11.

If the `libatomic_ops` is used as underlying implementation, then it
translate them with the best effort.

###### [!Function] `atomic-load` (_a_ `atomic?`) :optional (_order_ `memory-order?`)

Loads the current value of the given atomic of _a_.

###### [!Function] `atomic-store!` (_a_ `atomic?`) _v_ :optional (_order_ `memory-order?`)

Stores the given value _v_ into the atomic _a_.

###### [!Function] `atomic-fixnum-load` (_a_ `atomic-fixnum?`) :optional (_order_ `memory-order?`)

Loads the current value of the given atomic of _a_.

###### [!Function] `atomic-fixnum-store!` (_a_ `atomic-fixnum?`) (_n_ `fixnum?`) :optional (_order_ `memory-order?`)

Stores the given value _n_ into the atomic _a_.
The return value is unspecified.

###### [!Function] `atomic-fixnum-add!` (_a_ `atomic-fixnum?`) (_n_ `fixnum?`) :optional (_order_ `memory-order?`)
###### [!Function] `atomic-fixnum-sub!` (_a_ `atomic-fixnum?`) (_n_ `fixnum?`) :optional (_order_ `memory-order?`)
###### [!Function] `atomic-fixnum-or!` (_a_ `atomic-fixnum?`) (_n_ `fixnum?`) :optional (_order_ `memory-order?`)
###### [!Function] `atomic-fixnum-xor!` (_a_ `atomic-fixnum?`) (_n_ `fixnum?`) :optional (_order_ `memory-order?`)
###### [!Function] `atomic-fixnum-and!` (_a_ `atomic-fixnum?`) (_n_ `fixnum?`) :optional (_order_ `memory-order?`)

Operates the addition, subtraction, bitwise or, bitwise xor and bitwise and
against the value of atomic _a_ with given _n_ and updates the value of _a_.

The return value is unspecified.

###### [!Function] `atomic-exchange!` (_a_ `atomic?`) _v_ :optional (_order_ `memory-order?`)
###### [!Function] `atomic-fixnum-exchange!` (_a_ `atomic-fixnum?`) (_n_ `fixnum?`) :optional (_order_ `memory-order?`)

Exchange the value of _a_ with given _v_ or _n_ and returns the old value
of _a_.

###### [!Function] `atomic-compare-and-swap!` (_a_ `atomic?`) _v_ :optional (_order_ `memory-order?`)

CAS operation.

###### [!Function] `atomic-fixnum-inc!` (_a_ `atomic-fixnum?`) :optional (_order_ `memory-order?`)
###### [!Function] `atomic-fixnum-dec!` (_a_ `atomic-fixnum?`) :optional (_order_ `memory-order?`)

Increment or decrement the value of given _a_ and updates the value.
