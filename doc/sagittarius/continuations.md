[Â§2] Continuations libraries {#lib.sagittarius.continuations}
--------------------------------------------------------------

##### [!Library] (sagittarius continuations) **[@since] `0.9.15`**

This library provides continuation enhancement. The main capabilities
this library provieds below capabilities:

- Continuation prompt
- Composable continuation
- Delimited continuation


###### [!Function] `continuation?` _obj_

Returns `#t` if the given _obj_ is a continuation object, otherwise `#f`.

NOTE: a continuation object is also a procedure.
