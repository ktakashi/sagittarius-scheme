[§2] (util bytevector) - Bytevector utility library {#util.bytevector}
-------------

###### [!Library] `(util bytevector)` 

This library provides bytevector utilities which are not provided as
builtin procedures such as `bytevector->integer`.

All procedures take bytevector as its arguments.


###### [!Function] `bytevector-xor`  _bv1_ _bv2_ _..._
###### [!Function] `bytevector-xor!`  _out_ _bv1_ _bv2_ _..._
###### [!Function] `bytevector-ior`  _bv1_ _bv2_ _..._
###### [!Function] `bytevector-ior!`  _out_ _bv1_ _bv2_ _..._
###### [!Function] `bytevector-and`  _bv1_ _bv2_ _..._
###### [!Function] `bytevector-and!`  _out_ _bv1_ _bv2_ _..._

Compute exclusive or, logical or and logical and for each given
bytevectors, respectively.

The procedures without `!` freshly allocate a new bytevector as it's return
value. If the given bytevectors are not the same sized, then the smallest 
size will be allocated.

The procedures with `!` takes first argument as the storage of the result
and return it.


###### [!Function] `bytevector-slices`  _bv_ _k_ _:key_ _(padding_ _#f)_

Slices the given bytevector _bv_ into _k_ size and returns
a list of bytevectors.

The keyword argument _padding_ is given and it must be a procedure accept
one argument, then it will be called when the last chunk of bytevector is not
size of _k_. The procedure should return padded bytevector and it doesn't
check the returned value nor it's size so it is caller's responsibility to
make sure the returned value is a bytevector and the size is _k_.

``(bytevector-slices #vu8(1 2 3 4 5 6) 3)`` => ``(#vu8(1 2 3) #vu8(4 5 6))``

``(bytevector-slices #vu8(1 2 3 4) 3)`` => ``(#vu8(1 2 3) #vu8(4))``

``````````scheme
;; the given bytevector bv is #vu8(4)
(bytevector-slices #vu8(1 2 3 4) 3 :padding (lambda (bv) #vu8(4 5 6)))
``````````
=> ``(#vu8(1 2 3) #vu8(4 5 6))``

``````````scheme
;; this is valid as well so that bytevector-slices doesn't check the 
;; return value
(bytevector-slices #vu8(1 2 3 4) 3 :padding (lambda (bv) #f))
``````````
=> ``(#vu8(1 2 3) #f)``



###### [!Function] `bytevector-split-at*`  _bv_ _k_ _:key_ _(padding_ _#f)_

Splits bytevector into 2 bytevectors and returns 2 values of bytevectors.

The first returned bytevector size will be _k_ and its content is given
bytevector's value starting from 0 to _k_ - 1. The second returned value
is the rest of values of _bv_.

If size of the given bytevector _bv_ is less than _k_ then the second
value of returned bytevector will be empty bytevector.

The keyword argument _padding_ is given and it must be a procedure accept
one argument, then it will be called when given bytevector's size is less than
_k_ and first returned value will the result of _padding_.

``(bytevector-split-at* #vu8(1 2 3 4 5) 3)`` => ``#vu8(1 2 3) and #vu8(4 5)``

``````````scheme
(bytevector-split-at* #vu8(1 2) 3 :padding (lambda (bv) #vu8(1 2 3)))
``````````
=> ``#vu8(1 2 3) and #vu8()``

``````````scheme
(bytevector-split-at* #vu8(1 2) 3 :padding (lambda (bv) #f))
``````````
=> ``#f and #vu8()``



###### [!Function] `->odd-parity`  _bv_ _:optional_ _(start_ _0)_ _(end_ _(bytevector-length_ _bv))_
###### [!Function] `->odd-parity!`  _bv_ _:optional_ _(start_ _0)_ _(end_ _(bytevector-length_ _bv))_

Compute odd parity of the given bytevector _bv_ and return the
result of bytevector.

If the second procedure is used, then _bv_ will be modified.


###### [!Function] `bytevector<?`  _bv1_ _bv2_ _rest_ _..._
###### [!Function] `bytevector>?`  _bv1_ _bv2_ _rest_ _..._
###### [!Function] `bytevector<=?`  _bv1_ _bv2_ _rest_ _..._
###### [!Function] `bytevector>=?`  _bv1_ _bv2_ _rest_ _..._

Comparing given bytevectors.

The comparison is done by comparing the elements of bytevectors from
index `0`. The comparison procedures are `<`, `>`, `<=`and `>=`, respectively.


###### [!Function] `bytevector->hex-string`  _bv_ _:key_ _(upper?_ _#t)_

Converts given bytevector _bv_ to hex string.

The keyword argument _upper?_ is specified with true value, then the
procedures converts to upper case hex values, otherwise lower case.


###### [!Function] `hex-string->bytevector`  _string_

Converts given hex string _string_ to bytevector.

###### [!Function] `bytevector->escaped-string`  _bv_

Converts given bytevector to string without transcoder.

The conversion is the same as the following code:
``(list->string (map integer->char (bytevector->u8-list bv)))``

This procedure is implemented in a memory efficient way.


###### [!Function] `bytevector-reverse!`  _bv_ _:optional_ _(start_ _0)_ _(end_ _(bytevector-length_ _bv))_
###### [!Function] `bytevector-reverse`  _bv_ _:optional_ _(start_ _0)_ _(end_ _(bytevector-length_ _bv))_

Reverse the given bytevector _bv_.

Optional arguments _start_ and _end_ controls from and until where
the procedure reverses the bytevector. _end_ is exclusive.

The `bytevector-reverse!` reverses destructively.


### [§3] SRFI-13 convension APIs

#### [§4] U8 sets

U8 set is a list of integers which range is in between `0 <= n <= 255`.
This is useful to handle bytevectors as if they are ASCII strings.

###### [!Function] `u8?`  _o_

Returns #t if given _o_ is an integer in range of 
`0 <= _o_ <= 255`, otherwise #f.

###### [!Function] `u8-set?`  _o_

Returns #t if given _o_ is a list and its all elements satisfy
`u8?`. Otherwise #f.


###### [!Function] `u8-set-contains?`  _u8-set_ _u8_

_u8-set_ must satisfy `u8-set?`. _u8_ should satisfy
`u8`. The procedure doesn't check if arguments satify this.

Returns #t if given _u8-set_ contains _u8_.


###### [!Function] `string->u8-set`  _string_

Converts given _string_ to list of integers. Given _string_ should
only contains in range of ASCII characters but the procedure doesn't check. 
Thus the procedure may return a list doesn't satify `u8-set?`.
It is users' responsibility to pass ASCII string.


###### [!Function] `char-set->u8-set`  _cset_

Converts given char-set _cset_ to u8 set. This procedure returns
a list that satify `u8-set?` by dropping outside of ASCII characters.


#### [§4] Bytevectors as ASCII strings

###### [!Function] `bytevector-fold`  _kons_ _knil_ _bv_ _:optional_ _start_ _end_
###### [!Function] `bytevector-fold-right`  _kons_ _knil_ _bv_ _:optional_ _start_ _end_

Iterate given _bv_ from _start_ until _end_. _kons_ is
called by each element with result of the _kons_. The inital value is
_knil_.

This is analogy of _fold-left_ and _fold-right_.


###### [!Function] `bytevector-take`  _bv_ _n_
###### [!Function] `bytevector-take-right`  _bv_ _n_

Subtract bytevector _bv_ until index _n_ (exclusive).

The `bytevector-take` takes from left and the `bytevector-take-right`takes from right.


###### [!Function] `bytevector-drop`  _bv_ _n_
###### [!Function] `bytevector-drop-right`  _bv_ _n_

Drops given _bv_ until index _n_ (exclusive).

The `bytevector-drop` drops from left and the `bytevector-drop-right`drops from right.


###### [!Function] `bytevector-trim`  _bv_ _:optional_ _criterion_ _start_ _end_
###### [!Function] `bytevector-trim-right`  _bv_ _:optional_ _criterion_ _start_ _end_
###### [!Function] `bytevector-trim-both`  _bv_ _:optional_ _criterion_ _start_ _end_

Trims given bytevector _bv_ from left, right and both, respectively.

The optional argument _criterion_ specifies how to trim. By default, it
uses whitespaces. `" \r\f\v\n\t"`.

The optional arguments _start_ and _end_ specify from and until where
the procedure trims. The default value is 0 for _start_ and the length
of given bytevector for _end_.


###### [!Function] `bytevector-pad`  _bv_ _n_ _:optional_ _(u8_ _0)_ _start_ _end_
###### [!Function] `bytevector-pad-right`  _bv_ _n_ _:optional_ _(u8_ _0)_ _start_ _end_

Pads given bytevector _bv_ with _n_ elements of _u8_.
The `bytevector-pad` pads left side of given _bv_. The 
`bytevector-pad-right` pads right side of given _bv_.

The optional arguments _start_ and _end_ specify from and until where
the procedure pads. The default value is 0 for _start_ and the length
of given bytevector for _end_.


###### [!Function] `bytevector-prefix-length`  _bv1_ _bv2_ _:optional_ _start1_ _end1_ _start2_ _end2_
###### [!Function] `bytevector-suffix-length`  _bv1_ _bv2_ _:optional_ _start1_ _end1_ _start2_ _end2_

Return the length of the longest common prefix/suffix of the two 
bytevectors.

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of _bv1_ and _bv2_.


###### [!Function] `bytevector-prefix?`  _bv1_ _bv2_ _:optional_ _start1_ _end1_ _start2_ _end2_
###### [!Function] `bytevector-suffix?`  _bv1_ _bv2_ _:optional_ _start1_ _end1_ _start2_ _end2_

Returns #t if _bv1_ is a prefix/suffix of _bv2_. Otherwise #f. 

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of _bv1_ and _bv2_.


###### [!Function] `bytevector-index`  _bv_ _criterion_ _:optional_ _start_ _end_
###### [!Function] `bytevector-index-right`  _bv_ _criterion_ _:optional_ _start_ _end_

Searches through the given bytevector _bv_ from the left (right), 
returning the index of the first occurrence of an element which satisfies
the _criterion_.

_criterion_ can be a u8 value, a u8 set or a procedure.

If the procedure doesn't find any element satisfies _criterion_, then
returns #f.


###### [!Function] `bytevector-skip`  _bv_ _criterion_ _:optional_ _start_ _end_
###### [!Function] `bytevector-skip-right`  _bv_ _criterion_ _:optional_ _start_ _end_

Search through the given bytevector _bv_ from the left (right), 
returning the index of the first occurrence of an element which does not
satisfy the _criterion_.

_criterion_ can be a u8 value, a u8 set or a procedure.

If the procedure doesn't find any element which does not satisfy 
_criterion_, then returns #f.


###### [!Function] `bytevector-contains`  _bv1_ _bv2_ _:optional_ _start1_ _end1_ _start2_ _end2_

Returns index of _bv1_ where _bv2_ is found. If _bv1_ doesn't
contain _bv2_ then returns #f.

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of _bv1_ and _bv2_.


###### [!Function] `bytevector-replace`  _bv1_ _bv2_ _start1_ _end2_ _:optional_ _start2_ _end2_

Returns

``````````scheme
(bytevector-append (bytevector-copy s1 0 start1)
                   (bytevector-copy s2 start2 end2)
                   (bytevector-copy s1 end1 (string-length s1)))
``````````



###### [!Function] `bytevector-tokenize`  _bv_ _:optional_ _token-set_ _start_ _end_

Split the given bytevector _bv_ into a list of sub bytevectors,
where each sub bytevector is a maximal non-empty contigunous sequence of
elements from the u8 set _token-set_.

Optional argument _token-set_ must be a u8 set. By default, it's a
list of bytes of ASCII graphical characters.

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of _bv_.


###### [!Function] `bytevector-filter`  _criterion_ _bv_ _:optional_ _start_ _end_
###### [!Function] `bytevector-delete`  _criterion_ _bv_ _:optional_ _start_ _end_

Filter the bytevector _bv_, retaining only those elements that 
satisfy / do not satisfy the _criterion_ argument.

_criterion_ can be a u8 value, a u8 set or a procedure.

The optional start/end indices restrict the comparison to the indicated 
sub bytevectors of _bv_.


