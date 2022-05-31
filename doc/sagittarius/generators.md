[§2] (sagittarius generators) - Generators {#lib.sagittarius.generators}
-------------

###### [!Library] `(sagittarius generators)` 

This library provides procedures for generator.

A generator is simply a procedure with no arguments that works as a source
of a series of values. Every time it is called, it yields a value. Generators
may be finite or infinite; a finite generator returns an EOF object to 
indicate that it is exhausted. For example, read-char is a generator that
generates characters from the current input port. Generators provide 
lightweight laziness. 


### [§3] Generator constructors

The following procedures creates a generator. Except `null-generator`,
all procedures have prefix `'g'`. Arguments named _generator_indicates a generator.

###### [!Function] `null-generator` 

Returns a generator which always returns EOF object.

###### [!Function] `circular-generator`  _obj_ _obj*_ _..._

Returns a generator which repeats the given _obj_s.

###### [!Function] `giota`  _:optional_ _(count_ _+inf.0)_ _(start_ _0)_ _(step_ _1)_

Returns a generator which returns _count_ number of numbers.
The returning numbers start with _start_ and increased by _step_.

``(generator->list (giota 5))`` => ``(0 1 2 3 4)``

``(generator->list (giota 5 10))`` => ``(10 11 12 13 14)``

``(generator->list (giota 5 10 2))`` => ``(10 12 14 16 18)``

If _count_ is not given, then the generator is inifinte.


###### [!Function] `grange`  _:optional_ _(start_ _0)_ _(end_ _+inf.0)_ _(step_ _1)_

Returns a generator which returns numbers in range of _start_ and
_end_. The returning numbers are increased by _step_.


###### [!Function] `gunfold`  _stop?_ _mapper_ _successor_ _seed_ _:optional_ _(tail-gen_ _#f)_

A generator constructor similar to `unfold`.


``````````scheme
(generator->list (gunfold
                      (lambda (s) (> s 5))
                      (lambda (s) (* s 2))
                      (lambda (s) (+ s 1))
                      0))
``````````
=> ``(0 2 4 6 8 10)``



###### [!Function] `list->generator`  _list_
###### [!Function] `vector->generator`  _vector_
###### [!Function] `reverse-vector->generator`  _vector_
###### [!Function] `string->generator`  _string_
###### [!Function] `bytevector->generator`  _bytevector_

Generator constructors. The returning generator returns the items
taken from given argument from the beginning of the given sequence to
the end. Except `reverse-vector->generator` which return end to beginning.


###### [!Function] `port->char-generator`  _port_
###### [!Function] `port->byte-generator`  _port_

Generator constructors. The returning generator returns the items read
from the given _port_. The `port->char-generator` uses `get-char`to read the port. The `port->byte-generator` uses `get-u8`.


###### [!Generic] `->generator` 

Generic constructor of generators. By default, the following methods are
defined and dispatched above generator constrocturs.

`<list>`, `<vector>`, `<string>`, `<bytevector>` and
`<port>`.

If the given argument is type of  `<vector>`, then `vector->generator`is used. If the given argument is type of `<port>`, then it checks if
it's binary or textual and dispatches apropriate procedure.


### [§3] Generator operations

###### [!Function] `gcons*`  _object_ _..._ _generator_

Returns a generator which adds _object_s in front of _generator_.


###### [!Function] `gappend`  _generator_ _..._

Returns a generator which yields values from the first _generator_and when it's exhausted continues to next.


###### [!Function] `gcombine`  _proc_ _seed_ _generator_ _generators_ _..._

Returns a generator for mapping with state. It yields a sequence of 
sub-folds over _proc_. 

The _proc_ argument is a procedure which takes as many arguments as 
the input generators plus one. It is called as 
`(_proc_ _v1_ _v2_ ... _seed_)`, where 
_v1_, _v2_,`...` are the values yielded from the input 
generators, and _seed_ is the current seed value. It must return two
values, the yielding value and the next seed. 


###### [!Function] `gfilter`  _pred_ _generator_
###### [!Function] `gremove`  _pred_ _generator_

Return generators which yield the items from the source generator, 
except those on which _pred_ returns false or true respectively. 


###### [!Function] `gtake`  _generator_ _k_ _:optional_ _padding_
###### [!Function] `gdrop`  _generator_ _k_

Return generators which take or drop _k_ items from _generator_,
respectively. Returning generators won't raise errors when it's exhausted
before reaching _k_.

Optional argument _padding_ for `gtake` is passed, then the value
is filled until the procedure reaches _k_.

These procedures are analogues of SRFI-1 `take` and `drop`.


###### [!Function] `gtake-while`  _generator_ _pred_
###### [!Function] `gdrop-while`  _generator_ _pred_

Return generators which take or drop until procedure _pred_ returns
false value respectively.

These procedures are analogues of SRFI-1 `take-while` 
and `drop-while`.


###### [!Function] `gdelete`  _item_ _generator_ _:optional_ _(=_ _equal?)_

Returns a generator which returns items _generator_ returns, except
items which are the same as _item_ in sense of _=_.


###### [!Function] `gdelete-neighbor-dups`  _generator_ _:optional_ _(=_ _equal?)_

Returns a generator which returns items _generator_ returns, except
items which are the same as the proceeding item in sense of _=_.


###### [!Function] `gselect`  _value-generator_ _index-generator_

Returns a generator which returns the items generated by
_value-generator_ of specified by the indice generated by 
_index-generator_. The indice must be non negative integer and
increased strictly. Otherwise an error is raised.

``````````scheme
(generator->list (gindex (list->generator '(a b c d e f))
                 (list->generator '(0 2 4))))
``````````
=> ``(a c e)``



###### [!Function] `gselect`  _value-generator_ _truth-generator_

Returns a generator which returns the items generated by
_value-generator_ that correspond to the items generated by 
_truth-generator_. If _truth-generator_ returns true value,
then the current value of _value-generator_ is returned. Otherwise not.

``````````scheme
(generator->list (gselect (list->generator '(a b c d e f))
                          (list->generator '(#t #f #f #t #t #f))))
``````````
=> ``(a d e)``



###### [!Function] `gconcatenate`  _generator_

_generator_ must be an generator generates generators.

Returns a generator which returns the items generated by the generators
generated by given _generator_. It is similar to the following:

``````````scheme
(apply gappend (generator->list generator))
``````````

The difference is that this procedure can handle infinite generator.


###### [!Function] `gflatten`  _generator_

_generator_ must be a generator which returns lists as its items.

Returns a generator which flatten the items generated by given _generator_.

``````````scheme
(generator->list (gflatten (list->generator (list '(1 2 3 4)
                                                  '(a b c d)
                                                  '(A B C D)))))
``````````
=> ``(1 2 3 4 a b c d A B C D)``

If the _generator_ returns non list item, then it is ignored.

``````````scheme
(generator->list (gflatten (list->generator (list 'ignored
                                                  '(a b c d)
                                                  'ignored
                                                  '(A B C D)))))
``````````
=> ``(a b c d A B C D)``

This behaviour is an error behaviour so it might be changed in future.
So users should not depend on this.


###### [!Function] `gmerge`  _compare_ _generator1_ _generator2_ _..._

_compare_ must be a procedure which accepts 2 arguments and returns
boolean value. The procedure should compare the given 2 arguments and return
true value if the first argument is smaller than the second argument.

Returns a generator which returns the ordered items determined by _compare_procedure. If the `gmerge` procedure is called only one argument, then 
it simply returns a generator (if _generator1_ isn't a generator then
it is coerced).

``````````scheme
(generator->list (gmerge < (list->generator '(1 4 5)) (list->generator '(0 2 3))))
``````````
=> ``(0 1 2 3 4 5)``



###### [!Function] `gmap`  _proc_ _generator1_ _generator2_ _..._

Returns a generator which returns the items returned by `proc`.

The `proc` is called with the items returned by _generator1_ and
_generator2_ if it's given.

The _gmap_ procedure accepts uneven length of generators however one
of the generator must be finite length, otherwise it won't be exhausted.

It is an analogy of `map`.


###### [!Function] `gfilter-map`  _proc_ _generator1_ _generator2_ _..._

Returns a generator which returns the items returnd by `proc`.

This procedure is similar with `gmap`. The difference is that the 
returning item is filtered if the returning value of _proc_ is #f.

It is an analogy of `filter-map`.


###### [!Function] `generate`  _proc_

Returns a coroutine generator which return the item returned by 
_proc_.

The given argument _proc_ must accept one argument, _yield_ which
is a procedure accepts variable arguments. The _proc_ procedure can return
values via _yield_ procedure.

``````````scheme
(define g
  (make-coroutine-generator
    (lambda (yield) 
      (let loop ((i 0))
        (when (< i 3) 
          (yield i) 
          (loop (+ i 1)))))))

(generator->list g)
``````````
=> ``(0 1 2)``



###### [!Macro] `glet*`  _(bindings_ _..._ _)_ _body1_ _body2_ _..._

`glet*` is a macro which is similar to `let*`. The difference
is that `glet*` check if the bindings are EOF object or not and if it 
detects EOF object, then it returns EOF object immediately.

_bindings_ must be one of the following forms:

- `(_var_ _gen-expr_)`
- `( _gen-expr_ )`

If the first form is used, then the _gen-expr_ is bound to _var_. 
Otherwise the `glet*` just check if the value is EOF or not.

``````````scheme
(define g (list->generator '(1 2 3)))

(list 
  (glet* ((a (g))) a)
  (glet* ((a (g))) (define b 2) (+ a b))
  (glet* ((a (g)) (b (g))) (+ a b)))
``````````
=> ``(1 2 #\<eof>)``



###### [!Macro] `glet1`  _var_ _expr_ _body1_ _body2_ _..._

Convenient macro for only one binding of `glet*`. This is defined
like the following:

``````````scheme
(define-syntax glet1
  (syntax-rules ()
    ((_ var expr body body1 ...)
     (glet* ((var expr)) body body1 ...))))
``````````



###### [!Macro] `do-generator`  _(var_ _gen-expr)_ _body_ _..._

Iterates generator of then given _gen-expr_.


