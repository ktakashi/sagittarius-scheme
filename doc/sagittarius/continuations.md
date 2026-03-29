[§2] Continuations libraries {#lib.sagittarius.continuations}
--------------------------------------------------------------

##### [!Library] (sagittarius continuations) **[@since] `0.9.15`**

This library provides continuation enhancement. The main capabilities
this library provides are:

- Continuation prompt
- Composable continuation
- Delimited continuation
- Continuation marks

### [§3] Continuation Predicates

###### [!Function] `continuation?` _obj_

Returns `#t` if the given _obj_ is a continuation object, otherwise `#f`.

NOTE: a continuation object is also a procedure.

###### [!Function] `composable-continuation?` _obj_

Returns `#t` if the given _obj_ is a composable continuation object,
otherwise `#f`.

If the _obj_ is a composable continuation, then it is also a
continuation object.

### [§3] Continuation Prompts

###### [!Function] `call-with-continuation-prompt` _proc_ :optional _tag_ _abort-handler_ :rest _args_
###### [!Function] `call/prompt` _proc_ :optional _tag_ _abort-handler_ :rest _args_

Installs a new prompt into the current continuation frame with the
given _tag_ and _abort-handler_, then calls the _proc_ with the _args_.

If the _tag_ is not specified, then `(default-continuation-prompt-tag)` is used.  
If the _abort-handler_ is not specified or `#f`, then default abort handler is used.
The default abort handler accepts one argument, which must be a thunk. It's
approximately the same as like this:

```scheme
(call-with-continuation-prompt thunk tag #f)
```

###### [!Function] `continuation-prompt-available?` _tag_ :optional (_cont_ #f)

Returns if the prompt with _tag_ is available in the continuation.

If _cont_ is `#f` or omitted, then it checks the current continuation.
Otherwise, _cont_ must satisfy `continuation?`, then checks the given
continuation.

###### [!Function] `abort-current-continuation` _tag_ . _args_
###### [!Function] `abort/cc` _tag_ . _args_

Aborts the current continuation up until the specified prompt tag _tag_.
Then invokes the abort handler of the prompt.

If the prompt is not found, then it raises an error.

The rest arguments _args_ will be passed to the abort handler.

### [§3] Prompt Tags

###### [!Function] `continuation-prompt-tag?` _obj_

Returns `#t` if the given _obj_ is a continuation prompt tag.

Currently, continuation prompt tag is implemented with a list, however
this may change in the future. Users shouldn't depend on it.

###### [!Function] `make-continuation-prompt-tag` :optional (_name_ `symbol?`)

Returns a newly created continuation prompt tag.

If _name_ is specified, then it uses the _name_ as the prompt tag name.
Otherwise generates one.

###### [!Function] `default-continuation-prompt-tag`

Returns a default continuation prompt tag.

### [§3] Composable and Delimited Continuations

###### [!Function] `call-with-composable-continuation` _proc_ :optional _tag_
###### [!Function] `call/comp` _proc_ :optional _tag_

Captures the current continuation up to the prompt specified by the given _tag_.

Unlike the continuation of `call/cc`, invoking composable continuation doesn't
abort the current continuation, but it simply extends the current one,
executes the captured continuation and returns the invocation point.
For example, the script below shows `1,3,2,3` instead of `1,3`

```scheme
(call-with-continuation-prompt
 (lambda ()
   (call-with-composable-continuation
    (lambda (k)
      (display "1,")
      (k 1)
      (display "2")))
   (display "3,")))
```

If the _tag_ is not specified, then `(default-continuation-prompt-tag)` is used.

If the prompt is not found, then it raises an error.

###### [!Function] `call-with-delimited-current-continuation` _proc_ :optional _tag_
###### [!Function] `call/delim-cc` _proc_ :optional _tag_

Similar to `call-with-current-continuation`, but only captures
the continuation up until the given _tag_.

Unlike `call-with-composable-continuation`, invoking the captured
continuation aborts the current continuation up to the enclosing
prompt, like a standard `call/cc` continuation.

###### [!Function] `call-in-continuation` _k_ _proc_ . _args_

Invokes _proc_ after installing the continuation _k_. The _proc_ is
called with _args_.

Unlike directly invoking a continuation, `call-in-continuation` allows
`dynamic-wind` post thunks to be executed and continuation marks to
be accessed in the installed continuation context.

```scheme
(+ 1
   (call/cc (lambda (k)
              (let ([n 0])
                (dynamic-wind
                    values
                    (lambda ()
                      ;; n is accessed after post thunk runs
                      (call-in-continuation k (lambda () n)))
                    (lambda ()
                      (set! n 4)))))))
;; => 5
```

### [§3] Continuation Barrier

###### [!Function] `call-with-continuation-barrier` _thunk_

Installs a continuation barrier and calls _thunk_.

A continuation barrier prevents jumps into more deeply nested active
procedure calls:

```scheme
((call-with-continuation-barrier
  (lambda ()
    (call/cc values))))
```

If a continuation is captured outside of the barrier and escaping
from the _thunk_, it's okay

```scheme
(call/cc
  (lambda (k)
    (call-with-continuation-barrier
      (lambda ()
        (k 'ok)))))
```

### [§3] Continuation Conditions

###### [!Condition Type] `&continuation`
###### [!Function] `make-continuation-violation` _tag_
###### [!Function] `continuation-violation?` _obj_
###### [!Function] `continuation-violation-prompt-tag` _condition_

Continuation violation condition. This condition is raised when a
delimited continuation or composable continuation operation fails
because the specified prompt tag is not found.

`make-continuation-violation` creates a continuation violation
condition with the given _tag_.

`continuation-violation?` returns `#t` if _obj_ is a continuation
violation condition.

`continuation-violation-prompt-tag` returns the prompt tag from
the _condition_.

### [§3] Continuation Marks

Continuation marks provide a mechanism to attach key-value pairs
to the call stack. These marks survive across continuation captures
and can be queried to examine the dynamic context.

###### [!Syntax] `with-continuation-mark` _key_ _value_ _expr_ _..._

Evaluates _expr_ ... with a continuation mark associating _key_ with
_value_ attached to the current continuation frame.

If the immediate context already has a mark for _key_, the new _value_
replaces it.

###### [!Syntax] `with-continuation-marks` ((_key_ _value_) _..._) _expr_ _..._

Evaluates _expr_ ... with multiple continuation marks attached to the
current continuation frame. This is equivalent to nesting multiple
`with-continuation-mark` forms in tail position.

```scheme
(with-continuation-marks ([key1 'val1]
                          [key2 'val2])
  (list
    (continuation-mark-set->list #f key1)
    (continuation-mark-set->list #f key2)))
;; => ((val1) (val2))
```

###### [!Function] `continuation-mark-set?` _obj_

Returns `#t` if _obj_ is a continuation mark set, otherwise `#f`.

###### [!Function] `current-continuation-marks` :optional (_tag_ `(default-continuation-prompt-tag)`)

Returns the continuation mark set for the current continuation up to
the nearest prompt with the given _tag_.

###### [!Function] `continuation-mark-set->list` _mark-set_ _key_ :optional (_tag_ `(default-continuation-prompt-tag)`)

Returns a list of values associated with _key_ in _mark-set_, from
the most recent to the oldest.

If _mark-set_ is `#f`, uses `(current-continuation-marks tag)`.

```scheme
(with-continuation-mark 'key 'outer
  (list (with-continuation-mark 'key 'inner
          (continuation-mark-set->list #f 'key))))
;; => ((inner outer))
```

###### [!Function] `continuation-mark-set->list*` _mark-set_ _keys_ :optional (_default_ `#f`) (_tag_ `(default-continuation-prompt-tag)`)

Similar to `continuation-mark-set->list`, but returns a list of vectors.
Each vector contains the values for all _keys_ in a single continuation
frame that has at least one of the keys. If a key is not present in a
frame, _default_ is used.

```scheme
(let ([key1 (make-continuation-mark-key)]
      [key2 (make-continuation-mark-key)])
  (with-continuation-marks ([key1 'val1]
                            [key2 'val2])
    (continuation-mark-set->list* #f (list key1 key2))))
;; => (#(val1 val2))
```

###### [!Function] `continuation-mark-set-first` _mark-set_ _key_ :optional (_default_ `#f`) (_tag_ `(default-continuation-prompt-tag)`)

Returns the value associated with _key_ in the first frame of _mark-set_
that has such an association, or _default_ if none is found.

If _mark-set_ is `#f`, uses `(current-continuation-marks tag)`.

###### [!Function] `continuation-mark-set->iterator` _mark-set_ _keys_ :optional (_default_ `#f`) (_tag_ `(default-continuation-prompt-tag)`)

Returns an iterator procedure over the continuation mark set. Each
call to the iterator returns two values: the current vector of values
(like from `continuation-mark-set->list*`) and the next iterator
procedure. When exhausted, the first value is `#f`.

###### [!Function] `call-with-immediate-continuation-mark` _key_ _proc_ :optional (_default_ `#f`)

Calls _proc_ with the value associated with _key_ in the immediate
continuation frame, or _default_ if no such mark exists.

If the call is not in tail position relative to a `with-continuation-mark`
form with the given _key_, _default_ is passed to _proc_.

```scheme
(with-continuation-mark 'key 'mark
  (call-with-immediate-continuation-mark 'key values))
;; => mark

(with-continuation-mark 'key 'mark
  (list (call-with-immediate-continuation-mark 'key values)))
;; => (#f)  ; not in tail position
```

###### [!Function] `make-continuation-mark-key` :optional _name_

Returns a new continuation mark key. Keys created with this function
are guaranteed to be unique and can be used as keys in continuation
marks.

If _name_ is provided, it is used for display purposes.

###### [!Function] `continuation-mark-key?` _obj_

Returns `#t` if _obj_ is a continuation mark key created by
`make-continuation-mark-key`, otherwise `#f`.

### [§3] Delimited Control Operators

The following macros provide convenient syntax for common delimited
continuation patterns. They are based on the shift/reset and
control/prompt paradigms.

###### [!Syntax] `reset` _expr_ _..._
###### [!Syntax] `reset-at` _tag_ _expr_ _..._

Installs a prompt and evaluates _expr_ .... `reset` uses the default
prompt tag, while `reset-at` uses the specified _tag_.

###### [!Syntax] `shift` _k_ _expr_ _..._
###### [!Syntax] `shift-at` _tag_ _k_ _expr_ _..._

Captures the current continuation up to the nearest `reset` (or
`reset-at` with matching _tag_) as _k_, then aborts to that prompt
and evaluates _expr_ ....

When _k_ is invoked, it reinstalls a prompt before applying the
captured continuation, making the continuation composable.

```scheme
(+ 1 (reset (* 2 (shift k (k 4)))))
;; => 9  ; (+ 1 (* 2 4))

(+ 1 (reset (* 2 (shift k (k (k 4))))))
;; => 17 ; (+ 1 (* 2 (* 2 4)))
```

###### [!Syntax] `prompt` _expr_ _..._
###### [!Syntax] `prompt-at` _tag_ _expr_ _..._

Similar to `reset` and `reset-at`, but uses an abort handler that
calls the thunk returned by `control`.

###### [!Syntax] `control` _k_ _expr_ _..._
###### [!Syntax] `control-at` _tag_ _k_ _expr_ _..._

Similar to `shift` and `shift-at`, but does not reinstall a prompt
when _k_ is invoked. This means invoking _k_ captures up to the
enclosing `prompt`, not the current position.

```scheme
(prompt (+ 2 (control k (k 5))))
;; => 7

(prompt (+ 2 (control k 5)))
;; => 5
```
