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

###### [!Function] `call-with-continuation-prompt` _proc_ :optional _tag_ _abort-handler_ :rest _args_
###### [!Function] `call/prompt` _proc_ :optional _tag_ _abort-handler_ :rest _args_

Installs a new prompt into the current continuation frame with the
given _tag_ and _abort-handler_, then call the _proc_ with the _args_.

If the _tag_ is not specified, then `(default-continuation-prompt-tag)` is used.  
If the _abort-handler_ is not specified or `#f`, then default abort handler is used.
The default abort handler accepts one argument, which must be a thunk. It's
approximately the same as like this:

```scheme
(call-with-continuation-prompt thunk tag #f)
```

###### [!Function] `continuation-prompt-available?` _tag_ :optional (_cont_ #f)

Returns if the prompt with _tag_ is available in the continuation.

If _cont_ is `#f` or omit, then it checks the current continuation.__
Otherwise, _cont_ must satisfy `continuation?`, then check the given
continuation.


###### [!Function] `abort-current-continuation` _tag_ . _args_
###### [!Function] `abort/cc` _tag_ . _args_

Aborts the current continuation up until the specified prompt tag _tag_.
Then invokes the abort handler of the prompt.

If the prompt is not found, then it raises an error.

The rest arguments _args_ will be passed to the abort handler.

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

###### [!Function] `continuation-prompt-tag?` _obj_

Returns `#t` if the given _obj_ is a continuation prompt tag.

Currently, continuation prompt tag is implented with a list, however
this may change in the future.  So, users shouldn't depend on it.

###### [!Function] `make-continuation-prompt-tag` :optional (_name_ `symbol?`)

Returns a newly created continuation prompt tag.

If _name_ is specified, then it uses the _name_ as the prompt tag name.
Otherwise generates one.

###### [!Function] `default-continuation-prompt-tag`

Returns a default continuation prompt tag.

###### [!Function] `call-with-continuation-barrier` _thunk_

Installs a continuation barrier and call _thunk_.

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
