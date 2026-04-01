[§2] (sagittarius parameters) - Parameters library {#lib.sagittarius.parameters}
-------------------------------------------------------------

###### [!Library] `(sagittarius parameters)`

This library provides parameter objects, which are dynamically scoped
variables.  Parameters allow programs to temporarily override values
in a specific dynamic context without affecting other concurrent
execution contexts.

### [§3] Parameter Objects

###### [!Class] `<parameter>`

Base class for parameter objects.

###### [!Function] `parameter?` _obj_

Returns `#t` if _obj_ is a parameter object, otherwise `#f`.

###### [!Function] `make-parameter` _init_ :optional _converter_

Creates a new parameter object with initial value _init_.

If _converter_ is provided, it must be a procedure that accepts one
argument.  The _converter_ is applied to _init_ and any value assigned
to the parameter later.

```scheme
(import (sagittarius parameters))

(define current-level (make-parameter 0))
(current-level)  ; => 0

(current-level 5)
(current-level)  ; => 5

;; With converter
(define current-port-name 
  (make-parameter "default" 
    (lambda (v) (if (string? v) v (symbol->string v)))))
(current-port-name 'stdout)  ; Converts symbol to string
(current-port-name)  ; => "stdout"
```

### [§3] Thread Parameters

Thread parameters are parameters whose values are stored in
thread-local storage.

Each thread maintains its own value for a
thread parameter.

###### [!Class] `<thread-parameter>`

Subclass of `<parameter>` for thread-local parameters.

###### [!Function] `thread-parameter?` _obj_

Returns `#t` if _obj_ is a thread parameter object, otherwise `#f`.

###### [!Function] `make-thread-parameter` _init_ :optional _converter_

Creates a new thread parameter with initial value _init_.

If _converter_ is provided, it is applied to values as with `make-parameter`.

```scheme
(import (sagittarius parameters))

;; Thread parameters store values in thread-local storage
(define thread-id (make-thread-parameter 0))

;; Each thread would see its own value
(thread-id 42)
(thread-id)  ; => 42

;; Thread parameters can also have converters
(define thread-name 
  (make-thread-parameter "main" 
    (lambda (v) (if (string? v) v (symbol->string v)))))
(thread-name 'worker)
(thread-name)  ; => "worker"
```

### [§3] Parameterization

Parameterization objects capture a snapshot of parameter bindings.

###### [!Function] `current-parameterization`

Returns the current parameterization object.

###### [!Function] `parameterization?` _obj_

Returns `#t` if _obj_ is a parameterization object, otherwise `#f`.

###### [!Function] `call-with-parameterization` _parameterization_ _thunk_

Calls _thunk_ with the given _parameterization_ installed as the current 
parameterization. Returns the value(s) returned by _thunk_.

```scheme
(define p (make-parameter 1))
(define saved (current-parameterization))

(parameterize ((p 2))
  (set! saved (current-parameterization)))

(p)  ; => 1
(call-with-parameterization saved (lambda () (p)))  ; => 2
```

### [§3] Thread-Local Storage

Low-level thread-local storage API.

###### [!Class] `<thread-local>`

Class for thread-local storage objects.

###### [!Function] `thread-local?` _obj_

Returns `#t` if _obj_ is a thread-local storage object, otherwise `#f`.

###### [!Function] `make-thread-local` _value_ :optional _inheritable?_

Creates a thread-local storage object with initial _value_.

If _inheritable?_ is true, the value is inherited by child threads.

###### [!Function] `tlref` _thread-local_

Returns the current thread's value for _thread-local_.

###### [!Function] `tlset!` _thread-local_ _obj_

Sets the current thread's value for _thread-local_ to _obj_.

### [§3] Parameterization Syntax

###### [!Syntax] `parameterize` ((_parameter_ _value_) ...) _body_ ...

Evaluates _body_ with each _parameter_s temporarily bound to its corresponding 
_value_s. The previous values are restored when control leaves _body_ either 
normally or by a continuation jump.

This form uses continuation marks and integrates properly with delimited 
continuations.

```scheme
(define p (make-parameter 1))

(p)  ; => 1
(parameterize ((p 2))
  (p))  ; => 2
(p)  ; => 1

;; Multiple parameters
(define p1 (make-parameter 10))
(define p2 (make-parameter 20))

(parameterize ((p1 100) (p2 200))
  (list (p1) (p2)))  ; => (100 200)
```

###### [!Syntax] `parameterize/dw` ((_parameter_ _value_) ...) _body_ ...

[SRFI-39] Similar to `parameterize`, but uses `dynamic-wind` for
parameter restoration to adhear to SRFI-39 semantics.

###### [!Syntax] `temporarily` ((_parameter_ _expr_) ...) _body_ ... {#syntax.temporarily}

[SRFI-226] Temporarily swaps the values of _parameter_s the results of 
_expr_s evaluating _body_.

Similar to `parameterize`, but uses direct value swapping. The
original values are restored using `dynamic-wind`.

```scheme
(define p1 (make-parameter 1))
(define p2 (make-parameter 2))

(temporarily ((p1 10) (p2 20))
  (list (p1) (p2)))  ; => (10 20)

(list (p1) (p2))  ; => (1 2)
```
