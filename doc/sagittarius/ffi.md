[§2] (sagittarius ffi) - Foreign Function Interface {#lib.sagittarius.ffi}
-------------

###### [!Library] `(sagittarius ffi)` 

This library provides FFI (Foreign Function Interface) procedures. The
library is constructed on 
[libffi](http://sourceware.org/libffi/).

This library makes user to be able to re-use existing useful C library. However
it might cause SEGV or unexpected behaviours. It is users responsibility to
avoid those errors.

Following is the simple example to use;

```c
/* C file, must be compiled as a shared library and named 'my-quick-sort.so' */
#include <stdlib.h>
#include <string.h>

#ifdef _MSC_VER
# define EXPORT __declspec(dllexport)
#else
# define EXPORT
#endif

static void quicksort_(uintptr_t base,const size_t num,const size_t size,
		       void *temp,int (*compare)(const void *,const void *))
{
  size_t pivot = 0,first2last = 0,last2first = num-1;
  while(pivot+1 != num && !compare(base+size*pivot,base+size*(pivot+1))){
    pivot++;
  }
  if(pivot+1 == num){
    return;
  }
  if(0 > compare(base+size*pivot,base+size*(pivot+1))){
    pivot++;
  }
  while(first2last < last2first){
    while(0 < compare(base+size*pivot,base+size*first2last)
	  && first2last != num-1){
      first2last++;
    }
    while(0 >= compare(base+size*pivot,base+size*last2first)
	  && last2first){
      last2first--;
    }
    if(first2last < last2first){
      if(pivot == first2last || pivot == last2first){
	pivot = pivot^last2first^first2last;
      }
      memcpy(temp,base+size*first2last,size);
      memcpy(base+size*first2last,base+size*last2first,size);
      memcpy(base+size*last2first,temp,size);
    }
  }
  quicksort_(base,first2last,size,temp,compare);
  quicksort_(base+size*first2last,num-first2last,size,temp,compare);
}

EXPORT int quicksort(void *base, const size_t num, const size_t size,
	      int (*compare)(const void *, const void *))
{
  void *temp = malloc(size);
  if(!temp){
    return -1;
  }
  quicksort_((uintptr_t)base,num,size,temp,compare);
  free(temp);
  return 0;
}
```

```scheme
;; Scheme file
;; load shared library (on Windows the extension might be '.dll')
;; On Unix like environment, the shared library must be full path or
;; located in the same directory as the script.
;; On Windows it can be on PATH environment variable as well.
(define so-library (open-shared-library "my-quick-sort.so"))
(define quick-sort 
  (c-function so-library ;; shared library
              void       ;; return type
              quicksort  ;; exported symbol
              ;; argument types
              ;; if you need pass a callback procedure, 'callback' mark needs to
              ;; be passed to the arguments list
              (void* size_t size_t callback)))

(let ((array (u8-list->bytevector '(9 3 7 5 2 6 1 4 8)))
      ;; callback procedure
      (compare (c-callback int           ;; return type
                           (void* void*) ;; argument types
                           (lambda (a b)
                             (- (pointer-ref-c-uint8 x 0)
                                (pointer-ref-c-uint8 y 0))))))
  ;; call c function. all loaded c functions are treated the same as
  ;; usual procedures.
  (quick-sort array (bytevector-length array) 1 compare)
  ;; release callback procedure.
  ;; NOTE: callback won't be GCed so users need to release it manually
  (free-c-callback compare)
  array)

;; Close shared library.
(close-shared-library so-library)
```
=> `#vu8(1 2 3 4 5 6 7 8 9)`

The document describes higher APIs to lower APIs.



### [§3] Shared library  operations

###### [!Function] `open-shared-library`  _file_ _:optional_ _(raise_ _#f)_

_file_ must be a string.

Opens given _file_ shared library and returns its pointer.

The internal process of `open-shared-library` is depending on the
platform, for example if your platform is POSIX envirionment then it will use
`dlopen`. So the resolving the _file_ depends on it. If you know the
absolute path of the shared library, then it's always better to use it.

If then internal process of the procedure failed and _raise_ is #f then it
returns NULL pointer, if _raise_ is #t then it raises an error.


###### [!Function] `close-shared-library`  _pointer_

Closes given shared library pointer and returns unspecified value.

If the given pointer does not indicate proper shared library, the behaviour
is platform dependent. It might cause SEGV.


###### [!Function] `shared-object-suffix` 

Returns platform specific shared library extension as a string value.
eg. `".dll"` in Windows, `".so"` in Linux or Unix.


### [§3] Creating C functions

This section describes more details to create a corresponding C functions.

###### [!Macro] `c-function`  _shared-library_ _return-type_ _name_ _(argument-types_ _..._ _)_

Creates a c-function object and returns a Scheme procedure.

_shared-library_ must be opened shared-library.

_return-type_ must be one of the followings;

```scheme
  void 
  bool char wchar_t
  short int long long-long
  unsigned-short unsigned-int unsigned-long unsigned-long-long
  intptr_t uintptr_t
  float double
  void* char* wchar_t*
  int8_t  int16_t  int32_t  int64_t
  uint8_t uint16_t uint32_t uint64_t
```

The return value will be converted corresponding Scheme value. Following
describes the conversion;

`bool`
: Scheme boolean

`char*`
: Scheme string from UTF-8

`wchar_t*`
: Scheme string from UTF-16 (Windows) or UTF-32. 

`char`
`short int long long-long`
`unsigned-short unsigned-int unsigned-long unsigned-long-long`
`intptr_t uintptr_t`
`int8_t  int16_t  int32_t  int64_t`
`uint8_t uint16_t uint32_t uint64_t`
: Scheme integer

`float double`
: Scheme flonum

`wchar_t`
: Scheme character.

`void*`
: Scheme FFI pointer type

NOTE: `char` returns a Scheme integer not Scheme character.

_name_ must be a symbol indicating a exported C function name.

_argument-types_ must be zero or more followings;

```scheme
  bool
  char short int long long-long
  unsigned-short unsigned-int unsigned-long unsigned-long-long
  size_t 
  wchar_t
  void* char* wchar_t*
  float double
  int8_t  int16_t  int32_t  int64_t
  uint8_t uint16_t uint32_t uint64_t
  callback
  ___
```

When the C function is called, given Scheme arguments will be converted to
corresponding C types. Following describes the conversion;

`bool`
: Scheme boolean to C 0 (#f) or 1 (#t).

`char short int long long-long unsigned-short`
`int8_t int16_t int32_t uint8_t uint16_t`
: Scheme integer to C signed long int

`unsigned-int unsigned-long uint32_t size_t`
: Scheme integer to C unsigned long int

`int64_t long-long`
: Scheme integer to C int64_t

`uint64_t unsigned-long-long`
: Scheme integer to C uint64_t

`float`
: Scheme flonum to C float

`double`
: Scheme flonum to C double

`wchar_t`
: Scheme character to C wide character
  It doesn't check the range, in case of 16 bits wide character, it's
  users' responsibility to pass the appropriate character. Passing
  out of range character doesn't raise an error but may truncate the
  value to be passed.

`void* char*`
: `void*` and `char*` are actually treated the same, internally.
  The conversion will be like this;

  Scheme string
  : Converts to UTF-8 C char\*

  Scheme bytevector
  : Convert to C char\*

  Scheme FFI pointer
  : Convert to C void\*

`wchar_t*`
: Wide character string conversion only happens when the given argument was
  Scheme string and depends on the platform. On Windows, more specifically 
  size of wchar_t is 2 platform, it converts to UTF-16 string without BOM.
  On other platform, size of wchar_t is 4 platform, it converts to UTF-32.
  Both case detects endianness automatically.
  If the given argument was bytevector, it won't convert. This case is useful
  when users need to pass buffer to a C-function.

`callback`
: Scheme FFI callback to C void\*

Note: passing Scheme string needs to be careful when users want to use it
as a buffer. It doesn't work like it. Use bytevector or FFI pointer object
for that purpose.

`___` is for variable length argument and it must be the last position
of the argument type list, otherwise it raises an error.



###### [!Function] `make-c-function`  _shared-library_ _return-type_ _name_ _argument-types_

Creates C function. This procedure is underlying procedure for
`c-function` macro. The arguments are the same as `c-function`,
only _argument-types_ must be a list of types.


###### [!Macro] `address`  _pointer_
###### [!Macro] `address`  _pointer_ _offset_

Convenient macro for address passing.

When you need to pass an address of a pointer to C function, you can write like
this;
`(c-func (address _pointer_))`

This is equivalent of following C code;
`c_func(&pointer)`

_pointer_ can be a pointer object or a bytevector.

If the second form is used, then the passing address is offset of _offset_.
It is user's responsibility to make sure the given _pointer_ has enough
space when _offset_ is passed. If the _pointer_ is a bytevector and
_offset_ is more than the bytevector size, then an error is signaled.


###### [!Macro] `c-callback`  _return-type_ _(argument-types_ _..._ _)_ _proc_

Creates a C callback.

_return-type_ must be a symbol and the same as `c-function`'s
_return-type_.

_argument-types_ must be zero or following;

```scheme
  bool
  char short int long long-long intptr_t
  unsigned-char unsigned-short unsigned-int unsigned-long-long uintptr_t
  int8_t int16_t int32_t int64_t
  uint8_t uint16_t uint32_t int64_t
  float double
  size_t wchar_t
  void* char* wchar_t*
```

The conversion of C to Scheme is the same as `c-function`'s
_return-type_.

NOTE: if the content of `void*` won't be copied, thus if you modify it in
the callback procedure, corresponding C function will get affected.

NOTE: `char*` and `wchar_t*` lose the original pointer address, this
is due to the Scheme string to C string conversion. If the pointer address
is required, then use `void*` and bytevector.

_proc_ must be a procedure takes the same number of arguments as
_argument-types_ list.

Created callbacks are stored intarnal static storage to avoid to get GCed.
This is because C functions which accept callback may hold the given callback
in their storage which could be outside of Sagittarius GC root. So it is
users' responsibility to release created callback to avoid memory leak. To
release callbacks, you need to use `free-c-callback`.


###### [!Macro] `make-c-callback`  _return-type_ _argument-types_ _proc_

Creates C callback. This procedure is underlying procedure for
`c-callback` macro. The arguments are the same as `c-callback`,
only _argument-types_ must be a list of types.


###### [!Function] `free-c-callback`  _callback_

Release _callback_.

### [§3] Pointer operations

Using C functions, users can not avoid to use raw pointers. This section
describes how to create or convert a pointer.

###### [!Function] `pointer?`  _obj_

Returns #t if _obj_ is FFI pointer object, otherwise #f.

###### [!Function] `integer->pointer`  _integer_

Converts given _integer_ to pointer object.

To represents NULL pointer, you can write like this;
``(integer->pointer 0)`` => ``#\<pointer 0x0>``



###### [!Function] `pointer->integer` _pointer_ _:optional_ _bits_
###### [!Function] `pointer->uinteger` _pointer_ _:optional_ _bits_

Converts given _pointer_ to integer/uinteger, respectively.

The optional argument _bits_ must be an exact integer range of fixnum.

If the optional argument _bits_ is specified, then the procedure mask
the pointer value. If the _bits_ is negative or more than pointer size
bits then it returns non masked value.

This is useful when C procedure sets the pointer value however it only sets
a half of bits and returning value is needed only a half of bits. 
For example, a C procedure takes 2 arguments, one is buffer pointer the
other one is buffer size pointer. When buffer size is -1 then it allocates 
sufficient buffer and sets buffer size pointer the allocated size. In this
case. In this case, if you are using 64 bit environment and buffer size
pointer is 32 bit value's pointer returning value's upper 32 bit would be
0xFFFFFFFF. If the optional argument is specified to 32 then the procedure
only returns lower 32 bit value.


###### [!Function] `pointer->string` _pointer_ _:optional_ _(transcoder_ _(native-transcoder))_

Converts given _pointer_ to Scheme string.

The given _pointer_ must be terminated by 0 otherwise it won't stop until
it reaches 0.

If NULL pointer is given, it raises `&assertion`.


###### [!Function] `pointer->bytevector` _pointer_ _size_ _:optional_ _(offset_ _0)_ _(shared_ _#t)_

_Size_ must be an exact integer.

Converts given _pointer_ to Scheme bytevector from given _offset_. 

If optional argument _shared_ is #f, then the content of pointer won't
be shared between pointer and bytevector. Default value is #t, thus if the
given _pointer_ is modified, then the created bytevector gets affected.

If NULL pointer is given, it raises `&assertion`.


###### [!Function] `bytevector->pointer` _bv_ _:optional_ _(offset_ _0)_ _(shared_ _#t)_

Converts given _bytevector_ to pointer from given _offset_. 

If optional argument _shared_ is #f, then the content of bytevector won't
be shared between bytevector and pointer. Default value is #t, thus if the
given _bv_ is modified, then the created pointer gets affected.

If NULL pointer is given, it raises `&assertion`.


###### [!Function] `object->pointer` _obj_
###### [!Function] `pointer->object` _pointer_

CAUTION: These operations are really dangerous especially
`pointer->object`.

Converts Scheme object to pointer and pointer to Scheme object respectively.
The operations are useful to pass Scheme object to callbacks and restore
it.


###### [!Function] `deref` _pointer_ _offset_

_offset_ must be a fixnum.

Returns a pointer offset _offset_ of given _pointer_. The same as
following C code;

```scheme
void* deref(void **pointer, int offset) {
  return pointer[offset]; 
}
```

If `NULL` pointer is given, it raises `&assertion`.

###### [!Function] `pointer-address` _pointer_ _:optional_ _offset_

Returns an address of given _pointer_.

If optional argument _offset_ is given, then the returning address of
_pointer_ is the offset of given _offset_.

NOTE: This creates a newly allocated Scheme FFI pointer object.

NOTE: If the returned value is modified then given _pointer_ will be
affected.


###### [!Function] `allocate-pointer` _size_ _:optional_ _(fill_ _0)_

_size_ must be a fixnum.

Allocates a _size_ of byte memory and returns an pointer object.

If optional argument _fill_ is given, it must be a fixnum, then the
procedure fill the given _fill_ into the allocated memory using
`memset(3)`. 

NOTE: the _fill_ will be converted to an unsigned char by the 
`memset(3)`.

The allocated memory will be GCed.


###### [!Function] `c-malloc`  _size_

_size_ must be a fixnum.

Allocates a _size_ of byte memory and returns an pointer object using 
C's `malloc`.

The allocated memory won't be GCed. So releasing the memory is users'
responsibility.


###### [!Function] `c-free`  _pointer_

_pointer_ must be a pointer created by _c-malloc_.

Release the _pointer_.

The procedure won't check if the pointer is allocated by _c-malloc_ or not.
And the behaviour when GCable pointer is passed is undefined.


###### [!Variable] `null-pointer` 

A pointer represents NULL.

This value is not a constant and if you modify this by using `address`,
you might break some codes.


###### [!Function] `null-pointer?`  _obj_

Returns #t when _obj_ is a pointer representing NULL otherwise #f.

###### [!Function] `empty-pointer` 

Creates a NULL pointer. This is for convenience.

###### [!Function] `pointer-ref-c-`  _type_ _pointer_ _offset_

_offset_ must be a fixnum.

Returns an integer value of offset _offset_ of _pointer_ depending
on the _type_.

Following _type_ are supported;
```
int8 int16 int32 int64
uint8 uint16 uint32 uint64
char wchar short int long long-long
unsigned-char unsigned-short unsigned-int unsigned-long unsigned-long-long
intptr uintptr
float double
pointer
```
NOTE: if the _type_ is `flonum` or `double`, then it returns
Scheme flonum

NOTE: if the _type_ is `pointer`, then it returns Scheme FFI pointer.


###### [!Function] `pointer-set-c-`  _type_ _!_ _pointer_ _offset_ _value_

_offset_ must be a fixnum.

Sets _value_ to offset _offset_ of _pointer_. Supporting _type_s
are the same as `pointer-ref-c-_type_`The type conversion is the same as `c-function`'s _return-type_.

There is no direct procedures to handle C arrays. Following is an example
of how to handle array of pointers;

```scheme
(import (rnrs) (sagittarius ffi))

(define (string-vector->c-array sv)
  (let ((c-array (allocate-pointer (* (vector-length sv) size-of-void*))))
    (do ((i 0 (+ i 1)))
        ((= i (vector-length sv)) c-array)
      ;; pointer-set-c-pointer! handles Scheme string (converts to UTF-8)
      ;; If you need other encoding, then you need to write other conversion
      ;; procedure.
      (pointer-set-c-pointer! c-array (* i size-of-void*) (vector-ref sv i)))))

;; how to use
(let ((p (string-vector->c-array #("abc" "def" "ghijklmn"))))
  (do ((i 0 (+ i 1)))
      ((= i 3))
    ;; deref handles pointer offset.
    ;; it can be also (pointer-ref-c-pointer p (* i size-of-void*))
    (print (pointer->string (deref p i)))))
```

Following is an example for Scheme string to UTF-16 bytevector;

```scheme
(import (rnrs) (sagittarius ffi))
;; Converts to UTF16 big endian (on little endian environment)
(define (string->c-string s)
  (let* ((bv (string->utf16 s (endianness big)))
         ;; add extra 2 bytes for null terminated string
         (p  (allocate-pointer (+ (bytevector-length bv) 2))))
    (do ((i 0 (+ i 2)))
        ((= i (bytevector-length bv)) p)
      ;; pointer-set-c-uint16! uses native endianness to set the value
      ;; so this is platform dependent code.
      (pointer-set-c-uint16! p i 
        (bytevector-u16-ref bv i (endianness little))))))
```


###### [!Function] `set-pointer-value!`  _pointer_ _value_

_value_ must be exact integer up to `size-of-void*` bytes.

Sets the pointer value. This is useful to reuse the existing pointer object.

CAUTION: this operation is really dangerous so be aware of it!


### [§3] C struct operations

C's struct is mere memory chunk so it is possible to access its member directly,
if you know exact offset of it. However it is convenient if you can operate 
similar structure. This section describes how to define C structure in Scheme
world.

###### [!Macro] `define-c-struct`  _name_ _clauses_ _..._
###### [!Macro] `define-c-struct`  _name_ _(alignment_ _n)_ _clauses_ _..._

Defines C structure.

_clauses_ must be following form;

```scheme
(_type_ _name_)
(_type_ `array` _size_ _name_)
(`struct` _struct-name_ _name_)
(`bit-field` _type_ (_name_ _bit_) ...)
(`bit-field` (_type_ _endian_) (_name_ _bit_) ...)
```

_name_ must be a symbol.

If the second form is used, then `alignment` is an auxiliary syntax
and _n_ must be an integer which must be either negative number or
one of `1`, `2`, `4`, `8`, or `16`. This form
specifies the alignemtn of the struct. If the _n_ is negative number,
then it uses platform default alignment, if it's one of the above number,
then the alignment is according to the given number.

The first form is the simple C type form. _type_ must be a symbol and the
same as one of the `c-function`'s _return-types_ or `callback`.
Following describes the concrete example and the equivalent C structure:

```scheme
(define-c-struct st
  (int foo)
  (callback fn))
#|
struct st
{
  int foo;
  void* fn; /* function pointer */
};
|#
```

The second form is defining C _type_ array with _size_.
Following describes the concrete example and the equivalent C structure:

```scheme
(define-c-struct st
  (int array 10 foo))
#|
struct st
{
  int foo[10];
};
|#
```

The third form is defining internal structure.
Following describes the concrete example and the equivalent C structure:

```scheme
(define-c-struct st1
  (int array 10 foo))
(define-c-struct st2
  (struct st1 st)
  (int bar))
#|
struct st1
{
  int foo[10];
};
struct st2
{
  struct st1 st;
  int bar;
};
|#
```

So far, we don't support direct internal structure so users always need to
extract internal structures.

The forth and fifth forms are bit fields. _type_ must be an integer
type such as `unsigned-int`. If the given _type_ is not an integer,
then `&assertion` is raised.

Following describes the concrete example and the equivalent C structure:

```scheme
(define-c-struct st1
  (bit-field unsigned-int (a 10) (b 20)))
#|
struct st1
{
  unsigned int a : 10;
  unsigned int b : 20;
};
|#
```

If the fifth form is used, then _endian_ must be an identifier which has
valid name for `endianness` macro. Then the created structure packs
the value according to the given _endian_.

If the total amount of bits is greater than given _type_, then 
`&assertion` is raised.

NOTE: Even though, this can accept signed integer the returning value would
not be signed. It is safe to specify unsigned type.


The macro also defines accessors for the c-struct. Following naming rules are
applied;

- For getter: _name_-_member-name_-ref
- For setter: _name_-_member-name_-set!

The macro also defines size variable for the c-struct. If the name of the
c-struct if _foo_, then the variable name will be `size-of-foo`.


###### [!Function] `size-of-c-struct`  _struct_

_struct_ must be a C structure defined by `define-c-struct`.

Returns the size of given _struct_.


###### [!Function] `allocate-c-struct`  _struct_

Allocates memory for _struct_ and returns a pointer.

###### [!Function] make-`_struct-name_` :key _members_ _..._ **[@since] `0.9.13`**

A convenient constructor of _struct-name_ c-struct.

Keyword arguments represents the member name, if it's provided, then the value
is populdated to the returning pointer.

###### [!Function] `_struct-name_`  _-_ _member-name_ _-ref_ _struct-pointer_ _inner-member-names_ _..._
###### [!Function] `_struct-name_`  _-_ _member-name_ _-set!_ _struct-pointer_ _value_ _inner-member-names_ _..._

A getter/setter of _struct-name_ c-struct.

This is automatically defined by `define-c-struct` macro.

The optional argument _inner-member-names_ can be passed to get inner
struct values.

Following describes how it works.

```scheme
(define-c-struct in
  (int  i)
  (char c))
(define-c-struct out
  (int  i)
  (struct in in0))

(define out (allocate-c-struct out))
(out-i-set! out 100 'i)   ;; -> unspecified
(out-in0-set! out 200 'i) ;; -> unspecified
(out-i-ref out)           ;; -> 100
(out-in0-ref out 'i)      ;; -> 200
(out-in0-ref out)         ;; -> pointer object (indicating the inner struct address)
```


#### [§4] Low level C struct accessors

###### [!Function] `c-struct-ref`  _pointer_ _struct_ _name_

_struct_ must be a C structure defined with `define-c-struct`.
_name_ must be a symbol and _struct_ has the same member.
_pointer_ should be a pointer allocated by `allocate-c-struct` with
_struct_.

Returns a member _name_'s value of _struct_ from _pointer_.


###### [!Function] `c-struct-set!`  _pointer_ _struct_ _name_ _value_

_struct_ must be a C structure defined with `define-c-struct`.
_name_ must be a symbol and _struct_ has the same member.
_pointer_ should be a pointer allocated by `allocate-c-struct` with
_struct_.

Sets _value_ to _pointer_ offset of member _name_ of _struct_.


### [§3] Typedef operations

###### [!Macro] `define-c-typedef`  _original_ _new-names_ _..._

Convenient macro.

Defines other name of _original_ with _new-names_.

_new-names_ must be following forms;

```scheme
()
((`*` _new-p_) _rest_ ...)
((`s*` _new-sp_) _rest_ ...)
(_new_ _rest_ ...)
```

The first for defines nothing.

If the rest of form is used and _rest_ is not null, then it will recursively
define.

The second form's `*` defines _new-p_ as `void*`.

The third form's `s*` defines _new-sp_ as `char*`.

The forth form defines _new_ as _original_.

Following example describes how to will be expanded approximately.

```scheme
(define-c-typedef char (* char_ptr) byte (s* string))

=> 

(begin
  (define char_ptr void*)
  (define byte char)
  (define string char*)
)
```


### [§3] Sizes and aligns

###### [!Constant] `size-of-`  _type_
###### [!Constant] `align-of-`  _type_

a size or align of _type_, respectively.

Following types are supported;

```scheme
  bool char
  short int long long-long
  unsigned-short unsigned-int unsigned-long unsigned-long-long
  intptr_t uintptr_t size_t
  float double
  int8_t  int16_t  int32_t  int64_t
  uint8_t uint16_t uint32_t uint64_t
  void* wchar_t
```

The values are platform dependent.


### [§3] Finalizer operations

Some of C resource must be released but if you can not decide or do not want to
restrict when, then you can release it at GC time.

NOTE: GC might not happen if your script is very short, so it is better not to
relay these procedures too much.

###### [!Function] `register-ffi-finalizer`  _pointer_ _proc_

_pointer_ must be a pointer allocated with GCable memory.

_proc_ must be a procedure and accepts one argument. The argument will be
the _pointer_.

Register _proc_ as _pointer_'s finalizer and returns _pointer_.


###### [!Function] `unregister-ffi-finalizer`  _pointer_

_pointer_ must be a pointer allocated with GCable memory.

Remove finalizer form _pointer_ and returns _pointer_.


