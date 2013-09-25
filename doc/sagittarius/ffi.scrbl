@; -*- coding:utf-8; -*-

@subsection[:tag "lib.sagittarius.ffi"]{(sagittarius ffi) - Foreign Function Interface}

@define[Library]{@name{(sagittarius ffi)}}
@desc{This library provides FFI (Foreign Function Interface) procedures. The
library is constructed on 
@hyperlink[:href "http://sourceware.org/libffi/"]{libffi}.

This library makes user to be able to re-use existing useful C library. However
it might cause SEGV or unexpected behaviours. It is users responsibility to
avoid those errors.

Following is the simple example to use;
@codeblock[=> #vu8(1 2 3 4 5 6 7 8 9)]{
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
  ;; NOTE: this will be release at GC time.
  (free-c-callback compare)
  array)

;; Close shared library.
(close-shared-library so-library)

;; End of Scheme file

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

}
The document describes higher APIs to lower APIs.

}

@subsubsection{Shared library  operations}

@define[Function]{@name{open-shared-library} @args{file :optional (raise #f)}}
@desc{@var{file} must be a string.

Opens given @var{file} shared library and returns its pointer.

The internal process of @code{open-shared-library} is depending on the
platform, for example if your platform is POSIX envirionment then it will use
@code{dlopen}. So the resolving the @var{file} depends on it. If you know the
absolute path of the shared library, then it's always better to use it.

If then internal process of the procedure failed and @var{raise} is #f then it
returns NULL pointer, if @var{raise} is #t then it raises an error.
}

@define[Function]{@name{close-shared-library} @args{pointer}}
@desc{Closes given shared library pointer and returns unspecified value.

If the given pointer does not indicate proper shared library, the behaviour
is platform dependent. It might cause SEGV.
}

@define[Function]{@name{shared-object-suffix}}
@desc{Returns platform specific shared library extension as a string value.
eg. @code{".dll"} in Windows, @code{".so"} in Linux or Unix.
}

@subsubsection{Creating C functions}

This section describes more details to create a corresponding C functions.

@define[Macro]{@name{c-function}
 @args{shared-library return-type name (argument-types @dots{})}}
@desc{Creates a c-function object and returns a Scheme procedure.

@var{shared-library} must be opened shared-library.

@var{return-type} must be one of the followings;
@codeblock{
  void 
  bool  char
  short int long long-long
  unsigned-short unsigned-int unsigned-long unsigned-long-long
  intptr_t uintptr_t
  float double
  void* char* wchar_t*
  int8_t  int16_t  int32_t  int64_t
  uint8_t uint16_t uint32_t uint64_t
}
The return value will be converted corresponding Scheme value. Following
describes the conversion;
@dl-list{
@dl-item[@code{bool}]{Scheme boolean}
@dl-item[@code{char*}]{Scheme string from UTF-8}
@dl-item[@code{wchar_t*}]{Scheme string from UTF-16 (Windows) or UTF-32. }
@dl-itemx[6
          @code{char}
          @code{short int long long-long}
          @code{unsigned-short unsigned-int unsigned-long unsigned-long-long}
          @code{intptr_t uintptr_t}
          @code{int8_t  int16_t  int32_t  int64_t}
          @code{uint8_t uint16_t uint32_t uint64_t}]{Scheme integer}
@dl-item[@code{float double}]{Scheme flonum}
@dl-item[@code{void*}]{Scheme FFI pointer type}
}
NOTE: @code{char} returns a Scheme integer not Scheme character.

@var{name} must be a symbol indicating a exported C function name.

@var{argument-types} must be zero or more followings;
@codeblock{
  bool
  char short int long long-long
  unsigned-short unsigned-int unsigned-long unsigned-long-long
  size_t
  void* char* wchar_t*
  float double
  int8_t  int16_t  int32_t  int64_t
  uint8_t uint16_t uint32_t uint64_t
  callback
  ___
}
When the C function is called, given Scheme arguments will be converted to
corresponding C types. Following describes the conversion;
@dl-list{
@dl-item[@code{bool}]{Scheme boolean to C 0 (#f) or 1 (#t).}
@dl-itemx[2
          @code{char short int long long-long unsigned-short}
          @code{int8_t int16_t int32_t uint8_t uint16_t}
         ]{Scheme integer to C signed long int}
@dl-item[@code{unsigned-int unsigned-long uint32_t size_t}]{
  Scheme integer to C unsigned long int}
@dl-item[@code{int64_t long-long}]{Scheme integer to C int64_t}
@dl-item[@code{uint64_t unsigned-long-long}]{Scheme integer to C uint64_t}
@dl-item[@code{float}]{Scheme flonum to C float}
@dl-item[@code{double}]{Scheme flonum to C double}
@dl-item[@code{void* char*}]{
  @code{void*} and @code{char*} are actually treated the same, internally.
  The conversion will be like this;
  @dl-list{
   @dl-item["Scheme string"]{Converts to UTF-8 C char*}
   @dl-item["Scheme bytevector"]{Convert to C char*}
   @dl-item["Scheme FFI pointer"]{Convert to C void*}
  }
}
@dl-item[@code{wchar_t*}]{
  Wide character string conversion only happens when the given argument was
  Scheme string and depends on the platform. On Windows, more specifically 
  size of wchar_t is 2 platform, it converts to UTF-16 string without BOM.
  On other platform, size of wchar_t is 4 platform, it converts to UTF-32.
  Both case detects endianness automatically.

  If the given argument was bytevector, it won't convert. This case is useful
  when users need to pass buffer to a C-function.
}
@dl-item[@code{callback}]{Scheme FFI callback to C void*}
}
Note: passing Scheme string needs to be careful when users want to use it
as a buffer. It doesn't work like it. Use bytevector or FFI pointer object
for that purpose.

@code{___} is for variable length argument and it must be the last position
of the argument type list, otherwise it raises an error.

}

@define[Macro]{@name{address} @args{pointer}}
@desc{Convenient macro for address passing.

When you need to pass an address of a pointer to C function, you can write like
this;
@snipet{(c-func (address @var{pointer}))}

This is equivalent of following C code;
@snipet{c_func(&pointer)}

NOTE: this works only FFI pointer object.
}

@define[Macro]{@name{c-callback}
 @args{return-type (argument-types @dots{}) proc}}
@desc{Creates a C callback.

@var{return-type} must be a symbol and the same as @code{c-function}'s
@var{return-type}.

@var{argument-types} must be zero or following;
@codeblock{
  bool
  char short int long long-long intptr_t
  unsigned-char unsigned-short unsigned-int unsigned-long-long uintptr_t
  int8_t int16_t int32_t int64_t
  uint8_t uint16_t uint32_t int64_t
  float double
  size_t
  void*
}
The conversion of C to Scheme is the same as @code{c-function}'s
@var{return-type}.

NOTE: if the content of @code{void*} won't be copied, thus if you modify it in
the callback procedure, corresponding C function will get affected.

NOTE: callback doesn't support @code{char*} nor @code{wchar_t*}. It is because
the conversion loses original pointer address and you might not want it. So
it is users responsibility to handle it.

@var{proc} must be a procedure takes the same number of arguments as
@var{argument-types} list.
}

@define[Function]{@name{free-c-callback} @args{callback}}
@desc{Release @var{callback}.}

@subsubsection{Pointer operations}

Using C functions, users can not avoid to use raw pointers. This section
describes how to create or convert a pointer.

@define[Function]{@name{pointer?} @args{obj}}
@desc{Returns #t if @var{obj} is FFI pointer object, otherwise #f.}

@define[Function]{@name{integer->pointer} @args{integer}}
@desc{Converts given @var{integer} to pointer object.

To represents NULL pointer, you can write like this;
@snipet[=> "#<pointer 0x0>"]{(integer->pointer 0)}
}

@define[Function]{@name{pointer->integer} @args{pointer}}
@desc{Converts given @var{pointer} to integer.}

@define[Function]{@name{pointer->string}
 @args{pointer :optional (transcoder (native-transcoder))}}
@desc{Converts given @var{pointer} to Scheme string.

The given @var{pointer} must be terminated by 0 otherwise it won't stop until
it reaches 0.

If NULL pointer is given, it raises @code{&assertion}.
}

@define[Function]{@name{pointer->bytevector}
 @args{pointer size}}
@desc{@var{Size} must be an exact integer.

Converts given @var{pointer} to Scheme bytevector.

If NULL pointer is given, it raises @code{&assertion}.
}

@define[Function]{@name{object->pointer} @args{obj}}
@define[Function]{@name{pointer->object} @args{pointer}}
@desc{
CAUTION: These operations are really dangerous especially
@code{pointer->object}.

Converts Scheme object to pointer and pointer to Scheme object respectively.
The operations are useful to pass Scheme object to callbacks and restore
it.
}

@define[Function]{@name{deref} @args{pointer offset}}
@desc{@var{offset} must be a fixnum.

Returns a pointer offset @var{offset} of given @var{pointer}. The same as
following C code;
@codeblock{
void* deref(void **pointer, int offset) {
  return pointer[offset]; 
}
}
If NULL pointer is given, it raises @code{&assertion}.
}

@define[Function]{@name{pointer-address} @args{pointer}}
@desc{Returns an address of given @var{pointer}.

NOTE: This creates a newly allocated Scheme FFI pointer object.

NOTE: If the returned value is modified then given @var{pointer} will be
affected.
}

@define[Function]{@name{allocate-pointer} @args{size}}
@desc{@var{size} must be a fixnum.

Allocates a @var{size} of byte memory and returns an pointer object.

The allocated memory will be GCed.
}

@define[Function]{@name{c-malloc} @args{size}}
@desc{@var{size} must be a fixnum.

Allocates a @var{size} of byte memory and returns an pointer object using 
C's @code{malloc}.

The allocated memory won't be GCed. So releasing the memory is users'
responsibility.
}

@define[Function]{@name{c-free} @args{pointer}}
@desc{@var{pointer} must be a pointer created by @var{c-malloc}.

Release the @var{pointer}.

The procedure won't check if the pointer is allocated by @var{c-malloc} or not.
And the behaviour when GCable pointer is passed is undefined.
}

@define[Variable]{@name{null-pointer}}
@desc{A pointer represents NULL.

This value is not a constant and if you modify this by using @code{address},
you might break some codes.
}

@define[Function]{@name{null-pointer?} @args{obj}}
@desc{Returns #t when @var{obj} is a pointer representing NULL otherwise #f.}

@define[Function]{@name{empty-pointer}}
@desc{Creates a NULL pointer. This is for convenience.}

@define[Function]{@name{pointer-ref-c-@var{type}} @args{pointer offset}}
@desc{@var{offset} must be a fixnum.

Returns an integer value of offset @var{offset} of @var{pointer} depending
on the @var{type}.

Following @var{type} are supported;
@code{
  int8  int16  int32  int64
  uint8 uint16 uint32 uint64
  char wchar short  int long long-long
  unsigned-char unsigned-short unsigned-int unsigned-long unsigned-long-long
  intptr uintptr
  float double
  pointer
}
NOTE: if the @var{type} is @code{flonum} or @code{double}, then it returns
Scheme flonum

NOTE: if the @var{type} is @code{pointer}, then it returns Scheme FFI pointer.
}

@define[Function]{@name{pointer-set-c-@var{type}!} @args{pointer offset value}}
@desc{@var{offset} must be a fixnum.

Sets @var{value} to offset @var{offset} of @var{pointer}. Supporting @var{type}s
are the same as @code{pointer-ref-c-@var{type}}

The type conversion is the same as @code{c-function}'s @var{return-type}.

There is no direct procedures to handle C arrays. Following is an example
of how to handle array of pointers;
@codeblock{
(import (rnrs) (sagittarius ffi))

(define (string->c-string s)
  (let* ((bv (string->utf8 s))
         (p  (allocate-pointer (+ (bytevector-length bv) 1))))
    (do ((i 0 (+ i 1)))
        ((= i (bytevector-length bv)) p)
      (pointer-set-c-uint8! p i (bytevector-u8-ref bv i)))))
(define (string-vector->c-array sv)
  (let ((c-array (allocate-pointer (* (vector-length sv) size-of-void*))))
    (do ((i 0 (+ i 1)))
        ((= i (vector-length sv)) c-array)
      (let ((p (string->c-string (vector-ref sv i))))
        (pointer-set-c-pointer! c-array (* i size-of-void*) p)))))

;; how to use
(let ((p (string-vector->c-array #("abc" "def" "ghijklmn"))))
  (do ((i 0 (+ i 1)))
      ((= i 3))
    ;; deref handles pointer offset.
    ;; it can be also (pointer-ref-c-pointer p (* i size-of-void*))
    (print (pointer->string (deref p i)))))
}

}

@define[Function]{@name{set-pointer-value!} @args{pointer value}}
@desc{@var{value} must be exact integer up to @code{size-of-void*} bytes.

Sets the pointer value. This is useful to reuse the existing pointer object.

CAUTION: this operation is really dangerous so be aware of it!
}

@subsubsection{C struct operations}

C's struct is mere memory chunk so it is possible to access its member directly,
if you know exact offset of it. However it is convenient if you can operate 
similar structure. This section describes how to define C structure in Scheme
world.

@define[Macro]{@name{define-c-struct} @args{name clauses @dots{}}}
@;@define[Macro]{@name{define-c-struct} @args{name :packed clauses @dots{}}}
@desc{Defines C structure.

@var{clauses} must be following form;
@codeblock{
(@var{type} @var{name})
(@var{type} @code{array} @var{size} @var{name})
(@code{struct} @var{struct-name} @var{name})
}
@var{name} must be a symbol.

The first form is the simple C type form. @var{type} must be a symbol and the
same as one of the @code{c-function}'s @var{return-types} or @code{callback}.
Following describes the equivalent C structure is like this;
@codeblock{
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
}

The second form is defining C @var{type} array with @var{size}.
Following describes the equivalent C structure is like this;
@codeblock{
(define-c-struct st
  (int array 10 foo))
#|
struct st
{
  int foo[10];
};
|#
}

The third form is defining internal structure.
Following describes the equivalent C structure is like this;
@codeblock{
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
}
So far, we don't support direct internal structure so users always need to
extract internal structures.

@; If @var{:packed} keyword is given, then defined structure is packed (without
@; padding).

The macro also defines accessors for the c-struct. Following naming rules are
applied;

@itemlist{
@item{For getter: @var{name}-@var{member-name}-ref}
@item{For setter: @var{name}-@var{member-name}-set!}
}

}

@define[Function]{@name{size-of-c-struct} @args{struct}}
@desc{@var{struct} must be a C structure defined by @code{define-c-struct}.

Returns the size of given @var{struct}.
}

@define[Function]{@name{allocate-c-struct} @args{struct}}
@desc{Allocates memory for @var{struct} and returns a pointer.}

@define[Function]{@name{@var{struct-name}-@var{member-name}-ref}
 @args{struct-pointer inner-member-names @dots{}}}
@define[Function]{@name{@var{struct-name}-@var{member-name}-set!}
 @args{struct-pointer value inner-member-names @dots{}}}
@desc{A getter/setter of @var{struct-name} c-struct.

This is automatically defined by @code{define-c-struct} macro.

The optional argument @var{inner-member-names} can be passed to get inner
struct values.

Following describes how it works.
@codeblock{
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
}
}

@sub*section{Low level C struct accessors}

@define[Function]{@name{c-struct-ref} @args{pointer struct name}}
@desc{@var{struct} must be a C structure defined with @code{define-c-struct}.
@var{name} must be a symbol and @var{struct} has the same member.
@var{pointer} should be a pointer allocated by @code{allocate-c-struct} with
@var{struct}.

Returns a member @var{name}'s value of @var{struct} from @var{pointer}.
}

@define[Function]{@name{c-struct-set!} @args{pointer struct name value}}
@desc{@var{struct} must be a C structure defined with @code{define-c-struct}.
@var{name} must be a symbol and @var{struct} has the same member.
@var{pointer} should be a pointer allocated by @code{allocate-c-struct} with
@var{struct}.

Sets @var{value} to @var{pointer} offset of member @var{name} of @var{struct}.
}

@subsubsection{Typedef operations}

@define[Macro]{@name{define-c-typedef} @args{original new-names @dots{}}}
@desc{Convenient macro.

Defines other name of @var{original} with @var{new-names}.

@var{new-names} must be following forms;
@codeblock{
()
((@code{*} @var{new-p}) @var{rest} @dots{})
((@code{s*} @var{new-sp}) @var{rest} @dots{})
(@var{new} @var{rest} @dots{})
}
The first for defines nothing.

If the rest of form is used and @var{rest} is not null, then it will recursively
define.

The second form's @code{*} defines @var{new-p} as @code{void*}.

The third form's @code{s*} defines @var{new-sp} as @code{char*}.

The forth form defines @var{new} as @var{original}.

Following example describes how to will be expanded approximately.
@codeblock{
(define-c-typedef char (* char_ptr) byte (s* string))

=> 

(begin
  (define char_ptr void*)
  (define byte char)
  (define string char*)
)
}
}

@subsubsection{Sizes and aligns}

@define[Constant]{@name{size-of-@var{type}}}
@define[Constant]{@name{align-of-@var{type}}}
@desc{a size or align of @var{type}, respectively.

Following types are supported;
@codeblock{
  bool char
  short int long long-long
  unsigned-short unsigned-int unsigned-long unsigned-long-long
  intptr_t uintptr_t size_t
  float double
  int8_t  int16_t  int32_t  int64_t
  uint8_t uint16_t uint32_t uint64_t
  void*
}
The values are platform dependent.
}

@subsubsection{Finalizer operations}

Some of C resource must be released but if you can not decide or do not want to
restrict when, then you can release it at GC time.

NOTE: GC might not happen if your script is very short, so it is better not to
relay these procedures too much.

@define[Function]{@name{register-ffi-finalizer} @args{pointer proc}}
@desc{@var{pointer} must be a pointer allocated with GCable memory.

@var{proc} must be a procedure and accepts one argument. The argument will be
the @var{pointer}.

Register @var{proc} as @var{pointer}'s finalizer and returns @var{pointer}.
}
@define[Function]{@name{unregister-ffi-finalizer} @args{pointer}}
@desc{@var{pointer} must be a pointer allocated with GCable memory.

Remove finalizer form @var{pointer} and returns @var{pointer}.
}