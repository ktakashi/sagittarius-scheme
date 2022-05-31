[§2] (binary data) - Binary data read/write {#util.binary.data}
-------------

###### [!Library] `(binary data)` 

This library provides yet another binary data structure read/write. The
difference between [(binary pack)](#util.binary.pack) and this is the
level of abstraction. This library provides higher abstraction layer of the 
way how to handle binary data.


###### [!Macro] `define-simple-datum-define`  _name_ _reader_ _writer_
###### [!Macro] `define-composite-data-define`  _name_ _reader_ _writer_

Defines a macro named _name_ to read binary data by generic method
_reader_ and write binary data by _writer_.

To use defining macros effectively, both forms' _reader_ and _writer_should be the same name.

The first form defines a macro which takes 5 required arguments and optional
keyword arguments. Let's say the _name_ is `define-simple`, the
_reader_ is `simple-read` and the _write_ is `simple-write`,
Then the definition of defined macro would be like this;
###### [!Macro] `define-simple`  _name_ _parents_ _slots_ _read-proc_ _write-proc_ _:key_ _(parent-metaclass_ _<class>)_

Defines _name_ class whose parent classes are _parents_ and slots
are _slots_.

_read-proc_ must be a procedure which takes one argument, a input port and
return number of slots values.

_write-proc_ must be a procedure which takes number of slots plus 1
arguments. The first one is an output port and the rest are the value of the
slots of the defined class' instance.

The keyword argument _parent-metaclass_ is a parent class of the metaclass
of this class.

The _slots_ form must be a list of slot definitions which must be 
followings;

- _name_
- _(name)_
- _(name default)_

The first and second form define a slot which name is _name_ and its
initial value is #f. The third form defines a slot which name is _name_and its initial is _default_.

Note that current implemenation does not handle parent classes slots. This is
only for seamless operations with other CLOS class.


The second form defines a macro which takes 3 required arguments and optional
keyword arguments. Let's say the _name_ is `define-composite`, the
_reader_ is `simple-read` and the _write_ is `simple-write`,
Then the definition of defined macro would be like this;
###### [!Macro] `define-simple`  _name_ _parents_ _slots_ _:key_ _(parent-metaclass_ _<class>)_

Defines a composite data class named _name_ whose parent classes are
_parents_ and slots are _slots_.

It is similar form with `define-class` however _slots_ must be a list
of one of the followings.

- _(name type)_
- _(name type default)_
- _(name (type count))_
- _(name (type count) default)_

_name_ must be a symbol which represents the slot name.

_type_ can be class name or `eqv?` comparable datum. e.g. keyword.

_default_ can be any object.

_count_ must be a non negative exact integer.

The first form is equivalent with the following form;
`(name type #f)`.
And the third form is equivalent with the following form;
`(name (type count) #f)`.

The first 2 forms defines a datum slot which the datum is read by _reader_passing _type_ and written by _writer_.

The rest forms defines an array data represented by a vector.

If the _type_ is not defined by neither of the definition forms, then
it is users responsibility to define a method which handles the _type_.




Following is the simple example to show how to use the macros above.

``````````scheme
(import (clos user) (binary data))

;; use the same name of reader and writer
(define-simple-datum-define   define-simple    sample-read sample-write)
(define-composite-data-define define-composite sample-read sample-write)

(define-simple <simple> ()
  (a b (c 0))
  (lambda (in) (values (get-u8 in) (get-u8 in) (get-u8 in)))
  (lambda (out a b c) (put-u8 out a) (put-u8 out b) (put-u8 out c)))

(define-composite <composite> ()
  ((d :byte 1)
   (e (:byte 4) #vu8(1 2 3 4))
   (f <simple>)))

;; :byte reader and writer
(define-method sample-read ((o (eql :byte)) in array-size?)
  (if array-size?
      (get-bytevector-n in array-size?)
      (get-u8 in)))

(define-method sample-write ((type (eql :byte)) o out array-size?)
  (if array-size?
     (put-bytevector out o)
     (put-u8 out o)))
``````````

How to use the defined data structure.

``````````scheme
;; read as a <composite> object
;; "deeeeabc" in ascii
(define bv #vu8(#x64 #x65 #x65 #x65 #x65 #x61 #x62 #x63))
(call-with-port (open-bytevector-input-port bv)
  (lambda (in)
    (let ((s (sample-read <composite> in)))
      (slot-ref s 'd) ;; => #\d
      (slot-ref s 'f) ;; => <simple>
      )))

;; write <composite> object
(call-with-bytevector-output-port
  (lambda (out)
    (let* ((s (make <simple> :a 10 :b 20))
           (c (make <composite> :f s)))
      ;; this can be written like this as well (sample-write o out)
      (sample-write <composite> c out))))
;; => #vu8(1 1 2 3 4 10 20 0)
``````````

