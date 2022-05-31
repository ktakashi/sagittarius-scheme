[§2] Sagittarius MOP {#sagittarius.mop}
-------------

MOP is "meta object protocol". As far as I know, there is no standard
specification even the name is really famous and most of CLOS is implemented
on MOP.

Then we decided to take the APIs and its behaviour from Tiny CLOS. The following
libraries are implemented with the APIs and can be examples for Sagittarius'
MOP.

### [§3] (sagittarius mop allocation) {#sagittarius.mop.allocation}

###### [!Library] `(sagittarius mop allocation)` 

Supporting `:allocation` option for `define-class`.

###### [!Class] `<allocation-meta>` 
###### [!Class] `<allocation-mixin>` 

Meta class and mixin class to support `:allocation` option for
class slot definition, respectively.

The meta class must be used with `:metaclass` option of
`define-class`.

The mixin class must be a parent class.

Currently, we only support `:instance` and `:class` keywords.

The following code is the whole definition of this classes.

``````````scheme
(define-class <allocation-meta> (<class>) ())
(define-method compute-getter-and-setter ((class <allocation-meta>) slot)
  (cond ((slot-definition-option slot :allocation :instance)
         => (lambda (type)
              (case type
                ((:instance) '())
                ((:class)
                 (let* ((init-value (slot-definition-option
                                     slot :init-value #f))
                        (init-thunk (slot-definition-option 
                                     slot :init-thunk #f))
                        (def (if init-thunk (init-thunk) init-value)))
                   (list
                    (lambda (o) def)
                    (lambda (o v) (set! def v)))))
                (else
                 (assertion-violation '<allocation-meta>
                                      "unknown :allocation type"
                                      type)))))
        (else (call-next-method))))

(define-class <allocation-mixin> () () :metaclass <allocation-meta>)
``````````



### [§3] (sagittarius mop validator)

###### [!Library] `(sagittarius mop validator)` 

Supporting `:validator` and `observer` options for
`define-class`.

###### [!Class] `<validator-meta>` 
###### [!Class] `<validator-mixin>` 

Make user be able to add own validation mechanism to slots.

`:validator` is for before set the value to the slot so that user can check
the value if it's correct or not.

`:observer` is for after set the value to the slot so that user can check
which value is set to the slot.


### [§3] (sagittarius mop eql)

The eql specializer is now builtin so this library is only for backward
compatibility.

###### [!Library] `(sagittarius mop eql)` 

Supporting eql specializer methods.

The following code describes how to use;

``````````scheme
(import (clos user) (sagittarius mop eql))
(define-generic eql-fact :class <eql-specializable-generic>)
(define-method eql-fact ((n (eql 0))) 1)
(define-method eql-fact ((n <integer>)) (* n (eql-fact (- n 1))))
(eql-fact 10)
``````````
=> ``3628800``

Note: The eql specializer is really slow approximately 200 time slower than
usual procedure call.


###### [!Class] `<eql-specializable-generic>` 

Subclass of `<generic>`.

To use eql specializer, generic functions must have this class as a metaclass.


