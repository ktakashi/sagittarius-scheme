[§2] (sagittarius io) - Extra IO library {#sagittarius.io}
-------------

###### [!Library] `(sagittarius io)` 

This library provided extra IO related procedures.

###### [!Function] `with-input-from-port`  _port_ _thunk_
###### [!Function] `with-output-to-port`  _port_ _thunk_
###### [!Function] `with-error-to-port`  _port_ _thunk_

Calls _thunk_. During evaluation of _thunk_, the current input
port, current output port, current error port are set to _port_,
respectively.


###### [!Function] `call-with-input-string`  _str_ _proc_
###### [!Function] `call-with-output-string`  _proc_
###### [!Function] `with-input-from-string`  _str_ _thunk_
###### [!Function] `with-output-to-string`  _thunk_

These utility functions are trivially defined as follows;

``````````scheme
(define (call-with-input-string str proc)
  (proc (open-input-string str)))

(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))

(define (with-input-from-string str thunk)
  (with-input-from-port (open-input-string str) thunk))

(define (with-output-to-string thunk)
  (let ((port (open-output-string)))
    (with-output-to-port port thunk)
    (get-output-string port)))
``````````



###### [!Function] `buffered-port`  _port_ _buffer-mode_ _:key_ _buffer_
###### [!Function] `transcoded-port`  _port_ _transcoder_

Re-export of `buffered-port` and `transcoded-port`.

### [§3] Custom ports

Sagittarius provides means to create user defined ports. One of the ways
is using R6RS custom port procedures. The other one is extending custom
port class. The followings show how to extend it.

``````````scheme
;; example for input port
(import (rnrs) (sagittarius io) (clos user))

;; make a custom binary input port with 'read slot
(get-u8 (make <custom-binary-input-port>
          :read (lambda (bv start count)
                  (bytevector-u8-set! bv start 1)
                1)))
``````````

``````````scheme
;; example for output port
(import (rnrs) (sagittarius io) (clos user))

;; user defined custom binary output port
(define-class <my-port> (<custom-binary-output-port>) 
  ;; this port has own buffer 
  ((buffer :init-form (make-bytevector 5 0))))

;; create the port
(let ((out (make <my-port>)))
  ;; set 'write slot
  (slot-set! out 'write
    (lambda (bv start count)
       ;; just get the first element of given bytevector
       ;; and set it to own buffer
       (bytevector-copy! bv start (slot-ref out 'buffer) 0 count)
       count))
  ;; 
  (put-bytevector out #vu8(1 2 3 4 5))
  (slot-ref out 'buffer))
;; -> #vu8(1 0 0 0 0)
``````````

###### [!Class] `<custom-binary-input-port>` 
###### [!Class] `<custom-binary-output-port>` 
###### [!Class] `<custom-binary-input/output-port>` 
###### [!Class] `<custom-textual-input-port>` 
###### [!Class] `<custom-textual-output-port>` 
###### [!Class] `<custom-textual-input/output-port>` 

Custom port classes. All of these classes have the following slots:

###### [!Slot] `id` 

Identifier of the port. Must be string is specified.

###### [!Slot] `position` 
###### [!Slot] `set-position` 
###### [!Slot] `read` 
###### [!Slot] `write` 
###### [!Slot] `flush` 
###### [!Slot] `close` 

All of them must be either procedure or #f.

`position` procedure must accept 0 argument. The procedure should
return the position of the port.

`set-position` procedure must accept 2 argument, _position_and_whence_. _Whence_ shall be a symbol of `begin`, 
`current` or `end`. The procedure should set the position
of the port according to the given _whence_ and _position_.

`read` procedure must accept 3 argument. _bv_ or _string_,
_start_ and _count_. The first argument is decided by the port
type. If the port is binary port, then bytevector _bv_ is passed.
If the port is textual port, then string _string_ is passed.
The procedure should fill given _bv_ or _string_ in _count_data elements starting _start_. And return number of data filled.

`write` procedure must accept 3 argument. _bv_ or _string_,
_start_ and _count_. The first argument is decided by the port
type. If the port is binary port, then bytevector _bv_ is passed.
If the port is textual port, then string _string_ is passed.
The procedure should retrieve data from  given _bv_ or _string_upto _count_ data elements starting _start_. And return number
of data read.

`ready` procedure must accept 0 argument. The procedure should
return true value if the port is ready to read. Otherwise #f.

`flush` procedure must accept 0 argument. The procedure should
flush the port.

`close` procedure must accept 0 argument. The procedure should
close the port.

If the creating port is input port, then `read` must be set before
any port operation. If the creating port is output port, then `write`must be set before any port operation. Other slots are optional.




