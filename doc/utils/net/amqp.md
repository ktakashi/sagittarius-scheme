[§2] (net mq amqp) - AMQP library {#net.amqp}
-------------

###### [!Library] `(net mq amqp)` 

Providing APIs for [AMQP](http://www.amqp.org),
Advanced Message Queuing Protocol.

Currently, the library lacks security layer such as TLS and SAML, and
transaction support.


### [§3] Example

Following examples describes how to send and receive messages from
remote queues.

``````````scheme
(import (rnrs) (net mq amqp api))

;; Sends text message to SAMPLE.QUEUE on localhost:5672
(call-with-amqp-connection "localhost" "5672"
  (lambda (conn)
    (call-with-amqp-session conn
      (lambda (session)
        (let ((sender (create-amqp-sender session "SAMPLE.QUEUE"))
              (message (create-amqp-text-message "Hello AMQP!!")))
          (send-amqp-message sender message)
          (destroy-amqp-sender sender))))))

;; Receives text message from SAMPLE.QUEUE on localhost:5672
(call-with-amqp-connection "localhost" "5672"
  (lambda (conn)
    (call-with-amqp-session conn
      (lambda (session)
        (let* ((receiver (create-amqp-receiver session "SAMPLE.QUEUE"))
               (message (receive-amqp-message receiver)
          (destroy-amqp-sender receiver)
          message)))))))
``````````

### [§3] High level APIs

###### [!Function] `amqp-connection?`  _obj_

Returns #t if given _obj_ is AMQP connection. Otherwise #f.

###### [!Function] `open-amqp-connection`  _host_ _port_

Creates an AMQP connection object.

###### [!Function] `close-amqp-connection!`  _connection_

Closes given AMQP connection.

###### [!Function] `call-with-amqp-connection`  _host_ _port_ _proc_

Creates an AMQP connection and pass it to _proc_. Then returns
the result of _proc_.

The created connection will be closed both _proc_ returns or raises
an error. Thus Invoking captured continuation inside of _proc_ would not
work.


###### [!Function] `amqp-session?`  _obj_

Returns #t if given _obj_ is AMQP session. Otherwise #f.

###### [!Function] `begin-amqp-session!`  _connection_

Starts AMQP session on the given _connection_

###### [!Function] `end-amqp-session!`  _session_

Ends given AMQP session.

###### [!Function] `call-with-amqp-connection`  _connection_ _proc_

Starts an AMQP session and pass it to _proc_. Then returns
the result of _proc_.

The stated session will be ended both _proc_ returns or raises
an error. Thus Invoking captured continuation inside of _proc_ would not
work.


###### [!Function] `amqp-sender?`  _obj_
###### [!Function] `amqp-receiver?`  _obj_

Returns #t if given _obj_ is AMQP sender and receiver, respectively.
Otherwise #f.

###### [!Function] `create-amqp-sender`  _session_ _source-queue_
###### [!Function] `create-amqp-receiver`  _session_ _target-queue_

Creates an AMQP sender or receiver, respectively.

_source-queue_ and _target-queue_ must be strings and indicating
queue names on the remote server.


###### [!Function] `destroy-amqp-sender`  _sender_
###### [!Function] `destroy-amqp-receiver`  _receiver_

Destory given _sender_ and _receiver_, respectively.

###### [!Function] `send-amqp-message`  _sender_ _message_

Sends _message_ to _sender_'s queue.

_message_ must be am AMQP message object.


###### [!Function] `receive-amqp-message`  _receiver_ _:key_ _(timeout_ _#f)_

Receives an AMQP message from _receiver_'s queue.

Keyword argument _timeout_ must be #f or integer. If this is specified
then the procedure waits only specified milliseconds.


###### [!Function] `amqp-message?`  _obj_

Returns #t if given _obj_ is AMQP message. Otherwise #f.

###### [!Function] `create-amqp-text-message text` 
###### [!Function] `create-amqp-binary-message data` 
###### [!Function] `create-amqp-mime-message content-type data` 

Creates an AMQP text message, binary message and data message,
respectively.

_text_ must be a string. _data_ must be a bytevector. 
_content-type_ must be a string.


