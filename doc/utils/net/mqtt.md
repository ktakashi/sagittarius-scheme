[§2] (net mq mqtt) - MQTT library {#net.mqtt}
-------------

###### [!Library] `(net mq mqtt)` 

Providing MQTT v3.1.1 and partially v3.1 client APIs.

Reference [OASIS MQTT](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/mqtt-v3.1.1.html).


Following examples describe how to receive and publish messages.

``````````scheme
(import (rnrs) (net mq mqtt))

(let ((conn (open-mqtt-connection "localhost" "1883")))
  ;; subscribes to "topic" topic with accepting QoS exactly once
  (mqtt-subscribe conn "topic" +qos-exactly-once+
                  (lambda (topic payload)
                    (let ((msg (get-bytevector-all payload)))
                      (cond ((not (eof-object? msg))
                             (print (utf8->string msg))
                             (string=? (utf8->string msg) "END"))
                            (else #f)))))
  (let loop ()
    ;; receives until "END" message was sent
    (unless (mqtt-receive-message conn)
      (loop)))
  ;; unsubscribe from "topic"
  (mqtt-unsubscribe conn "topic")
  (close-mqtt-connection! conn))
``````````

``````````scheme
(import (rnrs) (net mq mqtt))

(let ((conn (open-mqtt-connection "localhost" "1883")))
  ;; publish message to "topic" topic.
  (mqtt-publish conn "topic" (string->utf8 "Hello MQTT"))
  (close-mqtt-connection! conn))
``````````

###### [!Function] `mqtt-connection?`  _obj_

Returns #t if given _obj_ is MQTT connection. Otherwise #f.

###### [!Function] `open-mqtt-connection`  _host_ _port_ _opts_ _..._

Creates a socket connected to _host_:_port_ and
pass it to _port->mqtt-connection_ with _opts_.

The returning value is an MQTT connection object.


###### [!Function] `port->mqtt-connection`  _in/out_ _:key_ _client-id_ _username_ _password_ _keep-alive_ _version_

_in/out_ must be a binary input/outport port.

Creates an MQTT connection object using _in/out_. 

_client-id_, _username_, _password_ and _keep-alive_keyword arguments are for optional payload of CONNECT packet. If they are
given, then first 3 must be strings and _keep-alive_ must be an integer.

_version_ keyword argument is switches which version it should use.
The value must be one of the followings;

###### [!Constant] `+mqtt-3.1+` 
###### [!Constant] `+mqtt-3.1.1+` 
By default it uses `+mqtt-3.1.1+`.

This procedure is for future extension such as supporting websocket.


###### [!Function] `close-mqtt-connection!`  _conn_

Closes given MQTT connection.

###### [!Function] `mqtt-subscribe`  _conn_ _topic_ _qos_ _callback_

Subscribes to given _topic_ with QoS _qos_.

_callback_ must be a procedure and accept 2 arguments. _topic_ and
_payload_. _payload_ is an binary input port.

To receive messages, use `mqtt-receive-message`.


###### [!Function] `mqtt-receive-message`  _conn_

Receives one message from one of subscribed topics and call registered
callback.


###### [!Function] `mqtt-unsubscribe`  _conn_ _topic_

Unsubscribes _conn_ from _topic_.

###### [!Function] `mqtt-publish`  _conn_ _topic_ _message_ _:key_ _qos_

Publishes application message _message_ to _topic_.

The _topic_ must be a string. The _message_ must be a bytevector.

If keyword argument _qos_ is specified, it must be one of the followings.

###### [!Constant] `+qos-at-most-once+` 
###### [!Constant] `+qos-at-least-once+` 
###### [!Constant] `+qos-exactly-once+` 
By default, it uses `+qos-at-most-once+`.


###### [!Function] `mqtt-ping`  _conn_

Sends PINGREQ packet to the server.

### [§3] MQTT broker

###### [!Library] `(net mq mqtt broker)` 

This library provides simple MQTT broker implementation.

The broker only does what broker suppose to do, thus there is no user
customised behaviour.


The simplest broker script would look like this:

``````````scheme
(import (rnrs) (net mq mqtt broker))

;; Wait on port 9000
(define broker (make-mqtt-broker "9000")))

;; start the broker.
(mqtt-broker-start! broker)
``````````

###### [!Function] `make-mqtt-broker`  _:key_ _config_ _authentication-handler_ _:allow-other-keys_

Creates MQTT broker. 

The returning broker is a sub type of `<simple-server>`.

If _config_ keyword argument is specified, the value must be an
configuration object created by `make-mqtt-broker-config`, then the
specified configuration is used. Otherwise default configuration
which can be created by `(make-mqtt-broker-config)` is used.

If _authentication-handler_ keyword argument is specified, then
the specified value which must be a procedure takes 2 arguments, username
and password, handles authentication. If the procedure doesn't return
true value then authentication error packet is sent to the client.


###### [!Function] `make-mqtt-broker-config`  _:key_ _(max-thread_ _10)_ _:allow-other-keys_

Creates a MQTT broker configuration object.

The returning value is a sub type of `<server-config>` with
`:non-blocking?` option.


###### [!Function] `mqtt-broker-start!`  _broker_ _opt_ _..._
###### [!Function] `mqtt-broker-stop!`  _broker_ _opt_ _..._

Start and stop procedure for MQTT broker. 

These procedures are mere redefinitions of `server-start!` and 
`server-stop!`.


