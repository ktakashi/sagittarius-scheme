@; -*- mode:scribble; coding:utf-8; -*-

@subsection[:tag "net.mqtt"]{(net mq mqtt) - MQTT library}

@define[Library]{@name{(net mq mqtt)}}
@desc{Providing MQTT v3.1.1 and partially v3.1 client APIs.

Reference @hyperlink[:href "http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/mqtt-v3.1.1.html"]{OASIS MQTT}.
}

Following examples describe how to receive and publish messages.

@codeblock{
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
}

@codeblock{
(import (rnrs) (net mq mqtt))

(let ((conn (open-mqtt-connection "localhost" "1883")))
  ;; publish message to "topic" topic.
  (mqtt-publish conn "topic" (string->utf8 "Hello MQTT"))
  (close-mqtt-connection! conn))
}


@define[Function]{@name{mqtt-connection?} @args{obj}}
@desc{Returns #t if given @var{obj} is MQTT connection. Otherwise #f.}

@define[Function]{@name{open-mqtt-connection} @args{host port opts @dots{}}}
@desc{Creates a socket connected to @var{host}:@var{port} and
pass it to @var{port->mqtt-connection} with @var{opts}.

The returning value is an MQTT connection object.
}

@define[Function]{@name{port->mqtt-connection}
 @args{in/out :key client-id username password keep-alive version}}
@desc{@var{in/out} must be a binary input/outport port.

Creates an MQTT connection object using @var{in/out}. 

@var{client-id}, @var{username}, @var{password} and @var{keep-alive}
keyword arguments are for optional payload of CONNECT packet. If they are
given, then first 3 must be strings and @var{keep-alive} must be an integer.

@var{version} keyword argument is switches which version it should use.
The value must be one of the followings;

@define[Constant]{+mqtt-3.1+}
@define[Constant]{+mqtt-3.1.1+}

By default it uses @code{+mqtt-3.1.1+}.

This procedure is for future extension such as supporting websocket.
}

@define[Function]{@name{close-mqtt-connection!} @args{conn}}
@desc{Closes given MQTT connection.}


@define[Function]{@name{mqtt-subscribe} @args{conn topic qos callback}}
@desc{Subscribes to given @var{topic} with QoS @var{qos}.

@var{callback} must be a procedure and accept 2 arguments. @var{topic} and
@var{payload}. @var{payload} is an binary input port.

To receive messages, use @code{mqtt-receive-message}.
}

@define[Function]{@name{mqtt-receive-message} @args{conn}}
@desc{Receives one message from one of subscribed topics and call registered
callback.
}

@define[Function]{@name{mqtt-unsubscribe} @args{conn topic}}
@desc{Unsubscribes @var{conn} from @var{topic}.}

@define[Function]{@name{mqtt-publish}
 @args{conn topic message :key qos}}
@desc{Publishes application message @var{message} to @var{topic}.

The @var{topic} must be a string. The @var{message} must be a bytevector.

If keyword argument @var{qos} is specified, it must be one of the followings.

@define[Constant]{+qos-at-most-once+}
@define[Constant]{+qos-at-least-once+}
@define[Constant]{+qos-exactly-once+}

By default, it uses @code{+qos-at-most-once+}.
}

@define[Function]{@name{mqtt-ping} @args{conn}}
@desc{Sends PINGREQ packet to the server.}


@subsubsection{MQTT broker}

@define[Library]{@name{(net mq mqtt broker)}}
@desc{This library provides simple MQTT broker implementation.

The broker only does what broker suppose to do, thus there is no user
customised behaviour.
}

The simplest broker script would look like this:
@codeblock{
(import (rnrs) (net mq mqtt broker))

;; Wait on port 9000
(define broker (make-mqtt-broker "9000")))

;; start the broker.
(mqtt-broker-start! broker)
}

@define[Function]{@name{make-mqtt-broker}
 @args{:key config authentication-handler :allow-other-keys}}
@desc{Creates MQTT broker. 

The returning broker is a sub type of @code{<simple-server>}.

If @var{config} keyword argument is specified, the value must be an
configuration object created by @code{make-mqtt-broker-config}, then the
specified configuration is used. Otherwise default configuration
which can be created by @code{(make-mqtt-broker-config)} is used.

If @var{authentication-handler} keyword argument is specified, then
the specified value which must be a procedure takes 2 arguments, username
and password, handles authentication. If the procedure doesn't return
true value then authentication error packet is sent to the client.
}

@define[Function]{@name{make-mqtt-broker-config}
 @args{:key (max-thread 10) :allow-other-keys}}
@desc{Creates a MQTT broker configuration object.

The returning value is a sub type of @code{<server-config>} with
@code{:non-blocking?} option.
}

@define[Function]{@name{mqtt-broker-start!} @args{broker opt @dots{}}}
@define[Function]{@name{mqtt-broker-stop!} @args{broker opt @dots{}}}
@desc{Start and stop procedure for MQTT broker. 

These procedures are mere redefinitions of @code{server-start!} and 
@code{server-stop!}.
}
