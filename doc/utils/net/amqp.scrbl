@; -*- coding: utf-8 -*-

@subsection[:tag "net.amqp"]{(net mq amqp) - AMQP library}
@desc{Providing APIs for @hyperlink[:href "http://www.amqp.org"]{AMQP},
Advanced Message Queuing Protocol.

Currently, the library lacks security layer such as TLS and SAML, and
transaction support.
}

@subsubsection{Example}

Following examples describes how to send and receive messages from
remote queues.

@codeblock{
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
}

@subsubsection{High level APIs}

@define[Function]{@name{open-amqp-connection} @args{host port}}
@desc{Creates an AMQP connection object.}

@define[Function]{@name{close-amqp-connection!} @args{connection}}
@desc{Closes given AMQP connection.}

@define[Function]{@name{call-with-amqp-connection} @args{host port proc}}
@desc{Creates an AMQP connection and pass it to @var{proc}. Then returns
the result of @var{proc}.

The created connection will be closed both @var{proc} returns or raises
an error. Thus Invoking captured continuation inside of @var{proc} would not
work.
}

@define[Function]{@name{begin-amqp-session!} @args{connection}}
@desc{Starts AMQP session on the given @var{connection}}

@define[Function]{@name{end-amqp-session!} @args{session}}
@desc{Ends given AMQP session.}

@define[Function]{@name{call-with-amqp-connection} @args{connection proc}}
@desc{Starts an AMQP session and pass it to @var{proc}. Then returns
the result of @var{proc}.

The stated session will be ended both @var{proc} returns or raises
an error. Thus Invoking captured continuation inside of @var{proc} would not
work.
}

@define[Function]{@name{create-amqp-sender} @args{session source-queue}}
@define[Function]{@name{create-amqp-receiver} @args{session target-queue}}
@desc{Creates an AMQP sender or receiver, respectively.

@var{source-queue} and @var{target-queue} must be strings and indicating
queue names on the remote server.
}

@define[Function]{@name{destroy-amqp-sender} @args{sender}}
@define[Function]{@name{destroy-amqp-receiver} @args{receiver}}
@desc{Destory given @var{sender} and @var{receiver}, respectively.}

@define[Function]{@name{send-amqp-message} @args{sender message}}
@desc{Sends @var{message} to @var{sender}'s queue.

@var{message} must be am AMQP message object.
}

@define[Function]{@name{receive-amqp-message} @args{receiver :key (timeout #f)}}
@desc{Receives an AMQP message from @var{receiver}'s queue.

Keyword argument @var{timeout} must be #f or integer. If this is specified
then the procedure waits only specified milliseconds.
}

@define[Function]{@name{create-amqp-text-message text}}
@define[Function]{@name{create-amqp-binary-message data}}
@define[Function]{@name{create-amqp-mime-message content-type data}}
@desc{Creates an AMQP text message, binary message and data message,
respectively.

@var{text} must be a string. @var{data} must be a bytevector. 
@var{content-type} must be a string.
}

@; TBD accessors

@; TBD low level APIs
@; (net mq amqp types)
@; (net mq amqp transport)
@; (net mq amqp messaging)
@; I think AMQP can be extensible that's why it has 4 layer of specifications.
@; Basically, types and transport are *not* MQ dependent protocol thus
@; it is possible to construct a protocol other than MQ on this transport
@; layer. To make this happen we need to describe this low level APIs
@; especially send-transfer and disposition thing.
@; but for now there is no such thing needed
