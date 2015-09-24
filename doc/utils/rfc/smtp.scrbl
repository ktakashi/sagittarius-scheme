@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.smtp"]{(rfc smtp) - SMTP client}

@define[Library]{@name{(rfc smtp)}}
@desc{This library provides simple SMTP client defined in 
@hyperlink[:href "https://www.ietf.org/rfc/rfc5321.txt"]{RFC 5321}.

The library provides high level APIs to use SMTP.
}

Following is the simple example to send an email.

@codeblock{
(import (rnrs) (rfc client))

;; creates SMTP mail object
(define mail 
  (smtp:mail
   (smtp:from "Takashi Kato" "ktakashi@atmark{}ymail.com")
   (smtp:subject "Subject")
   "Message"
   (smtp:cc "ktakashi@atmark{}ymail.com")))

;; creates an SMTP connection
(define smtp-conn (make-smtp-connection "your.smtp.server.com" "587"))

;; connect to the server
(smtp-connect! smtp-conn) ;; returns SMTP connection

;; Authenticate if required.
(when (smtp-authentication-required? smtp-conn)
  ;; NOTE: only AUTH PLAIN is supported for now
  (cond ((memq 'PLAIN (smtp-connection-authentication-methods smtp-conn))
         (smtp-authenticate! smtp-conn (smtp-plain-authentication
                                        "username"
                                        "password")))
        (else (smtp-disconnect! smtp-conn)
              (error #f "authentication method not supported" 
                     (smtp-connection-authentication-methods smtp-conn)))))

;; Send it
(smtp-send! smtp-conn mail) ;; returns SMTP connection

;; Clean up
(smtp-disconnect! smtp-conn)
}

The followings describe how to create SMTP mail object with attachment(s)
or inlined image.

@codeblock{
;; Messae with attachment
(smtp:mail
 (smtp:from "Takashi Kato" "ktakashi@atmark{}ymail.com")
 (smtp:subject "Message with attachment")
 "Message"
 (smtp:to "ktakashi@atmark{}ymail.com")
 (smtp:attachment "application" "octet-stream" 
                  (get-file-content) ;; file content suitable for this
                  "file.odt"))

;; HTML message with inlined image.
(smtp:mail
 (smtp:from "Takashi Kato" "ktakashi@atmark{}ymail.com")
 (smtp:subject "HTML message with inlined image")
 "<html><body><img src='cid:image' /></body></html>"
 (smtp:to "ktakashi@atmark{}ymail.com")
 (smtp:attachment "image" "jpeg" 
                  ;; port can also be used
                  ;; NB: if you call smtp-mail->string twice in this case
                  ;;     then this may break. So make sure you use this
                  ;;     mail object once (includeing smtp-send!)
                  (open-file-input-port "image/file.jpg")
                  "image.jpg" 
                  ;; specifies content-disposition parameter
                  "inline"
                  ;; adding content-id of this attachment
                  '("content-id" "<image>"))
 ;; make this mail HTML
 (smtp:header "Content-Type" "text/html"))
}

Alternative message can be created like this:
@codeblock{
;; Message with alternative
;; Content must not be specified otherwise raise an syntax error
(smtp:mail
 (smtp:from "Takashi Kato" "ktakashi@atmark{}ymail.com")
 (smtp:subject "Message with alternative")
 (smtp:to "ktakashi@atmark{}ymail.com")
 (smtp:alternative
  ("text" "plain" "Plain text message")
  ("text" "html" "<html><body>HTML message</body><html>"))
 )
}


@subsubsection{High level APIs}

@define[Function]{@name{smtp-connection?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an SMTP connection object, 
Otherwise #f.}

@define[Function]{@name{make-smtp-connection} 
 @args{server port :optiona domain}}
@desc{Creates an SMTP connection object.

If optional argument @var{domain} is specified, then the value is used
for @code{EHLO} or @code{HELO} command parameter. Otherwise the result of
@code{machine-name} (defined in SRFI 112) is used.
}

@define[Function]{@name{smtp-connection-authentication-methods} 
 @args{smtp-connection}}
@desc{Returns a symbol list of possible authentication methods.

This procedure returns a proper value only after @code{smtp-connect!} 
is called.
}

@define[Function]{@name{smtp-authentication-required?} 
 @args{smtp-connection}}
@desc{Returns #t if the given SMTP connection requires authentication,
otherwise #f.

This procedure returns a proper value only after @code{smtp-connect!} 
is called.
}

@define[Function]{@name{smtp-connect!} @args{smtp-connection}}
@desc{Establishes connection and return SMTP connection object.}

@define[Function]{@name{smtp-authenticate!}
 @args{smtp-connection initial-response-generator :optional next-step}}
@desc{@var{initial-response-generator} must be a procedure accepts 0 
argument and must return 2 values, authentication method and initial
response value (credential).

Authenticates the given @var{smtp-connection}. If the response status
is @code{354} and @var{next-step} is specified, then the @var{next-step}
is called with @var{smtp-connection}, status and response string. The
procedure must return 2 values, next command and next-next procedure or #f.
The @code{smtp-authenticate!} procedure stops when @var{next-next} 
procedure is #f or returning response is @code{235}.

The following is a simple LOGIN @var{next-step}.

@codeblock{
(define (smtp-login-authentication)
  (define (prompt next?)
    (lambda (conn status resp)
      (or (and (= status 334)
               ;; response is encoded to BASE 64 so decode it
               (display (base64-decode-string resp))
               (flush-output-port (current-output-port))
               ;; next command (either username or password)
               ;; must be encoded to BASE 64
               (values (base64-encode-string (get-line (current-input-port)))
                       (and next? (prompt #f))))
          (values #f #f))))
  (values (lambda () (values "LOGIN" #f)) (prompt #t)))
}
}

@define[Function]{@name{smtp-send!} @args{smtp-connection smtp-mail}}
@desc{Sends the given @var{smtp-mail} and returns SMTP connection object.

The procedure internally calls @code{smtp-mail->string}. So if the
@var{smtp-mail} object contains an attachment with input port, then
the port position will be forwarded. Thus second call of the 
@code{smtp-mail->string} or @code{smtp-send!} doesn't create/send
attachment message properly.
}

@define[Function]{@name{smtp-connect!} @args{smtp-connection}}
@desc{Disconnect the @var{smtp-connection} and returns SMTP connection.

The closed SMTP connection can be re-used.
}

@define[Function]{@name{smtp-plain-authentication} @args{username password}}
@desc{Returns a thunk that can be used @code{smtp-authenticate!} procedure
for PLAIN authentication.
}

@define[Macro]{@name{smtp:mail} @args{from message-elements ...}}
@define["Auxiliary Syntax"]{@name{smtp:from}}
@define["Auxiliary Syntax"]{@name{smtp:subject}}
@define["Auxiliary Syntax"]{@name{smtp:to}}
@define["Auxiliary Syntax"]{@name{smtp:cc}}
@define["Auxiliary Syntax"]{@name{smtp:bcc}}
@define["Auxiliary Syntax"]{@name{smtp:attachment}}
@define["Auxiliary Syntax"]{@name{smtp:header}}
@desc{Constructs an SMTP mail object.

@var{from} must be one of the following forms:
@snipet{(smtp:from email)}
@snipet{(smtp:from name email)}

@var{message-elements} must be one of the following forms:
@snipet{(smtp:subject subject)}
@snipet{(smtp:to email)}
@snipet{(smtp:to name email)}
@snipet{(smtp:cc email)}
@snipet{(smtp:cc name email)}
@snipet{(smtp:bcc email)}
@snipet{(smtp:bcc name email)}
@snipet{(smtp:attachment type subtype content)}
@snipet{(smtp:attachment type subtype content filename)}
@snipet{(smtp:attachment type subtype content filename disposition-parameter)}
@snipet{(smtp:attachment type subtype content filename disposition-parameter headers ...)}
@snipet{(smtp:alternative type subtype content)}
@snipet{(smtp:alternative type subtype content headers ...)}
@snipet{(smtp:header name value)}
@snipet{string}
The order of the appearance does not matter, except @var{string} which will be
the content of the creating SMTP mail. Except the @code{smtp:subject} and
@code{smtp:alternative}, all elements can be appear multiple times.
}

@define[Function]{@name{smtp-mail->string} @args{smtp-mail}}
@desc{Returns MIME string of given @var{smtp-mail}.

NOTE: the procedure consumes input port attachments if exists.
}

@subsubsection{Middle level APIs}

@define[Function]{@name{make-smtp-mail}
 @args{from subject content :rest recipients}}
@desc{Creates an SMTP mail object.}
@define[Function]{@name{smtp-mail?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an SMTP mail object, otherwise #f.}

@define[Function]{@name{make-smtp-from} @args{email}}
@define[Function]{@name{make-smtp-from} @args{name email}}
@desc{creates an SMTP from object.}
@define[Function]{@name{smtp-from?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an SMTP from object, otherwise #f.}

@define[Function]{@name{smtp-recipient?} @args{obj}}
@desc{Returns #t if the given @var{obj} is an SMTP recipient object, 
otherwise #f.

SMTP recipient objects are one of the following objects:
@itemlist{
@item{SMTP to object}
@item{SMTP cc object}
@item{SMTP bcc object}
}
}

@define[Function]{@name{make-smtp-to} @args{email}}
@define[Function]{@name{make-smtp-to} @args{name email}}
@define[Function]{@name{make-smtp-cc} @args{email}}
@define[Function]{@name{make-smtp-cc} @args{name email}}
@define[Function]{@name{make-smtp-bcc} @args{email}}
@define[Function]{@name{make-smtp-bcc} @args{name email}}
@desc{Creates SMTP to, SMTP cc and SMTP bcc objects respectively.}

@define[Function]{@name{smtp-to?} @args{obj}}
@define[Function]{@name{smtp-cc?} @args{obj}}
@define[Function]{@name{smtp-bcc?} @args{obj}}
@desc{Returns #t if given @var{obj} is SMTP to, SMTP cc or SMTP bcc,
respectively. Otherwise #f.
}

@define[Function]{@name{make-smtp-attachment}
 @args{type subtype content :optional filename disposition-parameter headers ...}}
@desc{Creates a MIME object which represents SMTP attachment.

Together @var{type} and @var{subtype} represent MIME type. e.g.
@code{text/plain}

@var{content} can be string, bytevector, input-port.
@; TODO describe a list of mimes

Optional argument @var{filename} is specified, then @code{content-disposition}
header will have @code{filename} parameter.

Optional argument @var{disposition-parameter} is specified, then
@code{content-disposition} header will have specified parameter. Default
values is @code{attachment}.

Rest argument @var{headers} is specified, then the created attachment has
extra mime header. Each @var{header} must be a list which contains 2
elements, header name and value. The value part can also contain 
MIME parameters.
}

@define[Function]{@name{make-smtp-alternative-component}
 @args{type subtype content headers ...}}
@desc{Creates a component of alternative message. The returning component
is a mere MIME object.

@var{type} and @var{subtype} are the same as @code{make-smtp-attachment}.
}

@define[Function]{@name{make-smtp-alternative} @args{alternatives ...}}
@desc{Creates an alternative message. The returning message
is a mere MIME object.

NOTE: the returning message should be added by @code{smtp-mail-add-attachment!}
and not @var{content} argument of @code{make-smtp-mail}. To make mail
construction easier, use @code{smtp:mail} macro.
}

@define[Function]{@name{smtp-mail-add-recipent!} @args{smtp-mail recipient}}
@desc{Adds SMTP recipient @var{recipient} to @var{smtp-mail} and returns
SMTP mail object.
}

@define[Function]{@name{smtp-mail-add-attachment!} @args{smtp-mail attachment}}
@desc{Adds SMTP attachment @var{attachment} to @var{smtp-mail} and returns
SMTP mail object.
}

@define[Function]{@name{smtp-mail-add-header!} @args{smtp-mail name value}}
@desc{Adds header to @var{smtp-mail} and returns SMTP mail object.

If there is the same header insense of @code{string-ci=?}, then the old
one is overwritten.
}

