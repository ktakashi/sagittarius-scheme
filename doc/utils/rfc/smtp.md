[§2] (rfc smtp) - SMTP client {#rfc.smtp}
-------------

###### [!Library] `(rfc smtp)` 

This library provides simple SMTP client defined in 
[RFC 5321](https://www.ietf.org/rfc/rfc5321.txt).

The library provides high level APIs to use SMTP.


Following is the simple example to send an email.

``````````scheme
(import (rnrs) (rfc client))

;; creates SMTP mail object
(define mail 
  (smtp:mail
   (smtp:from "Takashi Kato" "ktakashi@ymail.com")
   (smtp:subject "Subject")
   "Message"
   (smtp:cc "ktakashi@ymail.com")))

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
``````````

The followings describe how to create SMTP mail object with attachment(s)
or inlined image.

``````````scheme
;; Messae with attachment
(smtp:mail
 (smtp:from "Takashi Kato" "ktakashi@ymail.com")
 (smtp:subject "Message with attachment")
 "Message"
 (smtp:to "ktakashi@ymail.com")
 (smtp:attachment "application" "octet-stream" 
                  (get-file-content) ;; file content suitable for this
                  "file.odt"))

;; HTML message with inlined image.
(smtp:mail
 (smtp:from "Takashi Kato" "ktakashi@ymail.com")
 (smtp:subject "HTML message with inlined image")
 "<html><body><img src='cid:image' /></body></html>"
 (smtp:to "ktakashi@ymail.com")
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
``````````

Alternative message can be created like this:

``````````scheme
;; Message with alternative
;; Content must not be specified otherwise raise an syntax error
(smtp:mail
 (smtp:from "Takashi Kato" "ktakashi@ymail.com")
 (smtp:subject "Message with alternative")
 (smtp:to "ktakashi@ymail.com")
 (smtp:alternative
  ("text" "plain" "Plain text message")
  ("text" "html" "<html><body>HTML message</body><html>"))
 )
``````````

### [§3] High level APIs

###### [!Function] `smtp-connection?`  _obj_

Returns #t if the given _obj_ is an SMTP connection object, 
Otherwise #f.

###### [!Function] `make-smtp-connection`  _server_ _port_ _:optiona_ _domain_

Creates an SMTP connection object.

If optional argument _domain_ is specified, then the value is used
for `EHLO` or `HELO` command parameter. Otherwise the result of
`machine-name` (defined in SRFI 112) is used.


###### [!Function] `smtp-connection-authentication-methods`  _smtp-connection_

Returns a symbol list of possible authentication methods.

This procedure returns a proper value only after `smtp-connect!` 
is called.


###### [!Function] `smtp-authentication-required?`  _smtp-connection_

Returns #t if the given SMTP connection requires authentication,
otherwise #f.

This procedure returns a proper value only after `smtp-connect!` 
is called.


###### [!Function] `smtp-connect!`  _smtp-connection_

Establishes connection and return SMTP connection object.

###### [!Function] `smtp-authenticate!`  _smtp-connection_ _initial-response-generator_ _:optional_ _next-step_

_initial-response-generator_ must be a procedure accepts 0 
argument and must return 2 values, authentication method and initial
response value (credential).

Authenticates the given _smtp-connection_. If the response status
is `354` and _next-step_ is specified, then the _next-step_is called with _smtp-connection_, status and response string. The
procedure must return 2 values, next command and next-next procedure or #f.
The `smtp-authenticate!` procedure stops when _next-next_ 
procedure is #f or returning response is `235`.

The following is a simple LOGIN _next-step_.

``````````scheme
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
``````````



###### [!Function] `smtp-send!`  _smtp-connection_ _smtp-mail_

Sends the given _smtp-mail_ and returns SMTP connection object.

The procedure internally calls `smtp-mail->string`. So if the
_smtp-mail_ object contains an attachment with input port, then
the port position will be forwarded. Thus second call of the 
`smtp-mail->string` or `smtp-send!` doesn't create/send
attachment message properly.


###### [!Function] `smtp-connect!`  _smtp-connection_

Disconnect the _smtp-connection_ and returns SMTP connection.

The closed SMTP connection can be re-used.


###### [!Function] `smtp-plain-authentication`  _username_ _password_

Returns a thunk that can be used `smtp-authenticate!` procedure
for PLAIN authentication.


###### [!Function] `smtp-login-authentication`  _:rest_ _data_

Returns a thunk that can be used `smtp-authenticate!` procedure
for LOGIN authentication.

The rest argument _date_ is given, all of the values must be string, 
then each authentication prompt uses the given data as if it's typed by
user. If there is no data or sufficient date, then input prompt will be
shown and let user type the authentication information.


###### [!Macro] `smtp:mail`  _from_ _message-elements_ _..._
###### [!Auxiliary Syntax] `smtp:from` 
###### [!Auxiliary Syntax] `smtp:subject` 
###### [!Auxiliary Syntax] `smtp:to` 
###### [!Auxiliary Syntax] `smtp:cc` 
###### [!Auxiliary Syntax] `smtp:bcc` 
###### [!Auxiliary Syntax] `smtp:attachment` 
###### [!Auxiliary Syntax] `smtp:header` 

Constructs an SMTP mail object.

_from_ must be one of the following forms:
``(smtp:from email)``

``(smtp:from name email)``

_message-elements_ must be one of the following forms:
``(smtp:subject subject)``

``(smtp:to email)``

``(smtp:to name email)``

``(smtp:cc email)``

``(smtp:cc name email)``

``(smtp:bcc email)``

``(smtp:bcc name email)``

``(smtp:attachment type subtype content)``

``(smtp:attachment type subtype content filename)``

``(smtp:attachment type subtype content filename disposition-parameter)``

``(smtp:attachment type subtype content filename disposition-parameter headers ...)``

``(smtp:alternative type subtype content)``

``(smtp:alternative type subtype content headers ...)``

``(smtp:header name value)``

``string``

The order of the appearance does not matter, except _string_ which will be
the content of the creating SMTP mail. Except the `smtp:subject` and
`smtp:alternative`, all elements can be appear multiple times.


###### [!Function] `smtp-mail->string`  _smtp-mail_

Returns MIME string of given _smtp-mail_.

NOTE: the procedure consumes input port attachments if exists.


### [§3] Middle level APIs

###### [!Function] `make-smtp-mail`  _from_ _subject_ _content_ _:rest_ _recipients_

Creates an SMTP mail object.

###### [!Function] `smtp-mail?`  _obj_

Returns #t if the given _obj_ is an SMTP mail object, otherwise #f.

###### [!Function] `make-smtp-from`  _email_
###### [!Function] `make-smtp-from`  _name_ _email_

creates an SMTP from object.

###### [!Function] `smtp-from?`  _obj_

Returns #t if the given _obj_ is an SMTP from object, otherwise #f.

###### [!Function] `smtp-recipient?`  _obj_

Returns #t if the given _obj_ is an SMTP recipient object, 
otherwise #f.

SMTP recipient objects are one of the following objects:

- SMTP to object
- SMTP cc object
- SMTP bcc object



###### [!Function] `make-smtp-to`  _email_
###### [!Function] `make-smtp-to`  _name_ _email_
###### [!Function] `make-smtp-cc`  _email_
###### [!Function] `make-smtp-cc`  _name_ _email_
###### [!Function] `make-smtp-bcc`  _email_
###### [!Function] `make-smtp-bcc`  _name_ _email_

Creates SMTP to, SMTP cc and SMTP bcc objects respectively.

###### [!Function] `smtp-to?`  _obj_
###### [!Function] `smtp-cc?`  _obj_
###### [!Function] `smtp-bcc?`  _obj_

Returns #t if given _obj_ is SMTP to, SMTP cc or SMTP bcc,
respectively. Otherwise #f.


###### [!Function] `make-smtp-attachment`  _type_ _subtype_ _content_ _:optional_ _filename_ _disposition-parameter_ _headers_ _..._

Creates a MIME object which represents SMTP attachment.

Together _type_ and _subtype_ represent MIME type. e.g.
`text/plain`_content_ can be string, bytevector, input-port.

Optional argument _filename_ is specified, then `content-disposition`header will have `filename` parameter.

Optional argument _disposition-parameter_ is specified, then
`content-disposition` header will have specified parameter. Default
values is `attachment`.

Rest argument _headers_ is specified, then the created attachment has
extra mime header. Each _header_ must be a list which contains 2
elements, header name and value. The value part can also contain 
MIME parameters.


###### [!Function] `make-smtp-alternative-component`  _type_ _subtype_ _content_ _headers_ _..._

Creates a component of alternative message. The returning component
is a mere MIME object.

_type_ and _subtype_ are the same as `make-smtp-attachment`.


###### [!Function] `make-smtp-alternative`  _alternatives_ _..._

Creates an alternative message. The returning message
is a mere MIME object.

NOTE: the returning message should be added by `smtp-mail-add-attachment!`and not _content_ argument of `make-smtp-mail`. To make mail
construction easier, use `smtp:mail` macro.


###### [!Function] `smtp-mail-add-recipent!`  _smtp-mail_ _recipient_

Adds SMTP recipient _recipient_ to _smtp-mail_ and returns
SMTP mail object.


###### [!Function] `smtp-mail-add-attachment!`  _smtp-mail_ _attachment_

Adds SMTP attachment _attachment_ to _smtp-mail_ and returns
SMTP mail object.


###### [!Function] `smtp-mail-add-header!`  _smtp-mail_ _name_ _value_

Adds header to _smtp-mail_ and returns SMTP mail object.

If there is the same header insense of `string-ci=?`, then the old
one is overwritten.


