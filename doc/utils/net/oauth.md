[§2] (net oauth) - OAuth library {#net.oauth}
-------------

This section describes the APIs for OAuth. OAuth is new secure authentication
method for web service. For more detail, see 
[OAuth Community Site](http://oauth.net/).

The following example shows how to obtain an access token from Twitter.

``````````scheme
(import (rnrs) (net oauth) (sagittarius control) (srfi :13 strings))
;; obtain a request token.
;; type consumer key and secret you have got issued by Twitter
(define token (obtain-request-token
	       "http://api.twitter.com/oauth/request_token"
	       (make-consumer-token
		:key "consumer key"
		:secret "consumer secret")))

(define (get-pin url)
  (print "Open the following url and type in the shown PIN.")
  (print url)
  (let loop ()
    (display "Input PIN: ") (flush-output-port (current-output-port))
    (let1 pin (get-line (current-input-port))
      (cond ((eof-object? pin) #f)
            ((string-null? pin) (loop))
            (else pin)))))

(define (report token)
  (print "(begin")
  (print " (define consumer-key \"" (token-key (token-consumer token)) "\")")
  (print " (define consumer-secret \""
	 (token-secret (token-consumer token))"\")")
  (print " (define access-token \""(token-key token)"\")")
  (print " (define access-token-secret \""(token-secret token)"\")")
  (print ")"))

(define (main args)
  (let1 pin (get-pin (make-authorization-uri
		      "http://api.twitter.com/oauth/authorize"
		      token))
    ;; authorize the request token manually.
    (authorize-request-token token pin)
    ;; obtain the access token
    (let1 access-token (obtain-access-token 
			"http://api.twitter.com/oauth/access_token"
			token)
      (report access-token))))
``````````

Now you get the access token to tweet, let's tweet something on Sagittarius:

``````````scheme
(import (rnrs) (srfi :26 cut) (text sxml ssax) (net oauth) (sagittarius io))
(define consumer-key "your consumer key")
(define consumer-secret "your consumer secret")
(define access-token "your access token")
(define access-token-secret "your access token secret")

;; creates an access token to tweet.
(define access-token
  (make-access-token :key access-token
		     :secret access-token-secret
		     :consumer
		     (make-consumer-token
		      :key consumer-key
		      :secret consumer-secret)))

(define (call/twitter-api->sxml token method path params . opts)
  (define (call)
    (access-protected-resource 
     (string-append "http://api.twitter.com" path)
     token
     :request-method method
     :user-parameters params))
  (define (retrieve body status hint advice)
    (if hint (print hint))
    (if advice (print advice))
    (call-with-input-string body (cut ssax:xml->sxml <> '())))
  (call-with-values call retrieve))

(define (twitter-update/sxml token message . opts)
  (call/twitter-api->sxml 
   token 'POST "/1/statuses/update.xml" 
   `(("status" ,message))))

;; if you want to use this from command line.
(import (getopt))

(define (main args)
  (with-args args
      ((message (#\m "message") #t (usage)))
    (print (twitter-update/sxml access-token message))))
``````````

The examples explain basic flows. To obtain an access token, and access to
protected resource with it.

###### [!Library] `(net oauth)` 

This library provides OAuth 1.0 procedures. The API's names are compatible
with cl-oauth.


###### [!Function] `obtain-request-token`  _uri_ _consumer-token_ _:key_ _(version_ _:1.0)_ _(user-parameters_ _'())_ _
_ _(timestamp_ _(time-second_ _(current-time)))_ _(auth-location_ _:header)_ _
_ _(request-method_ _'POST)_ _(callback-uri_ _#f)_ _(additional-headers_ _'())_ _
_ _(signature-method_ _:hmac-sha1)_ _(error-translator_ _default-message-translator)_

_uri_ must be string and URI format.

_consumer-token_ must be a consumer token object.

Obtains request token from given uri with given consumer token.

if the keyword arguments are specified:

_version_ specifies which version uses. We only support 1.0 so this must not
be specified.

_user-parameters_ is an alist of the extra parameters to be sent to the
server. This parameters are in the body message if the _request-method_ is
`POST` or query string if the _request-method_ is `GET`.

_timestamp_ specifies timestamp to send to the server. 

_auth-location_ specifies the place where the authentication information
located. This can be either `:header` or `:parameters`.

_request-method_ specifies which request method is used. This can be either
`GET` or `POST`. `POST` is recommended.

_callback-uri_ specifies call back uri described in OAuth specification. If
users don't use specific location to be redirected, this must not be specified.

_additional-headers_ is alist of additional header to be sent to the server.

_signature-method_ specifies which hash method is used. For now we only
support `:hmac-sha1`.

_error-translator_ specifies how to treat the error message sent by the
server when error occurred. This must be a procedure which accepts 3 arguments,
http status, headers and body respectively.


###### [!Function] `make-authorization-uri`  _uri_ _request-token_ _:key_ _(version_ _:1.0)_ _(callback-uri_ _#f)_ _
_ _(user-parameters_ _'())_

_uri_ must be string and URI format.

_request-token_ must be a request token retrieved by the procedure
`obtain-request-token`.

Creates a authorization URI which user must agree.

The other keyword arguments are the same as `obtain-request-token`.


###### [!Function] `authorize-request-token`  _request-token_ _verificateion-code_

_request-token_ must be a request token which retrieved by the
procedure `obtain-request-token`.

_verificateion-code_ must be a string returned by the URI generated by the
procedure `make-authorization-uri`Sets the given _verificateion-code_ to the request token and authorized flag
#t, manually.


###### [!Function] `obtain-access-token`  _uri_ _token_ _:key_ _(consumer-token_ _(token-consumer_ _token))_ _
_ _(version_ _:1.0)_ _(user-parameters_ _'())_ _
_ _(timestamp_ _(time-second_ _(current-time)))_ _(auth-location_ _:header)_ _
_ _(request-method_ _'POST)_ _(callback-uri_ _#f)_ _(additional-headers_ _'())_ _
_ _(signature-method_ _:hmac-sha1)_ _(error-translator_ _default-message-translator)_

_uri_ must a string URI formatted.

_token_ must be either request token or access token.

Obtains access token from the given URI.

The keyword arguments _consumer-token_ specifies which consumer token is
used. And must the same one as when you request the request token.

The rest keyword arguments are the same as `obtain-request-token`.


###### [!Function] `access-protected-resource`  _uri_ _access-token_ _:key_ _(consumer-token_ _(token-consumer_ _access-token))_ _
_ _(on-refresh_ _#f)_ _
_ _(version_ _:1.0)_ _(user-parameters_ _'())_ _
_ _(timestamp_ _(time-second_ _(current-time)))_ _(auth-location_ _:header)_ _
_ _(request-method_ _'POST)_ _(callback-uri_ _#f)_ _(additional-headers_ _'())_ _
_ _(signature-method_ _:hmac-sha1)_ _(error-translator_ _default-message-translator)_

_uri_ must a string URI formatted.

_access-token_ must be an access token which obtained by the procedure
`obtain-access-token` or created by `make-access-token`.

Accesses to protected resource.

The keyword argument _on-refresh_ is a hook for when token is expired and
refreshed. It must accept be a procedure which accepts 1 argument that is a
refreshed access token.

The rest keyword arguments are the same as the `obtain-request-token`.


###### [!Function] `oauth-uri-encode`  _string_

Encodes given URI and make it OAuth required form.


###### [!Function] `oauth-compose-query`  _parameters_

Composes given alist to http query form and make it OAuth required form.


###### [!Function] `make-consumer-token`  _:key_ _(key_ _(random-key))_ _(secret_ _random-secret)_ _(user-data_ _#f)_ _
_ _(last-timestamp_ _0)_

Creates a consumer token.

###### [!Function] `make-access-token`  _:key_ _(key_ _(random-key))_ _(secret_ _random-secret)_ _(user-data_ _#f)_ _
_ _consumer_ _(session-handle_ _#f)_ _(expires_ _#f)_ _(authorization-expires_ _#f)_ _
_ _(origin-uri_ _#f)_

Creates a access token.

###### [!Method] `token-consumer`  _token_

_token_ must be an access token or request token.

Retrieves consumer token from given _token_.


