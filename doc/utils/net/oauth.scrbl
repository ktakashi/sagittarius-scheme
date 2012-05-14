@; -*- coding: utf-8 -*-
@subsection[:tag "net.oauth"]{(net oauth) - OAuth library}

This section describes the APIs for OAuth. OAuth is new secure authentication
method for web service. For more detail, see 
@hyperlink[:href "http://oauth.net/"]{OAuth Community Site}.

The following example shows how to obtain an access token from Twitter.

@codeblock{
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
}

Now you get the access token to tweet, let's tweet something on Sagittarius:
@codeblock{
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
}

The examples explain basic flows. To obtain an access token, and access to
protected resource with it.

@define[Library]{@name{(net oauth)}}
@desc{This library provides OAuth 1.0 procedures. The API's names are compatible
with cl-oauth.
}

@define[Function]{@name{obtain-request-token}
 @args{uri consumer-token :key (version :1.0) (user-parameters '())
 (timestamp (time-second (current-time))) (auth-location :header)
 (request-method 'POST) (callback-uri #f) (additional-headers '())
 (signature-method :hmac-sha1) (error-translator default-message-translator)}}
@desc{@var{uri} must be string and URI format.

@var{consumer-token} must be a consumer token object.

Obtains request token from given uri with given consumer token.

if the keyword arguments are specified:

@var{version} specifies which version uses. We only support 1.0 so this must not
be specified.

@var{user-parameters} is an alist of the extra parameters to be sent to the
server. This parameters are in the body message if the @var{request-method} is
@code{POST} or query string if the @var{request-method} is @code{GET}.

@var{timestamp} specifies timestamp to send to the server. 

@var{auth-location} specifies the place where the authentication information
located. This can be either @code{:header} or @code{:parameters}.

@var{request-method} specifies which request method is used. This can be either
@code{GET} or @code{POST}. @code{POST} is recommended.

@var{callback-uri} specifies call back uri described in OAuth specification. If
users don't use specific location to be redirected, this must not be specified.

@var{additional-headers} is alist of additional header to be sent to the server.

@var{signature-method} specifies which hash method is used. For now we only
support @code{:hmac-sha1}.

@var{error-translator} specifies how to treat the error message sent by the
server when error occurred. This must be a procedure which accepts 3 arguments,
http status, headers and body respectively.
}

@define[Function]{@name{make-authorization-uri}
 @args{uri request-token :key (version :1.0) (callback-uri #f)
 (user-parameters '())}}
@desc{@var{uri} must be string and URI format.

@var{request-token} must be a request token retrieved by the procedure
@code{obtain-request-token}.

Creates a authorization URI which user must agree.

The other keyword arguments are the same as @code{obtain-request-token}.
}

@define[Function]{@name{authorize-request-token}
 @args{request-token verificateion-code}}
@desc{@var{request-token} must be a request token which retrieved by the
procedure @code{obtain-request-token}.

@var{verificateion-code} must be a string returned by the URI generated by the
procedure @code{make-authorization-uri}

Sets the given @var{verificateion-code} to the request token and authorized flag
#t, manually.
}

@define[Function]{@name{obtain-access-token} 
 @args{uri token :key (consumer-token (token-consumer token))
 (version :1.0) (user-parameters '())
 (timestamp (time-second (current-time))) (auth-location :header)
 (request-method 'POST) (callback-uri #f) (additional-headers '())
 (signature-method :hmac-sha1) (error-translator default-message-translator)}}
@desc{@var{uri} must a string URI formatted.

@var{token} must be either request token or access token.

Obtains access token from the given URI.

The keyword arguments @var{consumer-token} specifies which consumer token is
used. And must the same one as when you request the request token.

The rest keyword arguments are the same as @code{obtain-request-token}.
}

@define[Function]{@name{access-protected-resource}
 @args{uri access-token :key (consumer-token (token-consumer access-token))
 (on-refresh #f)
 (version :1.0) (user-parameters '())
 (timestamp (time-second (current-time))) (auth-location :header)
 (request-method 'POST) (callback-uri #f) (additional-headers '())
 (signature-method :hmac-sha1) (error-translator default-message-translator)}}
@desc{@var{uri} must a string URI formatted.

@var{access-token} must be an access token which obtained by the procedure
@code{obtain-access-token} or created by @code{make-access-token}.

Accesses to protected resource.

The keyword argument @var{on-refresh} is a hook for when token is expired and
refreshed. It must accept be a procedure which accepts 1 argument that is a
refreshed access token.

The rest keyword arguments are the same as the @code{obtain-request-token}.
}

@define[Function]{@name{oauth-uri-encode} @args{string}}
@desc{Encodes given URI and make it OAuth required form.
}
@define[Function]{@name{oauth-compose-query} @args{parameters}}
@desc{Composes given alist to http query form and make it OAuth required form.
}

@define[Function]{@name{make-consumer-token}
 @args{:key (key (random-key)) (secret random-secret) (user-data #f)
 (last-timestamp 0)}}
@desc{Creates a consumer token.}

@define[Function]{@name{make-access-token}
 @args{:key (key (random-key)) (secret random-secret) (user-data #f)
 consumer (session-handle #f) (expires #f) (authorization-expires #f)
 (origin-uri #f)}}
@desc{Creates a access token.}

@define[Method]{@name{token-consumer} @args{token}}
@desc{@var{token} must be an access token or request token.

Retrieves consumer token from given @var{token}.
}