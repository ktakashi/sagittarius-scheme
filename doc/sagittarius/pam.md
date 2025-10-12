[§2] (sagittarius pam) - Privileged Access Module {#lib.sagittarius.pam}
-------------

This library provides a wrapper layer of, Linux PAM, Open PAM, 
Windows `LogonUser` and BSD Auth.

This example shows how to authenticate a user.
```scheme
(import (rnrs)
        (sagittarius pam)
        (sagittarius stty))

(define (prompts-handler prompts)
  (define (prompt-handler prompt)
    (display (cdr prompt)) (flush-output-port (current-output-port))
    (if (eq? (car prompt) 'echo-off)
        (with-stty '((not echo) echonl) 
         (lambda () (get-line (current-input-port))))
        (get-line (current-input-port))))
  (vector-map prompt-handler prompts))

(cond ((pam-authenticate "login" "username" prompts-handler) =>
       (lambda (token)
         ;; do whatever with the token
         (pam-invalidate-token! token)))
      (else 
        ;; handle authentication error
       ))
```

### CAVEAT

This module interacts with platform specific authentication mechanis.
It doesn't change the required configuration. Below are known limitation
per platform.

#### Linux PAM / Open PAM

Authenticating the logged on user may not invoke prompt, but simply
return the authenticate token, depending on the PAM 
(Pluggable Authentication Module) configuration.

Also, authenticating other users on behalf may require root / `sudo`
permission.

#### Windows

The `service` argument is used only if UPN format is used for username,
i.e. `username@domain`. And the value is passed to `LogonUser` as domain
argument.
Otherwise, the domain argument is extracted from the passwd name, which
is constructed `domain\name` format.

#### BSD Auth

BSD Auth requires access to `/usr/libexec/auth` directory to execute
authentication. The directory is usually not accessible other than
`root`. So, to run the authentication, the process must be executed
by root permission.


###### [!Library] `(sagittarius pam)`  **[@since] `0.9.14`**

Provising platform user authentication mechanism.

###### [!Function] `passwd?` _obj_

Returns `#t` if the given _obj_ is passwd object, otherwise `#f`.

The passwd object represents `struct passwd` in C.

###### [!Function] `get-passwd` (_user_ `string?`)

Retrieves information of _user_, if the _user_ doesn't exists,
then returns `#f`.

###### [!Function] `passwd-name` (_pw_ `passwd?`)

Returns associated user name of the _pw_.

###### [!Function] `passwd-gecos` (_pw_ `passwd?`)

Returns associated full user name of the _pw_.

###### [!Function] `passwd-dir` (_pw_ `passwd?`)

Returns associated home directory of the _pw_.

###### [!Function] `passwd-shell` (_pw_ `passwd?`)

Returns associated shell of the _pw_.

###### [!Function] `auth-token?` _obj_

Return `#t` if the given _obj_ is auth token, otherwise `#f`.

###### [!Function] `auth-token-passwd` (_token_ `auth-token?`)

Return passwd object associated to this auth token.


###### [!Function] `pam-authenticate` (_service_ `string?`) (_user_ (or `string?` `passwd?`)) (_conversation_ `procedure?`)

Invokes the underlying platform authentication mechanism.

For Linux / Open PAM, the _service_ argument must be one of the
names configured under `/etc/pam.d` directory. e.g. `login` or
`passwd`.

For BSD Auth, the _service_ argument must be one of the names
configured in `/etc/login.conf`. e.g. `passwd`.

For Windows, the _service_ argument must be domain or empty string when
the _user_ is UPN format. For local machine login, this argument can be
`"."`.

The _user_ argument is the username or passwd to authenticate.  
If the _user_ is string, and the user is not found, then the procedure
returns `#f` without trying to do authentication.

The _conversation_ argument must accept one argument, a vector and
must return a vector of string, the size of the vector must be the
size of the input vector.

The argument of _conversation_ procedure contains a pair of prompt.
A prompt format is below

`(type . message)`

_type_ is a symbol of one of the followings

`echo-off`
: Show the `message` and recieve user input without echoing, then
  return the value

`echo-on`
: Show the `message` and recieve user input, then return the value

`error`
: Show the `message` on error output

`text`
: Show the `message`

If the authentication succeeds, then the procedure returns auth token,
otherwise `#f`.

###### [!Function] `pam-invalidate-token!` (_token_ `auth-token?`)

Invalidates the given _token_.

###### [!Parameter] `*pam:conversation-error-handler*!`

Error handler when _conversation_ argument of the `pam-authenticate`
raises an error. The result of the handler procedure will be ignored,
this is only for diagnosis purpose.

A error handler must be a procedure which accepts exactly one argument,
which is the raised condition.
