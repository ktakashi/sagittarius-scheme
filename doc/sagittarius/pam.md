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

The `service` argument need to be empty if UPN format is used,
i.e. `username@domain` format.

Also, the `service` argument is translated to translated to domain
argument for `LogonUser`.

During the authentication process, it calls `LoadUserProfile`, which
requires User Access Control to be disabled. The successful call is
not mandatory, so token might be created, however user profile won't
be loaded if the function call failed.

#### BSD Auth

BSD Auth requires access to `/usr/libexec/auth` directory to execute
authentication. The directory is usually not accessible other than
`root`. So, to run the authentication, the process must be executed
by root permission.


###### [!Library] `(sagittarius pam)`  **[@since] `0.9.14`**

Provising platform user authentication mechanism.

###### [!Function] `auth-token?` _obj_

Return `#t` if the given _obj_ is auth token, otherwise `#f`.

###### [!Function] `auth-token-name` (_token_ `auth-token?`)

Return the user name of the given _token_.

For POSIX, this is `pw_name` of `struct passwd`.  
For Windows, this is `NameSamCompatible` value from `GetUserName`.

###### [!Function] `auth-token-full-name` (_token_ `auth-token?`)

Return the full name of the user of the given _token_.

For POSIX, this is `pw_gecos` of `struct passwd`.  
For Windows, this is `NameDisplay` value from `GetUserName`, if it's
not available, fallback to `NameSamCompatible`


###### [!Function] `auth-token-dir` (_token_ `auth-token?`)

Return the home directory or user profile directory of the given _token_.

For POSIX, this is `pw_dir` of `struct passwd`.  
For Windows, this is `HOME` or `USERPROFILE` environment value.


###### [!Function] `auth-token-shell` (_token_ `auth-token?`)

Return the login shell of the given _token_.

For POSIX, this is `pw_shell` of `struct passwd`.  
For Windows, this is `ComSpec` environment value.


###### [!Function] `pam-authenticate` (_service_ `string?`) (_user_ `string?`) (_conversation_ `procedure?`)

Invokes the underlying platform authentication mechanism.

For Linux / Open PAM, the _service_ argument must be one of the
names configured under `/etc/pam.d` directory. e.g. `login` or
`passwd`.

For BSD Auth, the _service_ argument must be one of the names
configured in `/etc/login.conf`. e.g. `passwd`.

For Windows, the _service_ argument must be domain or empty string when
the _user_ is UPN format. For local machine login, this argument can be
`"."`.

The _user_ argument is the username to authenticate.

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
