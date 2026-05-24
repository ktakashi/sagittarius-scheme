[§2] (rfc ssh) - SSH library {#rfc.ssh}
-------------

###### [!Library] `(rfc ssh)`

This library provides SSH2 protocol client capabilities for Sagittarius Scheme,
implementing the SSH transport layer, user authentication, and connection 
protocols as defined in RFC 4250-4254. It supports modern cryptographic 
algorithms and key exchange methods.

The library exports all symbols from the following sub-libraries:
- `(rfc ssh constants)` - SSH protocol constants
- `(rfc ssh types)` - SSH type definitions and messages
- `(rfc ssh transport)` - Transport layer protocol
- `(rfc ssh client)` - Client implementation
- `(rfc ssh identity)` - Identity and key file handling
- `(rfc ssh crypto)` - Cryptographic operations

Following example demonstrates basic SSH client usage:

```scheme
(import (rnrs) (rfc ssh))

(let ((socket (make-client-socket "example.com" "22")))
  (let ((transport (socket->client-ssh-transport socket)))
    (open-client-ssh-transport! transport)
    (when (ssh-authenticate transport +ssh-auth-method-password+
                           "username" "password")
      (call-with-ssh-channel (open-client-ssh-session-channel transport)
        (lambda (channel)
          (ssh-request-exec channel "ls -la")
          (let ((output (ssh-recv-channel-data channel)))
            (display (utf8->string output)))))
      (close-client-ssh-transport! transport))))
```

### [§3] Transport Operations

###### [!Function] `make-client-ssh-transport`  _:key_ _(socket_ _#f)_ _(server_ _#f)_ _(port_ _#f)_

Creates a new SSH client transport object. The transport manages the SSH
connection state, encryption, and packet handling.

The keyword arguments are optional and can be used to pre-configure the
transport with connection details.

###### [!Function] `socket->client-ssh-transport`  _socket_

_socket_ must be a connected client socket.

Creates an SSH client transport from an existing socket connection.
The socket should already be connected to an SSH server.

###### [!Function] `open-client-ssh-transport!`  _transport_

_transport_ must be an SSH client transport.

Opens the SSH transport by performing version exchange and key exchange.
This must be called before authentication or opening channels.

###### [!Function] `close-client-ssh-transport!`  _transport_ _:optional_ _(reason_ _+ssh-disconnect-by-application+)_

_transport_ must be an SSH client transport.

Closes the SSH transport, sending a disconnect message to the server
with the specified reason code.

###### [!Function] `ssh-client-transport?`  _obj_

Returns `#t` if _obj_ is an SSH client transport, otherwise `#f`.

###### [!Function] `ssh-client-service-request`  _transport_ _service-name_

_transport_ must be an SSH client transport.
_service-name_ must be a string.

Requests a service from the SSH server. Common services are
`"ssh-userauth"` and `"ssh-connection"`.

###### [!Parameter] `*ssh-client-kex-list*`

Parameter containing the list of supported key exchange algorithms.
Default includes modern algorithms like Curve25519, Curve448, ECDH variants,
and Diffie-Hellman groups. Algorithms using SHA-1 are excluded by default.

###### [!Parameter] `*ssh-client-public-key-list*`

Parameter containing the list of supported public key algorithms.
Default includes Ed448, Ed25519, ECDSA variants, and RSA with SHA-256/SHA-512.
SSH-RSA with SHA-1 is excluded by default.

###### [!Parameter] `*ssh-client-encryption-list*`

Parameter containing the list of supported encryption algorithms.

###### [!Parameter] `*ssh-client-mac-list*`

Parameter containing the list of supported MAC algorithms.

###### [!Parameter] `*ssh:debug-package-handler*`

Parameter for handling SSH debug messages. Set to a procedure accepting
a debug message to customize debug handling.

###### [!Parameter] `*ssh:ignore-package-handler*`

Parameter for handling SSH ignore messages.

###### [!Parameter] `*ssh:ext-info-handler*`

Parameter for handling SSH extension information messages.

###### [!Parameter] `*ssh-version-string*`

The SSH version string sent during version exchange.

###### [!Function] `ssh-data-ready?`  _transport_

Returns `#t` if there is data ready to be read from the transport.

###### [!Function] `ssh-write-packet`  _transport_ _data_

Writes a packet to the transport.

###### [!Function] `ssh-read-packet`  _transport_

Reads a packet from the transport and returns it as a bytevector.

###### [!Function] `ssh-write-ssh-message`  _transport_ _message_

Writes an SSH message object to the transport.

###### [!Function] `ssh-key-exchange`  _transport_

Performs SSH key exchange negotiation with the server.

###### [!Function] `ssh-compute-keys!`  _transport_

Computes and installs encryption keys after key exchange.

###### [!Function] `ssh-version-exchange`  _transport_

Performs SSH version string exchange with the server.

###### [!Function] `ssh-kex-digest`  _transport_ _...args_

Computes the key exchange digest value.

### [§3] Authentication

###### [!Function] `ssh-authenticate`  _transport_ _method_ _username_ _...args_

_transport_ must be an SSH client transport.
_method_ must be an authentication method constant.
_username_ must be a string.

Authenticates the user with the SSH server using the specified method.
Returns `#t` on success, `#f` on failure.

Supported authentication methods:

###### [!Constant] `+ssh-auth-method-public-key+`

Public key authentication method. When using this method, provide the
username and a key-pair as arguments.

Example:
```scheme
(ssh-authenticate transport +ssh-auth-method-public-key+ 
                 "username" key-pair)
```

###### [!Constant] `+ssh-auth-method-password+`

Password authentication method. Provide username and password as arguments.

Example:
```scheme
(ssh-authenticate transport +ssh-auth-method-password+
                 "username" "password")
```

###### [!Constant] `+ssh-auth-method-keyboard-interactive+`

Keyboard-interactive authentication method. Provide username and an optional
prompt handler procedure.

Example:
```scheme
(ssh-authenticate transport +ssh-auth-method-keyboard-interactive+
                 "username" 
                 :prompt-handler (lambda (prompts) ...))
```

###### [!Function] `ssh-read-auth-response`  _transport_ _error-handler_

Reads an authentication response from the server. The _error-handler_ is
called if the response is not a success or failure message.

###### [!Function] `ssh-authenticate-method`  _method_

Generic method for implementing custom authentication methods.

###### [!Function] `register-auth-method`  _name_ _proc_

Registers a custom authentication method. This is provided for backward
compatibility but defining methods on `ssh-authenticate-method` is preferred.

### [§3] Channel Operations

###### [!Function] `open-client-ssh-channel`  _transport_ _open-channel-proc_ _handle-confirmation-proc_ _:key_ _(initial-window_ _#x100000)_ _(maximum-packet_ _#x4000)_

_transport_ must be an SSH client transport.
_open-channel-proc_ must be a procedure that creates a channel open message.
_handle-confirmation-proc_ must be a procedure handling the confirmation.

Opens a new SSH channel. This is a low-level procedure; prefer
`open-client-ssh-session-channel` for typical use.

###### [!Function] `open-client-ssh-session-channel`  _transport_ _:key_ _(initial-window_ _#x100000)_ _(maximum-packet_ _#x4000)_

_transport_ must be an SSH client transport.

Opens a session channel, which is the most common channel type used for
executing commands, running shells, or requesting subsystems.

Returns an SSH channel object.

###### [!Function] `ssh-channel-eof`  _channel_

_channel_ must be an SSH channel.

Sends an EOF message to the channel, indicating no more data will be sent.

###### [!Function] `close-ssh-channel`  _channel_

_channel_ must be an SSH channel.

Closes the SSH channel.

###### [!Function] `call-with-ssh-channel`  _channel_ _proc_

_channel_ must be an SSH channel.
_proc_ must be a procedure accepting one argument.

Calls _proc_ with the channel and ensures the channel is properly closed
afterwards, even if an error occurs.

### [§3] Channel Requests

###### [!Function] `ssh-request-pseudo-terminal`  _channel_ _term-env_ _width_ _height_ _:key_ _(pixel-width_ _0)_ _(pixel-height_ _0)_ _(modes_ _#vu8())_

_channel_ must be an SSH channel.
_term-env_ must be a string (e.g., "xterm", "vt100").
_width_ and _height_ must be integers representing terminal dimensions.

Requests a pseudo-terminal on the channel. This is typically needed before
requesting a shell.

###### [!Function] `ssh-request-shell`  _channel_

_channel_ must be an SSH channel.

Requests an interactive shell on the channel. Usually requires a
pseudo-terminal to be allocated first.

###### [!Function] `ssh-request-exec`  _channel_ _command_

_channel_ must be an SSH channel.
_command_ must be a string.

Requests execution of a command on the remote server.

Example:
```scheme
(ssh-request-exec channel "ls -la /tmp")
```

###### [!Function] `ssh-request-subsystem`  _channel_ _subsystem-name_

_channel_ must be an SSH channel.
_subsystem-name_ must be a string.

Requests a subsystem on the channel. Common subsystems include "sftp".

Example:
```scheme
(ssh-request-subsystem channel "sftp")
```

### [§3] Channel Data Transfer

###### [!Function] `ssh-send-channel-data`  _channel_ _data_

_channel_ must be an SSH channel.
_data_ must be a bytevector.

Sends data through the channel to the remote server.

###### [!Function] `ssh-recv-channel-data`  _channel_ _:optional_ _receiver_

_channel_ must be an SSH channel.
_receiver_ must be a procedure accepting received data (optional).

Receives data from the channel. If no receiver is provided, returns
received data as a bytevector. Otherwise, calls the receiver with the data.

###### [!Function] `ssh-binary-data-receiver`

Returns a receiver procedure that accumulates received data into a
bytevector.

###### [!Function] `ssh-oport-receiver`  _output-port_

_output-port_ must be a binary output port.

Returns a receiver procedure that writes received data to the given port.

###### [!Function] `ssh-execute-command`  _transport_ _command_ _:optional_ _receiver_

_transport_ must be an SSH transport.
_command_ must be a string.
_receiver_ is an optional data receiver procedure.

Convenience procedure that opens a channel, executes a command, and
returns the output. Automatically handles channel creation and cleanup.

Example:
```scheme
(let ((output (ssh-execute-command transport "whoami")))
  (display (utf8->string output)))
```

### [§3] Identity and Key Management

###### [!Function] `ssh-read-identity-file`  _file_ _password_

_file_ must be a string path to a private key file.
_password_ must be a string or `#f`.

Reads an OpenSSH private key file and returns a key-pair object.
The file should be in OpenSSH private key format. If the key is encrypted,
provide the password; otherwise, use `#f`.

Example:
```scheme
(define key-pair (ssh-read-identity-file "~/.ssh/id_ed25519" #f))
```

###### [!Function] `ssh-read-identity`  _input-port_ _password_

_input-port_ must be a binary input port.
_password_ must be a string or `#f`.

Reads an OpenSSH private key from an input port and returns a key-pair.

###### [!Function] `ssh-read-openssh-public-key-file`  _file_

_file_ must be a string path to a public key file.

Reads an OpenSSH public key file and returns a public key object.
The file should be in OpenSSH public key format (e.g., `id_rsa.pub`).

###### [!Function] `ssh-read-openssh-public-key`  _input-port_

_input-port_ must be a textual input port.

Reads an OpenSSH public key from an input port and returns a public key.

### [§3] SSH Types and Messages

The library defines several record types representing SSH messages and data
structures. These are primarily used internally but can be useful for
advanced use cases.

###### [!Record Type] `<ssh-type>`

Base class for all SSH type definitions.

###### [!Record Type] `<ssh-message>`

Base class for all SSH message types.

###### [!Function] `ssh-message->bytevector`  _message_

Converts an SSH message object to a bytevector.

###### [!Function] `ssh-message->binary-port`  _message_

Converts an SSH message object to a binary input port. Returns two values:
the port and its size.

###### [!Function] `bytevector->ssh-message`  _class_ _bytevector_

Converts a bytevector to an SSH message of the specified class.

###### [!Function] `ssh-read-message`  _class-or-type_ _input-port_

Reads an SSH message or type from the input port.

###### [!Function] `ssh-write-message`  _class-or-type_ _obj_ _output-port_

Writes an SSH message or type to the output port.

###### [!Record Type] `<name-list>`

Represents a comma-separated name list as used in SSH protocol negotiation.

###### [!Function] `name-list?`  _obj_

Returns `#t` if _obj_ is a name-list, otherwise `#f`.

###### [!Function] `name-list`  _name_ _..._

Creates a name-list from the given name strings.

###### [!Function] `list->name-list`  _list_

Converts a list of strings to a name-list.

### [§3] SSH Message Types

The following message types are exported for advanced use:

###### [!Record Type] `<ssh-msg-keyinit>`

Key exchange initialization message.

###### [!Record Type] `<ssh-msg-ext-info>`

Extension information message (RFC 8308).

###### [!Record Type] `<ssh-msg-ext-info-extension>`

Individual extension in an ext-info message.

###### [!Record Type] `<ssh-msg-kexdh-init>`

Diffie-Hellman key exchange initialization.

###### [!Record Type] `<ssh-msg-kexdh-reply>`

Diffie-Hellman key exchange reply.

###### [!Record Type] `<ssh-msg-kex-dh-gex-request-old>`

Old-style DH group exchange request.

###### [!Record Type] `<ssh-msg-kex-dh-gex-request>`

DH group exchange request (RFC 4419).

###### [!Record Type] `<ssh-msg-kex-dh-gex-group>`

DH group exchange group message.

###### [!Record Type] `<ssh-msg-kex-dh-gex-init>`

DH group exchange initialization.

###### [!Record Type] `<ssh-msg-kex-dh-gex-reply>`

DH group exchange reply.

###### [!Record Type] `<ssh-msg-kex-ecdh-init>`

ECDH key exchange initialization (RFC 5656).

###### [!Record Type] `<ssh-msg-kex-ecdh-reply>`

ECDH key exchange reply.

###### [!Record Type] `<ssh-msg-disconnect>`

Disconnection message.

###### [!Record Type] `<ssh-msg-debug>`

Debug message.

###### [!Record Type] `<ssh-msg-service-request>`

Service request message.

###### [!Record Type] `<ssh-msg-service-accept>`

Service accept message.

### [§3] Authentication Messages

###### [!Record Type] `<ssh-msg-userauth-request>`

Base class for user authentication request messages.

###### [!Record Type] `<ssh-msg-public-key-userauth-request>`

Public key authentication request.

###### [!Record Type] `<ssh-msg-password-userauth-request>`

Password authentication request.

###### [!Record Type] `<ssh-msg-keyboard-interactive-userauth-request>`

Keyboard-interactive authentication request (RFC 4256).

###### [!Record Type] `<ssh-msg-userauth-passwd-changereq>`

Password change request message.

###### [!Record Type] `<ssh-msg-userauth-failure>`

Authentication failure message.

###### [!Record Type] `<ssh-msg-userauth-banner>`

Authentication banner message.

###### [!Record Type] `<ssh-msg-userauth-pk-ok>`

Public key OK message.

###### [!Record Type] `<ssh-msg-userauth-info-request>`

Keyboard-interactive info request.

###### [!Record Type] `<ssh-msg-userauth-info-response>`

Keyboard-interactive info response.

###### [!Record Type] `<ssh-msg-userauth-prompt>`

Keyboard-interactive prompt.

### [§3] Channel Messages

###### [!Record Type] `<ssh-msg-channel-open>`

Channel open request message.

###### [!Record Type] `<ssh-msg-channel-open-confirmation>`

Channel open confirmation message.

###### [!Record Type] `<ssh-msg-channel-open-failure>`

Channel open failure message.

###### [!Record Type] `<ssh-msg-channel-eof>`

Channel EOF message.

###### [!Record Type] `<ssh-msg-channel-close>`

Channel close message.

###### [!Record Type] `<ssh-msg-channel-request>`

Channel request message.

###### [!Record Type] `<ssh-msg-channel-pty-request>`

Pseudo-terminal request.

###### [!Record Type] `<ssh-msg-channel-window-change>`

Window size change message.

###### [!Record Type] `<ssh-msg-channel-success>`

Channel request success message.

###### [!Record Type] `<ssh-msg-channel-failure>`

Channel request failure message.

###### [!Record Type] `<ssh-msg-channel-data>`

Channel data message.

###### [!Record Type] `<ssh-msg-channel-window-adjust>`

Window size adjustment message.

###### [!Record Type] `<ssh-msg-channel-exec-request>`

Execute command request.

###### [!Record Type] `<ssh-msg-channel-subsystem-request>`

Subsystem request.

###### [!Record Type] `<ssh-msg-exit-status>`

Exit status message.

###### [!Record Type] `<ssh-msg-exit-signal>`

Exit signal message.

### [§3] Public Key Types

###### [!Record Type] `<ssh-dss-public-key>`

DSA public key.

###### [!Record Type] `<ssh-rsa-public-key>`

RSA public key.

###### [!Record Type] `<ssh-eddsa-public-key>`

EdDSA public key (Ed25519, Ed448).

###### [!Record Type] `<ssh-ecdsa-public-key>`

ECDSA public key.

###### [!Record Type] `<ssh-ecdsa-public-key-blob>`

ECDSA public key blob.

###### [!Record Type] `<ssh-signature>`

SSH signature.

### [§3] Connection Types

###### [!Record Type] `<ssh-transport>`

SSH transport layer object maintaining the connection state, encryption
ciphers, and protocol state.

###### [!Record Type] `<ssh-connection>`

SSH connection layer object.

###### [!Record Type] `<ssh-channel>`

SSH channel object.

###### [!Function] `ssh-channel-connection`  _channel_

Returns the connection associated with a channel.

### [§3] SSH Protocol Constants

The library exports all SSH protocol constants defined in RFC 4250.

#### [§4] Message Numbers

```
+ssh-msg-disconnect+                    1
+ssh-msg-ignore+                        2
+ssh-msg-unimplemented+                 3
+ssh-msg-debug+                         4
+ssh-msg-service-request+               5
+ssh-msg-service-accept+                6
+ssh-msg-ext-info+                      7
+ssh-msg-newcompress+                   8
+ssh-msg-kexinit+                      20
+ssh-msg-newkeys+                      21
+ssh-msg-kexdh-init+                   30
+ssh-msg-kexdh-reply+                  31
+ssh-msg-kex-dh-gex-request-old+       30
+ssh-msg-kex-dh-gex-request+           34
+ssh-msg-kex-dh-gex-group+             31
+ssh-msg-kex-dh-gex-init+              32
+ssh-msg-kex-dh-gex-reply+             33
+ssh-msg-kex-ecdh-init+                30
+ssh-msg-kex-ecdh-reply+               31
+ssh-msg-userauth-request+             50
+ssh-msg-userauth-failure+             51
+ssh-msg-userauth-success+             52
+ssh-msg-userauth-banner+              53
+ssh-msg-userauth-passwd-changereq+    60
+ssh-msg-userauth-pk-ok+               60
+ssh-msg-userauth-info-request+        60
+ssh-msg-userauth-info-response+       61
+ssh-msg-global-request+               80
+ssh-msg-request-success+              81
+ssh-msg-request-failure+              82
+ssh-msg-channel-open+                 90
+ssh-msg-channel-open-confirmation+    91
+ssh-msg-channel-open-failure+         92
+ssh-msg-channel-window-adjust+        93
+ssh-msg-channel-data+                 94
+ssh-msg-channel-extended-data+        95
+ssh-msg-channel-eof+                  96
+ssh-msg-channel-close+                97
+ssh-msg-channel-request+              98
+ssh-msg-channel-success+              99
+ssh-msg-channel-failure+             100
```

#### [§4] Disconnection Reason Codes

```
+ssh-disconnect-host-not-allowed-to-connect+      1
+ssh-disconnect-protocol-error+                   2
+ssh-disconnect-key-exchange-failed+              3
+ssh-disconnect-reserved+                         4
+ssh-disconnect-mac-error+                        5
+ssh-disconnect-compression-error+                6
+ssh-disconnect-service-not-available+            7
+ssh-disconnect-protocol-version-not-supported+   8
+ssh-disconnect-host-key-not-verifiable+          9
+ssh-disconnect-connection-lost+                 10
+ssh-disconnect-by-application+                  11
+ssh-disconnect-too-many-connections+            12
+ssh-disconnect-auth-cancelled-by-user+          13
+ssh-disconnect-no-more-auth-methods-available+  14
+ssh-disconnect-illegal-user-name+               15
```

#### [§4] Channel Open Failure Reason Codes

```
+ssh-open-administratively-prohibited+  1
+ssh-open-connect-failed+               2
+ssh-open-unknown-channel-type+         3
+ssh-open-resource-shortage+            4
```

#### [§4] Extended Data Type Codes

```
+ssh-extended-data-stderr+  1
```

#### [§4] Terminal Mode Opcodes

```
+TTY-OP-END+      0
+VINTR+           1
+VQUIT+           2
+VERASE+          3
+VKILL+           4
+VEOF+            5
+VEOL+            6
+VEOL2+           7
+VSTART+          8
+VSTOP+           9
+VSUSP+          10
+VDSUSP+         11
+VREPRINT+       12
+VWERASE+        13
+VLNEXT+         14
+VFLUSH+         15
+VSWTCH+         16
+VSTATUS+        17
+VDISCARD+       18
+IGNPAR+         30
+PARMRK+         31
+INPCK+          32
+ISTRIP+         33
+INLCR+          34
+IGNCR+          35
+ICRNL+          36
+IUCLC+          37
+IXON+           38
+IXANY+          39
+IXOFF+          40
+IMAXBEL+        41
+ISIG+           50
+ICANON+         51
+XCASE+          52
+ECHO+           53
+ECHOE+          54
+ECHOK+          55
+ECHONL+         56
+NOFLSH+         57
+TOSTOP+         58
+IEXTEN+         59
+ECHOCTL+        60
+ECHOKE+         61
+PENDIN+         62
+OPOST+          70
+OLCUC+          71
+ONLCR+          72
+OCRNL+          73
+ONOCR+          74
+ONLRET+         75
+CS7+            90
+CS8+            91
+PARENB+         92
+PARODD+         93
+TTY-OP-ISPEED+ 128
+TTY-OP-OSPEED+ 129
```

#### [§4] Service Names

```
+ssh-userauth+    "ssh-userauth"
+ssh-connection+  "ssh-connection"
```

#### [§4] Key Exchange Algorithms

```
+kex-curve25519-sha256+                       "curve25519-sha256"
+kex-curve448-sha512+                         "curve448-sha512"
+kex-ecdh-sha2-nistp256+                      "ecdh-sha2-nistp256"
+kex-ecdh-sha2-nistp384+                      "ecdh-sha2-nistp384"
+kex-ecdh-sha2-nistp521+                      "ecdh-sha2-nistp521"
+kex-diffie-hellman-group-exchange-sha256+    "diffie-hellman-group-exchange-sha256"
+kex-diffie-hellman-group-exchange-sha1+      "diffie-hellman-group-exchange-sha1"
+kex-diffie-hellman-group14-sha1+             "diffie-hellman-group14-sha1"
+kex-diffie-hellman-group1-sha1+              "diffie-hellman-group1-sha1"
+kex-diffie-hellman-group14-sha256+           "diffie-hellman-group14-sha256"
+kex-diffie-hellman-group15-sha512+           "diffie-hellman-group15-sha512"
+kex-diffie-hellman-group16-sha512+           "diffie-hellman-group16-sha512"
+kex-diffie-hellman-group17-sha512+           "diffie-hellman-group17-sha512"
+kex-diffie-hellman-group18-sha512+           "diffie-hellman-group18-sha512"
```

#### [§4] Public Key Algorithms

```
+public-key-ssh-rsa+                "ssh-rsa"
+public-key-ssh-dss+                "ssh-dss"
+public-key-rsa-sha2-256+           "rsa-sha2-256"
+public-key-rsa-sha2-512+           "rsa-sha2-512"
+public-key-ssh-ed25519+            "ssh-ed25519"
+public-key-ssh-ed448+              "ssh-ed448"
+public-key-ecdsa-sha2-nistp256+    "ecdsa-sha2-nistp256"
+public-key-ecdsa-sha2-nistp384+    "ecdsa-sha2-nistp384"
+public-key-ecdsa-sha2-nistp521+    "ecdsa-sha2-nistp521"
```

#### [§4] Encryption Algorithms

```
+enc-aes256-ctr+      "aes256-ctr"
+enc-aes192-ctr+      "aes192-ctr"
+enc-aes128-ctr+      "aes128-ctr"
+enc-3des-ctr+        "3des-ctr"
+enc-blowfish-ctr+    "blowfish-ctr"
+enc-aes256-cbc+      "aes256-cbc"
+enc-aes128-cbc+      "aes128-cbc"
+enc-3des-cbc+        "3des-cbc"
+enc-blowfish-cbc+    "blowfish-cbc"
```

#### [§4] MAC Algorithms

```
+mac-hmac-sha1+        "hmac-sha1"
+mac-hmac-sha2-256+    "hmac-sha2-256"
+mac-hmac-sha2-512+    "hmac-sha2-512"
```

#### [§4] Extension Information (RFC 8308)

```
+ext-info-c+                    "ext-info-c"
+ext-info-s+                    "ext-info-s"
+extension-server-sig-algs+     "server-sig-algs"
+extension-delay-compression+   "delay-compression"
+extension-no-flow-control+     "no-flow-control"
+extension-elevation+           "elevation"
```

### [§3] See Also

* [RFC 4250 - SSH Protocol Assigned Numbers](https://tools.ietf.org/html/rfc4250)
* [RFC 4251 - SSH Protocol Architecture](https://tools.ietf.org/html/rfc4251)
* [RFC 4252 - SSH Authentication Protocol](https://tools.ietf.org/html/rfc4252)
* [RFC 4253 - SSH Transport Layer Protocol](https://tools.ietf.org/html/rfc4253)
* [RFC 4254 - SSH Connection Protocol](https://tools.ietf.org/html/rfc4254)
* [(rfc sftp)](#rfc.sftp) - SFTP library built on top of SSH
