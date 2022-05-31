[§2] (rfc hotp) - HMAC-based One-Time Password library {#rfc.hotp}
-------------

###### [!Library] `(rfc hotp)` 

Provides HMAC-based One-Time Password (HOTP) generating procedures. HOTP is
defined in RFC 4226:
[RFC 4226](http://tools.ietf.org/html/rfc4226).


###### [!Function] `generate-hmac-based-one-time-password`  _K_ _C_ _digit_
###### [!Function] `hotp`  _K_ _C_ _digit_

_K_ must be a bytevector represents shared secret.

_C_ must be an exact integer represents counter.

_digit_ must be an exact integer.

Generates an HMAC-based one-time password.

`hotp` is an alias of `generate-hmac-based-one-time-password`.


[§2] (rfc totp) - Time-based One-Time Password library {#rfc.totp}
-------------

###### [!Library] `(rfc totp)` 

Provides Time-based One-Time Password (TOTP) generating procedures. TOTP is
defined in RFC 6238:
[RFC 6238](http://tools.ietf.org/html/rfc6238).


###### [!Function] `generate-time-based-one-time-password`  _K_ _digit_ _:key_ _time_ _start-time_ _step_ _mode_
###### [!Function] `totp`  _K_ _digit_ _:key_ _time_ _start-time_ _step_ _mode_

_K_ must be a bytevector represents shared secret.

_digit_ must be a positive exact integer.

The keyword argument _time_ is given, then it must be a UTC time object
of SRFI-19. The procedure computes returning one-time password according to
the given time. The default value is `(current-time)`.

NOTE: _time_ keyword should not be specified unless its testing purpose.

The keyword arguments described below are system parameters, means these
parameters must be agreed between authenticator.

The keyword argument _start-time_ is given, then it must be a UTC time
object of SRFI-19. The procedure computes returning one-time password according
to the given time. The defaule value is `(make-time time-utc 0 0)`.

The keyword argument _step_ is given, then it must be a positive exact
integer. The procedure computes returning one-time password according
to the given time. The defaule value is `30`.

The keyword argument _step_ is given, then it must be a positive exact
integer. The procedure computes returning one-time password according
to the given time. The defaule value is `30`.

The keyword argument _mode_ is given, then it must be a hash algorithm name
either builtin or custom. The procedure computes returning one-time password
according to the given time. The defaule value is `SHA-1`.


Generates an Time-based one-time password.

`totp` is an alias of `generate-time-based-one-time-password`.


