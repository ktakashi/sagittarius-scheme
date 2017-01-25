@; -*- coding: utf-8 -*-
@subsection[:tag "rfc.hotp"]{(rfc hotp) - HMAC-based One-Time Password library}

@define[Library]{@name{(rfc hotp)}}
@desc{
Provides HMAC-based One-Time Password (HOTP) generating procedures. HOTP is
defined in RFC 4226:
@hyperlink[:href "http://tools.ietf.org/html/rfc4226"]{RFC 4226}.
}

@define[Function]{@name{generate-hmac-based-one-time-password} @args{K C digit}}
@define[Function]{@name{hotp} @args{K C digit}}
@desc{
@var{K} must be a bytevector represents shared secret.

@var{C} must be an exact integer represents counter.

@var{digit} must be an exact integer.

Generates an HMAC-based one-time password.

@code{hotp} is an alias of @code{generate-hmac-based-one-time-password}.
}


@subsection[:tag "rfc.totp"]{(rfc totp) - Time-based One-Time Password library}

@define[Library]{@name{(rfc totp)}}
@desc{
Provides Time-based One-Time Password (TOTP) generating procedures. TOTP is
defined in RFC 6238:
@hyperlink[:href "http://tools.ietf.org/html/rfc6238"]{RFC 6238}.
}

@define[Function]{@name{generate-time-based-one-time-password}
  @args{K digit :key time start-time step mode}}
@define[Function]{@name{totp}
  @args{K digit :key time start-time step mode}}
@desc{
@var{K} must be a bytevector represents shared secret.

@var{digit} must be a positive exact integer.

The keyword argument @var{time} is given, then it must be a UTC time object
of SRFI-19. The procedure computes returning one-time password according to
the given time. The default value is @code{(current-time)}.

NOTE: @var{time} keyword should not be specified unless its testing purpose.

The keyword arguments described below are system parameters, means these
parameters must be agreed between authenticator.

The keyword argument @var{start-time} is given, then it must be a UTC time
object of SRFI-19. The procedure computes returning one-time password according
to the given time. The defaule value is @code{(make-time time-utc 0 0)}.

The keyword argument @var{step} is given, then it must be a positive exact
integer. The procedure computes returning one-time password according
to the given time. The defaule value is @code{30}.

The keyword argument @var{step} is given, then it must be a positive exact
integer. The procedure computes returning one-time password according
to the given time. The defaule value is @code{30}.

The keyword argument @var{mode} is given, then it must be a hash algorithm name
either builtin or custom. The procedure computes returning one-time password
according to the given time. The defaule value is @code{SHA-1}.


Generates an Time-based one-time password.

@code{totp} is an alias of @code{generate-time-based-one-time-password}.
}
