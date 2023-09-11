[§2] (security password) - Password policy {#security.policy}
------------------------------------------

###### [!Library] `(security password)`

Strong password is a base component for strong security. This library
provides a password policies, validator and generator.

This example shows how to validate if the given password is compliant
to the given password policy.

```scheme
(import (rnrs)
        (security password))

;; Password requires
;; - at least length of 16 characters,
;; - at least one lower char (a-z)
;; - at least one upper char (A-Z)
;; - at least one digit char (0-9)
;; - at least one symbol char ($#!)
(define policy (password-policies
                (make-length-policy 16)
                (make-lower-case-policy 1)
                (make-upper-case-policy 1)
                (make-digit-policy 1)
                (make-symbol-policy 1)))

(password-policy-compliant? policy "password")         ;; -> #f
(password-policy-compliant? policy "password12345678") ;; -> #f
(password-policy-compliant? policy "pa$$word12345678") ;; -> #f
(password-policy-compliant? policy "Pa$$word12345678") ;; -> #t
```

And this example shows how to generate a password which is compliant
to the given password policy.

```scheme
(import (rnrs)
        (security password))

;; Password requires
;; - at least length of 16 characters,
;; - at least one lower char (a-z)
;; - at least one upper char (A-Z)
;; - at least one digit char (0-9)
;; - at least one symbol char ($#!)
(define policy (password-policies
                (make-length-policy 16)
                (make-lower-case-policy 1)
                (make-upper-case-policy 1)
                (make-digit-policy 1)
                (make-symbol-policy 1)))

(generate-password policy) ;; -> "1uI#tfUzL_]H-<$%" (as an example)
```

The password generation is based on a random seed, so the result
may differ on every invocation.

### [§3] Password policy operations

###### [!Function] `password-policy?` _obj_

Returns `#t` if the given _obj_ is a password policy, otherwise `#f`.

###### [!Function] `password-policies` (_policy_ `password-policy?`) _..._

Construct a composite password policy which contains _policies_.

###### [!Function] `password-policy-compliant?` (_policy_ `password-policy?`) (_password_ `string?`)

Returns `#t` if the given _password_ is compliant to the given _policy_,
otherwise `#f`.

###### [!Function] `length-policy?` _obj_

Returns `#t` if the given _obj_ is a length policy, otherwise `#f`.

###### [!Function] `make-length-policy` _n_

Creates a length policy of length _n_.

###### [!Function] `password-policy-length` _policy_

Returns length of the policy, iff _policy_ is a length policy or
a composite policy which contains length policy, otherwise `#f`.

###### [!Function] `character-policy?` _obj_

Returns `#t` if the given _obj_ is a character policy, otherwise `#f`.

###### [!Function] `make-character-policy` (_cset_ `char-set?`) _at-least_

Creates a character policy of from _cset_ which requires at least _at-least_
characters.

###### [!Function] `password-policy-char-set` _policy_

Returns charset of the policy, iff _policy_ is a character policy or
a composite policy which contains character policy, otherwise `#f`.

If the _policy_ contains multiple character policy, then the returning
charset is a unioned charset.

###### [!Function] `make-lower-case-policy` _at-least_

Creates a character policy of `[a-z]` requires at least _at-least_.

###### [!Function] `make-upper-case-policy` _at-least_

Creates a character policy of `[A-Z]` requires at least _at-least_.

###### [!Function] `make-symbol-policy` _at-least_

Creates a character policy of ASCII symbols requires at least _at-least_.

###### [!Function] `make-digit-policy` _at-least_

Creates a character policy of `[0-9]` requires at least _at-least_.

###### [!Function] `generate-password` _policy_ :key _prng_

Generates a password which is compliant to _policy_.

If the keyword argument _prng_ is specified, then it must be a 
`peudo-random-generator` defined in `(sagittarius crypto random)`.

NOTE: using weak PRNG is not recommended. By default, the procedure
uses ChaCha20

The procedure uses below parameter if the given _policy_ doesn't
contain some policies.

###### [!Parameter] `*password-policy:default-length*`

Default password length, if the policy doesn't contain length policy.

###### [!Parameter] `*password-policy:default-char-set*`

Default password charset, if the policy doesn't contain character policy.

###### [!Function] `password-policy-entropy` _policy_

Calculates password entropy of the given _policy_.

The calculation formula: `log(expt(cs, l) 2)` where `cs` is the
number of possible characters of the _policy_, and `l` is the
length of the password.

###### [!Function] `password-policy->predicate` _policy_

A convenient procedure. Returns a predicate which accepts a string
and check if the given string is a compliant password or not.


### [§3] Generator integration

###### [!Function] `password-policy->generator` _policy_

Creates a generator which generates string. Generated strings are
compliant to the given _policy_.

This procedure can be used with `(sagittarius generators)` library.
Suppose, you want to generate 5 random password which are compliant
to your password policy, then it can be written like this

```scheme
(import (rnrs)
        (security password))

(define policy (password-policies
                (make-length-policy 16)
                (make-lower-case-policy 1)
                (make-upper-case-policy 1)
                (make-digit-policy 1)
                (make-symbol-policy 1)))

(let ((g (gtake (password-policy->generator policy) 5)))
  (generator->list g))
;; a list containing 5 elements, all of them are compliant to policy
```
