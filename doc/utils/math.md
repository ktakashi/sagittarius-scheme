[§2] (math) - Mathematics library {#math}
-------------

This section describes matheatics operations which are used by `(crypto)`library.

This library also uses `libtomcrypt` as its implemention except prime
number operations.

###### [!Library] `(math)` 

The top most level library of mathematics. It exports all of procedures
from `(math random)`, `(math hash)`, `(math prime)` and
`(math helper)`

### [§3] Random number operations

###### [!Library] `(math random)` 

This library exports procedures for random numbers.

###### [!Function] `pseudo-random`  _type_ _:key_ _(seed_ _#f)_ _(reader_ _#f)_

_type_ must be a string.

Creates a pseudo random object (prng). If keyword argument _reader_ is given
it creates a custom prng. The _reader_ must be a procedure which accepts two
argument, a bytevector and integer. It must fill the given bytevector with
random numbers.

_type_ is used to specify the builtin pseudo random algorithm. The possible
algorithms are below:
###### [!Constant] `Yarrow` 
###### [!Constant] `Fortuna` 
###### [!Constant] `RC4` 
###### [!Constant] `SOBER-128` 
The following is also a psuedo random but this may or may not be cryptographic
random generator. 
###### [!Constant] `System` 

`System` prng uses platform random generator. For example, on 
Unix like environment, this uses `/dev/urandom` or `/dev/random` if
the first option is not available.


_seed_ is entropy of the pseudo random.

Note: each time if you create pseudo random, it returns exactly the same value.
For example:

``````````scheme
(do ((i 0 (+ i 1)) (r '() (cons (random (pseudo-random RC4) 10) r)))
    ((= i 10) r))
``````````
=> ``'(0 0 0 0 0 0 0 0 0 0)``

So if you need different number as I believe, you need to reuse the prng object
like this

``````````scheme
(let ((rc (pseudo-random RC4)))
  (do ((i 0 (+ i 1)) (r '() (cons (random rc 10) r)))
      ((= i 10) r)))
``````````
=> ``'(3 4 0 6 7 4 3 4 2 0)``

If you don't want to care this behaviour, use `secure-random` below.


###### [!Function] `secure-random`  _type_ _:key_ _(bits_ _128)_

_type_ must be one of the pseudo random algorithms.

Creates secure random object. 

_bit_ is initial entropy of the pseudo random. It must be in between 64 to
1028.


###### [!Function] `prng?`  _obj_
###### [!Function] `pseudo-random?`  _obj_
###### [!Function] `secure-random?`  _obj_

Returns #t if _obj_ is prng object, builtin pseudo random objcet,
custom random object or secure random object respectively.


###### [!Function] `random-seed-set!`  _prng_ _seed_

_seed_ must be a bytevector or integer.

Add entropy to given _prng_.


###### [!Function] `random`  _prng_ _size_ _:key_ _(read-size_ _100)_

Returns random number according to given _prng_ algorithm. The result
number will be less than _size_.

Keyword argument _read-size_ will be passed to `read-random-bytes`.


###### [!Function] `random`  _prng_ _size_

_size_ must a positive fixnum.

Reads _size_ bytes of random byte from _prng_.


###### [!Method] `prng-state`  _(prng_ _<prng>)_

Returns given _prng_'s state if the pseudo random implementation
allows.

For default built in pseudo randoms return #f.

NOTE: if \<secure-random> is implemented, then the pseudo random implementation
should not return the state.


###### [!Function] `read-random-bytes`  _prng_ _size_
###### [!Function] `read-random-bytes!`  _prng_ _bv_ _size_

Reads random bytes from given _prng_.

The first form creates fresh bytevector with size _size_.

The second form reads random bytes from _prng_ and sets the result into the
given _bv_ destructively.

If the second form is used, _bv_ must have the length at least _size_.


###### [!Function] `read-sys-random` _bits_

Returns given the `(+ (* bits 8) (if (zero? (mod bits 8)) 1 0))`
bits of random bytevector.

###### [!Function] `read-sys-random!` _bv_
###### [!Function] `read-sys-random!` _bv_ _start_
###### [!Function] `read-sys-random!` _bv_ _start_ _len_

_bv_ must be a bytevector.  
_start_ must be an exact non negative integer.  
_len_ must be an exact non negative integer.  

Reads random bytes from a system random generator and store it into _bv_.  
If the second form is used, then it stores from the _start_ position.  
If the third form is used, then it stores the _len_ bytes from the _start_.


#### [§4] Custom pseudo random operations

Since version 0.3.2, pseudo random also has custom operations. Similar with
cipher spi or hash algorithm.

The following example describes how to make it.

``````````scheme
;; the code snipet is from math/mt-random
(define-class <mersenne-twister> (<user-prng>)
  (;; The array for the state vector
   ;; using bytevector, it needs to be 64 bit aligned.
   (state :init-keyword :state :init-form (make-bytevector (* NN 8)))
   ;; mti==NN+1 means MT[NN] is not initialized
   (mti   :init-keyword :mti   :init-form (+ NN 1))))
(define-method initialize ((o <mersenne-twister>) initargs)
  (call-next-method)
  (let ((seed (get-keyword :seed initargs #f)))
    (slot-set! o 'set-seed! mt-set-seed)
    (slot-set! o 'read-random! mt-read-random!)
    (when seed
      (mt-set-seed o seed))))
``````````

User just need to set the slots `set-seed!` and `read-random!`. Then
other process is done by lower layer.

Following describes the meaning of these slots.

The slot `set-seed!` requires a procedure which accepts 2 arguments,
target pseudo random and _seed_. _seed_ must be bytevector.

The slot `read-random!` requires a pseudo which accepts 3 arguments,
target pseudo random _buffer_ and _bytes_. _buffer_ must be a
bytevector and have bigger size than given _bytes_. _bytes_ must be
a non negative fixnum.

NOTE: The custom pseudo random interface has been changed since version 0.3.6.
Make sure which version of Sagittarius your application using.

### [§3] Hash operations {#math.hash}

###### [!Library] `(math hash)` 

This library exports procedures for hash (digest) operations.

###### [!Function] `hash-algorithm`  _name_ _._ _options_

_name_ must be a string.

Creates a hash-algorithm object. _name_ specifies its algorithm. The
predefined algorithms are blow:
###### [!Constant] `WHIRLPOOL` 
###### [!Constant] `SHA-512` 
###### [!Constant] `SHA-384` 
###### [!Constant] `SHA-256` 
###### [!Constant] `SHA-224` 
###### [!Constant] `SHA-512/256` 
###### [!Constant] `SHA-512/224` 
###### [!Constant] `Tiger-192` 
###### [!Constant] `SHA-1` 
###### [!Constant] `RIPEMD-320` 
###### [!Constant] `RIPEMD-256` 
###### [!Constant] `RIPEMD-160` 
###### [!Constant] `RIPEMD-128` 
###### [!Constant] `MD5` 
###### [!Constant] `MD4` 
###### [!Constant] `MD2` 
###### [!Constant] `SHA-3-224` 
###### [!Constant] `SHA-3-256` 
###### [!Constant] `SHA-3-384` 
###### [!Constant] `SHA-3-512` 
###### [!Constant] `BLAKE2s-128` 
###### [!Constant] `BLAKE2s-160` 
###### [!Constant] `BLAKE2s-224` 
###### [!Constant] `BLAKE2s-256` 
###### [!Constant] `BLAKE2b-160` 
###### [!Constant] `BLAKE2b-256` 
###### [!Constant] `BLAKE2b-384` 
###### [!Constant] `BLAKE2b-512` 
If you want to use other hash algorithm, you can also create a new hash
algorithm. It is described the section
[Custom hash algorithm](#custom.hash).


###### [!Function] `hash-algorithm?`  _obj_

Return #t if _obj_ is hash-algorithm object otherwise #f.

###### [!Function] `hash-oid`  _hash-algorithm_

Return OID of given _hash-algorithm_ if it has otherwise #f.

#### [§4] User level APIs of hash operations

###### [!Function] `hash`  _type_ _bv_ _._ _options_

_type_ must be a string which specifies hash algorithms or
hash-algorithm object.

The `hash` procedure generates digest from given bytevector _bv_according to the given algorithm. The result digest will be a bytevector.

If _type_ is not a hash algorithm object nor predefined hash algorithm,
then _options_ will be passed to the custom hash algorithm creation.


###### [!Function] `hash-size`  _hash-algorithm_

Returns hash size of given _hash-algorithm_.

###### [!Function] `hash-block-size`  _hash-algorithm_

Returns hash block size of given _hash-algorithm_.

#### [§4] Low level APIs of hash operations

Most of the time User level APIs are sufficient enough, however for some cases,
for example multiple input datas, you might need to use these low leve APIs.

###### [!Function] `hash-init!`  _hash-algorithm_

Initialise given _hash-algorithm_.


###### [!Function] `hash-process!`  _hash-algorithm_ _bv_ _:optional_ _(start_ _0)_ _(end_ _(bytevector-length_ _bv))_

_bv_ must be a bytevector.

Process hash process with input data _bv_. The result will be stored in the
_hash-algorithm_.

Optional arguments _start_ and _end_ limits the input _bv_.


###### [!Function] `hash-done!`  _hash-algorithm_ _out_ _:optional_ _(start_ _0)_ _(end_ _(bytevector-length_ _bv))_

_out_ must be a bytevector and must have hash size which the
`hash-size` procedure returns.

Flushes stored hash result in _hash-algorithm_ into _out_.

Once this procedure is called _hash-algorithm_'s state will be changed. If
you want to reuse it, you need to call `hash-init!`.

Optional arguments _start_ and _end_ specifies the offset of
output bytevector.


#### [§4] Custom hash algorithm {#custom.hash}

Since version 0.3.1, user can create a custom hash algorithm. Similar with
cipher spi described section [Creating own cipher](#custom.cipher).

The following example describes how to make it.

``````````scheme
(import (rnrs) (sagittarius) (math) (clos user))
;; hash operations
(define (foo-init hash) #t)
(define (foo-process hash bv)
  (let ((len (bytevector-length bv)))
    (bytevector-copy! bv 0 (slot-ref hash 'buffer) 0 (min len 16))))
(define (foo-done hash out)
  (let ((v (integer->bytevector (equal-hash (slot-ref hash 'buffer)))))
    (bytevector-copy! v 0 out 0 (min 8 (bytevector-length v)))))

(define-class <foo-hash> (<user-hash-algorithm>)
  ((buffer :init-form (make-bytevector 16))))
(define-method initialize ((o <foo-hash>) initargs)
  (call-next-method)
  (slot-set! o 'init foo-init)
  (slot-set! o 'process foo-process)
  (slot-set! o 'done foo-done)
  (slot-set! o 'block-size 16)
  (slot-set! o 'hash-size 8)
  (slot-set! o 'oid #f)
  (slot-set! o 'state #f))
;; marker
(define-class <foo-marker> () ())
(define FOO (make <foo-marker>))
(register-hash FOO <foo-hash>)

;; use with APIs
(hash FOO (string->utf8 "hash")) ;; -> #vu8(245 221 54 232 0 0 0 0)
``````````

The slots `init`, `process` and `done` must be set with a
procedure which will be called by `hash-init!`, `hash-process!` and
`hash-done!` respectively. 

The procedure set to `process` and `done`  must accept 2 or 4
arguments. If it can accept 4 arguments, then optional argumens _start_and _end_ are passed. Implementation can use these information to 
ptimise memory allocation. If it can accept 2 argumens, then framework 
handles the range. In this case, uneccesarry allocation may happen.

The slots `block-size` and `hash-size` must be non negative exact
integer and will be returned by `hash-block-size` and `hash-size`procedures respectively.

The slot `oid` must be set #f or string which represent OID of the custom
hash algorithm. If you don't have it, it's better to set #f.

The slot `state` can be anything, this slot is for storing the hash state
if you need.

### [§3] Prime number operations

###### [!Library] `(math prime)` 

This library exports procedures for prime number operations.

###### [!Function] `prime?`  _q_ _:optional_ _(k_ _50)_ _(rand_ _(secure-random_ _RC4))_
###### [!Function] `is-prime?`  _q_ _:optional_ _(k_ _50)_ _(rand_ _(secure-random_ _RC4))_

Tests if given _q_ is a prime number or not.

This procedure uses Miller Rabin primality test. So there is slight possibility
to pass non prim number.

The optional argument _k_ is the test times. The default 50 makes failure
ratio very low. And _rand_ specifies whith pseudo random algorithm uses in
the test.

The latter form is for backward compatibility.


###### [!Function] `random-prime`  _size_ _:key_ _(prng_ _(secure-random_ _RC4))_

Find a prime number from _size_ bytes. So the minimum range will be
`1 <= p <= 251`.

Keyword argument _prng_ specifies which pseudo random uses to find a prime
number.


### [§3] Misc arithmetic operations

###### [!Library] `(math helper)` 

This library exports procedures for misc arithmetic operations.

###### [!Function] `mod-inverse`  _x_ _m_

Re exporting `mod-inverse` defined in `(sagittarius)` library.

###### [!Function] `mod-expt`  _x_ _e_ _m_

Re exporting `mod-expt` defined in `(sagittarius)` library.

