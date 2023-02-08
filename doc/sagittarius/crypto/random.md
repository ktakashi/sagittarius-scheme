[§3] Random library - (sagittarius crypto random) {#sagittarius.crypto.random}
------------------------------------------------------

The random library provides cryptographically secure random number
generator (CSPRNG) operations.

###### [!Library] `(sagittarius crypto random)`

The random library, this library exports the procedures listed below sections.

### [§4] PRNG descriptor

PRNG descriptor is a representation of pseudo random generator (PRNG).

###### [!Library] `prng-descriptor?` _obj_

Returns `#t` if the given _obj_ is a PRNG descriptor, otherwise `#f`.

###### [!Library] `prng-descriptor-name` (_descriptor_ `prng-descriptor?`)

Returns a human readable name of the given _descriptor_.

---
Below are the supported PRNGs.

###### [!PRNG descriptor] `*prng:yarrow*`
###### [!PRNG descriptor] `*prng:fortuna*` 
###### [!PRNG descriptor] `*prng:rc4*` 
###### [!PRNG descriptor] `*prng:sober-128*`
###### [!PRNG descriptor] `*prng:chacha20*`

These are the PRNG algorithms, Yarrow, Fortuna RC4, Sober-128 and ChaCha20,
respectively.

###### [!PRNG descriptor] `*prng:system*`

This is a system, platform dependent, PRNG. For example, this PRNG reads
from file descriptor of `/dev/urandom` on Linux or other POSIX environment
if the device exists.

Ths PRNG is slow as it requires I/O. Also can't be pseudo random generator
described below.

### [§4] Random generator

Random generator has a hierachical class structure. If you only use
the provided PRNGs, you don't have to care about it. However, if you
want to make a custom algorithm which is not supported by the library,
then you may need to know how to enhance it. Creating a custom random 
generator is described the below section.

```
                               <random-generator>
                                       |
           +---------------------------+------------------------+
           |                           |                        |
<builtin-random-generator> <secure-random-generator> <custom-random-generator>
           |                           |
           +-------------+-------------+
                         |
         <builtin-secure-random-generator>
```


###### [!Function] `random-generator?` _obj_

Returns `#t` if the given _obj_ is a random generator, otherwise `#f`.

###### [!Function] `builtin-random-generator?` _obj_

Returns `#t` if the given _obj_ is a builtin random generator, otherwise `#f`.

###### [!Function] `secure-random-generator?` _obj_

Returns `#t` if the given _obj_ is a secure random generator, otherwise `#f`.

###### [!Function] `custom-random-generator?` _obj_

Returns `#t` if the given _obj_ is a custom random generator, otherwise `#f`.

###### [!Function] `random-generator-read-random-bytes` (_random-generator_ `random-generator?`) _size_

Returns a bytevector of length _size_ filled with random data read from the
given _random-generator_.

###### [!Function] `random-generator-read-random-bytes!` (_random-generator_ `random-generator?`) (_out_ `bytevector?`) :optional _start_ _length_

Reads random data from _random-generator_, and fill them into _out_.  
If optional arguments _starts_ and _length_ are specified, then it specifies
the range of the output bytevector.

###### [!Function] `random-generator-randomize!` (_random-generator_ `random-generator?`) (_seed_ `bytevector?`) :optional _start_ _length_

Add entropy _seed_ to the given _random-generator_.  
If optional arguments _starts_ and _length_ are specified, then it specifies
the range of the _seed_ bytevector.

###### [!Function] `random-generator-random-integer` (_random-generator_ `random-generator?`) _bound_

Returns a random integer of range 0 <= n <= _bound_.

###### [!Function] `pseudo-random-generator` _descriptor_ _opts_ _..._
###### [!Function] `secure-random-generator` _descriptor_ _opts_ _..._

Creates a pseudo or secure random generator, respectively.  
If the given _descriptor_ is PRNG descrptor, then these procedure
creates builtin random generator ignoreing the given _opts_.  
If the given _descriptor_ is not PRNG descrptor, then these procedure
call `make-custom-random-generator` method passing _descriptor_ and
_opts_.

If `secure-random-generator` is used, then `make-custom-random-generator`
receives `:secure-random` keyword argument with value of `#t`, so that
the receiver can handle it properly.

###### [!Method] `random-generator-state` `<random-generator>`

Gets the state of given `<random-generator>` if it's supported.  
By default this method returns `#f`, means not supported.

###### [!Method] `random-generator-state` `<random-generator>` `<any>`

Sets the given state represented by `<any>` to `<random-generator>` 
if it's supported.  
By default this method does nothing and returns `#f`, means not supported.

### [§4] Custom random generator

###### [!Class] `<custom-random-generator>`
###### [!Generic] `make-custom-random-generator`
