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
  + <builtin-random-generator>
      + <secure-random-generator>
  + <custom-random-generator>
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

###### [!Function] `random-generator-randomize!`
###### [!Function] `random-generator-random-integer`
###### [!Function] `random-generator-state`
###### [!Function] `pseudo-random-generator`
###### [!Function] `secure-random-generator`

### [§4] Custom random generator


###### [!Class] `<custom-random-generator>`
###### [!Generic] `make-custom-random-generator`
