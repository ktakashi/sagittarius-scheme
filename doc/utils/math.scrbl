@; -*- coding: utf-8 -*-
@subsection[:tag "math"]{(math) - Mathematics library}

This section describes matheatics operations which are used by @code{(crypto)}
library.

This library also uses @code{libtomcrypt} as its implemention except prime
number operations.

@define[Library]{@name{(math)}}
@desc{The top most level library of mathematics. It exports all of procedures
from @code{(math random)}, @code{(math hash)}, @code{(math prime)} and
@code{(math helper)}
}

@subsubsection{Random number operations}

@define[Library]{@name{(math random)}}
@desc{This library exports procedures for random numbers.}

@define[Function]{@name{pseudo-random} @args{type :key (seed #f) (reader #f)}}
@desc{@var{type} must be a string.

Creates a pseudo random object (prng). If keyword argument @var{reader} is given
it creates a custom prng. The @var{reader} must be a procedure which accepts two
argument, a bytevector and integer. It must fill the given bytevector with
random numbers.

@var{type} is used to specify the builtin pseudo random algorithm. The possible
algorithms are below:
@define[Constant]{@name{Yarrow}}
@define[Constant]{@name{Fortuna}}
@define[Constant]{@name{RC4}}
@define[Constant]{@name{SOBER-128}}

@var{seed} is entropy of the pseudo random.

Note: each time if you create pseudo random, it returns exactly the same value.
For example:
@codeblock[=> '(0 0 0 0 0 0 0 0 0 0)]{
(do ((i 0 (+ i 1)) (r '() (cons (random (pseudo-random RC4) 10) r)))
    ((= i 10) r))
}
So if you need different number as I believe, you need to reuse the prng object
like this
@codeblock[=> '(3 4 0 6 7 4 3 4 2 0)]{
(let ((rc (pseudo-random RC4)))
  (do ((i 0 (+ i 1)) (r '() (cons (random rc 10) r)))
      ((= i 10) r)))
}
If you don't want to care this behaviour, use @code{secure-random} below.
}

@define[Function]{@name{secure-random} @args{type :key (bits 128)}}
@desc{@var{type} must be one of the pseudo random algorithms.

Creates secure random object. 

@var{bit} is initial entropy of the pseudo random. It must be in between 64 to
1028.
}

@define[Function]{@name{prng?} @args{obj}}
@define[Function]{@name{pseudo-random?} @args{obj}}
@define[Function]{@name{custom-random?} @args{obj}}
@define[Function]{@name{secure-random?} @args{obj}}
@desc{Returns #t if @var{obj} is prng object, builtin pseudo random objcet,
custom random object or secure random object respectively.
}

@define[Function]{@name{random-seed-set!} @args{prng seed}}
@desc{@var{seed} must be a bytevector or integer.

Add entropy to given @var{prng}.
}

@define[Function]{@name{random} @args{prng size :key (read-size 100)}}
@desc{Returns random number according to given @var{prng} algorithm. The result
number will be less than @var{size}.

Keyword argument @var{read-size} will be passed to @code{read-random-bytes}.
}

@define[Function]{@name{random} @args{prng size}}
@desc{@var{size} must a positive fixnum.

Reads @var{size} bytes of random byte from @var{prng}.
}

@subsubsection{Hash operations}

@define[Library]{@name{(math hash)}}
@desc{This library exports procedures for hash (digest) operations.}

@define[Function]{@name{hash-algorithm} @args{name :key (process #f)}}
@desc{@var{name} must be a string.

Creates a hash-algorithm object. @var{name} specifies its algorithm. The
possible algorithms are blow:
@define[Constant]{@name{WHIRLPOOL}}
@define[Constant]{@name{SHA-512}}
@define[Constant]{@name{SHA-384}}
@define[Constant]{@name{RIPEMD-320}}
@define[Constant]{@name{SHA-256}}
@define[Constant]{@name{RIPEMD-256}}
@define[Constant]{@name{SHA-224}}
@define[Constant]{@name{SHA-224}}
@define[Constant]{@name{Tiger-192}}
@define[Constant]{@name{SHA-1}}
@define[Constant]{@name{RIPEMD-160}}
@define[Constant]{@name{RIPEMD-128}}
@define[Constant]{@name{MD5}}
@define[Constant]{@name{MD4}}
@define[Constant]{@name{MD2}}

If keyword argument @var{process} is given, it creates custom hash-algorithm
object. It uses @var{process} as its hash provider. The @var{process} must be
a procedure and accept 4 arguments. The first argument is state of its hash
state. The second and the third are #f or a bytevector. The forth is processing
stage, one of @code{init}, @code{process}, @code{done}, @code{size} or
@code{oid}.

Note: @var{process} argument mechanism is experimental, this is not well tested.
}

@define[Function]{@name{hash-algorithm?} @args{obj}}
@desc{Return #t if @var{obj} is hash-algorithm object otherwise #f.}

@define[Function]{@name{hash-oid} @args{hash-algorithm}}
@desc{Return OID of given @var{hash-algorithm} if it has otherwise #f.}

@sub*section{User level APIs of hash operations}

@define[Function]{@name{hash} @args{type bv :key (process #f)}}
@desc{@var{type} must be a string which specifies hash algorithms or
hash-algorithm object.

The @code{hash} procedure generates digest from given bytevector @var{bv}
according to the given algorithm. The result digest will be a bytevector.

If keyword argument @var{process} is given and @var{type} is not hash-algorithm
object. It will be used as a hash provider.
}

@define[Function]{@name{hash-size} @args{hash-algorithm}}
@desc{Returns hash size of given @var{hash-algorithm}.}

@sub*section{Low level APIs of hash operations}

Most of the time User level APIs are sufficient enough, however for some cases,
for example multiple input datas, you might need to use these low leve APIs.

@define[Function]{@name{hash-init!} @args{hash-algorithm}}
@desc{Initialise given @var{hash-algorithm}.
}

@define[Function]{@name{hash-process!} @args{hash-algorithm bv}}
@desc{@var{bv} must be a bytevector.

Process hash process with input data @var{bv}. The result will be stored in the
@var{hash-algorithm}.
}

@define[Function]{@name{hash-done!} @args{hash-algorithm out}}
@desc{@var{out} must be a bytevector and must have hash size which the
@code{hash-size} procedure returns.

Flushes stored hash result in @var{hash-algorithm} into @var{out}.

Once this procedure is called @var{hash-algorithm}'s state will be changed. If
you want to reuse it, you need to call @code{hash-init!}.
}

@subsubsection{Prime number operations}

@define[Library]{@name{(math prime)}}
@desc{This library exports procedures for prime number operations.}

@define[Function]{@name{is-prime?}
 @args{q :optional (k 50) (rand (secure-random RC4))}}
@desc{Tests if given @var{q} is a prime number or not.

This procedure uses Miller Rabin primality test. So there is slight possibility
to pass non prim number.

The optional argument @var{k} is the test times. The default 50 makes failure
ratio very low. And @var{rand} specifies whith pseudo random algorithm uses in
the test.
}

@define[Function]{@name{random-prime}
 @args{size :key (prng (secure-random RC4))}}
@desc{Find a prime number from @var{size} bytes. So the minimum range will be
@code{1 <= p <= 251}.

Keyword argument @var{prng} specifies which pseudo random uses to find a prime
nubmer.
}

@subsubsection{Misc arithmetic operations}

@define[Library]{@name{(math helper)}}
@desc{This library exports procedures for misc arithmetic operations.}

@define[Function]{@name{mod-inverse} @args{m k}}
@desc{Calculates @code{2^@var{k} mod @var{m}}}

@define[Function]{@name{mod-expt} @args{x n d}}
@desc{Calculates @code{@var{x}^@var{n} mod @var{d}}}
