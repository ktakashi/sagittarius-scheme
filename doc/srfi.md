[§1] Supporting SRFIs {#srfi}
=============

SRFI is a great libraries, so there is no reason not to support. Without
exception Sagittarius also supports several SRFIs. The following list is the
supported SRFI. Documents are not written for now. So if you need to refer the
functions, please look for SRFI's site. I might write it later.

For now, I just put pointer to [the SRFI's web site](http://srfi.schemers.org/)

| SRFI number                                                 | Library name                                        |
|-------------------------------------------------------------|-----------------------------------------------------|
| [SRFI-0](http://srfi.schemers.org/srfi-0/srfi-0.html)       | (srfi :0 cond-expand)                               |
| [SRFI-1](http://srfi.schemers.org/srfi-1/srfi-1.html)       | (srfi :1 lists)                                     |
| [SRFI-2](http://srfi.schemers.org/srfi-2/srfi-2.html)       | (srfi :2 and-let\*)                                 |
| [SRFI-4](http://srfi.schemers.org/srfi-4/srfi-4.html)       | (srfi :4 numeric-vectors)[^SRFI-4]                  |
| [SRFI-6](http://srfi.schemers.org/srfi-6/srfi-6.html)       | (srfi :6 basic-string-ports)                        |
| [SRFI-8](http://srfi.schemers.org/srfi-8/srfi-8.html)       | (srfi :8 receive)                                   |
| [SRFI-11](http://srfi.schemers.org/srfi-11/srfi-11.html)    | (srfi :11 let-values)                               |
| [SRFI-13](http://srfi.schemers.org/srfi-13/srfi-13.html)    | (srfi :13 strings)                                  |
| [SRFI-14](http://srfi.schemers.org/srfi-14/srfi-14.html)    | (srfi :14 char-set)                                 |
| [SRFI-16](http://srfi.schemers.org/srfi-16/srfi-16.html)    | (srfi :16 case-lambda)                              |
| [SRFI-17](http://srfi.schemers.org/srfi-17/srfi-17.html)    | (srfi :17 generalized-set!)                         |
| [SRFI-18](http://srfi.schemers.org/srfi-18/srfi-18.html)    | (srfi :18 multithreading)                           |
| [SRFI-19](http://srfi.schemers.org/srfi-19/srfi-19.html)    | (srfi :19 time)                                     |
| [SRFI-22](http://srfi.schemers.org/srfi-22/srfi-22.html)    | This SRFI does not provide any library.             |
| [SRFI-23](http://srfi.schemers.org/srfi-23/srfi-23.html)    | (srfi :23 error)                                    |
| [SRFI-25](http://srfi.schemers.org/srfi-25/srfi-25.html)    | (srfi :25 multi-dimensional-arrays)                 |
| [SRFI-26](http://srfi.schemers.org/srfi-26/srfi-26.html)    | (srfi :26 cut)                                      |
| [SRFI-27](http://srfi.schemers.org/srfi-27/srfi-27.html)    | (srfi :27 random-bits)                              |
| [SRFI-29](http://srfi.schemers.org/srfi-29/srfi-29.html)    | (srfi :27 localization)                             |
| [SRFI-31](http://srfi.schemers.org/srfi-31/srfi-31.html)    | (srfi :31 rec)                                      |
| [SRFI-37](http://srfi.schemers.org/srfi-37/srfi-37.html)    | (srfi :37 args-fold)                                |
| [SRFI-38](http://srfi.schemers.org/srfi-38/srfi-38.html)    | (srfi :38 with-shared-structure)                    |
| [SRFI-39](http://srfi.schemers.org/srfi-39/srfi-39.html)    | (srfi :39 parameters)                               |
| [SRFI-41](http://srfi.schemers.org/srfi-41/srfi-41.html)    | (srfi :41 streams)                                  |
| [SRFI-42](http://srfi.schemers.org/srfi-42/srfi-42.html)    | (srfi :42 eager-comprehensions)                     |
| [SRFI-43](http://srfi.schemers.org/srfi-43/srfi-43.html)    | (srfi :43 vectors)                                  |
| [SRFI-45](http://srfi.schemers.org/srfi-45/srfi-45.html)    | (srfi :45 lazy)                                     |
| [SRFI-49](http://srfi.schemers.org/srfi-49/srfi-49.html)    | (srfi :49)                                          |
| [SRFI-57](http://srfi.schemers.org/srfi-57/srfi-57.html)    | (srfi :57 records)                                  |
| [SRFI-60](http://srfi.schemers.org/srfi-60/srfi-60.html)    | (srfi :60 integer-bits)                             |
| [SRFI-61](http://srfi.schemers.org/srfi-61/srfi-61.html)    | (srfi :61 cond)[^builtin]                           |
| [SRFI-64](http://srfi.schemers.org/srfi-64/srfi-64.html)    | (srfi :64 testing)                                  |
| [SRFI-69](http://srfi.schemers.org/srfi-69/srfi-69.html)    | (srfi :69 basic-hash-tables)                        |
| [SRFI-78](http://srfi.schemers.org/srfi-78/srfi-78.html)    | (srfi :78 lightweight-testing)                      |
| [SRFI-86](http://srfi.schemers.org/srfi-86/srfi-86.html)    | (srfi :86 mu-and-nu)                                |
| [SRFI-87](http://srfi.schemers.org/srfi-87/srfi-87.html)    | (srfi :87 case)                                     |
| [SRFI-98](http://srfi.schemers.org/srfi-98/srfi-98.html)    | (srfi :98 os-environment-variables)                 |
| [SRFI-99](http://srfi.schemers.org/srfi-99/srfi-99.html)    | (srfi :99 records)                                  |
| [SRFI-100](http://srfi.schemers.org/srfi-100/srfi-100.html) | (srfi :100 define-lambda-object)[^longname]         |
| [SRFI-101](http://srfi.schemers.org/srfi-101/srfi-101.html) | (srfi :101 random-access-lists)                     |
| [SRFI-105](http://srfi.schemers.org/srfi-105/srfi-105.html) | (srfi :105)[^SRFI-105]                              |
| [SRFI-106](http://srfi.schemers.org/srfi-106/srfi-106.html) | (srfi :106 socket)                                  |
| [SRFI-110](http://srfi.schemers.org/srfi-110/srfi-110.html) | (srfi :110)[^SRFI-110]                              |
| [SRFI-111](http://srfi.schemers.org/srfi-111/srfi-111.html) | (srfi :111 boxes)[^longname]                        |
| [SRFI-112](http://srfi.schemers.org/srfi-112/srfi-112.html) | (srfi :112 inquery)[^longname]                      |
| [SRFI-113](http://srfi.schemers.org/srfi-113/srfi-113.html) | (srfi :113 sets)[^longname]                         |
| [SRFI-114](http://srfi.schemers.org/srfi-114/srfi-114.html) | (srfi :114 comparators)[^longname]                  |
| [SRFI-115](http://srfi.schemers.org/srfi-115/srfi-115.html) | (srfi :115 regex)[^longname]                        |
| [SRFI-116](http://srfi.schemers.org/srfi-116/srfi-116.html) | (srfi :116 ilists)[^longname]                       |
| [SRFI-117](http://srfi.schemers.org/srfi-117/srfi-117.html) | (srfi :117 list-queues)[^longname]                  |
| [SRFI-120](http://srfi.schemers.org/srfi-120/srfi-120.html) | (srfi :120 timer)[^longname]                        |
| [SRFI-121](http://srfi.schemers.org/srfi-121/srfi-121.html) | (srfi :121 generators)[^longname]                   |
| [SRFI-123](http://srfi.schemers.org/srfi-123/srfi-123.html) | (srfi :123 generic-ref)                             |
| [SRFI-124](http://srfi.schemers.org/srfi-124/srfi-124.html) | (srfi :124 ephemerons)[^longname]                   |
| [SRFI-125](http://srfi.schemers.org/srfi-125/srfi-125.html) | (srfi :125 hashtables)[^longname]                   |
| [SRFI-126](http://srfi.schemers.org/srfi-126/srfi-126.html) | (srfi :126 r6rs-hashtables)[^longname]              |
| [SRFI-127](http://srfi.schemers.org/srfi-127/srfi-127.html) | (srfi :127 lazy-sequences)[^longname]               |
| [SRFI-128](http://srfi.schemers.org/srfi-128/srfi-128.html) | (srfi :128 comparators)[^longname]                  |
| [SRFI-129](http://srfi.schemers.org/srfi-129/srfi-129.html) | (srfi :129 titlecase)[^longname]                    |
| [SRFI-130](http://srfi.schemers.org/srfi-130/srfi-130.html) | (srfi :130 string-cursors)[^longname]               |
| [SRFI-131](http://srfi.schemers.org/srfi-131/srfi-131.html) | (srfi :131 records)[^longname]                      |
| [SRFI-132](http://srfi.schemers.org/srfi-132/srfi-132.html) | (srfi :132 sorting)[^longname]                      |
| [SRFI-133](http://srfi.schemers.org/srfi-133/srfi-133.html) | (srfi :133 vectors)[^longname]                      |
| [SRFI-134](http://srfi.schemers.org/srfi-134/srfi-134.html) | (srfi :134 ideque)[^longname]                       |
| [SRFI-135](http://srfi.schemers.org/srfi-135/srfi-135.html) | (srfi :135 texts)[^longname]                        |
| [SRFI-139](http://srfi.schemers.org/srfi-139/srfi-139.html) | (srfi :139 syntax-parameters)[^longname]            |
| [SRFI-141](http://srfi.schemers.org/srfi-141/srfi-141.html) | (srfi :141 integer-division)[^longname]             |
| [SRFI-142](http://srfi.schemers.org/srfi-142/srfi-142.html) | (srfi :142 bitwise)[^longname]                      |
| [SRFI-143](http://srfi.schemers.org/srfi-143/srfi-143.html) | (srfi :143 fixnums)[^longname]                      |
| [SRFI-144](http://srfi.schemers.org/srfi-144/srfi-144.html) | (srfi :144 flonums)[^longname]                      |
| [SRFI-145](http://srfi.schemers.org/srfi-145/srfi-145.html) | (srfi :145 assumptions)[^longname]                  |
| [SRFI-146](http://srfi.schemers.org/srfi-146/srfi-146.html) | (srfi :146 mapping)[^longname] and (srfi :146 hash) |
| [SRFI-151](http://srfi.schemers.org/srfi-151/srfi-151.html) | (srfi :151 bitwise-operations)[^longname]           |
| [SRFI-152](http://srfi.schemers.org/srfi-152/srfi-152.html) | (srfi :152 strings)                                 |
| [SRFI-154](http://srfi.schemers.org/srfi-154/srfi-154.html) | (srfi :154 dynamic-extents)[^longname]              |
| [SRFI-156](http://srfi.schemers.org/srfi-156/srfi-156.html) | (srfi :156 predicate-combiners)[^longname]          |
| [SRFI-158](http://srfi.schemers.org/srfi-158/srfi-158.html) | (srfi :158 generators-and-accumulators)[^longname]  |
| [SRFI-159](http://srfi.schemers.org/srfi-159/srfi-159.html) | (srfi :159)                                         |
| [SRFI-160](http://srfi.schemers.org/srfi-160/srfi-160.html) | (srfi :160)[^SRFI-160]                              |
| [SRFI-193](http://srfi.schemers.org/srfi-193/srfi-193.html) | (srfi :193 command-line)[^longname]                 |
| [SRFI-195](http://srfi.schemers.org/srfi-195/srfi-195.html) | (srfi :195 boxes)[^longname]                        |
| [SRFI-197](http://srfi.schemers.org/srfi-197/srfi-197.html) | (srfi :197 pipeline)[^longname]                     |
| [SRFI-210](http://srfi.schemers.org/srfi-210/srfi-210.html) | (srfi :210 multiple-values)[^longname]              |
| [SRFI-219](http://srfi.schemers.org/srfi-219/srfi-219.html) | (srfi :219 define)[^longname][^builtin]             |
| [SRFI-230](http://srfi.schemers.org/srfi-230/srfi-230.html) | (srfi :230 atomic)                                  |


[^SRFI-4]: This SRFI also contains reader macro described below this section.

[^SRFI-49]: The library exports `srfi-49-read`, `srfi-49-load` procedures. 
            And also be able to replace reader, For more detail,
            see [(sagittarius reader) - reader macro library](#lib.sagittarius.reader).

[^builtin]: The syntax provided by this SRFI is supported by the builtin syntax.
            This SRFI can be used for portability.

[^longname]: The long name is Sagittarius specific and the specified library name is `(srfi :_number_)`.
             So for the portability it's better to use the specified one.
             The long name is taken from the SRFI name.

[^SRFI-105]: The library exports `curly-infix-read` and `neoteric-read`
             procedures. These procedures read SRFI-105 the infix style code that
             SRFI-105 specifying. And this also exports reader macros, you can 
             activate it with `#!read-macro=srfi/:105` or `#!read-macro=curly-infix`.  
             Even though the specification said it MUST support `#!curly-infix`,
             however the library just ignore and not activate the reader macros. 
             So you need to explicitly write the one mentioned above. 
             To keep your code portable between implementations that support this SRFI, 
             you need to write both style as following;
             ``````````scheme
             ;; write both
             #!read-macro=curly-infix
             #!curly-infix
             ``````````
             The order doesn't matter, Sagittarius just ignores the latter style.
[^SRFI-110]: The library exports a replacible reader. To use it write following hash-bang directives.
             ``````````scheme
             ;; write both for compatibility
             #!reader=sweet
             #!sweet
             ``````````
             The order doesn't matter, Sagittarius just ignores the latter style.

[^SRFI-152]: This library doesn't extend comparisons, thus it behaves as the SRFI specifies.
             (e.g. Passing 0 or 1 argument to `string=?` raises an error)

[^SRFI-160]: The short name is Sagittarius specific which provides all the bindings
             from the below libraries which are specified in the SRFI:

             - `(srfi :160 base)`
             - `(srfi :160 u8)`
             - `(srfi :160 s8)`
             - `(srfi :160 u16)`
             - `(srfi :160 s16)`
             - `(srfi :160 u32)`
             - `(srfi :160 s32)`
             - `(srfi :160 u64)`
             - `(srfi :160 s64)`
             - `(srfi :160 f32)`
             - `(srfi :160 f64)`
             - `(srfi :160 c64)`
             - `(srfi :160 c128)`


Each library can be imported like this: 
``(import (srfi :1))``

So you don't have to type the long name.

All libraries have R7RS style library name as well. So SRFI-1 can be imported
like this:
``(import (srfi 1))``

[§2] Reader macros for SRFIs {#srfi.reader.macro}
-------------

### [§3] SRFI-4

The SRFI-4 also defines its reader macro. Sagittarius also suppots these. It
defines tagged vector and the tags can be `s8`, `u8`, `s16`,
`u16`, `s32`, `u32`, `s64`, `u64`, `f32` or
`f64`. For each value of tags, the external representation of instances of
the vector is `#_tag_(... elements ...)`On Sagittarius, these reader macros are not automatically enabled. You need to
explicitly import it. For more detail, see
[(sagittarius reader) - reader macro library](#lib.sagittarius.reader).

