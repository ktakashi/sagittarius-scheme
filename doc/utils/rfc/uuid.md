[§2] (rfc uuid) - UUID generate library {#rfc.uuid}
-------------

###### [!Library] `(rfc uuid)` 

This library provides RFC 9562 a Universally Unique IDentifier (UUID)
URN Namespace procedures.

###### [!Class] `<uuid>` 

The class representing UUID.

### [§3] Predicates

###### [!Function] `uuid?`  _obj_

Returns #t if the _obj_ is an instance of `<uuid>`, otherwise
 #f.

###### [!Function] `v1-uuid?`  _obj_
###### [!Function] `v3-uuid?`  _obj_
###### [!Function] `v4-uuid?`  _obj_
###### [!Function] `v5-uuid?`  _obj_
###### [!Function] `v6-uuid?`  _obj_ **[@since] `0.9.12`**
###### [!Function] `v7-uuid?`  _obj_ **[@since] `0.9.12`**

Returns `#t` if the _obj_ is an instance `<uuid>` and the named version,
otherwise `#f`.

###### [!Function] `uuid=?`  _uuid1_ _uuid2_

Compares given 2 uuids and return #t if both have the same values,
otherwise #f.

### [§3] Constructors

###### [!Function] `make-null-uuid` 
###### [!Function] `make-nil-uuid` **[@since] `0.9.12`**

Creates a null (empty) uuid.

The returning object will be represented like this UUID;

`00000000-0000-0000-0000-000000000000`.

###### [!Function] `make-max-uuid` **[@since] `0.9.12`**

Creates a max uuid.

The returning object will be represented like this UUID;

`FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF`.


###### [!Function] `make-v1-uuid` :optional _timestamp_
###### [!Function] `make-v3-uuid` _namespace_ _name_
###### [!Function] `make-v4-uuid` :optional _prng_
###### [!Function] `make-v5-uuid` _namespace_ _name_
###### [!Function] `make-v6-uuid` :optional _timestamp_ **[@since] `0.9.12`**
###### [!Function] `make-v7-uuid` :optional _timestamp_ _prng_ **[@since] `0.9.12`**

Creates version 1, 3, 4, 5, 6 and 7 UUIDs respectively.

For version 1 and 6, the procedures can take an optional argument
_timestamp_ which is a 60bit timestamp integer.

For version 3 and 5, the procedures need to take 2 arguments,
_namespace_and _name_. _namespace_ must be a UUID object, and _name_
must be a string.

For version 4, it can take an optional argument _prng_ which specifies
pseudo random generator. The default value is `(*uuid-random-state*)`.

For version 7, it can take optional arguments _timestamp_ and _prng_.
The _timestamp_ is 48bit unix time in milliseconds, The default value
is current time.
The _prng_ is the same as version 4.


### [§3] Predefined namespaces and parameters

###### [!Constant] `+namespace-dns+` 
###### [!Constant] `+namespace-url+` 
###### [!Constant] `+namespace-oid+` 
###### [!Constant] `+namespace-x500+` 

Constant predefined namespace of UUIDs.

###### [!Parameter] `*uuid-random-state*` 

Pseudo random generator used by version 4 UUID.

###### [!Parameter] `*uuids-per-tick*` 

The number of allowed UUIDs in the same time. Used by version 1 UUID.

The default value is 1000.

###### [!Parameter] `*uuid-node*` **[@since] `0.9.12`**
###### [!Parameter] `*uuid-clock-seq*` **[@since] `0.9.12`**

Parameters for testing purpose. It should not be used in a usual occasion.


### [§3] Converters

###### [!Function] `uuid->bytevector`  _uuid_

Returns bytevector converted from given _uuid_.

``(uuid->bytevector (make-null-uuid))`` => ``#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)``



###### [!Function] `uuid->string`  _uuid_

Returns string represented uuid converted from given _uuid_.

``(uuid->string (make-null-uuid))`` => ``00000000-0000-0000-0000-000000000000``



###### [!Function] `bytevector->uuid`  _bv_

Returns uuid object generated from _bv_.

Given bytevector must have length at least 16.


###### [!Function] `string->uuid`  _string_

Returns uuid object generated from _string_.

Given _string_ must be proper format of UUID defined in RFC 4122.

###### [!Function] `uuid->urn-format`  _uuid_

Returns URN formatted string of given _uuid_.


