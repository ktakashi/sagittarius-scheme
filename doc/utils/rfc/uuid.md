[§2] (rfc uuid) - UUID generate library {#rfc.uuid}
-------------

###### [!Library] `(rfc uuid)` 

This library provides RFC 4122 a Universally Unique IDentifier (UUID)
URN Namespace procedures.

###### [!Class] `<uuid>` 

The class representing UUID.

### [§3] Predicates

###### [!Function] `uuid?`  _obj_

Returns #t if the _obj_ is an instance of `<uuid>`, otherwise
 #f.

###### [!Function] `uuid=?`  _uuid1_ _uuid2_

Compares given 2 uuids and return #t if both have the same values,
otherwise #f.

### [§3] Constructors

###### [!Function] `make-null-uuid` 

Creates a null (empty) uuid.

The returning object will be represented like this UUID;

`00000000-0000-0000-0000-000000000000`.


###### [!Function] `make-v1-uuid` 
###### [!Function] `make-v3-uuid`  _namespace_ _name_
###### [!Function] `make-v4-uuid`  _:optional_ _prng_
###### [!Function] `make-v5-uuid`  _namespace_ _name_

Creates version 1, 3, 4 and 5 UUIDs respectively.

For version 3 and 5, procedures need to take 2 arguments, _namespace_and _name_. _namespace_ must be a UUID object, and _name_ must be a
string.

For version 4, it can take an optional argument _prng_ which specifies
pseudo random generator. The default value is `(*uuid-random-state*)`.


### [§3] Predefined namespaces and parameters

###### [!Constant] `+namespace-dns+` 
###### [!Constant] `+namespace-url+` 
###### [!Constant] `+namespace-oid+` 
###### [!Constant] `+namespace-x500+` 

Constant predefined namespace of UUIDs.

###### [!Variable] `*uuid-random-state*` 

Pseudo random generator used by version 4 UUID.

###### [!Variable] `*uuids-per-tick*` 

The number of allowed UUIDs in the same time. Used by version 1 UUID.

The default value is 1000.


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


