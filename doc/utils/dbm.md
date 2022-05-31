[§2] (dbm) - Generic DBM interface {#dbm}
-------------

###### [!Library] `(dbm)` 

The library provides the generic interface to access DBM.

Sagittarius currently supports following DBM implementation;

(dbm dumb)
: DBM-like library. Inspired by Python's dbm.dumb. This library must be used
  as the last resort. It has poor performance and memory usage.



The following code shows a typical usage;

``````````scheme
(import (dbm))

;; Open the database
(define *db* (dbm-open (dbm-type->class 'dumb) :path "dumb.db"))

;; Put the value to the database
(dbm-put! *db* "key1" "value1")

;; Get the value from the database
(dbm-get *db* "key1")

;; Iterate over the database
(dbm-for-each *db* (lambda (key val) #| do something useful |#))

;; Close the database
(dbm-close *db*)
``````````

### [§3] Opening and closing a dbm database

###### [!Class] `<dbm>` 

An abstract class for DBM-like database. The class has the following
slots. It must be filled by `dbm-open`.

path
: Pathname of the dbm database. 

rw-mode
: Specifies read/write mode. Can be one of the following keywords:
  :read
  : The database will be opened in read-only mode.
  :write
  : The database will be opened in read-write mode. If the database file
  does not exist, `dbm-open` creates one.
  :write
  : The database will be opened in read-write mode. If the database file
  exists, `dbm-open` truncates it.
  The keywords are indication so actual implementation may not behave as it
  described.

key-convert
value-convert
: By default, you can use only strings for both key and values. With
  this option, however, you can specify how to convert other Scheme
  values to/from string to be stored in the database. The possible values
  are the followings: 
  #f
  : The default value. Keys (values) are not converted.
  They must be a string.
  #t
  : Keys (values) are converted to its string representation, using
  `write/ss`, to store in the database and convert back Scheme
  values, using `read/ss`, to retrieve from the database.
  a list of two procedures
  : Both procedure must take a single argument. The first procedure must
  receive a Scheme object and returns a string. It is used to convert 
  the keys (values) to store in the database. The second procedure must
  receive a string and returns a Scheme object. It is used to convert
  the stored data in the database to a Scheme object.



###### [!Metaclass] `<dbm-meta>` 

A metaclass of `<dbm>` and its subclasses.

###### [!Method] `dbm-open`  _(dbm_ _<dbm>)_

Opens a dbm database. _dbm_ must be an instance of one of the
concrete classes derived from the `<dbm>`.


###### [!Method] `dbm-open`  _(dbm-class_ _<dbm-meta>)_ _options_ _..._

A convenient method that creates dbm instance and opens it.

###### [!Method] `dbm-close`  _(dbm_ _<dbm>)_

Closes a dbm database _dbm_. If the database is not closed, then
the database file may not be synchronised properly. So it is user's
responsibility to close it.


###### [!Method] `dbm-closed?`  _(dbm_ _<dbm>)_

Returns true if the dbm database _dbm_ is closed, otherwise #f.

The returned value may be non boolean value.


###### [!Function] `dbm-type->class`  _dbmtype_

Returns DBM class if DBM implementation _dbmtype_ exists, otherwise
#f.

The _dbmtype_ must be a symbol that names the type of dbm implementation,
and the implementation library name must be `(dbm _dbmtype_)`. For
example, to get the _foo_ DBM then the library name must be 
`(dbm foo)`.


### [§3] Accessing a dbm database

Once a database is opened, you can use the following methods to access
individual key/value pairs. 

###### [!Method] `dbm-put!`  _(dbm_ _<dbm>)_ _key_ _value_

Put a _value_ with _key_

###### [!Method] `dbm-get`  _(dbm_ _<dbm>)_ _key_ _:optional_ _default_

Get a value associated with _key_. If no value exists for _key_and _default_ is specified, it will be returned. If no value exists for 
_key_ and _default_ is not specified, then an error will be raised.


###### [!Method] `dbm-exists?`  _(dbm_ _<dbm>)_ _key_

Return true value if a value exists for _key_, #f otherwise.

###### [!Method] `dbm-delete!`  _(dbm_ _<dbm>)_ _key_

Delete a value associated with _key_.

### [§3] Iterating on a dbm database

To walk over the entire database, following methods are provided. 

###### [!Method] `dbm-fold`  _(dbm_ _<dbm>)_ _procedure_ _knil_

The basic iterator. For each key/value pair, _procedure_ is called
as `_procedure_ key value r`, where _r_ is _knil_ for the
first call of _procedure_, and the return value of the previous call for
subsequent calls. Returns the result of the last call of _procedure_.
If no data is in the database, _knil_ is returned.


###### [!Method] `dbm-for-each`  _(dbm_ _<dbm>)_ _procedure_

For each key/value pair in the database _dbm_, _procedure_ is
called. The _procedure_ must accept 2 arguments, a key and a value
respectively. The result of _procedure_ is discarded.


###### [!Method] `dbm-map`  _(dbm_ _<dbm>)_ _procedure_

For each key/value pair in the database _dbm_, _procedure_ is
called. The _procedure_ must accept 2 arguments, a key and a value
respectively. The result of _procedure_ is accumulated to a list which
is returned as a result of `dbm-map`.


### [§3] Managing dbm database instance

###### [!Method] `dbm-db-exists?`  _(class_ _<dbm-meta>)_ _name_

Returns #t if a database of class _class_ specified by _name_exists.


###### [!Method] `dbm-db-remove`  _(class_ _<dbm-meta>)_ _name_

Removes an entire database of class _class_ specified by _name_.


###### [!Method] `dbm-db-copy`  _(class_ _<dbm-meta>)_ _from_ _to_

Copy a database of _class_ specified by _from_ to _to_.


###### [!Method] `dbm-db-move`  _(class_ _<dbm-meta>)_ _from_ _to_

Moves or renames a database of _class_ specified by _from_to _to_.


