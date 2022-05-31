[§2] (dbi) - Database independent access layer {#dbi}
-------------

###### [!Library] `(dbi)` 

This library provides database independent access procedures. The
database specific operations are provided in database driver (DBD) libraries.


Following example describes how to query a database using `(dbd odbc)`DBD library.

``````````scheme
(import (dbi))
(define conn (dbi-connect "dbi:odbc:server=XE"
                          :username "username"
                          :password "password"))
(let* ((query (dbi-prepare conn "SELECT * FROM table WHERE id > ?"))
       (result (dbi-execute-query! query 1)))
  (do ((row (dbi-fetch! result) (dbi-fetch! result)))
      ((not row))
   (vector-for-each (lambda (col) (print col)) row)))
``````````

There is nothing specific to the underlying database system except the
argument `"dbi:odbc:server=XE"` passed to `dbi-connect`, from which
`dbi` library figures out that it is an access to `odbc`, loads
`(dbd odbc)` library and let it handle the specific operations.

If you want to use other database named _boo_, then you just need to pass
`"dbi:_boo_:_parameter_"` to `dbi-connect` instead. As long
as you have `(dbd boo)` installed then everything stays the same.

### [§3] DBI user APIs

#### [§4] Database connection

###### [!Function] `dbi-connect`  _dsn_ _._ _rest_

Connects to a database using a data source specified by _dsn_ 
(data source name). _dsn_ is a string with the following syntax.

`dbi:_driver_:_options_`_driver_ part names a specific driver. You need to have a corresponding
driver library, `(dbd _driver_)`, installed in your system.

Interpretation of the _options_ part is up to the driver. Usually it is
in the form of `key1=value1;key2=value2;...`. However the DBD
implementations can have different format so you need to check the document
of each driver for exact specification of _options_.

_rest_ argument will be passed to the underlying procedure.

NOTE: _username_, _password_ and _auto-commit_ are strongly
encouraged to be implemented in the DBD library.

If a connection to the database is successfully established, a connection
object is returned.


###### [!Method] `dbi-open?`  _c_ _<dbi-connection>_

Checks if the given connection is still open.

The method shall return #t if the given connection is still open, otherwise
it shall return #f.


###### [!Method] `dbi-close`  _c_ _<dbi-connection>_

Closes a connection to the database.

NOTE: Users are strongly encouraged to close a connection explicitly. DBD
might not close opened connections automatically.


###### [!Method] `dbi-commit!`  _(c_ _<dbi-connection>)_
###### [!Method] `dbi-rollback!`  _(c_ _<dbi-connection>)_

Commits or rollback transactions on the given connection, respectively.

#### [§4] Preparing and executing queries

###### [!Method] `dbi-prepare`  _conn_ _sql_ _._ _args_

From a string representation of SQL _sql_, creates and returns a
query object for the database connection _conn_.

_sql_ may contain parameter slot, denoted by `?`.

``(dbi-prepare conn "insert into tab (col1, col2) values (?, ?)")``

``(dbi-prepare conn "select * from tab where col1 = ?")``

If _args_ is not null, then the procedure shall bind given parameters
to the place holder.


###### [!Method] `dbi-open?`  _(query_ _<dbi-query>)_

Checks if the given query is still open.

The method shall return #t if the given query is still open, otherwise
it shall return #f.


###### [!Method] `dbi-close`  _(query_ _<dbi-query>)_

Closes a query.

NOTE: Users are strongly encouraged to close a query explicitly. DBD
might not close opened query automatically.


###### [!Method] `dbi-bind-parameter!`  _query_ _index_ _value_

Binds the given _value_ to _query_ at _index_.

The procedure shall accept integer _index_ and may accept other type of
_index_.


###### [!Method] `dbi-execute!`  _query_ _._ _args_
###### [!Method] `dbi-execute-query!`  _query_ _._ _args_

Executes given _query_. If the _args_ is not null, then
procedure shall bind the given _args_ as its parameters.

The `dbi-execute!` shall return a integer value representing affected
row count.

The `dbi-execute-query!` shall return a result set object which can be
used with `dbi-fetch!` or `dbi-fetch-all!`. The implementation may
allow to return a specific result set object and it is users' responsibility
to use it with fetching the result.

NOTE: There is a default implementation of `dbi-execute-query!` and 
returns the given _query_ as a result set object.


###### [!Method] `dbi-fetch!`  _query_
###### [!Method] `dbi-fetch-all!`  _query_

Fetches a row or all rows from the given _query_.

The `dbi-fetch!` shall return a vector representing the query result, if
there is no more data available it shall return #f.

The `dbi-fetch-all!` shall return a list of vectors representing the
query result.

NOTE: There is a default implementation of `dbi-fetch-all!`, it calls
`dbi-fetch!` until it returns #f.


###### [!Method] `dbi-commit!`  _(query_ _<dbi-query>)_
###### [!Method] `dbi-rollback!`  _(query_ _<dbi-query>)_

Commits or rollback transactions on the given query, respectively.

###### [!Method] `dbi-columns`  _query_

Returns a column names affected by the given _query_.

The procedure shall return a vector as its result.

#### [§4] Conditions

The DBI library provides some of conditions which should be raised by
underlying DBD implementations.

NOTE: The listed conditions may or may not be raised by underlying DBD
implementation.

###### [!Condition] `&dbi-error` 
###### [!Function] `dbi-error?`  _object_
###### [!Function] `make-dbi-error` 

Root condition of DBI condition. `&dbi-error` is a sub condition
of `&error`.

All DBI related condition should inherit this condition.


###### [!Condition] `&dbi-driver-not-exist` 
###### [!Function] `dbi-driver-not-exist?`  _object_
###### [!Function] `condition-driver-name`  _condition_

This condition is raised when DBD implementation can't be found.

`condition-driver-name` returns missing driver name.


###### [!Condition] `&dbi-unsupported` 
###### [!Function] `dbi-unsupported?`  _object_
###### [!Function] `make-dbi-unsupported` 

This condition indicates that DBI feature is not supported.

Implementations should raise this condition when methods can't be
implemented on the target DBMS.


###### [!Condition] `&dbi-sql-error` 
###### [!Function] `dbi-sql-error?`  _object_
###### [!Function] `make-dbi-sql-error` 
###### [!Function] `dbi-sql-error-code` 

This condition holds SQL status code.

Implementations should raise this condition when SQL execution is failed
with SQL status code.


### [§3] Writing drivers for DBI

Writing a driver for specific data base system means implementing a library
`(dbd _foo_)` where _foo_ is the name of the driver.

The library have to implement a creating a driver procedure and several 
classes and methods as explained below.

The method described above section must behave as it expected there, especially
behaviours described with **shall**.

#### [§4] DBI driver procedure

The driver will be created by the procedure named `make-_foo_-driver`.
And it is strongly encouraged to be implemented as a subclass of
`<dbi-driver>` for future extension.

#### [§4] DBI classes to implement

You have to define the following classes.


- Subclass `<dbi-connection>`. An instance of this class is created
  by `dbi-make-connection`. It needs to keep the information about the
  actual connections.
- Optional: subclass `<dbi-driver>` for actual driver instance.
- Optional: subclass `<dbi-query>` to keep driver specific information
  of prepared statement.

#### [§4] DBI methods to implement

The driver needs to implement the following methods.

###### [!Method] `dbi-make-connection`  _driver_ _(options_ _<string>)_ _(options-alist_ _<list>)_ _._ _rest_

This method is called from `dbi-connect`, and responsible to connect
to the database and to create a connection object. It must return a connection
object or raise an error which should be a sub condition of `&dbi-error`.

_options_ is the option part of the data source name (DSN) given to
`dbi-connect`. _options-alist_ is an assoc list of the result of
parsing _options_. Both are provided so that the driver may interpret
_options_ string in nontrivial way.

For example, given `"dbi:foo:myaddressbook;host=dbhost;port=8998"` 
as DSN, foo's `dbi-make-connection` will receive 
`"myaddressbook;host=dbhost;port=8998"` as _options_, and 
`(("myaddressbook" . #t) ("host" . "dbhost") ("port" . "8998"))` as
_options-alist_.

After _options-alist_, whatever given to `dbi-connect` are passed.
The driver is strongly encouraged to implement `:username`,
`:password` and `:auto-commit` (if the database is supported)
keyword arguments to specify the authentication information and commit mode.


###### [!Method] `dbi-prepare`  _(c_ _<foo-connection>)_ _(sql_ _<string>)_ _._ _rest_

The method must create and return a prepared query object which is an
instance of `<dbi-query>` or its subclass.

_sql_ is an SQL statement. It may contain placeholders represented by
`'?'`. The implementation must accept it to keep DBI portable even though
the database doesn't.


###### [!Method] `dbi-bind-parameter!`  _(q_ _<foo-query>)_ _index_ _value_

Binds a parameter _value_ at the place _index_.

###### [!Method] `dbi-open?`  _(c_ _<foo-connection>)_
###### [!Method] `dbi-open?`  _(q_ _<foo-query>)_
###### [!Method] `dbi-close`  _(c_ _<foo-connection>)_
###### [!Method] `dbi-close`  _(q_ _<foo-query>)_

Queries open/close status of a connection and a query, and closes
a connection and a query. The close method should cause releasing resources
used by connection/query. The driver has to allow `dbi-close` to be
called on a connection or a query which has already been closed.


###### [!Method] `dbi-commit!`  _(c_ _<foo-connection>)_
###### [!Method] `dbi-commit!`  _(q_ _<foo-query>)_
###### [!Method] `dbi-rollback!`  _(c_ _<foo-connection>)_
###### [!Method] `dbi-rollback`  _(q_ _<foo-query>)_

Commits/rollbacks a connection or a query.

###### [!Method] `dbi-execute!`  _(q_ _<foo-query>)_ _._ _args_
###### [!Method] `dbi-fetch!`  _(q_ _<foo-query>)_
###### [!Method] `dbi-columns`  _q_ _<foo-query>_

Implementation must behave as described above section.

#### [§4] Data conversion guide

Database data type and Scheme type are usually not the same. However to keep
DBI portable it is important to follow a guideline. Here I suggest the data
conversion between a database and Scheme object.

Following is database data type to Scheme type conversion guideline. The driver
implementation should follow.

Text (VARCHAR2 etc)
: String

Binary (BINARY etc)
: Bytevector

Date
: Date from SRFI-19

Time and Timestamp
: Time from SRFI-19

Blob
: Binary port, preferably not retrieving all data at once.

Clob
: Textual port, preferably not retrieving all data at once.

