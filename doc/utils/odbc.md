[§2] (odbc) - ODBC binding {#odbc}
-------------

###### [!Library] `(odbc)` 

This library provides ODBC access procedures.

ODBC is a common database access interface so that it doesn't depend on specific
APIs. This library is for implementing ODBC DBD for [DBI](#dbi), using
this library directly is not recommended.

NOTE: the library is supported only your platform is supporting ODBC.


###### [!Function] `create-odbc-env` 

Creates and returns ODBC environment object.

###### [!Function] `odbc-env?`  _obj_

Returns #t if the given _obj_ is ODBC environment object.

###### [!Function] `free-handle`  _odbc-ctx_

Releases given ODBC resource.

###### [!Function] `connect!`  _ocbc-env_ _server_ _username_ _password_ _:optional_ _(auto-commit_ _#t)_

_odbc-env_ must be an ODBC environment object created by
`create-odbc-env`.

_server_ must be a string indicating ODBC database name.

_username_ and _password_ must be string.

Connects to _server_ with authentication of _username_ and
_password_ and returns ODBC database connection object. If optional
argument _auto-commit_ is #f then the connection won't commit transaction
automatically.


###### [!Function] `odbc-connection?`  _obj_

Returns #t if the given _obj_ is ODBC connection object.

###### [!Function] `disconnect!`  _odbc-dbc_

Disconnect from the database.

###### [!Function] `connection-open?`  _odbc-dbc_

Returns #t if the given ODBC connection is available, otherwise #f.

###### [!Function] `prepare`  _odbc-dbc_ _sql_

Creates and returns a ODBC statement object.

###### [!Function] `odbc-statement?`  _obj_

Returns #t if the given _obj_ is ODBC statement object.

###### [!Function] `statement-open?`  _odbc-stmt_

Returns #t if the given ODBC statement is available, otherwise #f.

###### [!Function] `num-prams`  _odbc-stmt_

Returns number of parameters in an SQL statement.

###### [!Function] `bind-parameter!`  _odbc-stmt_ _index_ _value_

Binds the given _value_ at position _index_.

###### [!Function] `execute!`  _odbc-stmt_

Execute given ODBC statement.

###### [!Function] `fetch!`  _odbc-stmt_

Forwarding current cursor to next and returns #t if there is data
otherwise #f.

###### [!Function] `get-data!`  _odbc-stmt_ _index_

Retrieve data from statement at position _index_.

###### [!Function] `row-count`  _odbc-stmt_

Returns the number of rows affected by UPDATE, INSERT or DELETE
statement.

###### [!Function] `column-count`  _odbc-stmt_

Returns the number of columns in a result statement.

###### [!Function] `result-columns`  _odbc-stmt_

Returns the column names in a result statement.

###### [!Function] `commit!`  _odbc-ctx_
###### [!Function] `rollback!`  _odbc-ctx_

Commits/rollbacks given ODBC context.

### [§3] (dbd odbc) - DBD for ODBC {#dbd.odbc}

###### [!Library] `(dbd odbc)` 

This library provides database driver (DBD) for ODBC .

Importing this library is done by DBI automatically so users should not
use this directly.

The DSN should specify the connecting database name with `server` keyword.
Following DSN is connecting _foo_ database configured in ODBC.

`"dbi:odbc:server=_foo_"`The `dbi-connect` supports `:username`, `:password` and
`:auto-commit` keyword arguments. The detail about DBI see
[(dbi) - Database independent access layer](#dbi).



