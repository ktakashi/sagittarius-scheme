@; -*- coding: utf-8 -*-
@subsection[:tag "dbi"]{(dbi) - Database independent access layer}

@define[Library]{@name{(dbi)}}
@desc{This library provides database independent access procedures. The
database specific operations are provided in database driver (DBD) libraries.
}

Following example describes how to query a database using @code{(dbd odbc)}
DBD library.

@codeblock{
(import (dbi))
(define conn (dbi-connect "dbi:odbc:server=XE"
                          :username "username"
                          :password "password"))
(let* ((query (dbi-prepare conn "SELECT * FROM table WHERE id > ?"))
       (result (dbi-execute-query! query 1)))
  (do ((row (dbi-fetch! result) (dbi-fetch! result)))
      ((not row))
   (vector-for-each (lambda (col) (print col)) row)))
}

There is nothing specific to the underlying database system except the
argument @code{"dbi:odbc:server=XE"} passed to @code{dbi-connect}, from which
@code{dbi} library figures out that it is an access to @code{odbc}, loads
@code{(dbd odbc)} library and let it handle the specific operations.

If you want to use other database named @var{boo}, then you just need to pass
@code{"dbi:@var{boo}:@var{parameter}"} to @code{dbi-connect} instead. As long
as you have @code{(dbd boo)} installed then everything stays the same.

@subsubsection{DBI user API}

@sub*section{Database connection}

@define[Function]{@name{dbi-connect} @args{dsn . rest}}
@desc{Connects to a database using a data source specified by @var{dsn} 
(data source name). @var{dsn} is a string with the following syntax.

@code{dbi:@var{driver}:@var{options}}

@var{driver} part names a specific driver. You need to have a corresponding
driver library, @code{(dbd @var{driver})}, installed in your system.

Interpretation of the @var{options} part is up to the driver. Usually it is
in the form of @code{key1=value1;key2=value2;@dots{}}. However the DBD
implementations can have different format so you need to check the document
of each driver for exact specification of @var{options}.

@var{rest} argument will be passed to the underlying procedure.

NOTE: @var{username}, @var{password} and @var{auto-commit} are strongly
encouraged to be implemented in the DBD library.

If a connection to the database is successfully established, a connection
object is returned.
}

@define[Method]{@name{dbi-open?} @args{c <dbi-connection>}}
@desc{Checks if the given connection is still open.

The method shall return #t if the given connection is still open, otherwise
it shall return #f.
}

@define[Method]{@name{dbi-close} @args{c <dbi-connection>}}
@desc{Closes a connection to the database.

NOTE: Users are strongly encouraged to close a connection explicitly. DBD
might not close opened connections automatically.
}

@define[Method]{@name{dbi-commit!} @args{(c <dbi-connection>)}}
@define[Method]{@name{dbi-rollback!} @args{(c <dbi-connection>)}}
@desc{Commits or rollback transactions on the given connection, respectively.}

@sub*section{Preparing and executing queries }

@define[Method]{@name{dbi-prepare} @args{conn sql . args}}
@desc{From a string representation of SQL @var{sql}, creates and returns a
query object for the database connection @var{conn}.

@var{sql} may contain parameter slot, denoted by @code{?}.

@snipet{(dbi-prepare conn "insert into tab (col1, col2) values (?, ?)")}
@snipet{(dbi-prepare conn "select * from tab where col1 = ?")}

If @var{args} is not null, then the procedure shall bind given parameters
to the place holder.
}

@define[Method]{@name{dbi-open?} @args{(query <dbi-query>)}}
@desc{Checks if the given query is still open.

The method shall return #t if the given query is still open, otherwise
it shall return #f.
}

@define[Method]{@name{dbi-close} @args{(query <dbi-query>)}}
@desc{Closes a query.

NOTE: Users are strongly encouraged to close a query explicitly. DBD
might not close opened query automatically.
}


@define[Method]{@name{dbi-bind-parameter!} @args{query index value}}
@desc{Binds the given @var{value} to @var{query} at @var{index}.

The procedure shall accept integer @var{index} and may accept other type of
@var{index}.
}

@define[Method]{@name{dbi-execute!} @args{query . args}}
@define[Method]{@name{dbi-execute-query!} @args{query . args}}
@desc{Executes given @var{query}. If the @var{args} is not null, then
procedure shall bind the given @var{args} as its parameters.

The @code{dbi-execute!} shall return a integer value representing affected
row count.

The @code{dbi-execute-query!} shall return a result set object which can be
used with @code{dbi-fetch!} or @code{dbi-fetch-all!}. The implementation may
allow to return a specific result set object and it is users' responsibility
to use it with fetching the result.

NOTE: There is a default implementation of @code{dbi-execute-query!} and 
returns the given @var{query} as a result set object.
}

@define[Method]{@name{dbi-fetch!} @args{query}}
@define[Method]{@name{dbi-fetch-all!} @args{query}}
@desc{Fetches a row or all rows from the given @var{query}.

The @code{dbi-fetch!} shall return a vector representing the query result, if
there is no more data available it shall return #f.

The @code{dbi-fetch-all!} shall return a list of vectors representing the
query result.

NOTE: There is a default implementation of @code{dbi-fetch-all!}, it calls
@code{dbi-fetch!} until it returns #f.
}

@define[Method]{@name{dbi-commit!} @args{(query <dbi-query>)}}
@define[Method]{@name{dbi-rollback!} @args{(query <dbi-query>)}}
@desc{Commits or rollback transactions on the given query, respectively.}

@define[Method]{@name{dbi-columns} @args{query}}
@desc{Returns a column names affected by the given @var{query}.

The procedure shall return a vector as its result.}

@subsubsection{Writing drivers for DBI}

Writing a driver for specific data base system means implementing a library
@code{(dbd @var{foo})} where @var{foo} is the name of the driver.

The library have to implement a creating a driver procedure and several 
classes and methods as explained below.

The method described above section must behave as it expected there, especially
behaviours described with @b{shall}.

@sub*section{DBI driver procedure}

The driver will be created by the procedure named @code{make-@var{foo}-driver}.
And it is strongly encouraged to be implemented as a subclass of
@code{<dbi-driver>} for future extension.

@sub*section{DBI classes to implement}

You have to define the following classes.

@itemlist{
@item{Subclass @code{<dbi-connection>}. An instance of this class is created
by @code{dbi-make-connection}. It needs to keep the information about the
actual connections.}
@item{Optional: subclass @code{<dbi-driver>} for actual driver instance.}
@item{Optional: subclass @code{<dbi-query>} to keep driver specific information
of prepared statement.}
}

@sub*section{DBI methods to implement}

The driver needs to implement the following methods.

@define[Method]{@name{dbi-make-connection}
 @args{driver (options <string>) (options-alist <list>) . rest}}
@desc{This method is called from @code{dbi-connect}, and responsible to connect
to the database and to create a connection object. It must return a connection
object or raise an error which should be a sub condition of @code{&dbi-error}.

@var{options} is the option part of the data source name (DSN) given to
@code{dbi-connect}. @var{options-alist} is an assoc list of the result of
parsing @var{options}. Both are provided so that the driver may interpret
@var{options} string in nontrivial way.

For example, given @code{"dbi:foo:myaddressbook;host=dbhost;port=8998"} 
as DSN, foo's @code{dbi-make-connection} will receive 
@code{"myaddressbook;host=dbhost;port=8998"} as @var{options}, and 
@code{(("myaddressbook" . #t) ("host" . "dbhost") ("port" . "8998"))} as
@var{options-alist}.

After @var{options-alist}, whatever given to @code{dbi-connect} are passed.
The driver is strongly encouraged to implement @code{:username},
@code{:password} and @code{:auto-commit} (if the database is supported)
keyword arguments to specify the authentication information and commit mode.
}

@define[Method]{@name{dbi-prepare}
 @args{(c <foo-connection>) (sql <string>) . rest}}
@desc{The method must create and return a prepared query object which is an
instance of @code{<dbi-query>} or its subclass.

@var{sql} is an SQL statement. It may contain placeholders represented by
@code{'?'}. The implementation must accept it to keep DBI portable even though
the database doesn't.
}

@define[Method]{@name{dbi-bind-parameter!}
 @args{(q <foo-query>) index value}}
@desc{Binds a parameter @var{value} at the place @var{index}.}

@define[Method]{@name{dbi-open?} @args{(c <foo-connection>)}}
@define[Method]{@name{dbi-open?} @args{(q <foo-query>)}}
@define[Method]{@name{dbi-close} @args{(c <foo-connection>)}}
@define[Method]{@name{dbi-close} @args{(q <foo-query>)}}
@desc{Queries open/close status of a connection and a query, and closes
a connection and a query. The close method should cause releasing resources
used by connection/query. The driver has to allow @code{dbi-close} to be
called on a connection or a query which has already been closed.
}

@define[Method]{@name{dbi-commit!} @args{(c <foo-connection>)}}
@define[Method]{@name{dbi-commit!} @args{(q <foo-query>)}}
@define[Method]{@name{dbi-rollback!} @args{(c <foo-connection>)}}
@define[Method]{@name{dbi-rollback} @args{(q <foo-query>)}}
@desc{Commits/rollbacks a connection or a query.}

@define[Method]{@name{dbi-execute!} @args{(q <foo-query>) . args}}
@define[Method]{@name{dbi-fetch!} @args{(q <foo-query>)}}
@define[Method]{@name{dbi-columns} @args{q <foo-query>}}
@desc{Implementation must behave as described above section.}
