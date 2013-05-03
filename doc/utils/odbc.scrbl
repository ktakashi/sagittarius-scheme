@; -*- coding: utf-8 -*-
@subsection[:tag "odbc"]{(odbc) - ODBC binding}

@define[Library]{@name{(odbc)}}
@desc{This library provides ODBC access procedures.

ODBC is a common database access interface so that it doesn't depend on specific
APIs. This library is for implementing ODBC DBD for @secref["dbi"]{DBI}, using
this library directly is not recommended.

NOTE: the library is supported only your platform is supporting ODBC.
}

@define[Function]{@name{create-odbc-env}}
@desc{Creates and returns ODBC environment object.}

@define[Function]{@name{odbc-env?} @args{obj}}
@desc{Returns #t if the given @var{obj} is ODBC environment object.}

@define[Function]{@name{free-handle} @args{odbc-ctx}}
@desc{Releases given ODBC resource.}

@define[Function]{@name{connect!}
 @args{ocbc-env server username password :optional (auto-commit #t)}}
@desc{@var{odbc-env} must be an ODBC environment object created by
@code{create-odbc-env}.

@var{server} must be a string indicating ODBC database name.

@var{username} and @var{password} must be string.

Connects to @var{server} with authentication of @var{username} and
@var{password} and returns ODBC database connection object. If optional
argument @var{auto-commit} is #f then the connection won't commit transaction
automatically.
}

@define[Function]{@name{odbc-connection?} @args{obj}}
@desc{Returns #t if the given @var{obj} is ODBC connection object.}

@; supported but not properly yet.
@; @define[Function]{@name{set-connect-attr!} @args{odbc-dbc name value}}
@; @desc{Sets connection attribute to given @var{odbc-dbc}. }

@define[Function]{@name{disconnect!} @args{odbc-dbc}}
@desc{Disconnect from the database.}

@define[Function]{@name{connection-open?} @args{odbc-dbc}}
@desc{Returns #t if the given ODBC connection is available, otherwise #f.}

@; supported but not used anywhere and I don't why I create this...
@; @define[Function]{@name{statement} @args{odbc-dbc}}
@; @desc{Creates and returns a empty ODBC statement object. }

@define[Function]{@name{prepare} @args{odbc-dbc sql}}
@desc{Creates and returns a ODBC statement object.}

@define[Function]{@name{odbc-statement?} @args{obj}}
@desc{Returns #t if the given @var{obj} is ODBC statement object.}

@define[Function]{@name{statement-open?} @args{odbc-stmt}}
@desc{Returns #t if the given ODBC statement is available, otherwise #f.}

@define[Function]{@name{num-prams} @args{odbc-stmt}}
@desc{Returns number of parameters in an SQL statement.}

@define[Function]{@name{bind-parameter!} @args{odbc-stmt index value}}
@desc{Binds the given @var{value} at position @var{index}.}

@define[Function]{@name{execute!} @args{odbc-stmt}}
@desc{Execute given ODBC statement.}

@; not used and probably to use this we needed statement procedure
@; @define[Function]{@name{execute-direct!} @args{odbc-stmt sql}}
@; @desc{Execute given @var{sql}.}

@define[Function]{@name{fetch!} @args{odbc-stmt}}
@desc{Forwarding current cursor to next and returns #t if there is data
otherwise #f.}

@define[Function]{@name{get-data!} @args{odbc-stmt index}}
@desc{Retrieve data from statement at position @var{index}.}

@define[Function]{@name{row-count} @args{odbc-stmt}}
@desc{Returns the number of rows affected by UPDATE, INSERT or DELETE
statement.}

@define[Function]{@name{column-count} @args{odbc-stmt}}
@desc{Returns the number of columns in a result statement.}

@define[Function]{@name{result-columns} @args{odbc-stmt}}
@desc{Returns the column names in a result statement.}

@define[Function]{@name{commit!} @args{odbc-ctx}}
@define[Function]{@name{rollback!} @args{odbc-ctx}}
@desc{Commits/rollbacks given ODBC context.}

@; TODO ODBC date, time and timestamp types...

@subsubsection[:tag "dbd.odbc"]{(dbd odbc) - DBD for ODBC}

@define[Library]{@name{(dbd odbc)}}
@desc{This library provides database driver (DBD) for ODBC .

Importing this library is done by DBI automatically so users should not
use this directly.

The DSN should specify the connecting database name with @code{server} keyword.
Following DSN is connecting @var{foo} database configured in ODBC.

@code{"dbi:odbc:server=@var{foo}"}

The @code{dbi-connect} supports @code{:username}, @code{:password} and
@code{:auto-commit} keyword arguments. The detail about DBI see
@secref["dbi"]{(dbi) - Database independent access layer}.

}

