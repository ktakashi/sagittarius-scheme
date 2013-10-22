@; -*- coding: utf-8 -*-
@subsection[:tag "dbm"]{(dbm) - Generic DBM interface}

@; Most of the documents are from Gauche's manual.

@define[Library]{@name{(dbm)}}
@desc{The library provides the generic interface to access DBM.

Sagittarius currently supports following DBM implementation;

@dl-list[]{
@dl-item["(dbm dumb)"]{
 DBM-like library. Inspired by Python's dbm.dumb. This library must be used
 as the last resort. It has poor performance and memory usage.
}
}
}

The following code shows a typical usage;

@codeblock{
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
}

@subsubsection{Opening and closing a dbm database}

@define[Class]{@name{<dbm>}}
@desc{An abstract class for DBM-like database. The class has the following
slots. It must be filled by @code{dbm-open}.

@dl-list{
  @dl-item["path"]{ Pathname of the dbm database. }
  @dl-item["rw-mode"]{
    Specifies read/write mode. Can be one of the following keywords:
    @dl-list{
      @dl-item[":read"]{ The database will be opened in read-only mode.}
      @dl-item[":write"]{
        The database will be opened in read-write mode. If the database file
	does not exist, @code{dbm-open} creates one.
      }
      @dl-item[":write"]{
        The database will be opened in read-write mode. If the database file
	exists, @code{dbm-open} truncates it.
      }
    }
    The keywords are indication so actual implementation may not behave as it
    described.
  }
  @dl-itemx[2 "key-convert" "value-convert"]{
    By default, you can use only strings for both key and values. With
    this option, however, you can specify how to convert other Scheme
    values to/from string to be stored in the database. The possible values
    are the followings: 
    @dl-list{
      @dl-item["#f"]{
       The default value. Keys (values) are not converted.
       They must be a string.
      }
      @dl-item["#t"]{
       Keys (values) are converted to its string representation, using
       @code{write/ss}, to store in the database and convert back Scheme
       values, using @code{read/ss}, to retrieve from the database.
      }
      @dl-item["a list of two procedures"]{
       Both procedure must take a single argument. The first procedure must
       receive a Scheme object and returns a string. It is used to convert 
       the keys (values) to store in the database. The second procedure must
       receive a string and returns a Scheme object. It is used to convert
       the stored data in the database to a Scheme object.
      }
    }
  }
}

}

@define[Metaclass]{@name{<dbm-meta>}}
@desc{A metaclass of @code{<dbm>} and its subclasses.}

@define[Method]{@name{dbm-open} @args{(dbm <dbm>)}}
@desc{Opens a dbm database. @var{dbm} must be an instance of one of the
concrete classes derived from the @code{<dbm>}.
}

@define[Method]{@name{dbm-open} @args{(dbm-class <dbm-meta>) options @dots{}}}
@desc{A convenient method that creates dbm instance and opens it.}

@define[Method]{@name{dbm-close} @args{(dbm <dbm>)}}
@desc{Closes a dbm database @var{dbm}. If the database is not closed, then
the database file may not be synchronised properly. So it is user's
responsibility to close it.
}

@define[Method]{@name{dbm-closed?} @args{(dbm <dbm>)}}
@desc{Returns true if the dbm database @var{dbm} is closed, otherwise #f.

The returned value may be non boolean value.
}

@define[Function]{@name{dbm-type->class} @args{dbmtype}}
@desc{Returns DBM class if DBM implementation @var{dbmtype} exists, otherwise
#f.

The @var{dbmtype} must be a symbol that names the type of dbm implementation,
and the implementation library name must be @code{(dbm @var{dbmtype})}. For
example, to get the @var{foo} DBM then the library name must be 
@code{(dbm foo)}.
}

@subsubsection{Accessing a dbm database}

Once a database is opened, you can use the following methods to access
individual key/value pairs. 

@define[Method]{@name{dbm-put!} @args{(dbm <dbm>) key value}}
@desc{Put a @var{value} with @var{key}}

@define[Method]{@name{dbm-get} @args{(dbm <dbm>) key :optional default}}
@desc{Get a value associated with @var{key}. If no value exists for @var{key}
and @var{default} is specified, it will be returned. If no value exists for 
@var{key} and @var{default} is not specified, then an error will be raised.
}

@define[Method]{@name{dbm-exists?} @args{(dbm <dbm>) key}}
@desc{Return true value if a value exists for @var{key}, #f otherwise.}

@define[Method]{@name{dbm-delete!} @args{(dbm <dbm>) key}}
@desc{Delete a value associated with @var{key}.}

@subsubsection{Iterating on a dbm database}

To walk over the entire database, following methods are provided. 

@define[Method]{@name{dbm-fold} @args{(dbm <dbm>) procedure knil}}
@desc{The basic iterator. For each key/value pair, @var{procedure} is called
as @code{@var{procedure} key value r}, where @var{r} is @var{knil} for the
first call of @var{procedure}, and the return value of the previous call for
subsequent calls. Returns the result of the last call of @var{procedure}.
If no data is in the database, @var{knil} is returned.
}

@define[Method]{@name{dbm-for-each} @args{(dbm <dbm>) procedure}}
@desc{For each key/value pair in the database @var{dbm}, @var{procedure} is
called. The @var{procedure} must accept 2 arguments, a key and a value
respectively. The result of @var{procedure} is discarded.
}

@define[Method]{@name{dbm-map} @args{(dbm <dbm>) procedure}}
@desc{For each key/value pair in the database @var{dbm}, @var{procedure} is
called. The @var{procedure} must accept 2 arguments, a key and a value
respectively. The result of @var{procedure} is accumulated to a list which
is returned as a result of @code{dbm-map}.
}

@subsubsection{Managing dbm database instance}

@define[Method]{@name{dbm-db-exists?} @args{(class <dbm-meta>) name}}
@desc{Returns #t if a database of class @var{class} specified by @var{name}
exists.
}

@define[Method]{@name{dbm-db-remove} @args{(class <dbm-meta>) name}}
@desc{Removes an entire database of class @var{class} specified by @var{name}.
}

@define[Method]{@name{dbm-db-copy} @args{(class <dbm-meta>) from to}}
@desc{Copy a database of @var{class} specified by @var{from} to @var{to}.
}

@define[Method]{@name{dbm-db-move} @args{(class <dbm-meta>) from to}}
@desc{Moves or renames a database of @var{class} specified by @var{from}
to @var{to}.
}