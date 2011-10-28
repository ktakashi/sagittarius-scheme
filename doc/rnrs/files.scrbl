@; -*- mode:scribble; coding: utf-8 -*-

@subsection[:tag "rnrs.files.6"]{File system}

@define[Library]{@name{(rnrs files (6))}}
@desc{This library, in addition to the procedures described here, also exports
the I/O condition types described in @secref["io.condition.types"]{I/O condition types}.
}

@define[Function]{@name{file-exists?} @args{filename}}
@desc{[R6RS] @var{Filename} must be a file name (see @secref["rnrs.io.ports.6"]{Port I/O}).
The @code{file-exists?} procedure returns #t if the named file exists at the time
the procedure is called, #f otherwise. 
}

@define[Function]{@name{delete-file} @args{filename}}
@desc{[R6RS] @var{Filename} must be a file name (see @secref["rnrs.io.ports.6"]{Port I/O}).
The @code{delete-file} procedure deletes the named file if it exists and can be
deleted, and returns unspecified values. If the file does not exist or cannot be
deleted, an exception with condition type @code{&i/o-filename} is raised. 
}
