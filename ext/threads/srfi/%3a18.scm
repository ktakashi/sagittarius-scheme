#!compatible
(library (srfi :18)
    (export current-thread
	    thread?
	    make-thread
	    thread-name
	    thread-specific
	    thread-specific-set!
	    thread-start!
	    thread-yield!
	    thread-sleep!
	    thread-terminate!
	    thread-join!
	    mutex?
	    make-mutex
	    mutex-name
	    mutex-specific
	    mutex-specific-set!
	    mutex-state
	    mutex-lock!
	    mutex-unlock!
	    condition-variable?
	    make-condition-variable
	    condition-variable-name
	    condition-variable-specific
	    condition-variable-specific-set!
	    condition-variable-signal!
	    condition-variable-broadcast!
	    current-time
	    time?
	    time->seconds
	    seconds->time
	    current-exception-handler
	    (rename (with-error-handler with-exception-handler))
	    raise
	    join-timeout-exception?
	    abandoned-mutex-exception?
	    terminated-thread-exception?
	    uncaught-exception?
	    uncaught-exception-reason
     )
    (import (srfi :18 multithreading))
)
