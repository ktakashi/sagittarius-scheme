@; -*- coding: utf-8 -*-
@subsection[:tag "util.timer"]{(util timer) - Timer}

@define[Library]{@name{(util timer)}}
@desc{This library provides timer functionality.

@; TODO more useful document...
Timer is the mechanism to trigger an event on specified time.
}

The following describes how to use;
@codeblock{
(import (util timer))

(let ((timer (make-timer)))
  (timer-start! timer) ;; start timer
  ;; execute given thunk starting after 1000ms and each 1000ms
  (timer-schedule! timer (lambda () (display "timer!") (newline)) 1000 1000)
  ;; do something
  (timer-cancel! timer))
}

A timer is kind of task queue running on a timer thread. Once it's started,
then it waits until its queue is not empty or the first task reaches the
configured time. The tasks are executed sequentially however its order is
not reliable if there are multiple tasks queued on the same time.

@define[Function]{@name{timer?} @args{obj}}
@desc{Returns #t if @var{obj} is a timer object, otherwise #f.}

@define[Function]{@name{make-timer} @args{:key error-handler}}
@desc{Creates a timer object.

If keyword argument @var{error-handler} is specified, then it must be
a procedure accepts one argument. The @var{error-handler} is called
when timer procedure raises an error. If this is not specified, then
timer stops when one of the tasks raised an error.
}

@define[Function]{@name{timer-state} @args{timer}}
@desc{Returns the state of given timer. The possible states are the followings:

@dl-list{
@dl-item["created"]{The timer is created and not executed yet.}
@dl-item["running"]{The timer is running.}
@dl-item["stopped"]{The timer is stopped (can be resumed).}
@dl-item["cancelled"]{The timer is cancelled (cannot be resumed).}
}
}


@define[Function]{@name{timer-start!} @args{timer}}
@desc{Starts the given @code{created} state @var{timer}.

If the given timer's state is @code{stopped}, then this procedure resumes
the given timer.

If the timer state is not @code{created} or @code{stopped}, then
@code{&assertion} is raised.
}

@define[Function]{@name{timer-stop!} @args{timer}}
@desc{Stops the given @var{time}.}

@define[Function]{@name{timer-cancel!} @args{timer}}
@desc{Cancel the given @var{time}. If one of the tasks raised an error and
no error handler is specified, then this procedure will re-raise the error.

Once the timer is cancelled, then this timer is completely destroyed.}


@define[Function]{@name{timer-schedule!}
 @args{timer thunk first :optional (period 0)}}
@desc{Schedules a timer task.

@var{first} can be time object or exact integer. If this is time object,
then the timer executes the @var{thunk} with given time (absolute time).
If this is an exact integer, then the timer executes the @var{thunk} after
the given number milliseconds from current time.

Optional argument @var{period} specifies if the @var{thunk} is periodically 
executed or not. 0 is not periodical task.

The returning value is an ID of scheduled task. This is needed for
@code{timer-remove!} and @code{timer-exists?} procedures.
}

@define[Function]{@name{timer-reschedule!}
 @args{timer timer-id first :optional (period 0)}}
@desc{Reschedules the timer task associated with @var{timer-id} and 
returns @var{timer-id}

The @var{first} and @var{period} are the same as @code{timer-schedule!}.
}

@define[Function]{@name{timer-remove!} @args{timer id}}
@desc{Removes given @var{id} task from the @var{timer}.}

@define[Function]{@name{timer-exists?} @args{timer id}}
@desc{Returns #t if given @var{id} task exists in the @var{timer}, 
otherwise #f.}
