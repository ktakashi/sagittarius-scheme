[§2] (util timer) - Timer {#util.timer}
-------------

###### [!Library] `(util timer)` 

This library provides timer functionality.

Timer is the mechanism to trigger an event on specified time.


The following describes how to use;

``````````scheme
(import (util timer))

(let ((timer (make-timer)))
  (timer-start! timer) ;; start timer
  ;; execute given thunk starting after 1000ms and each 1000ms
  (timer-schedule! timer (lambda () (display "timer!") (newline)) 1000 1000)
  ;; do something
  (timer-cancel! timer))
``````````

A timer is kind of task queue running on a timer thread. Once it's started,
then it waits until its queue is not empty or the first task reaches the
configured time. The tasks are executed sequentially however its order is
not reliable if there are multiple tasks queued on the same time.

###### [!Function] `timer?`  _obj_

Returns #t if _obj_ is a timer object, otherwise #f.

###### [!Function] `make-timer`  _:key_ _error-handler_

Creates a timer object.

If keyword argument _error-handler_ is specified, then it must be
a procedure accepts one argument. The _error-handler_ is called
when timer procedure raises an error. If this is not specified, then
timer stops when one of the tasks raised an error.


###### [!Function] `timer-state`  _timer_

Returns the state of given timer. The possible states are the followings:

created
: The timer is created and not executed yet.

running
: The timer is running.

stopped
: The timer is stopped (can be resumed).

cancelled
: The timer is cancelled (cannot be resumed).



###### [!Function] `timer-start!`  _timer_

Starts the given `created` state _timer_.

If the given timer's state is `stopped`, then this procedure resumes
the given timer.

If the timer state is not `created` or `stopped`, then
`&assertion` is raised.


###### [!Function] `timer-stop!`  _timer_

Stops the given _time_.

###### [!Function] `timer-cancel!`  _timer_

Cancel the given _time_. If one of the tasks raised an error and
no error handler is specified, then this procedure will re-raise the error.

Once the timer is cancelled, then this timer is completely destroyed.

###### [!Function] `timer-schedule!`  _timer_ _thunk_ _first_ _:optional_ _(period_ _0)_

Schedules a timer task.

_first_ can be time object or exact integer. If this is time object,
then the timer executes the _thunk_ with given time (absolute time).
If this is an exact integer, then the timer executes the _thunk_ after
the given number milliseconds from current time.

Optional argument _period_ specifies if the _thunk_ is periodically 
executed or not. 0 is not periodical task.

The returning value is an ID of scheduled task. This is needed for
`timer-remove!` and `timer-exists?` procedures.


###### [!Function] `timer-reschedule!`  _timer_ _timer-id_ _first_ _:optional_ _(period_ _0)_

Reschedules the timer task associated with _timer-id_ and 
returns _timer-id_The _first_ and _period_ are the same as `timer-schedule!`.


###### [!Function] `timer-remove!`  _timer_ _id_

Removes given _id_ task from the _timer_.

###### [!Function] `timer-exists?`  _timer_ _id_

Returns #t if given _id_ task exists in the _timer_, 
otherwise #f.

