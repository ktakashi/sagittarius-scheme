[§2] (sagittarius timezone) - Timezone {#lib.sagittarius.timezone}
-------------

###### [!Library] `(sagittarius timezone)` 

This library provides timezone related procedures. The timezone database
is from 
[IANA - Time Zone Database](https://www.iana.org/time-zones).


###### [!Function] `timezone?`  _obj_

Returns #t if given _obj_ is a timezone object, otherwise #f.

###### [!Function] `timezone`  _name_

Retrieves timezone object related to _name_. The _name_must be a string and proper name of TZID such as `Europe/Amsterdam`.
If the given name is not found, then `GMT` is returned as the fallback.

###### [!Function] `timezone-offset`  _tz_ _:optional_ _when_

Returns give offset of timezone _tz_.

If optional argument _when_ is specified, it must be a time object,
then the offset is calculated the specified time. Otherwise `current-time`,
is used.

This procedure considers daylight saving time (DST). Means, if the timezone
has DST, then the return value is depending on the _when_. For example, 
`Europe/Amsterdam` has DST so if the _when_ is in DST, then the
returning offset is `7200`, otherwise `3600`.


###### [!Function] `timezone-dst?`  _tz_ _:optional_ _(when_ _(current-time))_

Returns #t if _when_ is in DST, otherwise #f.

###### [!Function] `timezone-short-name`  _tz_ _:optional_ _(when_ _(current-time))_

Returns the short name of given timezone _tz_.

This procedure considers DST. Means if _when_ is in DST, then short name
is DST name, otherwise standard name. For example, timezone 
`Europe/Amsterdam` has 2 names, `CET` and `CEST`. If the
given _when_ is in DST, then `CEST` is returned, otherwise `CET`is returned.


###### [!Function] `timezone-raw-offset`  _tz_ _:optional_ _when_

Returns GMT offset of given timezone _tz_. 

If optional argument _when_ is given and must be a time object, then
the returning offset is the historical offset. If it's not given, then
the procedure reutnrs current timezone offset.


Above procedures also considers when the timezone is started. Means, given
timezone has histories, such as when the daylight saving time is starting
or ending, when that timezone started, etc. It may return different value
according to the _when_. Following is the example of timezone history:

``````````scheme
(let ((tz (timezone "Europe/Dublin"))
      (now (date->time-utc (make-date 0 0 0 0 24 7 2015 0)))
      ;; 1:00	-	IST	1971 Oct 31  2:00u
      (no-rule-past (date->time-utc (make-date 0 0 0 0 24 7 1971 0)))
      ;; 0:00	GB-Eire	GMT/IST	1968 Oct 27
      (rule-past (date->time-utc (make-date 0 0 0 0 24 7 1968 0))))
  (timezone-short-name tz now)          ;; => "GMT/IST"
  (timezone-short-name tz no-rule-past) ;; => "IST"

  ;; no DST
  (timezone-offset tz no-rule-past)     ;; => 3600 

  (timezone-raw-offset tz)              ;; => 0
  (timezone-raw-offset tz no-rule-past) ;; => 3600
  (timezone-raw-offset tz rule-past)    ;; => 0

  (timezone-short-name tz rule-past)    ;; => "GMT/IST"
  )
``````````

###### [!Function] `timezone-name`  _tz_

Returns TZID of given timezone _tz_.

###### [!Function] `timezone-name-list` 

Returns supported TZIDs.

###### [!Function] `zone-offset->timezones`  _offset_ _:optional_ _when_

Returns list of timezones whose offsets matched _offset_.

This procedure considers the time. For example, if the given _offset_ is
`3600` which is UTC+1 however if it's summer time, then the returning
list doesn't contain some of timezones (e.g. Amsterdam).

The optional argument _when_ specifies the time to consider. If it's not
specified, then the returning value of `current-time` is used.

``````````scheme
(zone-offset->timezones 3600) ;; => '(#<timezone Etc/GMT-1> ...)
;; offset +15:00 doesn't exist
(zone-offset->timezones (* 15 3600)) ;; => '()
``````````



###### [!Function] `zone-offset->timezones*`  _offset_ _:optional_ _when_

Similar with `zone-offset->timezones*`, the difference is
this procedure creates an anonymous timezone if there's no registered timezone
matching with the given _offset_.

``````````scheme
(zone-offset->timezones* 3600) ;; => '(#<timezone Etc/GMT-1> ...)
;; offset +15:00 doesn't exist
(zone-offset->timezones* (* 15 3600)) ;; => '(#<timezone +15:00>)
``````````



