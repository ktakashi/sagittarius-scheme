@; -*- coding:utf-8; -*-

@subsection[:tag "lib.sagittarius.timezone"]{(sagittarius timezone) - Timezone}

@define[Library]{@name{(sagittarius timezone)}}
@desc{This library provides timezone related procedures. The timezone database
is from 
@hyperlink[:href "https://www.iana.org/time-zones"]{IANA - Time Zone Database}.
}

@define[Function]{@name{timezone?} @args{obj}}
@desc{Returns #t if given @var{obj} is a timezone object, otherwise #f.}

@define[Function]{@name{timezone} @args{name}}
@desc{Retrieves timezone object related to @var{name}. The @var{name}
must be a string and proper name of TZID such as @code{Europe/Amsterdam}.
If the given name is not found, then @code{GMT} is returned as the fallback.}

@define[Function]{@name{timezone-offset} @args{tz :optional when}}
@desc{Returns give offset of timezone @var{tz}.

If optional argument @var{when} is specified, it must be a time object,
then the offset is calculated the specified time. Otherwise @code{current-time},
is used.

This procedure considers daylight saving time (DST). Means, if the timezone
has DST, then the return value is depending on the @var{when}. For example, 
@code{Europe/Amsterdam} has DST so if the @var{when} is in DST, then the
returning offset is @code{7200}, otherwise @code{3600}.
}

@define[Function]{@name{timezone-dst?} 
 @args{tz :optional (when (current-time))}}
@desc{Returns #t if @var{when} is in DST, otherwise #f.}

@define[Function]{@name{timezone-short-name} 
 @args{tz :optional (when (current-time))}}
@desc{Returns the short name of given timezone @var{tz}.

This procedure considers DST. Means if @var{when} is in DST, then short name
is DST name, otherwise standard name. For example, timezone 
@code{Europe/Amsterdam} has 2 names, @code{CET} and @code{CEST}. If the
given @var{when} is in DST, then @code{CEST} is returned, otherwise @code{CET}
is returned.
}

@define[Function]{@name{timezone-raw-offset} @args{tz :optional when}}
@desc{Returns GMT offset of given timezone @var{tz}. 

If optional argument @var{when} is given and must be a time object, then
the returning offset is the historical offset. If it's not given, then
the procedure reutnrs current timezone offset.
}

Above procedures also considers when the timezone is started. Means, given
timezone has histories, such as when the daylight saving time is starting
or ending, when that timezone started, etc. It may return different value
according to the @var{when}. Following is the example of timezone history:
@codeblock{
(let ((tz (timezone "Europe/Dublin"))
      (now (date->time-utc (make-date 0 0 0 0 24 7 2015 0)))
      ;; 1:00	-	IST	1971 Oct 31  2:00u
      (no-rule-past (date->time-utc (make-date 0 0 0 0 24 7 1971 0)))
      ;; 0:00	GB-Eire	GMT/IST	1968 Oct 27
      (rule-past (date->time-utc (make-date 0 0 0 0 24 7 1968 0))))
  (timezone-short-name tz now)          ;; => "GMT/IST"
  (timezone-short-name tz no-rule-past) ;; => "IST

  ;; no DST
  (timezone-offset tz no-rule-past)     ;; => 3600 

  (timezone-raw-offset tz)              ;; => 0
  (timezone-raw-offset tz no-rule-past) ;; => 3600
  (timezone-raw-offset tz rule-past)    ;; => 0

  (timezone-short-name tz rule-past)    ;; => "GMT/IST"
  )
}

@define[Function]{@name{timezone-name} @args{tz}}
@desc{Returns TZID of given timezone @var{tz}.}

@define[Function]{@name{timezone-name-list}}
@desc{Returns supported TZIDs.}
