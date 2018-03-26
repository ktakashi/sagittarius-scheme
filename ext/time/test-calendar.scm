(import (sagittarius calendar)
	(sagittarius calendar gregorian)
	(sagittarius calendar iso)
	(sagittarius time-util))

(test-begin "calendar")

(test-assert (calendar? calendar:gregorian))
(test-assert (calendar? calendar:iso))
(test-assert (calendar? calendar:system))

(test-assert (calendar-date? (time-utc->calendar-date (current-time))))

(let ((time (current-time))
      (absolute-1-in-utc (make-time time-utc 0 -62135553600))
      (cet (timezone "Etc/GMT-1"))
      (gmt (timezone "GMT")))
  (test-assert (time=? time (calendar-date->time-utc
			     (time-utc->calendar-date time))))
  (test-equal 25/24
	      (calendar-date->gregorian-day
	       (time-utc->calendar-date absolute-1-in-utc cet)))
  (test-equal 1
	      (calendar-date->gregorian-day
	       (time-utc->calendar-date absolute-1-in-utc gmt)))
  (test-equal (time-utc->julian-day time)
	      (calendar-date->julian-day (time-utc->calendar-date time gmt))))

(let ((gc (make-gregorian-calendar-date 0 0 0 0 16 1 2018
					(timezone "Etc/GMT-1")))
      (->day calendar-date->gregorian-day))
  (test-error assertion-violation?
	      (calendar-date-add gc +calendar-unit:week+ 1))
  (test-equal (+ (calendar-date->gregorian-day gc) 365)
	      (->day (calendar-date-add gc +calendar-unit:year+ 1)))
  (test-equal (+ (calendar-date->gregorian-day gc) 31)
	      (->day (calendar-date-add gc +calendar-unit:month+ 1)))
  (test-equal (+ (calendar-date->gregorian-day gc) 1)
	      (->day (calendar-date-add gc +calendar-unit:day+ 1)))
  (test-equal (+ (calendar-date->gregorian-day gc) 1/24)
	      (->day (calendar-date-add gc +calendar-unit:hour+ 1)))
  (test-equal (+ (calendar-date->gregorian-day gc) (/ 1 (* 24 60)))
	      (->day (calendar-date-add gc +calendar-unit:minute+ 1)))
  (test-equal (+ (calendar-date->gregorian-day gc) (/ 1 (* 24 60 60)))
	      (->day (calendar-date-add gc +calendar-unit:second+ 1)))
  (test-equal (+ (calendar-date->gregorian-day gc) (/ 1 (* 24 60 60 tm:nano)))
	      (->day (calendar-date-add gc +calendar-unit:nanosecond+ 1))))

(test-assert (gregorian-leap-year? 4))
(test-assert (not (gregorian-leap-year? 2100)))
(test-equal '(0 0 0 12 1 1 1)
  (let-values ((r (absolute->gregorian-components 1 (timezone "GMT")))) r))
(test-equal '(0 0 0 13 1 1 1)
  (let-values ((r (absolute->gregorian-components 1 (timezone "Etc/GMT-1")))) r))
(test-equal '(0 0 0 12 31 12 -1)
  (let-values ((r (absolute->gregorian-components 0 (timezone "GMT")))) r))
(test-equal '(0 0 0 0 1 1 1)
  (let-values ((r (absolute->gregorian-components 0.5 (timezone "GMT")))) r))

(test-equal '(0 0 0 13 1 1 2)
  (let-values ((r (absolute->iso-components 365 (timezone "Etc/GMT-1")))) r))
(test-equal '(0 0 0 12 7 52 0)
  (let-values ((r (absolute->iso-components 0 (timezone "GMT")))) r))
(test-equal '(0 0 0 0 1 1 2)
  (let-values ((r (absolute->iso-components 364.5 (timezone "GMT")))) r))

(test-equal 0 (iso-components->absolute 0 0 0 12 7 52 0 (timezone "GMT")))
(test-equal 365 (iso-components->absolute 0 0 0 12 1 1 2 (timezone "GMT")))
(test-equal (exact 364.5)
	    (iso-components->absolute 0 0 0 0 1 1 2 (timezone "GMT")))

(test-end)
