(import (sagittarius calendar)
	(sagittarius calendar gregorian)
	(sagittarius calendar iso))

(test-begin "calendar")

(test-assert (calendar? calendar:gregorian))
(test-assert (calendar? calendar:julian))
(test-assert (calendar? calendar:system))

(test-assert (calendar-date? (time-utc->calendar-date (current-time))))

(let ((time (current-time))
      (absolute-1-in-utc (make-time time-utc 0 -62135553600))
      (cet (timezone "CET"))
      (gmt (timezone "GMT")))
  (test-assert (time=? time (calendar-date->time-utc
			     (time-utc->calendar-date time))))
  (test-equal 25/24
	      (calendar-date-absolute-date
	       (time-utc->calendar-date absolute-1-in-utc cet)))
  (test-equal 1
	      (calendar-date-absolute-date
	       (time-utc->calendar-date absolute-1-in-utc gmt)))
  (test-equal (time-utc->julian-day time)
	      (calendar-date->julian-day (time-utc->calendar-date time gmt))))

(test-assert (gregorian-leap-year? 4))
(test-assert (not (gregorian-leap-year? 2100)))
(test-equal '(0 0 0 12 1 1 1)
  (let-values ((r (absolute->gregorian-components 1 (timezone "GMT")))) r))
(test-equal '(0 0 0 13 1 1 1)
  (let-values ((r (absolute->gregorian-components 1 (timezone "CET")))) r))
(test-equal '(0 0 0 12 31 12 -1)
  (let-values ((r (absolute->gregorian-components 0 (timezone "GMT")))) r))
(test-equal '(0 0 0 0 1 1 1)
  (let-values ((r (absolute->gregorian-components 0.5 (timezone "GMT")))) r))

(test-equal '(0 0 0 13 1 1 2)
  (let-values ((r (absolute->iso-components 365 (timezone "CET")))) r))
(test-equal '(0 0 0 12 7 52 0)
  (let-values ((r (absolute->iso-components 0 (timezone "GMT")))) r))
(test-equal '(0 0 0 0 1 1 2)
  (let-values ((r (absolute->iso-components 364.5 (timezone "GMT")))) r))

(test-equal 0 (iso-components->absolute 0 0 0 12 7 52 0 (timezone "GMT")))
(test-equal 365 (iso-components->absolute 0 0 0 12 1 1 2 (timezone "GMT")))
(test-equal (exact 364.5)
	    (iso-components->absolute 0 0 0 0 1 1 2 (timezone "GMT")))

(test-end)
