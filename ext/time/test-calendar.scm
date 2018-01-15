(import (sagittarius calendar)
	(sagittarius calendar gregorian)
	(sagittarius calendar iso))

(test-begin "calendar")

(test-assert (calendar? calendar:rfc3339))
(test-assert (calendar? calendar:gregorian))
(test-assert (calendar? calendar:julian))
(test-assert (calendar? calendar:system))

(test-assert (calendar-date? (time-utc->calendar-date (current-time))))

(let ((time (current-time)))
  (test-assert (time=? time (calendar-date->time-utc
			     (time-utc->calendar-date time)))))

(test-assert (gregorian-leap-year? 4))
(test-assert (not (gregorian-leap-year? 2100)))
(test-equal '(0 0 0 12 1 1 1)
  (let-values ((r (absolute->gregorian-component 1 (timezone "GMT")))) r))
(test-equal '(0 0 0 13 1 1 1)
  (let-values ((r (absolute->gregorian-component 1 (timezone "CET")))) r))
(test-equal '(0 0 0 12 31 12 -1)
  (let-values ((r (absolute->gregorian-component 0 (timezone "GMT")))) r))
(test-equal '(0 0 0 0 1 1 1)
  (let-values ((r (absolute->gregorian-component 0.5 (timezone "GMT")))) r))

(test-equal '(0 0 0 13 1 1 2)
  (let-values ((r (absolute->iso-component 365 (timezone "CET")))) r))
(test-equal '(0 0 0 12 7 52 0)
  (let-values ((r (absolute->iso-component 0 (timezone "GMT")))) r))
(test-equal '(0 0 0 0 1 1 2)
  (let-values ((r (absolute->iso-component 364.5 (timezone "GMT")))) r))

(test-equal 0 (iso-component->absolute 0 0 0 12 7 52 0 (timezone "GMT")))
(test-equal 365 (iso-component->absolute 0 0 0 12 1 1 2 (timezone "GMT")))
(test-equal (exact 364.5)
	    (iso-component->absolute 0 0 0 0 1 1 2 (timezone "GMT")))

(test-end)
