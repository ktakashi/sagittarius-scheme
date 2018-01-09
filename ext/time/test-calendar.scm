(test-begin "calendar")

(test-assert (calendar? calendar:rfc3339))
(test-assert (calendar? calendar:gregorian))
(test-assert (calendar? calendar:julian))
(test-assert (calendar? calendar:system))

(test-assert (calendar-date? (time-utc->calendar-date (current-time))))

(test-end)
