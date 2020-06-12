(import (rnrs)
	(text xml schema)
	(sagittarius time)
	(srfi :64))

(test-begin "XML Schema")

(define-syntax base-duration-test
  (syntax-rules ()
    ((_ pred ctr duration-pattern months second)
     (begin
       (test-assert duration-pattern (pred (ctr duration-pattern)))
       (let ((d (ctr duration-pattern)))
	 (test-equal duration-pattern  months (xs:duration-months d))
	 (test-equal duration-pattern second (xs:duration-seconds d)))))))

(define-syntax duration-test
  (syntax-rules ()
    ((_ pattern months second)
     (base-duration-test xs:duration? xs:make-duration pattern months second))))

(define-syntax day-time-duration-test
  (syntax-rules ()
    ((_ pattern months second)
     (base-duration-test xs:day-time-duration?
			 xs:make-day-time-duration pattern months second))))

(define-syntax month-year-duration-test
  (syntax-rules ()
    ((_ pattern months second)
     (base-duration-test xs:year-month-duration?
			 xs:make-year-month-duration pattern months second))))

(duration-test "P1Y" 12 0.0)
(duration-test "P1Y1D" 12 (* 24 60 60.0))
(duration-test "P1YT1H" 12 3600.0)
(duration-test "P1Y1MT1H" 13 3600.0)
(duration-test "P1Y1M1D" 13 (* 24 60 60.0))
(duration-test "P0Y0M0DT1H" 0 3600.0)
(duration-test "P0Y0M0DT1H2M" 0 (+ 3600 120.0))
(duration-test "P0Y0M0DT1H2M3S" 0 (+ 3600 120 3.0))
(duration-test "PT1H" 0 3600.0)
(duration-test "PT1H2M" 0 (+ 3600 120.0))
(duration-test "PT1H2M3S" 0 (+ 3600 120 3.0))

(day-time-duration-test "P1D" 0 (* 24 60 60.0))
(day-time-duration-test "P1DT1H" 0 (+ (* 24 60 60) 3600.0))
(day-time-duration-test "PT1H" 0 3600.0)
(test-error assertion-violation? (xs:make-day-time-duration "P1Y"))

(month-year-duration-test "P1M" 1 0.0)
(test-error assertion-violation? (xs:make-year-month-duration "P1D"))

(test-assert (xs:datetime? (xs:make-datetime "1999-12-31T24:00:00")))
(let ((dt (xs:make-datetime "1999-12-31T24:00:00")))
  (test-equal "normalized year" 2000 (xs:datetime-year dt))
  (test-equal "normalized month" 1 (xs:datetime-month dt))
  (test-equal "normalized day" 1 (xs:datetime-day dt))
  (test-equal "normalized hour" 0 (xs:datetime-hour dt)))

(test-assert (xs:g-year? (xs:make-g-year "2020")))
(test-assert (xs:g-year? (xs:make-g-year "2020Z")))
(test-error "error (1)" time-error? (xs:make-g-year "2020-12"))
(test-assert (xs:g-year-month? (xs:make-g-year-month "2020-12")))
(test-assert (xs:g-year-month? (xs:make-g-year-month "2020-12Z")))
(test-error "error (2)" time-error? (xs:make-g-year-month "2020-12-01"))
(test-assert (xs:g-month? (xs:make-g-month "--12")))
(test-assert (xs:g-month? (xs:make-g-month "--12Z")))
(test-error "error (3)" time-error? (xs:make-g-month "2020-12"))
(test-assert (xs:g-month-day? (xs:make-g-month-day "--12-01")))
(test-assert (xs:g-month-day? (xs:make-g-month-day "--12-01Z")))
(test-error "error (4)" time-error? (xs:make-g-month-day "2020-12"))
(test-assert (xs:g-day? (xs:make-g-day "---01")))
(test-assert (xs:g-day? (xs:make-g-day "---01Z")))
(test-error "error (5)" time-error? (xs:make-g-day "2020-12"))

(test-end)
