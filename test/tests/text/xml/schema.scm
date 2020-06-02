(import (rnrs)
	(text xml schema)
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

(duration-test "P1Y" 12 0)
(duration-test "P1Y1D" 12 (* 24 60 60))
(duration-test "P1YT1H" 12 3600)
(duration-test "P1Y1MT1H" 13 3600)
(duration-test "P1Y1M1D" 13 (* 24 60 60))
(duration-test "P0Y0M0DT1H" 0 3600)
(duration-test "P0Y0M0DT1H2M" 0 (+ 3600 120))
(duration-test "P0Y0M0DT1H2M3S" 0 (+ 3600 120 3))
(duration-test "PT1H" 0 3600)
(duration-test "PT1H2M" 0 (+ 3600 120))
(duration-test "PT1H2M3S" 0 (+ 3600 120 3))

(day-time-duration-test "P1D" 0 (* 24 60 60))
(day-time-duration-test "P1DT1H" 0 (+ (* 24 60 60) 3600))
(day-time-duration-test "PT1H" 0 3600)
(test-error assertion-violation? (xs:make-day-time-duration "P1Y"))

(month-year-duration-test "P1M" 1 0)
(test-error assertion-violation? (xs:make-year-month-duration "P1D"))

(test-end)
