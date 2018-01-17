(import (sagittarius timezone))

(test-begin "timezone")

(test-assert "timezone?" (timezone? (timezone "GMT")))

(let ((summer (date->time-utc (make-date 0 0 0 0 20 7 2015  7200)))
      (winter (date->time-utc (make-date 0 0 0 0 20 12 2015 3600)))
      (tz (timezone "Europe/Amsterdam")))
  (test-equal "timezone-name" "Europe/Amsterdam" (timezone-name tz))
  (test-equal "timezone-raw-offset" 3600 (timezone-raw-offset tz))
  (test-equal "timezone-offset(1)" 7200 (timezone-offset tz summer))
  (test-equal "timezone-offset(2)" 3600 (timezone-offset tz winter))
  (test-assert "dst? (1)" (timezone-dst? tz summer))
  (test-assert "dst? (2)" (not (timezone-dst? tz winter))))

;; timezone rule (kinda boundary test)
;; Rule	C-Eur	1977	1980	-	Apr	Sun>=1	 2:00s	1:00	S
;; Rule	C-Eur	1945	only	-	Sep	16	 2:00s	0	-
;; 1977/4/3 is the first Sunday
(let ((summer (date->time-utc (make-date 0 0 0 2 3 4 1977 3600)))
      ;; TODO the time part is ignored for now
      (winter (date->time-utc (make-date 0 0 0 2 16 9 1945 3600)))
      (tz (timezone "CET")))
  (test-equal "timezone-name" "CET" (timezone-name tz))
  (test-equal "timezone-raw-offset" 3600 (timezone-raw-offset tz))
  (test-equal "timezone-offset(1)" 7200 (timezone-offset tz summer))
  (test-equal "timezone-offset(2)" 3600 (timezone-offset tz winter))
  (test-assert "dst? (1)" (timezone-dst? tz summer))
  (test-assert "dst? (2)" (not (timezone-dst? tz winter))))

;; history
(let ((tz (timezone "Europe/Dublin"))
      (now (date->time-utc (make-date 0 0 0 0 24 7 2015 0)))
      ;; 1:00	-	IST	1971 Oct 31  2:00u
      (no-rule-past (date->time-utc (make-date 0 0 0 0 24 7 1971 0)))
      ;; 0:00	GB-Eire	GMT/IST	1968 Oct 27
      (rule-past (date->time-utc (make-date 0 0 0 0 24 7 1968 0))))
  (test-equal "timezone-short-name (1)" "IST/GMT" (timezone-short-name tz now))
  (test-equal "timezone-short-name (2)" "IST/GMT" (timezone-short-name tz no-rule-past))
  (test-equal "timezone-offset" 3600 (timezone-offset tz no-rule-past))

  (test-equal "timezone-raw-offset" 3600 (timezone-raw-offset tz))
  (test-equal "timezone-raw-offset" 3600 (timezone-raw-offset tz no-rule-past))

  (test-equal "timezone-short-name (3)" "GMT/IST"
	      (timezone-short-name tz rule-past))
  (test-equal "timezone-raw-offset" 0 (timezone-raw-offset tz rule-past))
  )

;; name-list
(test-assert "timezone-name-list"
	     (member "Europe/Amsterdam" (timezone-name-list)))

;; call #150
;; Asia/Tokyo has 4 rules but none of them indicates later than 1951
;; (I don't know why then they put such a stupid rule but anyway).
(test-equal "Asia/Tokyo" 32400 (timezone-offset (timezone "Asia/Tokyo")))


;; string->date doesn't consider timezone properly
;; This test is only effective on the place where summer time is
;; adopted.
(let* ((d (string->date "2016/3/26 15:00:00" "~Y/~m/~d ~H:~M:~S"))
       (tz (local-timezone))
       (off (timezone-offset tz (date->time-utc d))))
  (test-equal "timezone offset" off (date-zone-offset d)))


(test-end)

