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
#|
From europe file
# Zone	NAME		GMTOFF	RULES	FORMAT	[UNTIL]
Zone	Europe/Dublin	-0:25:00 -	LMT	1880 Aug  2
			-0:25:21 -	DMT	1916 May 21  2:00s
			-0:25:21 1:00	IST	1916 Oct  1  2:00s
			 0:00	GB-Eire	%s	1921 Dec  6 # independence
			 0:00	GB-Eire	GMT/IST	1940 Feb 25  2:00s
			 0:00	1:00	IST	1946 Oct  6  2:00s
			 0:00	-	GMT	1947 Mar 16  2:00s
			 0:00	1:00	IST	1947 Nov  2  2:00s
			 0:00	-	GMT	1948 Apr 18  2:00s
			 0:00	GB-Eire	GMT/IST	1968 Oct 27
# From Paul Eggert (2018-01-18):
# The next line should look like this:
#			 1:00	Eire	IST/GMT
# However, in January 2018 we discovered that the Eire rules cause
# problems with tests for ICU:
# https://mm.icann.org/pipermail/tz/2018-January/025825.html
# and with tests for OpenJDK:
# https://mm.icann.org/pipermail/tz/2018-January/025822.html
# To work around this problem, use a traditional approximation for
# time stamps after 1971-10-31 02:00 UTC, to give ICU and OpenJDK
# developers breathing room to fix bugs.  This approximation has
# correct UTC offsets, but results in tm_isdst flags are the reverse
# of what they should be.  This workaround is temporary and should be
# removed reasonably soon.
			 1:00	-	IST	1971 Oct 31  2:00u
			 0:00	GB-Eire	GMT/IST	1996
			 0:00	EU	GMT/IST
# End of workaround for ICU and OpenJDK bugs.
|#
(let ((tz (timezone "Europe/Dublin"))
      (now (date->time-utc (make-date 0 0 0 0 24 7 2015 0)))
      ;; it's just past...
      (no-rule-past (date->time-utc (make-date 0 0 0 0 24 7 1971 0)))
      ;; 0:00	GB-Eire	GMT/IST	1968 Oct 27
      (rule-past (date->time-utc (make-date 0 0 0 0 24 7 1968 0)))

      ;; gb-eire of dublin
      #|
# S.R.&O. 1921, No. 363
Rule	GB-Eire	1921	only	-	Apr	 3	2:00s	1:00	BST
Rule	GB-Eire	1921	only	-	Oct	 3	2:00s	0	GMT
         :
			 0:00	GB-Eire	%s	1921 Dec  6 # independence
         :
      |#
      (gb-eire-dst (date->time-utc (make-date 0 0 0 0 3 4 1921 0)))
      (gb-eire     (date->time-utc (make-date 0 0 0 0 4 10 1921 0))) 
      )
  ;; These 2 will fail due to the comment above
  ;; Remove when it started to pass.
  (test-expect-fail 2)
  (test-equal "timezone-short-name (1)" "IST/GMT" (timezone-short-name tz now))
  (test-equal "timezone-short-name (2)" "IST/GMT" (timezone-short-name tz no-rule-past))
  (test-equal "timezone-offset (1)" 3600 (timezone-offset tz no-rule-past))

  ;; Ditto
  (test-expect-fail 1)
  (test-equal "timezone-raw-offset (1)" 3600 (timezone-raw-offset tz))
  (test-equal "timezone-raw-offset (2)" 3600 (timezone-raw-offset tz no-rule-past))

  (test-equal "timezone-short-name (3)" "GMT/IST"
	      (timezone-short-name tz rule-past))
  (test-equal "timezone-raw-offset (3)" 0 (timezone-raw-offset tz rule-past))

  (test-equal "timezone-short-name (4)" "BST" (timezone-short-name tz gb-eire-dst))
  (test-equal "timezone-offset (2)" 3600 (timezone-offset tz gb-eire-dst))
  (test-equal "timezone-short-name (5)" "GMT" (timezone-short-name tz gb-eire))
  (test-equal "timezone-offset (3)" 0 (timezone-offset tz gb-eire))
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

(let ((tz (timezone "Africa/Accra"))
      (tz2 (timezone "Asia/Tehran")))
  (test-equal (list tz2) (zone-offset->timezones 12600))
  (test-equal '()
	      (zone-offset->timezones -52 (make-time time-utc 0 -1643759947)))
  (test-equal (list tz)
	      (zone-offset->timezones -52 (make-time time-utc 0 -1643759948)))
  (test-equal (list tz)
	      (zone-offset->timezones -52 (make-time time-utc 0 -1643759949))))

(test-end)

