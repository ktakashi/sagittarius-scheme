;; -*- mode:scheme; coding:utf-8 -*-
#!read-macro=sagittarius/regex
(import (rnrs)
	(rfc :5322)
	(sagittarius regex)
	(util list)
	(srfi :1 lists)
	(srfi :13 strings)
	(srfi :19 time)
	(srfi :26 cut)
	(srfi :64 testing))


(test-begin "RFC 5322 tests")
;; test cases from Gauche
(define rfc5322-header1
  "Received: by foo.bar.com id ZZZ55555; Thu, 31 May 2001 16:38:04 -1000 (HST)
Received: from ooo.ooo.com (ooo.ooo.com [1.2.3.4])
	by foo.bar.com (9.9.9+3.2W/3.7W-) with ESMTP id ZZZ55555
	for <yoo@bar.com>; Thu, 31 May 2001 16:38:02 -1000 (HST)
Received: from zzz ([1.2.3.5]) by ooo.ooo.com  with Maccrosoft SMTPSVC(5.5.1877.197.19);
	 Thu, 31 May 2001 22:33:16 -0400
Message-ID: <beefbeefbeefbeef@ooo.ooo.com>
Subject: Bogus Tester
From: Bogus Sender <bogus@ooo.com>
To: You <you@bar.com>, Another <another@ooo.com>
Date: Fri, 01 Jun 2001 02:37:31 (GMT)
Mime-Version: 1.0
Content-Type: text/html
Content-Transfer-Encoding: quoted-printable
X-MSMail-Priority: Normal
X-mailer: FooMail 4.0 4.03 (SMT460B92F)
Content-Length: 4349

")

(define rfc5322-header1-list
  '(("received" "by foo.bar.com id ZZZ55555; Thu, 31 May 2001 16:38:04 -1000 (HST)")
    ("received" "from ooo.ooo.com (ooo.ooo.com [1.2.3.4])	by foo.bar.com (9.9.9+3.2W/3.7W-) with ESMTP id ZZZ55555	for <yoo@bar.com>; Thu, 31 May 2001 16:38:02 -1000 (HST)")
    ("received" "from zzz ([1.2.3.5]) by ooo.ooo.com  with Maccrosoft SMTPSVC(5.5.1877.197.19);	 Thu, 31 May 2001 22:33:16 -0400")
    ("message-id" "<beefbeefbeefbeef@ooo.ooo.com>")
    ("subject" "Bogus Tester")
    ("from" "Bogus Sender <bogus@ooo.com>")
    ("to" "You <you@bar.com>, Another <another@ooo.com>")
    ("date" "Fri, 01 Jun 2001 02:37:31 (GMT)")
    ("mime-version" "1.0")
    ("content-type" "text/html")
    ("content-transfer-encoding" "quoted-printable")
    ("x-msmail-priority" "Normal")
    ("x-mailer" "FooMail 4.0 4.03 (SMT460B92F)")
    ("content-length" "4349")
    ))

(test-equal "rfc5322-read-headers" #t
	    (equal? rfc5322-header1-list
		    (rfc5322-read-headers
		     (open-string-input-port rfc5322-header1))))

;; token parsers
(test-equal "rfc5322-field->tokens (basic)"
	    '(("aa") ("bb") ("cc") ("dd") ("ee") (" a\"aa\\aa (a)"))
	    (map rfc5322-field->tokens
		 '("aa"
		   "  bb   "
		   " (comment) cc(comment)"
		   " (co\\mm$$*##&$%ent) dd(com (me) nt)"
		   "\"ee\""
		   "  \" a\\\"aa\\\\aa (a)\" (comment\\))")))

(test-equal "rfc5322-field->tokens"
	    '("from" "aaaaa.aaa.org" "by" "ggg.gggg.net" "with" "ESMTP" "id" "24D50175C8")
	    (rfc5322-field->tokens
	     "from aaaaa.aaa.org (aaaaa.aaa.org [192.168.0.9]) by ggg.gggg.net (Postfix) with ESMTP id 24D50175C8"))


(test-equal "rfc5322-parse-date" '(2003 3 4 12 34 56 -3600 2)
	    (receive r (rfc5322-parse-date "Tue,  4 Mar 2003 12:34:56 -3600") r))

(test-equal "rfc5322-parse-date" '(2003 3 4 12 34 56 0 2)
	    (receive r (rfc5322-parse-date "Tue,  4 Mar 2003 12:34:56 UT") r))

(test-equal "rfc5322-parse-date (no weekday)" '(2003 3 4 12 34 56 -3600 #f)
	    (receive r (rfc5322-parse-date "4 Mar 2003 12:34:56 -3600") r))

(test-equal "rfc5322-parse-date (no timezone)" '(2003 3 4 12 34 56 #f #f)
	    (receive r (rfc5322-parse-date "4 Mar 2003 12:34:56") r))

(test-equal "rfc5322-parse-date (old tz)" '(2003 3 4 12 34 56 #f #f)
	    (receive r (rfc5322-parse-date "4 Mar 2003 12:34:56 jst") r))

(test-equal "rfc5322-parse-date (no seconds)" '(2003 3 4 12 34 #f 900 #f)
	    (receive r (rfc5322-parse-date "4 Mar 2003 12:34 +0900") r))

(test-equal "rfc5322-parse-date (no seconds)" '(2003 3 4 12 34 #f 900 2)
	    (receive r (rfc5322-parse-date "Tue, 04 Mar 2003 12:34 +0900") r))

(test-equal "rfc5322-parse-date (2digit year)" '(2003 3 4 12 34 56 -3600 2)
	    (receive r (rfc5322-parse-date "Tue,  4 Mar 03 12:34:56 -3600") r))

(test-equal "rfc5322-parse-date (2digit year)" '(1987 3 4 12 34 56 -3600 2)
	    (receive r (rfc5322-parse-date "Tue,  4 Mar 87 12:34:56 -3600") r))

(test-equal "rfc5322-parse-date (Weekday, exhausive)" '(0 1 2 3 4 5 6 #f)
	    (map-with-index
	     (lambda (ind wday)
	       (receive (y m d H M S tz wd)
		   (rfc5322-parse-date
		    (format "~a, ~a Jan 2000 00:00:00 +0000" wday (+ 2 ind)))
		 wd))
	     '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Znn")))

(test-equal "rfc5322-parse-date (Months, exhausive)"
	    '(1 2 3 4 5 6 7 8 9 10 11 12 #f)
	    (map (lambda (mon)
		   (receive (y m d H M S tz wd)
		       (rfc5322-parse-date
			(format "1 ~a 1999 00:00:00 +0000" mon))
		     m))
		 '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
		   "Sep" "Oct" "Nov" "Dec" "Zzz")))

(test-equal "rfc5322-parse-date (invalid)" '(#f #f #f #f #f #f #f #f)
	    (receive r (rfc5322-parse-date "Sun 2 Mar 2002") r))

(test-equal "date->rfc5322-date" "Sun, 29 Nov 2009 01:23:45 +0000"
	    (date->rfc5322-date (make-date 0 45 23 01 29 11 2009 0)))
(test-equal "date->rfc5322-date" "Sun, 29 Nov 2009 01:23:45 +0900"
	    (date->rfc5322-date (make-date 0 45 23 01 29 11 2009 32400)))
(test-equal "date->rfc5322-date" "Sun, 29 Nov 2009 01:23:45 -0830"
	    (date->rfc5322-date (make-date 0 45 23 01 29 11 2009 -30600)))
(test-equal "date->rfc5322-date" "Sun, 29 Nov 2009 01:23:45 +0030"
	    (date->rfc5322-date (make-date 0 45 23 01 29 11 2009 1800)))


(test-equal "rfc5322-invalid-header-field" #f
	    (rfc5322-invalid-header-field "abcde"))

(test-equal "rfc5322-invalid-header-field" 'bad-character
	    (rfc5322-invalid-header-field "abc\x3030; def"))

(test-equal "rfc5322-invalid-header-field" 'bad-character
	    (rfc5322-invalid-header-field "abc\x00; def"))
(test-equal "rfc5322-invalid-header-field" 'line-too-long
	    (rfc5322-invalid-header-field (make-string 1000 #\a)))
(test-equal "rfc5322-invalid-header-field" 'line-too-long
	    (rfc5322-invalid-header-field
	     (string-append (string-join (make-list 5 (make-string 78 #\a))
					 "\r\n ")
			    (make-string 1000 #\a))))
(test-equal "rfc5322-invalid-header-field" 'stray-crlf
	    (rfc5322-invalid-header-field
	     (string-join (make-list 5 (make-string 78 #\a)) "\r\n")))
(test-equal "rfc5322-invalid-header-field" 'stray-crlf
	    (rfc5322-invalid-header-field "abc\ndef"))
(test-equal "rfc5322-invalid-header-field" 'stray-crlf
	    (rfc5322-invalid-header-field "abc\rdef"))

(test-equal "rfc5322-write-headers"
	    "name: Shiro Kawai\r\n\
        address: 1234 Lambda St.\r\n \
        Higher Order Functions, HI, 99899\r\n\
        registration-date: 2007-12-10\r\n\r\n"
	    (call-with-string-output-port
	     (cut rfc5322-write-headers
		  '(("name" "Shiro Kawai")
		    ("address" "1234 Lambda St.\r\n Higher Order Functions, HI, 99899")
		    ("registration-date" "2007-12-10"))
		  :output <>)))

(test-equal "rfc5322-write-headers (ignore error)"
	    (make-list 2 "name: Shiro\x00;Kawai\r\n\r\n")
	    (map (lambda (x)
		   (call-with-string-output-port
		    (cut rfc5322-write-headers '(("name" "Shiro\x00;Kawai"))
			 :check x
			 :output <>)))
		 '(#f :ignore)))

(test-equal "rfc5322-write-headers (continue)"
	    "x: A\r\nx: B\r\nx: C\r\n\r\n"
	    (call-with-string-output-port
	      (lambda (p)
		(rfc5322-write-headers '(("x" "A") ("x" "B")) :output p :continue #t)
		(rfc5322-write-headers '(("x" "C")) :output p))))

(test-equal "rfc5322-header-ref"
	    "by foo.bar.com id ZZZ55555; Thu, 31 May 2001 16:38:04 -1000 (HST)"
	    (rfc5322-header-ref rfc5322-header1-list "received"))
(test-equal "rfc5322-header-ref*"
	    '("by foo.bar.com id ZZZ55555; Thu, 31 May 2001 16:38:04 -1000 (HST)"
	      "from ooo.ooo.com (ooo.ooo.com [1.2.3.4])	by foo.bar.com (9.9.9+3.2W/3.7W-) with ESMTP id ZZZ55555	for <yoo@bar.com>; Thu, 31 May 2001 16:38:02 -1000 (HST)"
	      "from zzz ([1.2.3.5]) by ooo.ooo.com  with Maccrosoft SMTPSVC(5.5.1877.197.19);	 Thu, 31 May 2001 22:33:16 -0400")
	    (rfc5322-header-ref* rfc5322-header1-list "received"))

(test-end)
