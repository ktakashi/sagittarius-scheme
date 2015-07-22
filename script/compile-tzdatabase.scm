#!/bin/sh
#| -*- mode: scheme; coding:utf-8 -*-
exec sagittarius $0 "$@"
|#
#!read-macro=sagittarius/regex
(import (rnrs)
	(rfc http)
	(rfc ftp)
	(rfc gzip)
	(archive)
	(sagittarius)
	(sagittarius regex)
	(sagittarius control)
	(srfi :13)
	(srfi :19)
	(srfi :39)
	(util file)
	(util hashtables)
	(match)
	(getopt)
	(only (binary io) open-chunked-binary-input/output-port)
	(text sxml ssax)
	(text sxml sxpath)
	(text sxml tools)
	(pp))

(define-constant +tz-archive+ "tzdata-latest.tar.gz")
(define-constant +ftp-host+ "ftp.iana.org")
(define-constant +tz-code+ (string-append "/tz/" +tz-archive+))
(define-constant +work-dir+ "tzdata")

(define-constant +windows-mappings-url+
  "http://unicode.org/repos/cldr/trunk/common/supplemental/windowsZones.xml")

;; zone history and rules older than this parameter are stripped 
(define *minimum-year* (make-parameter 0))
;; should we?
(define *maximum-year* (make-parameter 9999))

(define (minimum-year>=? y) (>= y (*minimum-year*)))

;; tz database files
#|
;; what we need are the followings
africa
antarctica
asia
australasia
europe
northamerica
southamerica
pacificnew
etcetera
backzone

;; we don't support them (old names and SystemV something)
backward
systemv

;; this is useless
factory

;; these are not interesting
iso3166.tab
leapseconds
zone1970.tab
zone.tab
leap-seconds.list
yearistype.sh
checklinks.awk
checktab.awk
leapseconds.awk
zoneinfo2tdf.pl
|#
(define-constant +targets+
  '("africa"
    "antarctica"
    "asia"
    "australasia"
    "europe"
    "northamerica"
    "southamerica"
    "pacificnew"
    "etcetera"
    "backzone"
    ))

(define (ftp-sized-file-receiver file size)
  (lambda (in)
    (call-with-output-file file
      (lambda (out)
	(define buf (make-bytevector 4096))
	(let loop ((copied 0))
	  (unless (= copied size)
	    (let ((n (get-bytevector-n! in buf 0 4096)))
	      (put-bytevector out buf 0 n)
	      (loop (+ n copied))))))
      :transcoder #f)))

(define (download-archive)
  (define (follow-symbolic-link s)
    (cond ((#/->\s*(.+)/ s) => (lambda (m) (m 1)))
	  (else (error 'download-archive "unknown stat format" s))))
  ;; input e.g.
  ;; "213-Status follows:-rw-r--r-- 1 49 49  185129 Jun 13 20:39 file
  (define (retrieve-size s)
    (let ((l (string-split  s #/\s+/)))
      (string->number (list-ref l 5))))
  (let ((conn (ftp-login +ftp-host+)))
    (guard (e (else (report-error e) (ftp-quit conn)))
      (let* ((file (follow-symbolic-link (ftp-stat conn +tz-code+)))
	     (real (string-append "/tz/" file))
	     (size (retrieve-size (ftp-stat conn real))))
      (ftp-get conn real
	       :receiver (ftp-sized-file-receiver +tz-archive+ size))
      (ftp-quit conn)))))

(define (parse&collect in zones aliases)
  (define (trim-comment line) (regex-replace-all #/#.*/ line ""))
  (define (parse-zone line in)
    (define (finish name latest prevs)
      (hashtable-set! zones name (cons latest prevs)))
    ;; Zone	NAME		GMTOFF	RULES	FORMAT	[UNTIL]
    (let ((ls (string-split (trim-comment line) #/\s+/)))
      (if (= (length ls) 5)
	  (finish (cadr ls) (cddr ls) ())
	  ;; ok UNTIL is there so keep goind
	  (let ((name (cadr ls)))
	    (let loop ((zones (list (cddr ls))))
	      (let ((line (get-line in)))
		(if (eof-object? line)
		    (finish name (car zones) (cdr zones))
		    (let ((ls (string-split (trim-comment line) #/\s+/)))
		      ;; now this doesn't have Zone and NAME
		      (cond ((null? ls) (loop zones))
			    ((= (length ls) 3) (finish name ls zones))
			    (else (loop (cons ls zones))))))))))))
  ;; Link original alias
  (define (parse-link line)
    (cond ((#/Link\s+([^\s]+)\s+([^\s]+)/ line) =>
	   (lambda (m) (hashtable-set! aliases (m 2) (m 1))))
	  (else (format (current-error-port) "unknown Link format ~a~%" line))))
  ;; only remove the frist Rule element
  (define (parse-rule line) (cdr (string-split (trim-comment line) #/\s+/)))
  (let loop ((rules '()))
    (let ((line (get-line in)))
      (cond ((eof-object? line) rules)
	    ((string-prefix? "Zone" line) 
	     (parse-zone line in)
	     (loop rules))
	    ((string-prefix? "Rule" line)
	     (loop (cons (parse-rule line) rules)))
	    ((string-prefix? "Link" line)
	     (parse-link line)
	     (loop rules))
	    (else (loop rules))))))

;; (offset . w|u|s)
(define (parse-time-at time)
  (cond ((#/(-)?(\d{1,2}):(\d{2})(?::)?(\d{2})?([wus])?/ time) =>
	 (lambda (m)
	   (let ((revert? (m 1))
		 (hour (m 2))
		 (min  (m 3))
		 (sec? (m 4))
		 (type? (m 5)))
	     (cons 
	      ((if revert? - +)
	       (+ (* (string->number hour) 3600)
		  (* (string->number min) 60)
		  (if sec? (string->number sec?) 0)))
	      (if type? (string->symbol type?) 'w)))))
	((#/\d+/ time) (cons (string->number time) 'w))
	((string=? time "-") (cons 0 'w))
	(else (error 'parse-time-at "unknown time format" time))))

(define (parse-date-on date)
  (cond ((#/^\d+$/ date) (string->number date))
	((#/last(\w+)/ date) =>
	 (lambda (m) (cons (string->symbol (m 1)) 'last)))
	((#/(\w+)>=(\d+)/ date) =>
	 (lambda (m) (cons (string->symbol (m 1)) (string->number (m 2)))))
	(else (error 'parse-time-at "unknown AT date format" date))))

(define-constant +month-prefix+
  '((jan . 1) (feb . 2) (mar . 3) (apr . 4) (may . 5) (jun . 6) 
    (jul . 7) (aug . 8) (sep . 9) (oct . 10) (nov . 11) (dec . 12)))
(define (name->month name)
  (let ((prefix (string-downcase (substring name 0 3))))
    (cdr (assoc (string->symbol prefix) +month-prefix+))))

(define (day-of-week y m d)
  #|
    dayofweek(y, m, d)/* 1 <= m <= 12,  y > 1752 (in the U.K.) */
    {
        static int t[] = {0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4};
        y -= m < 3;
        return (y + y/4 - y/100 + y/400 + t[m-1] + d) % 7;
    }
    from https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week
  |#
  (define t #(0 3 2 5 0 3 5 1 4 6 2 4))
  (let ((y (if (< m 3) (- y 1) y)))
    (mod (+ (- (+ y (div y 4)) (div y 100))
	    (div y 400) (vector-ref t (- m 1)) d)
	 7)))

(define-constant +day-of-week+
  '((Sun . 0) (Mon . 1) (Tue . 2) (Wed . 3) (Thu . 4) (Fri . 5) (Sat . 6)))
#|
    January - 31 days
    February - 28 days; 29 days in Leap Years
    March - 31 days
    April - 30 days
    May - 31 days
    June - 30 days
    July - 31 days
    August - 31 days
    September - 30 days
    October - 31 days
    November - 30 days
    December - 31 days 
|#
(define-constant +days-of-months+
  ;; 1  2  3  4  5  6  7  8  9 10 11 12
  #(31 28 31 30 31 30 31 31 30 31 30 31))
(define (leap-year? y) (and (not (zero? (mod y 1000))) (zero? (mod y 4))))

(define (resolve-day d y m)
  (define (last-day-of m)
    (let ((d (vector-ref +days-of-months+ (- m 1))))
      (if (and (= m 2) (leap-year? y))
	  (+ d 1)
	  d)))
  (if (number? d)
      d 
      (match d
	((day . 'last)
	 (unless (and y m) 
	   (error 'resolve-day "can't resolve without year and month"))
	 (let* ((last (last-day-of m))
		(last-day (day-of-week y m last))
		(target (cdr (assq day +day-of-week+)))
		(off (abs (- last-day target))))
	   (- last off)))
	((day . n)
	 (unless (and y m) 
	   (error 'resolve-day "can't resolve without year and month"))
	 ;; kinda trust the definition...
	 (let* ((last (+ n 7))
		(last-day (day-of-week y m last))
		(target (cdr (assq day +day-of-week+)))
		(off (abs (- last-day target))))
	   (- last off))))))
(define (->utc-second base-offset y m d off?)
  (let ((d (make-date 0 0 0 0 (if d (resolve-day d y m) 0) (if m m 0) y
		      ;; for now we ignore the difference between 'w' and 's'
		      ;; and treat them kinda 'u' (always the same offset)
		      ;; (honestly I don't know what the differenct between
		      ;;  u(universal) and s(standard). Are they not the
		      ;;  same?)
		      ;; NB: this is the end of the timezone thus, most of
		      ;;     the time it's neligible anyway.
		      (if off? (car off?) base-offset))))
    (time-second (date->time-utc d))))
  

(define (compile-zones zones)
  (define (compose name histories)
    (define (hour->second time) (car (parse-time-at time)))

    (define (basics . spec)
      (match spec
	((off rule-name zone-name)
	 (list (hour->second off)
	       (if (string=? rule-name "-") #f rule-name)
	       (regex-replace-all #/%s/ zone-name "~a")
	       ))
	(_ (error 'why "why" spec))))
    (define (parse-rest rest)
      (define (finish mon day time)
	(list (name->month mon)
	      (and day (parse-date-on day))
	      (and time (parse-time-at time))))
      (match rest
	(() '(#f #f #f))
	((mon) (finish mon #f #f))
	((mon day?) (finish mon day? #f))
	((mon day? time) (finish mon day? time))))

    (cons name
	  (reverse!
	   (fold-left (lambda (acc history)
			(match history
			  ((off rule-name zone-name year rest ...)
			   (let ((y (string->number year))
				 (rest (parse-rest rest))
				 (base (basics off rule-name zone-name)))
			     ;; we don't need you
			     (if (minimum-year>=? y)
				 (cons `(,@base
					 ,(apply ->utc-second
						 (car base) y rest))
				       acc)
				 acc)))
			  (_ (error 'why "why" name history))))
		      (list (apply basics (car histories)))
		      (cdr histories)))))

  (hashtable-map compose zones))

(define (compile-rules rules)
  (define store (make-string-hashtable))  
  (dolist (rule rules)
    (match rule
      ((name start end type in on at save letter)
       (define (sort-by-year r*)
	 ;; From man zic(8)
	 ;; even though it has only 'min' or 'max' but the manual says
	 ;; the word 'minumum' or 'maximum'. thus to keep it as portable
	 ;; as possible, we use prefix. (at least we can rely on the
	 ;; fact that this is all lower case.)
	 ;; TODO max and min year should be configurable.
	 (let* ((sy (if (string-prefix? "min" start) 
			(*minimum-year*)
			(string->number start)))
		(ey (cond ((string-prefix? "max" end) (*maximum-year*))
			  ((string=? "only" end) sy)
			  (else (string->number end))))
		;; seems there is no type other than '-'
		;; as my understanding this is mere validation, so
		;; ignore it for now.
		;; see, yearistype.sh in tzcode
		;; (type (string=? "-" type))
		(in   (name->month in))
		(on   (parse-date-on on))
		(at   (parse-time-at at))
		(save (car (parse-time-at save)))  ;; w and s are not used
		(letter (and (not (string=? letter "-")) letter)))
	   (if (minimum-year>=? sy)
	       (list-sort (lambda (a b) (< (car b) (car a)))
			  (cons (list sy ey in on at save letter) r*))
	       r*)))
			   
       (hashtable-update! store name sort-by-year '()))))
  (hashtable->alist store))

(define (compile-tz-database)
  (define zones (make-string-hashtable))
  (define rules '())
  (define aliases (make-string-hashtable))
  (define (collect)
    (define gin (open-gzip-input-port
		 (open-file-input-port (build-path +work-dir+ +tz-archive+)
				       (file-options no-fail)
				       (buffer-mode block) #f)
		 :owner? #t))
    (call-with-port gin
      (lambda (in)	 
        (call-with-input-archive-port 'tar in
	  (lambda (ain)
	    (do-entry (e ain)
	      (when (member (archive-entry-name e) +targets+)
		(let ((in/out (open-chunked-binary-input/output-port)))
		  (extract-entry e in/out)
		  (set-port-position! in/out 0)
		  ;; returns rules so append it
		  (let ((r (parse&collect (transcoded-port in/out 
							   (native-transcoder))
					  zones aliases)))
		    (set! rules (append! rules r)))))))))))
  (define (compile)
    (define (sort&->vector l)
      (list->vector (list-sort (lambda (a b) (string< (car a) (car b)))
			       l)))
    (let ((compiled-zones (compile-zones zones))
	  (compiled-rules (compile-rules rules))
	  (compiled-alias (hashtable->alist aliases)))
      ;; we store them in vector so that cache won't do some crazyly
      ;; deep recursion
      (vector (sort&->vector compiled-zones)
	      (sort&->vector compiled-rules)
	      compiled-alias)))
  (collect)
  (compile))

(define (create-win-mappings out)
  (define (compile-map-zone map-zone)
    (let ((tzid (sxml:attr map-zone 'type))
	  (territory (sxml:attr map-zone 'territory))
	  (zone-id (sxml:attr map-zone 'other)))
      (cons zone-id (vector territory (car (string-split tzid #/\s+/))))))

  (define (emit zones)
    (let ((sorted (list-sort (lambda (a b) (string<? (car a) (car b))) zones)))
      (when (file-exists? out) (delete-file out))
      (call-with-output-file out
	(lambda (o) (pp (list->vector sorted) o)))))

  (print "Downloading Windows ZoneID -> TZID mapping file")
  (let*-values (((server path) (url-server&path +windows-mappings-url+))
		((s h body) (http-get server path)))
    (print "HTTP status: " s)
    (let* ((sxml (with-exception-handler
		  (lambda (e) (and (serious-condition? e) (raise e)))
		  (lambda ()
		    (ssax:xml->sxml (open-string-input-port body) '()))))
	   (map-zones ((sxpath "//mapZone") sxml)))
      ;; other=win name
      ;; type=tzid
      ;; territory=country name (2 letter or 3 digits)
      (emit (map compile-map-zone map-zones)))))

(define (usage)
  (print "compile-tzdatabase.scm -o output -w windows-mappings [-r|--remove]")
  (exit -1))

;; TODO create minimum and maximum years arguments
(define (main args)
  (with-args (cdr args)
      ((out    (#\o "out") #t (usage))
       (remove (#\r "remove") #f #f)
       (clean? (#\c "clean") #f #f)
       (win-map (#\w "win-map") #t (usage))
       )
    (when clean?
      (print "Removing file:" out)
      (when (file-exists? out) (delete-file out))
      (exit 0))
    (unless (file-exists? +work-dir+) (create-directory +work-dir+))
    (parameterize ((current-directory +work-dir+)) ;; change directory
      (unless (file-exists? +tz-archive+) 
	(print "Downloading TZ database")
	(download-archive))
      (print "Start compiling"))
    (let ((r (compile-tz-database)))
      (when (file-exists? out) (delete-file out))
      (call-with-output-file out
	(lambda (out)
	  (display 
	   ";; This file is automatically generated. !! DO NOT EDIT !!" out)
	  (newline out)
	  (pp r out))))
    (when remove (delete-directory* +work-dir+))
    (create-win-mappings win-map)
    (print "Done!")))
