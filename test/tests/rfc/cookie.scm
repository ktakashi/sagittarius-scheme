(import (rnrs)
	(rfc cookie)
	(srfi :64)
	(srfi :19))

(test-begin "RFC cookie")

(test-assert "cookie? (1)" (not (cookie? 'a)))
(test-assert "cookie? (2)" (make-cookie "name" "value"))
(test-assert "cookie? (3)" (parse-cookie-string "name=value"))
(test-assert "cookie? (4)" (parse-cookie (open-string-input-port "name=value")))

(let ((cookie (parse-cookie-string "name=value;Max-Age=1")))
  (test-equal "cookie name" "name" (cookie-name cookie))
  (test-equal "cookie value" "value" (cookie-value cookie))
  (test-equal "cookie max-age" 1 (cookie-max-age cookie)))


(let ((cookie (parse-cookie-string "name=value;Expires=Thu, 2 Jan 2014")))
  (test-assert "cookie expires"  
	       (time=? (date->time-utc (make-date 0 0 0 0 2 1 2014 0))
		       (date->time-utc (cookie-expires cookie)))))

(let ((cookies (parse-cookies-string "c=1; c=2=;")))
  (test-equal 2 (length cookies))
  (test-assert (for-all cookie? cookies)))

(test-error condition? (parse-cookies-string "c=1; c"))

(test-assert (cookie-jar? (make-cookie-jar)))
(let ((jar (make-cookie-jar)))
  (test-assert (cookie-jar?
		(cookie-jar-add-cookie! jar (make-cookie "c" "v"))))
  (test-equal 1 (cookie-jar-size jar))
  (cookie-jar? (cookie-jar-add-cookie! jar (make-cookie "c" "v")))
  (test-equal 1 (cookie-jar-size jar))
  (test-assert (cookie-jar?
		(cookie-jar-delete-cookie! jar (make-cookie "c" "v"))))
  (test-equal 0 (cookie-jar-size jar))

  (test-assert (cookie-jar?
		(cookie-jar-add-cookie! jar
					(make-cookie "c" "v")
					(make-cookie "c" "v" :path "/"))))
  (test-equal 2 (cookie-jar-size jar))
  (test-assert (let ((cookies (cookie-jar->cookies
			       jar
			       (lambda (cookie)
				 (equal? (cookie-path cookie) "/")))))
		 (test-equal 1 (length cookies))
		 (equal? (cookie-path (car cookies)) "/"))))

(test-end)
