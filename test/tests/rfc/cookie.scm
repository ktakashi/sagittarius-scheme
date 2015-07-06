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

;; TODO more tests


(test-end)
