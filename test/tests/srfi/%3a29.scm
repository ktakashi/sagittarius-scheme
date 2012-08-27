(import (rnrs)
	(srfi :29)
	(srfi :64))

(test-begin "SRFI-29 tests")

(let ((translations
       '(((en) . ((time . "Its ~a, ~a.")
		  (goodbye . "Goodbye, ~a.")))
	 ((fr) . ((time . "~1@*~a, c'est ~a.")
		  (goodbye . "Au revoir, ~a."))))))
  (for-each (lambda (translation)
	      (let ((bundle-name (cons 'hello-program (car translation))))
		(if (not (load-bundle! bundle-name))
                    (begin
                      (declare-bundle! bundle-name (cdr translation))
                      (store-bundle! bundle-name)))))
	    translations))

(define localized-message
  (lambda (message-name . args)
    (apply format (cons (localized-template 'hello-program
					    message-name)
			args))))

(let ((myname "Fred"))
  (test-equal "localized-message (en)"
	      '("Its 12:00, Fred."  "Goodbye, Fred.")
	      (begin
		(current-language 'en)
		(current-country 'us)
		(list (localized-message 'time "12:00" myname)
		      (localized-message 'goodbye myname))))

  (test-equal "localized-message (fr)"
	      '("Fred, c'est 12:00."  "Au revoir, Fred.")
	      (begin
		(current-language 'fr)
		(current-country 'fr)
		(list (localized-message 'time "12:00" myname)
		      (localized-message 'goodbye myname))))
  )

(test-end)