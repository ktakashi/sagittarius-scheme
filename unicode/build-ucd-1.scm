#|

    produce followings from unicode data 6.1.0:

        case-folding.datum
        general-category-1.datum
        general-category-2.datum
        numeric-property.datum
        other-alphabetic.datum
        other-lowercase.datum
        other-uppercase.datum
        simple-lowercase.datum
        simple-titlecase.datum
        simple-uppercase.datum
        special-casing-lower.datum
        special-casing-title.datum
        special-casing-upper.datum

|#
#< (sagittarius regex) >
(library (anonymous)

  (export)

  (import (rnrs)
	  (sagittarius)
	  (sagittarius regex)
	  (sagittarius control)
	  (util hashtables)
	  (match))

  (define (pregexp-substring s match index)
    (let ((b (list-ref match index)))
      (substring s (car b) (cdr b))))

  (define (ucd-file filename)
    (string-append (current-directory) "/unicode-6.1.0/" filename))

  (define (datum-file filename)
    (string-append (current-directory) "/ucd/" filename))

  (define (skip-whitespace! port)
    (and (not (port-eof? port))
	 ;;(nonblock-byte-ready? port)
	 (char-whitespace? (lookahead-char port))
	 (get-char port)
	 (skip-whitespace! port)))

  (define (read-ucd-line port)
    (skip-whitespace! port)
    (let ((ch (lookahead-char port)))
      (cond ((eof-object? ch)
	     (eof-object))
	    ((eq? ch #\#)
	     (get-line port)
	     (read-ucd-line port))
	    (else
	     (get-line port)))))

  (define (for-each-ucd-line proc input)
    (let loop ()
      (let ((line (read-ucd-line input)))
	(cond ((eof-object? line)
	       (undefined))
	      (else
	       (proc line)
	       (loop))))))

  (define (shrink-range-list lst)
    (if (null? lst)
	'()
	(let loop ((lst lst) (ans '()))
	  (match lst
	    (((e1 . e2)) (reverse (cons (cons e1 e2) ans)))
	    (((e1 . e2) (e3 . e4) more ...)
	     (if (= e2 (- e3 1))
		 (loop (cons (cons e1 e4) more) ans)
		 (loop (cdr lst) (cons (cons e1 e2) ans))))))))

  ; (shrink-range-list '((1 . 2)(3 . 4)(6 . 7)(8 . 15)(16 . 18)))
  ; (code-point) name (general-category) canonical-combining bidi decomposition numeric bidi-mirrored 
  ; unicode-1-name comment (simple-uppercase) (simple-lowercase) (simple-titlecase)
  
  (define (parse-unicodedata)
    (let ((re #/^([A-F0-9]{4,6});[^;]*;([a-zA-Z]{2});[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;(.*);[^;]*;[^;]*;[^;]*;([A-F0-9]{0,6});([A-F0-9]{0,6});([A-F0-9]{0,6})$/
	      ))

      (define ht-general-category-1 (make-eqv-hashtable)) ; code-point < 0x400
      (define ht-general-category-2 (make-eqv-hashtable)) ; 
      (define ht-numeric-property (make-eqv-hashtable))
      (define ht-simple-uppercase (make-eqv-hashtable))
      (define ht-simple-lowercase (make-eqv-hashtable))
      (define ht-simple-titlecase (make-eqv-hashtable))

      (call-with-input-file (ucd-file "UnicodeData.txt")
	(lambda (input)
	  (format #t "~%parsing UnicodeData.txt ...~!");
	  (for-each-ucd-line
	   (lambda (line)
	     (let ((m (re line)))
	       (let ((code-point       (string->number (m 1) 16))
		     (general-category (string->symbol (m 2)))
		     (numeric-property (m 3))
		     (simple-uppercase (m 4))
		     (simple-lowercase (m 5))
		     (simple-titlecase (m 6)))
		 (if (< code-point #x400)
		     (hashtable-set! ht-general-category-1 code-point general-category)
		     (hashtable-set! ht-general-category-2 code-point general-category))
		 (or (string=? numeric-property "")
		     (hashtable-set! ht-numeric-property code-point (string->number numeric-property)))
		 (or (string=? simple-uppercase "")
		     (hashtable-set! ht-simple-uppercase code-point (string->number simple-uppercase 16)))
		 (or (string=? simple-lowercase "")
		     (hashtable-set! ht-simple-lowercase code-point (string->number simple-lowercase 16)))
		 (or (string=? simple-titlecase "")
		     (hashtable-set! ht-simple-titlecase code-point (string->number simple-titlecase 16))))))
	   input)
	  (format #t " done~%~!")

	  (call-with-port
	   (open-file-output-port (datum-file "general-category-1.datum")
				  (file-options no-fail) (buffer-mode block)
				  (native-transcoder))
	   (lambda (output)
	     (format #t "processing general-category-1.datum (~a)...~!"
		     (length (hashtable->alist ht-general-category-1)))
	     (put-datum output (hashtable->alist ht-general-category-1))
	     (format #t " done~%~!")))

	  (call-with-port
	   (open-file-output-port (datum-file "general-category-2.datum")
				  (file-options no-fail) (buffer-mode block)
				  (native-transcoder))
	   (lambda (output)
	     (format #t "processing general-category-2.datum (~a)...~!"
		     (length (hashtable->alist ht-general-category-2)))
	     (put-datum output (hashtable->alist ht-general-category-2))
	     (format #t " done~%~!")))

	  (call-with-port 
	   (open-file-output-port (datum-file "numeric-property.datum")
				  (file-options no-fail) (buffer-mode block)
				  (native-transcoder))
	   (lambda (output)
	     (format #t "processing numeric-property.datum ...~!")
	     (put-datum output (hashtable->alist ht-numeric-property))
	     (format #t " done~%~!")))

	  (call-with-port
	   (open-file-output-port (datum-file "simple-uppercase.datum")
				  (file-options no-fail) (buffer-mode block)
				  (native-transcoder))
	   (lambda (output)
	     (format #t "processing simple-uppercase.datum ...~!")
	     (put-datum output (hashtable->alist ht-simple-uppercase))
	     (format #t " done~%~!")))

	  (call-with-port
	   (open-file-output-port (datum-file "simple-lowercase.datum")
				  (file-options no-fail) (buffer-mode block)
				  (native-transcoder))
	   (lambda (output)
	     (format #t "processing simple-lowercase.datum ...~!")
	     (put-datum output (hashtable->alist ht-simple-lowercase))
	     (format #t " done~%~!")))

	  (call-with-port
	   (open-file-output-port (datum-file "simple-titlecase.datum")
				  (file-options no-fail) (buffer-mode block)
				  (native-transcoder))
	   (lambda (output)
	     (format #t "processing simple-titlecase.datum ...~!")
	     (put-datum output (hashtable->alist ht-simple-titlecase))
	     (format #t " done~%~!")))))))

  (define (parse-proplist)
    (let ((re1 #/^([A-F0-9]{4,6}) *; *([a-zA-Z_]+) # .*$/
	       )
	  (re2 #/^([A-F0-9]{4,6})\.\.([A-F0-9]{4,6}) *; *([a-zA-Z_]+) # .*$/
	       ))

      (define ht-other-uppercase (make-eqv-hashtable))  ; Other_Uppercase
      (define ht-other-lowercase (make-eqv-hashtable))  ; Other_Lowercase
      (define ht-other-alphabetic (make-eqv-hashtable)) ; Other_Alphabetic

      (call-with-input-file (ucd-file "PropList.txt")
       (lambda (input)
	 (format #t "~%parsing PropList.txt ...~!");
	 (for-each-ucd-line
	  (lambda (line)
	    (if (zero? (string-length line))
		#f
		(let ((m1 (re1 line)) (m2 (re2 line)))
		  (cond
		   (m1 (let ((code-point (string->number (m1 1) 16))
			     (property (m1 2)))
			 (and (string=? property "Other_Uppercase")
			      (hashtable-set! ht-other-uppercase code-point code-point))
			 (and (string=? property "Other_Lowercase")
			      (hashtable-set! ht-other-lowercase code-point code-point))
			 (and (string=? property "Other_Alphabetic")
			      (hashtable-set! ht-other-alphabetic code-point code-point)))
		       )
		   (m2 (let ((code-point-from (string->number (m2 1) 16))
			     (code-point-to (string->number (m2 2) 16))
			     (property (m2 3)))
			 (and (string=? property "Other_Uppercase")
			      (hashtable-set! ht-other-uppercase code-point-from code-point-to))
			 (and (string=? property "Other_Lowercase")
			      (hashtable-set! ht-other-lowercase code-point-from code-point-to))
			 (and (string=? property "Other_Alphabetic")
			      (hashtable-set! ht-other-alphabetic code-point-from code-point-to))))
			(else
			 (assertion-violation 'parse-unicode-data "failed to parse unicode data"))))))
	  input)
	 (format #t " done~%~!")

	 (call-with-port
	  (open-file-output-port (datum-file "other-uppercase.datum")
				 (file-options no-fail) (buffer-mode block)
				 (native-transcoder))
	  (lambda (output)
	    (format #t "processing other-uppercase.datum ...~!")
	    (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2)))
				  (hashtable->alist ht-other-uppercase))))
	      (put-char output #\() (put-char output #\linefeed)
	      (for-each (lambda (e)
			  (put-datum output e)
			  (put-char output #\linefeed))
			(shrink-range-list lst))
	      (put-char output #\)) (put-char output #\linefeed))
	    (format #t " done~%~!")))

	 (call-with-port
	  (open-file-output-port (datum-file "other-lowercase.datum")
				 (file-options no-fail) (buffer-mode block)
				 (native-transcoder))
	  (lambda (output)
	    (format #t "processing other-lowercase.datum ...~!")
	    (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2)))
				  (hashtable->alist ht-other-lowercase))))
	      (put-char output #\() (put-char output #\linefeed)
	      (for-each (lambda (e)
			  (put-datum output e)
			  (put-char output #\linefeed))
			(shrink-range-list lst))
	      (put-char output #\)) (put-char output #\linefeed))
	    (format #t " done~%~!")))

	 (call-with-port
	  (open-file-output-port (datum-file "other-alphabetic.datum")
				 (file-options no-fail) (buffer-mode block)
				 (native-transcoder))
	  (lambda (output)
	    (format #t "processing other-alphabetic.datum ...~!")
	    (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2)))
				  (hashtable->alist ht-other-alphabetic))))
	      (put-char output #\() (put-char output #\linefeed)
	      (for-each (lambda (e)
			  (put-datum output e)
			  (put-char output #\linefeed))
			(shrink-range-list lst))
	      (put-char output #\)) (put-char output #\linefeed))
	    (format #t " done~%~!")))))))

  (define (parse-specialcasing)

    (define ht-special-lower (make-eqv-hashtable))
    (define ht-special-title (make-eqv-hashtable))
    (define ht-special-upper (make-eqv-hashtable))

    (format #t "~%parsing SpecialCasing.txt ...~!");

    (let ((re1 #/^([A-F0-9]{4,6}); ([A-F0-9]{4,6});[^;]*;[^;]*; # .*$"/
	       )
	  (re2 #/^([A-F0-9]{4,6}); ([A-F0-9]{4,6}) ([A-F0-9]{4,6});[^;]*;[^;]*; # .*$/
	       )
	  (re3 #/^([A-F0-9]{4,6}); ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6});[^;]*;[^;]*; # .*$/
	       ))

      (call-with-input-file (ucd-file "SpecialCasing.txt")
       (lambda (input)
	 (for-each-ucd-line
	  (lambda (line)
	    (let ((m1 (re1 line))
		  (m2 (re2 line))
		  (m3 (re3 line)))
	      (and-let* (( m1 )
			 (code-point (string->number (m1 1) 16))
			 (c1 (string->number (m1 2) 16)))
		(hashtable-set! ht-special-lower code-point (list c1)))
	      (and-let* (( m2 )
			 (code-point (string->number (m2 1) 16))
			 (c1 (string->number (m2 2) 16))
			 (c2 (string->number (m2 3) 16)))
		(hashtable-set! ht-special-lower code-point (list c1 c2)))
	      (and-let* (( m3 )
			 (code-point (string->number (m3 1) 16))
			 (c1 (string->number (m3 2) 16))
			 (c2 (string->number (m3 2) 16))
			 (c3 (string->number (m3 2) 16)))
		(hashtable-set! ht-special-lower code-point (list c1 c2 c3)))))
	  input))))

    (let ((re1 #/^([A-F0-9]{4,6});[^;]*; ([A-F0-9]{4,6});[^;]*; # .*$/
	       )
	  (re2 #/^([A-F0-9]{4,6});[^;]*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6});[^;]*; # .*$/
	       )
	  (re3 #/^([A-F0-9]{4,6});[^;]*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6});[^;]*; # .*$/
	       ))

      (call-with-input-file (ucd-file "SpecialCasing.txt")
       (lambda (input)
	 (for-each-ucd-line
	  (lambda (line)
	    (let ((m1 (re1 line))
		  (m2 (re2 line))
		  (m3 (re3 line)))
	      (and-let* (( m1 )
			 (code-point (string->number (m1 1) 16))
			 (c1 (string->number (m1 2) 16)))
		(hashtable-set! ht-special-title code-point (list c1)))
	      (and-let* (( m2 )
			 (code-point (string->number (m2 1) 16))
			 (c1 (string->number (m2 2) 16))
			 (c2 (string->number (m2 3) 16)))
		(hashtable-set! ht-special-title code-point (list c1 c2)))
	      (and-let* (( m3 )
			 (code-point (string->number (m3 1) 16))
			 (c1 (string->number (m3 2) 16))
			 (c2 (string->number (m3 2) 16))
			 (c3 (string->number (m3 2) 16)))
		(hashtable-set! ht-special-title code-point (list c1 c2 c3)))))
	 input))))

    (let ((re1 #/^([A-F0-9]{4,6});[^;]*;[^;]*; ([A-F0-9]{4,6}); # .*$/
	       )
	  (re2 #/^([A-F0-9]{4,6});[^;]*;[^;]*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$/
	       )
	  (re3 #/^([A-F0-9]{4,6});[^;]*;[^;]*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$/
	       ))

      (call-with-input-file (ucd-file "SpecialCasing.txt")
       (lambda (input)
	 (for-each-ucd-line
	  (lambda (line)
	    (let ((m1 (re1 line))
		  (m2 (re2 line))
		  (m3 (re3 line)))
	      (and-let* (( m1 )
			 (code-point (string->number (m1 1) 16))
			 (c1 (string->number (m1 2) 16)))
		(hashtable-set! ht-special-upper code-point (list c1)))
	      (and-let* (( m2 )
			 (code-point (string->number (m2 1) 16))
			 (c1 (string->number (m2 2) 16))
			 (c2 (string->number (m2 3) 16)))
		(hashtable-set! ht-special-upper code-point (list c1 c2)))
	      (and-let* (( m3 )
			 (code-point (string->number (m3 1) 16))
			 (c1 (string->number (m3 2) 16))
			 (c2 (string->number (m3 2) 16))
			 (c3 (string->number (m3 2) 16)))
		(hashtable-set! ht-special-upper code-point (list c1 c2 c3)))))
	  input))))

    (format #t " done~%~!")

    (call-with-port
     (open-file-output-port (datum-file "special-casing-lower.datum")
			    (file-options no-fail) (buffer-mode block)
			    (native-transcoder))
     (lambda (output)
       (format #t "special-casing-lower.datum ...~!")
       (put-datum output (hashtable->alist ht-special-lower))
       (format #t " done~%~!")))

    (call-with-port
     (open-file-output-port (datum-file "special-casing-title.datum")
			    (file-options no-fail) (buffer-mode block)
			    (native-transcoder))
     (lambda (output)
       (format #t "special-casing-title.datum ...~!")
       (put-datum output (hashtable->alist ht-special-title))
       (format #t " done~%~!")))

    (call-with-port
     (open-file-output-port (datum-file "special-casing-upper.datum")
			    (file-options no-fail) (buffer-mode block)
			    (native-transcoder))
     (lambda (output)
       (format #t "special-casing-upper.datum ...~!")
       (put-datum output (hashtable->alist ht-special-upper))
       (format #t " done~%~!"))))

  (define (parse-casefolding)

    (define ht-casefolding (make-eqv-hashtable))

    (format #t "~%parsing CaseFolding.txt ...~!");

    (let ((re1 #/^([A-F0-9]{4,6}); C; ([A-F0-9]{4,6}); # .*$/
	       )
	  (re2 #/^([A-F0-9]{4,6}); C; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$/
	       )
	  (re3 #/^([A-F0-9]{4,6}); C; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$/
	       ))

      (call-with-input-file (ucd-file "CaseFolding.txt")
       (lambda (input)
	 (for-each-ucd-line
	  (lambda (line)
	    (let ((m1 (re1 line))
		  (m2 (re2 line))
		  (m3 (re3 line)))
	      (and-let* (( m1 )
			 (code-point (string->number (m1 1) 16))
			 (c1 (string->number (m1 2) 16)))
		(hashtable-set! ht-casefolding code-point (list c1)))
	      (and-let* (( m2 )
			 (code-point (string->number (m2 1) 16))
			 (c1 (string->number (m2 2) 16))
			 (c2 (string->number (m2 3) 16)))
		(hashtable-set! ht-casefolding code-point (list c1 c2)))
	      (and-let* (( m3 )
			 (code-point (string->number (m3 1) 16))
			 (c1 (string->number (m3 2) 16))
			 (c2 (string->number (m3 2) 16))
			 (c3 (string->number (m3 2) 16)))
		(hashtable-set! ht-casefolding code-point (list c1 c2 c3)))))
	  input))))

    (let ((re1 #/^([A-F0-9]{4,6}); F; ([A-F0-9]{4,6}); # .*$/
	       )
	  (re2 #/^([A-F0-9]{4,6}); F; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$/
	       )
	  (re3 #/^([A-F0-9]{4,6}); F; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$/
	       ))

      (call-with-input-file (ucd-file "CaseFolding.txt")
       (lambda (input)
	 (for-each-ucd-line
	  (lambda (line)
	    (let ((m1 (re1 line))
		  (m2 (re2 line))
		  (m3 (re3 line)))
	      (and-let* (( m1 )
			 (code-point (string->number (m1 1) 16))
			 (c1 (string->number (m1 2) 16)))
		(hashtable-set! ht-casefolding code-point (list c1)))
	      (and-let* (( m2 )
			 (code-point (string->number (m2 1) 16))
			 (c1 (string->number (m2 2) 16))
			 (c2 (string->number (m2 3) 16)))
		(hashtable-set! ht-casefolding code-point (list c1 c2)))
	      (and-let* (( m3 )
			 (code-point (string->number (m3 1) 16))
			 (c1 (string->number (m3 2) 16))
			 (c2 (string->number (m3 2) 16))
			 (c3 (string->number (m3 2) 16)))
		(hashtable-set! ht-casefolding code-point (list c1 c2 c3)))))
	  input))))
    (format #t " done~%~!")
    (call-with-port
     (open-file-output-port (datum-file "case-folding.datum")
			    (file-options no-fail) (buffer-mode block)
			    (native-transcoder))
     (lambda (output)
       (format #t "case-folding.datum ...~!")
       (put-datum output (hashtable->alist ht-casefolding))
       )))


      (parse-unicodedata)
      (parse-proplist)
      (parse-specialcasing)
      (parse-casefolding)
      (format #t " done~%~!") ;[end]

)