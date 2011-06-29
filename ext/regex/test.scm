;; -*- scheme -*-
;; Local Variables:
;; coding: utf-8-unix
;; End:

;; This test contains Japanese letters with utf-8.
;; So make sure this can see: λ (greek small lambda)

(add-load-path "./regex")
(library (regex test)
    (export run-regex-test)
    (import (srfi :64 testing)
	    (srfi :13 strings)
	    (rnrs)
	    (sagittarius regex))

  (define uri-text (string-append "abcde "
				  "<br> </br> & ' \""
				  "http://www.nilab.info/ "
				  "http://WWW.NILAB.INFO/ "
				  "HTTP://WWW.NILAB.INFO/ "
				  "http://www.nilab.info/index.html "
				  "http://www.nilab.info/in dex.html "
				  "xyzhttp://www.nilab.info/hoge "
				  "https://www.nilab.info/ "
				  "xyzhttps://www.nilab.info/abc "
				  "http://www.nilab.info/ http://nilab.info/index.html こんにちは "
				  "http://www.nilab.info/redirect.cgi?http://nilab.info/index.html "
				  "http://www.nilab.info/wiki?こんにちは "
				  "<a href=http://www.nilab.info/>ホームページ</a> "
				  "<a href=\"http://www.nilab.info/\">ホームページ</a> "
				  "hello http://localhost/test.cgi?%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A good-bye "
				  ""))

  (define standard-uri-regex "(http://|https://){1}[\\w\\.\\-/:\\#\\?\\=\\&\\;\\%\\~\\+]+")
  (define greedy-uri-regex "(http|https):([^\\x00-\\x20()\"<>\\x7F-\\xFF])*")
  
  (define (run-regex-test)
    (test-equal "basic regex"
		'(#t #t #t)
		(let* ((p (compile-regex "\\w+"))
		       (m (regex-matcher p "hello")))
		  (list (regex-pattern? p) (regex-matcher? m) (regex-matches m))))

    (test-assert "regex char class"
		 (let* ((p (compile-regex "[a-zA-Z0-9]+"))
			(m (regex-matcher p "abcABC123")))
		   (regex-matches m)))

    ;; TODO currently \\u only accepts 2 bytes. If Sagittarius Scheme can handle UCS4(4bytes)
    ;; should this also be able to handle 4 bytes?
    (test-assert "regex \\u"
		 (let* ((p (compile-regex "[\\u3041-\\u3096]+"))
			;; somehow emacs convert \u3096 -> \u30f6
			;; so we can not test unicode boundary with small KE.
			(m (regex-matcher p "ぁぃぅぇぉあいうえおわをん")))
		   (regex-matches m)))

    (test-equal "caret and dollar"
		'(#t #f #t #f)
		(let ((cp (compile-regex "^hello.+"))
		      (dp (compile-regex ".+?world$")))
		  (let ((good-cm (regex-matcher cp "hello world"))
			(bad-cm (regex-matcher cp "world hello world"))
			(good-dm (regex-matcher dp "hello world"))
			(bad-dm (regex-matcher dp "hello world hello")))
		    (list (regex-matches good-cm)
			  (regex-matches bad-cm)
			  (regex-matches good-dm)
			  (regex-matches bad-dm)))))

    (test-equal "quantity"
		'(#f #t #f		; p1
		  #f #t #t #f		; p2
		  #f #t #t		; p3

		  #f #t #f		; p4
		  #f #t #t #f		; p5
		  #f #t #t		; p6

		  #f #t #f		; p7
		  #f #t #t #f		; p8
		  #f #t #t		; p9
		  )
		(let ((p1 (compile-regex "bu{1}z"))
		      (p2 (compile-regex "bu{1,2}z"))
		      (p3 (compile-regex "bu{1,}z"))
		      (p4 (compile-regex "bu{1}?z"))
		      (p5 (compile-regex "bu{1,2}?z"))
		      (p6 (compile-regex "bu{1,}?z"))
		      (p7 (compile-regex "bu{1}+z"))
		      (p8 (compile-regex "bu{1,2}+z"))
		      (p9 (compile-regex "bu{1,}+z")))
		  (list (regex-matches (regex-matcher p1 "bz"))
			(regex-matches (regex-matcher p1 "buz"))
			(regex-matches (regex-matcher p1 "buuz"))

			(regex-matches (regex-matcher p2 "bz"))
			(regex-matches (regex-matcher p2 "buz"))
			(regex-matches (regex-matcher p2 "buuz"))
			(regex-matches (regex-matcher p2 "buuuz"))
			
			(regex-matches (regex-matcher p3 "bz"))
			(regex-matches (regex-matcher p3 "buz"))
			(regex-matches (regex-matcher p3 "buuz"))

			(regex-matches (regex-matcher p4 "bz"))
			(regex-matches (regex-matcher p4 "buz"))
			(regex-matches (regex-matcher p4 "buuz"))

			(regex-matches (regex-matcher p5 "bz"))
			(regex-matches (regex-matcher p5 "buz"))
			(regex-matches (regex-matcher p5 "buuz"))
			(regex-matches (regex-matcher p5 "buuuz"))

			(regex-matches (regex-matcher p6 "bz"))
			(regex-matches (regex-matcher p6 "buz"))
			(regex-matches (regex-matcher p6 "buuz"))

			(regex-matches (regex-matcher p7 "bz"))
			(regex-matches (regex-matcher p7 "buz"))
			(regex-matches (regex-matcher p7 "buuz"))

			(regex-matches (regex-matcher p8 "bz"))
			(regex-matches (regex-matcher p8 "buz"))
			(regex-matches (regex-matcher p8 "buuz"))
			(regex-matches (regex-matcher p8 "buuuz"))

			(regex-matches (regex-matcher p9 "bz"))
			(regex-matches (regex-matcher p9 "buz"))
			(regex-matches (regex-matcher p9 "buuz")))))
			
    (test-assert "regex escape"
		 (let* ((p (compile-regex "\\Q([])\\E"))
			(m (regex-matcher p "([])")))
		   (regex-matches m)))
    
    (test-equal "regex group"
		 '("hello world" "hello" "world")
		 (let* ((p (compile-regex "([a-zA-Z]+) ([a-zA-Z]+)"))
			(m (regex-matcher p "hello world")))
		   (if (regex-matches m)
		       (list (regex-group m 0)
			     (regex-group m 1)
			     (regex-group m 2))
		       '())))

    (test-equal "regex group without reference"
		 '("hello world" "world")
		 (let* ((p (compile-regex "(?:[a-zA-Z]+) ([a-zA-Z]+)"))
			(m (regex-matcher p "hello world")))
		   (if (regex-matches m)
		       (list (regex-group m 0)
			     (regex-group m 1))
		       '())))

    (test-equal "regex positive lookahead"
		'(#t #f)
		(let* ((p (compile-regex "hoge(?=\\.txt)"))
		       (m1 (regex-matcher p "hoge.txt"))
		       (m2 (regex-matcher p "hoge.pdf")))
		  (list (regex-find m1)
			(regex-find m2))))

    (test-equal "regex negative lookahead"
		'(#f #t)
		(let* ((p (compile-regex "hoge(?!\\.txt)"))
		       (m1 (regex-matcher p "hoge.txt"))
		       (m2 (regex-matcher p "hoge.pdf")))
		  (list (regex-find m1)
			(regex-find m2))))

    (test-equal "regex positive lookbehind"
		'(#t #f)
		(let* ((p (compile-regex "(?<=RX)-\\d"))
		       (m1 (regex-matcher p "RX-8"))
		       (m2 (regex-matcher p "FD-3")))
		  (list (regex-find m1)
			(regex-find m2))))

    (test-equal "regex negative lookbehind"
		'(#t #f)
		(let* ((p (compile-regex "(?<!FD)-\\dS"))
		       (m1 (regex-matcher p "RX-8S"))
		       (m2 (regex-matcher p "FD-3S")))
		  (list (regex-find m1)
			(regex-find m2))))

    (test-equal "regex group with flags"
		 '("hello world" "world")
		 (let* ((p (compile-regex "(?i:HELLO) (\\w+)"))
			(m (regex-matcher p "hello world")))
		   (if (regex-matches m)
		       (list (regex-group m 0)
			     (regex-group m 1))
		       '())))

    (test-equal "regex group with flags"
		 '()
		 (let* ((p (compile-regex "(?i:HELLO) (WORLD)"))
			(m (regex-matcher p "hello world")))
		   (if (regex-matches m)
		       (list (regex-group m 0)
			     (regex-group m 1))
		       '())))

    (test-equal "replace-first"
		"hello beautiful world"
		(let ((p (regex "fxxking")))
		  (regex-replace-first p "hello fxxking world" "beautiful")))

    (test-equal "replace-all"
		"hello beautiful beautiful world"
		(let ((p (regex "fxxking")))
		  (regex-replace-all p "hello fxxking fxxkingworld" "beautiful")))

    

    (test-equal "standard pattern case sensitive"
		'("http://www.nilab.info/"
		  "http://WWW.NILAB.INFO/"
		  "http://www.nilab.info/index.html"
		  "http://www.nilab.info/in"
		  "http://www.nilab.info/hoge"
		  "https://www.nilab.info/"
		  "https://www.nilab.info/abc"
		  "http://www.nilab.info/"
		  "http://nilab.info/index.html"
		  "http://www.nilab.info/redirect.cgi?http://nilab.info/index.html"
		  "http://www.nilab.info/wiki?"
		  "http://www.nilab.info/"
		  "http://www.nilab.info/"
		  "http://localhost/test.cgi?%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A")
		(let ((m (regex-matcher (compile-regex standard-uri-regex) uri-text)))
		  (let loop ((r '()))
		    (if (regex-find m)
			(loop (cons (regex-group m) r))
			(reverse r)))))

    (test-equal "standard pattern case insensitive"
		'("http://www.nilab.info/"
		  "http://WWW.NILAB.INFO/"
		  "HTTP://WWW.NILAB.INFO/"
		  "http://www.nilab.info/index.html"
		  "http://www.nilab.info/in"
		  "http://www.nilab.info/hoge"
		  "https://www.nilab.info/"
		  "https://www.nilab.info/abc"
		  "http://www.nilab.info/"
		  "http://nilab.info/index.html"
		  "http://www.nilab.info/redirect.cgi?http://nilab.info/index.html"
		  "http://www.nilab.info/wiki?"
		  "http://www.nilab.info/"
		  "http://www.nilab.info/"
		  "http://localhost/test.cgi?%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A")
		(let ((m (regex-matcher (compile-regex standard-uri-regex CASE-INSENSITIVE) uri-text)))
		  (let loop ((r '()))
		    (if (regex-find m)
			(loop (cons (regex-group m) r))
			(reverse r)))))

    (test-equal "greedy pattern case sensitive"
		'("http://www.nilab.info/"
		  "http://WWW.NILAB.INFO/"
		  "http://www.nilab.info/index.html"
		  "http://www.nilab.info/in"
		  "http://www.nilab.info/hoge"
		  "https://www.nilab.info/"
		  "https://www.nilab.info/abc"
		  "http://www.nilab.info/"
		  "http://nilab.info/index.html"
		  "http://www.nilab.info/redirect.cgi?http://nilab.info/index.html"
		  "http://www.nilab.info/wiki?こんにちは"
		  "http://www.nilab.info/"
		  "http://www.nilab.info/"
		  "http://localhost/test.cgi?%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A")
		(let ((m (regex-matcher (compile-regex greedy-uri-regex) uri-text)))
		  (let loop ((r '()))
		    (if (regex-find m)
			(loop (cons (regex-group m) r))
			(reverse r)))))

    (test-equal "greedy pattern case insensitive"
		'("http://www.nilab.info/"
		  "http://WWW.NILAB.INFO/"
		  "HTTP://WWW.NILAB.INFO/"
		  "http://www.nilab.info/index.html"
		  "http://www.nilab.info/in"
		  "http://www.nilab.info/hoge"
		  "https://www.nilab.info/"
		  "https://www.nilab.info/abc"
		  "http://www.nilab.info/"
		  "http://nilab.info/index.html"
		  "http://www.nilab.info/redirect.cgi?http://nilab.info/index.html"
		  "http://www.nilab.info/wiki?こんにちは"
		  "http://www.nilab.info/"
		  "http://www.nilab.info/"
		  "http://localhost/test.cgi?%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A")
		(let ((m (regex-matcher (compile-regex greedy-uri-regex CASE-INSENSITIVE) uri-text)))
		  (let loop ((r '()))
		    (if (regex-find m)
			(loop (cons (regex-group m) r))
			(reverse r)))))

    ;; wrapper
    (test-equal "wrapper test"
		'("hello world" "hello" "world")
		(let* ((rx (regex "(\\w+) (\\w+)")))
		  (cond ((matches rx "hello world")
			 => (lambda (m)
			      (list (m 0) (m 1) (m 2))))
			(else '()))))
		  
    )
)