(import (rnrs)
	(sagittarius)
	(rfc uri)
	(srfi :64 testing))

(test-begin "RFC URI tests")

;; Test cases are from Gauche
(test-equal "encode" "abc%3c%20%3e%20%22%20%23%25%7b%7c%7d%5c%5e"
	    (uri-encode-string "abc< > \" #%{|}\\^"))
(test-equal "encode (noescape)" ".a%21%2ap"
	    (uri-encode-string ".a!*p" :noescape *rfc3986-unreserved-char-set*))
(test-equal "decode" "abc< > \" #%?{|}\\^"
	    (uri-decode-string "abc%3c%20%3e%20%22%20%23%25%3f%7b%7c%7d%5c%5e"))
(test-equal "decode" "abc<+>+\"+#%?{|}\\^"
	    (uri-decode-string "abc%3c+%3e+%22+%23%25%3f%7b%7c%7d%5c%5e"))
(test-equal "decode" "abc< > \" #%?{|}\\^"
	    (uri-decode-string "abc%3c+%3e+%22+%23%25%3f%7b%7c%7d%5c%5e"
			       :cgi-decode #t))
(test-equal "decode" "%"    (uri-decode-string "%"))
(test-equal "decode" "a%"   (uri-decode-string "a%"))
(test-equal "decode" "a%y"  (uri-decode-string "a%y"))
(test-equal "decode" "a%ay" (uri-decode-string "a%ay"))
(test-equal "decode" ""     (uri-decode-string ""))

(test-equal "uri-scheme&specific" '("http" "//practical-scheme.net/gauche/")
	    (receive r
		(uri-scheme&specific "http://practical-scheme.net/gauche/")
	      r))

(test-equal "uri-scheme&specific" '(#f "/dev/tty")
	    (receive r
		(uri-scheme&specific "/dev/tty")
	      r))

(test-equal "uri-decompose-hierarchical" '("www.example.com:8080"
					   "/about/company"
					   "abc=def&ghi%20"
					   "zzz")
	    (receive r
		(uri-decompose-hierarchical
		 "//www.example.com:8080/about/company?abc=def&ghi%20#zzz")
	      r))

(test-equal "uri-decompose-hierarchical" '("www.example.com:8080"
					   "/about/company"
					   #f
					   "zzz")
	    (receive r
		(uri-decompose-hierarchical
		 "//www.example.com:8080/about/company#zzz")
	      r))

(test-equal "uri-decompose-hierarchical" '("www.example.com:8080"
					   "/"
					   "abc"
					   #f)
	    (receive r
		(uri-decompose-hierarchical
		 "//www.example.com:8080/?abc")
	      r))

(test-equal "uri-decompose-hierarchical" '("www.example.com:8080"
					   #f
					   #f
					   #f)
	    (receive r (uri-decompose-hierarchical "//www.example.com:8080") r))

(test-equal "uri-decompose-hierarchical" '((#f #f  #f #f)
					   ("" #f  #f #f)
					   (#f "/" #f #f)
					   ("" "/" #f #f))
	    (map (lambda (specific)
		   (receive r (uri-decompose-hierarchical specific) r))
		 '("" "//" "/" "///")))

(test-equal "uri-decompose-authority" '(#f "www.example.com" #f)
	    (receive r (uri-decompose-authority "www.example.com") r))
(test-equal "uri-decompose-authority" '(#f "www.example.com" "8080")
	    (receive r (uri-decompose-authority "www.example.com:8080") r))
(test-equal "uri-decompose-authority" '("foo:bar" "www.example.com" #f)
	    (receive r (uri-decompose-authority "foo:bar@www.example.com") r))

(test-equal "uri-parse" '("https" "shiro" "www.example.com" 443 "/login" "abc" "def")
	    (receive r (uri-parse "https://shiro@www.example.com:443/login?abc#def")
	      r))
(test-equal "uri-parse" '("ftp" "anonymous:anonymous" "ftp.example.com" #f
			  "/pub/foo" #f #f)
	    (receive r (uri-parse "ftp://anonymous:anonymous@ftp.example.com/pub/foo")
	      r))
(test-equal "uri-parse" '("file" #f #f #f "/usr/local/lib/abc" #f #f)
	    (receive r (uri-parse "file:/usr/local/lib/abc")
	      r))
(test-equal "uri-parse" '(#f #f #f #f "/usr/local/lib" #f #f)
	    (receive r (uri-parse "/usr/local/lib") r))
(test-equal "uri-parse" '("mailto" #f #f #f "shiro@example.com" #f #f)
	    (receive r (uri-parse "mailto:shiro@example.com") r))

(let ([base0 "http://a/b/c/d;p?q"])
  (define (t base rel expect) 
    (test-equal (format "merging ~s onto ~s" rel base)
		expect (uri-merge base rel)))
  (define t0 (lambda args (apply t base0 args)))
  ;; examples given in RFC3986 section 5.4
  ;; normal path
  (t0 "g:h" "g:h")
  (t0 "g" "http://a/b/c/g")
  (t0 "./g" "http://a/b/c/g")
  (t0 "g/" "http://a/b/c/g/")
  (t0 "/g" "http://a/g")
  (t0 "//g" "http://g")
  (t0 "?y" "http://a/b/c/d;p?y")
  (t0 "g?y" "http://a/b/c/g?y")
  (t0 "#s" "http://a/b/c/d;p?q#s")
  (t0 "g#s" "http://a/b/c/g#s")
  (t0 "g?y#s" "http://a/b/c/g?y#s")
  (t0 ";x" "http://a/b/c/;x")
  (t0 "g;x" "http://a/b/c/g;x")
  (t0 "g;x?y#s" "http://a/b/c/g;x?y#s")
  (t0 "" "http://a/b/c/d;p?q")
  (t0 "." "http://a/b/c/")
  (t0 "./" "http://a/b/c/")
  (t0 ".." "http://a/b/")
  (t0 "../" "http://a/b/")
  (t0 "../g" "http://a/b/g")
  (t0 "../.." "http://a/")
  (t0 "../../" "http://a/")
  (t0 "../../g" "http://a/g")
  ;; failure path
  (t0 "../../../g" "http://a/g")
  (t0 "../../../../g" "http://a/g")
  (t0 "/./g" "http://a/g")
  (t0 "/../g" "http://a/g")
  (t0 "g." "http://a/b/c/g.")
  (t0 ".g" "http://a/b/c/.g")
  (t0 "g.." "http://a/b/c/g..")
  (t0 "..g" "http://a/b/c/..g")
  (t0 "./../g" "http://a/b/g")
  (t0 "./g/." "http://a/b/c/g/")
  (t0 "g/./h" "http://a/b/c/g/h")
  (t0 "g/../h" "http://a/b/c/h")
  (t0 "g;x=1/./y" "http://a/b/c/g;x=1/y")
  (t0 "g;x=1/../y" "http://a/b/c/y")
  (t0 "g?y/./x" "http://a/b/c/g?y/./x")
  (t0 "g?y/../x" "http://a/b/c/g?y/../x")
  (t0 "g#s/./x" "http://a/b/c/g#s/./x")
  (t0 "g#s/../x" "http://a/b/c/g#s/../x")
  (t0 "http:g" "http:g") ;; for strict parser

  ;; some edge cases.  the first case works since we do pre-normalization
  ;; of the base URI (RFC3986 5.2.1), which is optional.
  (t "http://example.com/foo/.." "./" "http://example.com/")
  (t "http://example.com/" "./foo/bar/.." "http://example.com/foo/")

  ;; empty base-path case
  (t "http://example.com"  "foo" "http://example.com/foo")
  (t "http://example.com"  "./foo" "http://example.com/foo")
  (t "http://example.com"  "../foo" "http://example.com/foo")
  )


(test-end)