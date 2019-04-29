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
