(import (rnrs)
	(rfc uri parser)
	(sagittarius generators)
	(peg)
	(srfi :127 lseqs)
	(srfi :64))

(test-begin "RFC URI parser")

(define (parse parser input)
  (define lseq (generator->lseq (string->generator input)))
  (let-values (((s v nl) (parser lseq)))
    (test-assert (string-append "Parse success: " input) (parse-success? s))
    (test-assert (string-append "Input exhausted: " input) (null? nl))
    v))

(test-equal "alpha" #\a (parse uri:alpha "a"))
(test-equal "digit" #\1 (parse uri:digit "1"))
(test-equal "hexdig" #\1 (parse uri:hexdig "1"))
(test-equal "hexdig" #\f (parse uri:hexdig "f"))

(test-equal "unreserved (1)" #\a (parse uri:unreserved "a"))
(test-equal "unreserved (2)" #\- (parse uri:unreserved "-"))
(test-equal "unreserved (3)" #\. (parse uri:unreserved "."))
(test-equal "unreserved (4)" #\_ (parse uri:unreserved "_"))
(test-equal "unreserved (5)" #\~ (parse uri:unreserved "~"))
(test-equal "unreserved (6)" #\0 (parse uri:unreserved "0"))
(test-equal "pct-encoded (1)" #\x38 (parse uri:pct-encoded "%38"))
(test-equal "pct-encoded (2)" #\xff (parse uri:pct-encoded "%ff"))
(test-equal "sub-delims (1)" #\& (parse uri:sub-delims "&"))
(test-equal "sub-delims (2)" #\! (parse uri:sub-delims "!"))
(test-equal "pchar (1)" #\xff (parse uri:pchar "%ff"))
(test-equal "pchar (2)" #\@ (parse uri:pchar "@"))

(test-equal "scheme" "http" (parse uri:scheme-parser "http"))

(test-equal "host (1)" "localhost" (parse uri:host-parser "localhost"))
(test-equal "host (2)" "192.168.0.1" (parse uri:host-parser "192.168.0.1"))
(test-equal "host (3)" "[::00]" (parse uri:host-parser "[::00]"))
(test-equal "host (4)" "[2001:db8:a0b:12f0::1]"
	    (parse uri:host-parser "[2001:db8:a0b:12f0::1]"))
;; TODO add more tests for IPv6

(test-equal "authority (1)" '(#f "localhost" #f)
	    (parse uri:authority-parser "localhost"))
(test-equal "authority (2)" '("user" "localhost" #f)
	    (parse uri:authority-parser "user@localhost"))
(test-equal "authority (3)" '("user" "localhost" "8080")
	    (parse uri:authority-parser "user@localhost:8080"))

(test-equal "hier (1)" '(// ("user" "localhost" "8080") (/ "a" "b"))
	    (parse uri:hier-part-parser "//user@localhost:8080/a/b"))
(test-equal "hier (2)" '(/ "a" "b") (parse uri:hier-part-parser "/a/b"))
(test-equal "hier (3)" '(! "a" "b") (parse uri:hier-part-parser "a/b"))
(test-equal "hier (4)" '() (parse uri:hier-part-parser ""))

(test-equal "uri (1)" '("http" (// (#f "localhost" #f) (/ "a" "b")) "q=b" "f")
	    (parse uri:uri-parser "http://localhost/a/b?q=b#f"))
(test-equal "uri (2)" '("http" (// (#f "localhost" #f) (/ "a" "b")) "q=b" #f)
	    (parse uri:uri-parser "http://localhost/a/b?q=b"))
(test-equal "uri (3)" '("http" (// (#f "localhost" #f) (/ "a" "b")) #f #f)
	    (parse uri:uri-parser "http://localhost/a/b"))
(test-equal "uri (3)" '("http" (// (#f "localhost" #f) (/ "a" "b")) "" #f)
	    (parse uri:uri-parser "http://localhost/a/b?"))

(test-equal "absolute uri (1)" '("http" (// (#f "localhost" #f) (/ "a" "b")) "q=b")
	    (parse uri:absolute-uri-parser "http://localhost/a/b?q=b"))
(test-equal "absolute uri (2)" '("http" (// (#f "localhost" #f) (/ "a" "b")) #f)
	    (parse uri:absolute-uri-parser "http://localhost/a/b"))

(test-equal "uri reference (1)"
	    '(#f (// (#f "localhost" #f) (/ "a" "b")) #f #f)
	    (parse uri:uri-reference-parser "//localhost/a/b"))

(test-end)
