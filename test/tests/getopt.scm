(import (rnrs)
	(getopt)
	(srfi :64))

(test-begin "with-args")

(test-equal "short name"
	    "1234"
	    (with-args '("-p" "1234")
		((pin (#\p "pin") #t #f))
	      pin))

(test-equal "short name"
	    "1234"
	    (with-args '("-p1234")
		((pin (#\p "pin") #t #f))
	      pin))

(test-equal "long name"
	    "1234"
	    (with-args '("--pin" "1234")
		((pin (#\p "pin") #t #f))
	      pin))

(test-equal "long name"
	    "1234"
	    (with-args '("--pin=1234")
		((pin (#\p "pin") #t #f))
	      pin))

(test-equal "rest"
	    '("prog" "o1" "-n" "name")
	    (with-args '("prog" "--pin=1234" "o1" "-n" "name")
		((pin (#\p "pin") #t #f)
		 . rest)
	      rest))

(test-equal "multiple"
	    '("file1" "file2" "file3" "file4")
	    (with-args '("-ffile1" "-f" "file2" "--file" "file3" "--file=file4")
		((files (#\f "file") * #f))
	      files))

;; call #88
(test-equal "name couldn't use"
	    "name"
	    (with-args '("-n" "name")
		((name (#\n "name") #t #f))
	      name))

(test-end)
