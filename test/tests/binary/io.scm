(import (rnrs)
	(srfi :64)
	(rename (binary io) (get-line binary:get-line)))

(test-begin "binary io")

(define (string->binary-port s)
  (open-bytevector-input-port (string->utf8 s)))

(let ((bin (string->binary-port "hello\nworld")))
  (test-equal "linefeed \\n(1)" "hello" 
	      (binary:get-line bin :transcoder (native-transcoder)))
  (test-equal "linefeed \\n(2)" "world"
	      (binary:get-line bin :transcoder (native-transcoder)))
  (test-equal "linefeed \\n(3)" ""
	      (binary:get-line bin :transcoder (native-transcoder))))

(let ((bin (string->binary-port "hello\r\nworld")))
  (test-equal "linefeed \\r\\n(1)" "hello" 
	      (binary:get-line bin 
			       :eol #vu8(#x0d #x0a)
			       :transcoder (native-transcoder)))
  (test-equal "linefeed \\r\\n(2)" "world"
	      (binary:get-line bin
			       :eol #vu8(#x0d #x0a)
			       :transcoder (native-transcoder)))
  (test-equal "linefeed \\r\\n(3)" ""
	      (binary:get-line bin 
			       :eol #vu8(#x0d #x0a)
			       :transcoder (native-transcoder))))

(let ((bin (string->binary-port "hello\rworld")))
  (test-equal "linefeed \\r(1)" "hello" 
	      (binary:get-line bin 
			       :eol #vu8(#x0d)
			       :transcoder (native-transcoder)))
  (test-equal "linefeed \\r(2)" "world"
	      (binary:get-line bin
			       :eol #vu8(#x0d)
			       :transcoder (native-transcoder)))
  (test-equal "linefeed \\r(3)" ""
	      (binary:get-line bin 
			       :eol #vu8(#x0d)
			       :transcoder (native-transcoder))))

(let ((bin (string->binary-port "hello\rworld")))
  (test-equal "linefeed \\a no match(1)" 
	      ;; native-transcoder converts \r to \n...
	      (string->utf8 "hello\rworld")
	      (binary:get-line bin))
  (test-equal "linefeed \\a(2) no match" #vu8() (binary:get-line bin)))

(let ((bin (string->binary-port "hello\rworld\r")))
  (test-equal "linefeed \\a no match(1)" 
	      ;; native-transcoder converts \r to \n...
	      (string->utf8 "hello\rworld\r")
	      (binary:get-line bin :eol #vu8(#x0d #x0a)))
  (test-equal "linefeed \\a(2) no match" #vu8() (binary:get-line bin)))

(test-end)