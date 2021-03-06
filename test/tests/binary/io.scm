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
  (test-assert "linefeed \\n(3)"
	       (eof-object?
		(binary:get-line bin :transcoder (native-transcoder)))))

(let ((bin (string->binary-port "hello\r\nworld")))
  (test-equal "linefeed \\r\\n(1)" "hello" 
	      (binary:get-line bin 
			       :eol #vu8(#x0d #x0a)
			       :transcoder (native-transcoder)))
  (test-equal "linefeed \\r\\n(2)" "world"
	      (binary:get-line bin
			       :eol #vu8(#x0d #x0a)
			       :transcoder (native-transcoder)))
  (test-assert "linefeed \\r\\n(3)"
	      (eof-object?
	       (binary:get-line bin 
				:eol #vu8(#x0d #x0a)
				:transcoder (native-transcoder)))))

(let ((bin (string->binary-port "hello\rworld")))
  (test-equal "linefeed \\r(1)" "hello" 
	      (binary:get-line bin 
			       :eol #vu8(#x0d)
			       :transcoder (native-transcoder)))
  (test-equal "linefeed \\r(2)" "world"
	      (binary:get-line bin
			       :eol #vu8(#x0d)
			       :transcoder (native-transcoder)))
  (test-assert "linefeed \\r(3)" 
	      (eof-object?
	       (binary:get-line bin 
				:eol #vu8(#x0d)
				:transcoder (native-transcoder)))))

(let ((bin (string->binary-port "hello\rworld")))
  (test-equal "linefeed \\n no match(1)" 
	      ;; native-transcoder converts \r to \n...
	      (string->utf8 "hello\rworld")
	      (binary:get-line bin))
  (test-assert "linefeed \\n(2) no match"
	       (eof-object? (binary:get-line bin))))

(let ((bin (string->binary-port "hello\rworld\r")))
  (test-equal "linefeed \\r\\n no match(1)" 
	      ;; native-transcoder converts \r to \n...
	      (string->utf8 "hello\rworld\r")
	      (binary:get-line bin :eol #vu8(#x0d #x0a)))
  (test-assert "linefeed \\r\\n(2) no match" 
	       (eof-object? (binary:get-line bin))))

;; chunked port
;; test chunked port
(let ()
  (define (->chunked-port bv :key (chunk-size 4096) (threshold #f))
    (input-port->chunked-binary-input-port (open-bytevector-input-port bv)
					   :chunk-size chunk-size
					   :threshold threshold))
  (let ((in (->chunked-port #vu8())))
    (test-assert "port?" (port? in))
    (test-assert "input-port?" (input-port? in))
    (test-assert "has-port-position?" (port-has-port-position? in))
    (test-assert "has-set-port-position!?" (port-has-set-port-position!? in))
    (test-assert "EOF" (eof-object? (get-u8 in))))

  ;; some cases
  (let ((in (->chunked-port #vu8(1 2 3 4 5) :chunk-size 5)))
    (test-equal "one chunk" #vu8(1 2 3 4 5) (get-bytevector-n in 6))
    (test-assert "set-port-position!" (set-port-position! in 2))
    (test-equal "one chunk" #vu8(3 4 5) (get-bytevector-all in)))

  (let ((in (->chunked-port #vu8(1 2 3 4 5 6 7 8 9) :chunk-size 5)))
    (test-equal "two chunks" #vu8(1 2 3 4 5 6) (get-bytevector-n in 6))
    (test-assert "set-port-position!" (set-port-position! in 11))
    (test-equal "port-position 1" 9 (port-position in))
    (test-assert "two chunks (eof)" (eof-object? (get-bytevector-all in)))
    (test-assert "set-port-position!" (set-port-position! in 3))
    (test-equal "port-position 2" 3 (port-position in)))

  (let ((in (->chunked-port #vu8(1 2 3 4 5 6 7 8 9) 
			     :chunk-size 5 :threshold 4)))
    (test-equal "threshold chunks" #vu8(1 2 3 4) (get-bytevector-n in 6))
    (test-assert "set-port-position!" (set-port-position! in 11))
    (test-equal "port-position 4" 4 (port-position in))
    (test-assert "threshold chunks (eof)" 
		 (eof-object? (get-bytevector-all in)))
    (test-assert "set-port-position!" (set-port-position! in 3))
    (test-equal "port-position 3" 3 (port-position in)))

  (let ((in (->chunked-port #vu8(1 2 3 4 5 6 7 8 9) :chunk-size 2)))
    (test-equal "9 chunks" #vu8(1 2 3 4 5 6) (get-bytevector-n in 6))
    (test-assert "set-port-position!" (set-port-position! in 11))
    (test-equal "port-position" 9 (port-position in))
    (test-assert "9 chunks (eof)" (eof-object? (get-bytevector-all in)))
    (test-assert "set-port-position!" (set-port-position! in 3))
    (test-equal "port-position" 3 (port-position in)))
)

;; input/output
(let ()
  (define (->chunked-port vec :key (chunk-size 4096))
    (define (read-as-chunk chunk-size) 
      (vector-map bytevector-copy vec))
    (->chunked-binary-input/output-port read-as-chunk :chunk-size chunk-size))
  (let ((in/out (->chunked-port #(#vu8(1 2 3 4 5)))))
    (test-assert "input-port?"  (input-port? in/out))
    (test-assert "output-port?" (output-port? in/out))
    (test-equal "position (in/out)" 0 (port-position in/out))
    (test-assert "write some" (put-bytevector in/out #vu8(6 7 8 9 10)))
    (test-equal "position (in/out) (2)" 5 (port-position in/out))
    (test-assert "set-port-position! (1)" (set-port-position! in/out 0))
    (test-equal "get some (in/out)" #vu8(6 7 8 9 10)
		(get-bytevector-n in/out 5))
    ;;(print (port-position in/out))
    (test-assert "get-all eof" (eof-object? (get-bytevector-all in/out)))
    (test-assert "set-port-position! (1)" (set-port-position! in/out 2))
    (test-equal "get-all 3" #vu8(8 9 10) (get-bytevector-all in/out 5)))

  ;; multiple chunks
  (let ((in/out (->chunked-port #(#vu8(1 2 3 4 5)
				  #vu8(6 7 8 9))
				:chunk-size 5)))
    (test-assert "write some" (put-bytevector in/out #vu8(6 7 8 9 10)))
    (test-assert "set-port-position! (1)" (set-port-position! in/out 0))
    (test-equal "get some (in/out)" #vu8(6 7 8 9 10 6)
		(get-bytevector-n in/out 6))
    (test-equal "get-all 4" #vu8(7 8 9) (get-bytevector-all in/out))
    (test-assert "write more" (put-bytevector in/out #vu8(11 12 13 14)))
    (test-assert "set-port-position! (2)" (set-port-position! in/out 0))
    (test-equal "get-all all" #vu8(6 7 8 9 10 6 7 8 9 11 12 13 14)
		(get-bytevector-all in/out)))

  (let ((in/out (->chunked-port #() :chunk-size 5)))
    (test-assert "write some" (put-bytevector in/out #vu8(1 2 3 4 5)))
    (test-assert "set position" (set-port-position! in/out 10))
    (test-assert "write some more"
		 (put-bytevector in/out #vu8(11 12 13 14 15)))
    (test-assert "set position" (set-port-position! in/out 0))
    (test-equal "get-all" #vu8(1 2 3 4 5 0 0 0 0 0 11 12 13 14 15)
		(get-bytevector-all in/out))
    )

  (let ((in/out (open-chunked-binary-input/output-port :chunk-size 5)))
    (test-assert "write some" (put-bytevector in/out #vu8(1 2 3 4 5)))
    (test-assert "set position" (set-port-position! in/out 10))
    (test-assert "write some more"
		 (put-bytevector in/out #vu8(11 12 13 14 15)))
    (test-assert "set position" (set-port-position! in/out 0))
    (test-equal "get-all" #vu8(1 2 3 4 5 0 0 0 0 0 11 12 13 14 15)
		(get-bytevector-all in/out))
    )

  (let ((in/out (open-chunked-binary-input/output-port)))
    (test-assert "write some"
		 (put-bytevector in/out #vu8(1 2 3 4 5 6 7 8)))
    (test-assert "write-some" 
		 (put-bytevector in/out #vu8(9 10 11 12 13 14 15 16 17)))
    (set-port-position! in/out 0)
    (let ((buf (make-bytevector 8 0)))
      ;;(get-bytevector-n! in/out buf 4 4)
      (test-equal "get-bytevector-n!" 4 (get-bytevector-n! in/out buf 4 4))
      (test-equal "result" #vu8(0 0 0 0 1 2 3 4) buf)
      (test-equal "get-bytevector-n!" 8 (get-bytevector-n! in/out buf 0 8))
      (test-equal "result" #vu8(5 6 7 8 9 10 11 12) buf)
      )
    )
)

(let ()
  (define data "abcdefghijklmnopqrstuvwxyz1234567890")
  (call-with-port (open-bytevector-input-port (string->utf8 data))
    (lambda (in)
      (test-assert "size limit port"
		   (binary-port? (->size-limit-binary-input-port in 10)))
      (let ((in1 (->size-limit-binary-input-port in 10)))
	(test-equal "get all" 10 (bytevector-length (get-bytevector-all in1)))
	(test-assert "eof" (eof-object? (get-u8 in1)))
	;; reset this affects original port as well
	(set-port-position! in1 0)
	(test-equal "u8 (1)" #\a (integer->char (get-u8 in1)))
	;; set-port-position! affects original port if it supports
	;; port position operations.
	(test-equal "u8 (2)" #\b (integer->char (lookahead-u8 in)))
	(test-equal "get all(2)" 9 (bytevector-length (get-bytevector-all in1)))
	)
      ;; recreation can continue
      (let ((in1 (->size-limit-binary-input-port in 10)))
	(test-equal "u8 (3)" #\k (integer->char (get-u8 in1)))
	)))
  )

(let ()
  (define in/out (open-chunked-binary-input/output-port))
  (test-assert "empty chunked input/output port" (set-port-position! in/out 0)))


(let ((buffer (make-bytevector 10)))
  (define in/out (open-bytevector-input/output-port buffer))
  (test-assert "put-bytevector (0)" (put-bytevector in/out #vu8(1 2 3 4 5)))
  (test-assert "put-bytevector (1)" (put-bytevector in/out #vu8(1 2 3 4 5)))
  (test-error "put-bytevector (2)" (put-bytevector in/out #vu8(1 2 3 4 5)))
  (set-port-position! in/out 0)
  (test-equal "get-bytevector-n (0)" #vu8(1 2 3 4 5) (get-bytevector-n in/out 5))
  (test-equal "get-bytevector-n (1)" #vu8(1 2 3 4 5) (get-bytevector-n in/out 5))
  (test-assert "get-bytevector-n (2)" (eof-object? (get-bytevector-n in/out 5))))


(test-end)
