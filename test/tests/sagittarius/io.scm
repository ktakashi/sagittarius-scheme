(import (rnrs)
	(rnrs mutable-strings)
	(sagittarius io)
	(clos user)
	(srfi :64))

(test-begin "Sagittarius - Extra I/O")

(test-equal "buffered-port (read)" #vu8(1 2 3 4 5)
	    (call-with-port
	     (buffered-port (open-bytevector-input-port #vu8(1 2 3 4 5)))
	     get-bytevector-all))

(test-equal "buffered-port (write)" #vu8(1 2 3 4 5)
	    (let-values (((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out)
	        (lambda (out) 
		  (put-bytevector out #vu8(1 2 3 4 5))
		  ;; it's buffered so flush it
		  (flush-output-port out)
		  (extract)))))

(test-equal "buffered-port write with buffer(1)" #vu8(1 2 3 4 5)
	    (let-values (((buf) (make-bytevector 5))
			 ((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out :buffer buf)
	        (lambda (out) 
		  (put-bytevector out #vu8(1 2 3 4 5))))
	      buf))

;; This is rather unspecified behaviour but works like this
(test-equal "buffered-port write with buffer(2)" #vu8(5 2 3 4)
	    (let-values (((buf) (make-bytevector 4))
			 ((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out :buffer buf)
	        (lambda (out) 
		  (put-bytevector out #vu8(1 2 3 4 5))))
	      buf))

;; depending on default buffer size (8196)
(test-equal "buffered-port write with mode (1)" #vu8()
	    (let-values (((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out
					     :buffer-mode (buffer-mode block))
	        (lambda (out) 
		  (put-bytevector out #vu8(1 2 3 4 5))
		  (extract)))))
;; line mode should flush after EOL
(test-equal "buffered-port write with mode (2)" (string->utf8 "hello\n")
	    (let-values (((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out
					     :buffer-mode (buffer-mode line))
	        (lambda (out) 
		  (put-bytevector out (string->utf8 "hello\n"))
		  (extract)))))

;; seems R6RS doesn't specify which condition should be raised
;; but we raise &assertion
(test-error "buffered-port (pseudo closed)" assertion-violation?
	    (let* ((in (open-bytevector-input-port #vu8(1 2 3 4 5)))
		   (bin (buffered-port in)))
	      (get-bytevector-all in)))

;; NB: this is temporary decision to make my life easier
(test-error "buffered-port (error)" assertion-violation?
	    (buffered-port (open-string-input-port "abcde")))

;; we don't test transcoded-port here. It's tested on R6RS test suite

(test-equal "call-with-input-string" '(ok) (call-with-input-string "(ok)" read))
(test-equal "call-with-output-string" "(ok)" 
	    (call-with-output-string (lambda (o) (write '(ok) o))))

(test-equal "with-input-from-string" '(ok) (with-input-from-string "(ok)" read))
(test-equal "with-output-from-string" "(ok)"
	    (with-output-to-string (lambda () (write '(ok)))))

;; main tests form extra i/o
;; custom ports
(test-assert "custom port pred(1)" (port? (make <custom-binary-input-port>)))
(test-assert "custom port pred(2)" (port? (make <custom-binary-output-port>)))
(test-assert "custom port pred(3)"
	     (port? (make <custom-binary-input/output-port>)))
(test-assert "custom port pred(4)" (port? (make <custom-textual-input-port>)))
(test-assert "custom port pred(5)" (port? (make <custom-textual-output-port>)))
(test-assert "custom port pred(6)"
	     (port? (make <custom-textual-input/output-port>)))

(test-equal "get-u8(1)" 1
	    (get-u8 (make <custom-binary-input-port>
		      :read (lambda (bv start count)
			      (bytevector-u8-set! bv start 1)
			      1))))
(test-equal "get-u8(2)" 1
	    (get-u8 (make <custom-binary-input/output-port>
		      :read (lambda (bv start count)
			      (bytevector-u8-set! bv start 1)
			      1)
		      ;; NB: :write is needed but not required
		      ;; it'll raise an error when it's called
		      ;; This allow users to set slot after creation
		      )))

(test-error "get-u8(3)" assertion-violation?
	    (get-u8 (make <custom-binary-output-port>
		      ;; NB: again this won't be checked
		      :read (lambda (bv start count)
			      (bytevector-u8-set! bv start 1)
			      1))))
;; this is raised by VM so don't assume which condition
(test-error "get-u8(4)" condition? (get-u8 (make <custom-binary-input-port>)))

;; textual
(test-equal "get-char(1)" #\a
	    (get-char (make <custom-textual-input-port>
		      :read (lambda (str start count)
			      (string-set! str start #\a)
			      1))))
(test-equal "get-char(2)" #\a
	    (get-char (make <custom-textual-input/output-port>
		      :read (lambda (str start count)
			      (string-set! str start #\a)
			      1)
		      ;; NB: :write is needed but not required
		      ;; it'll raise an error when it's called
		      ;; This allow users to set slot after creation
		      )))

(test-error "get-char(3)" assertion-violation?
	    (get-char (make <custom-textual-output-port>
		      ;; NB: again this won't be checked
		      :read (lambda (str start count)
			      (string-set! str start #\a)
			      1))))
;; this is raised by VM so don't assume which condition
(test-error "get-char(4)" condition? 
	    (get-char (make <custom-textual-input-port>)))


(test-end)
