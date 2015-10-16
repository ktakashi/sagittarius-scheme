(import (rnrs)
	(rnrs mutable-strings)
	(sagittarius io)
	(clos user)
	(srfi :64))

(test-begin "Sagittarius - Extra I/O")

;; buffered-port
(let ((in (open-bytevector-input-port #vu8())))
  (test-error "literal buffer" assertion-violation? 
	      (buffered-port in (buffer-mode block)
			     :buffer #vu8(0 0 0 0)))
  (test-error "zero length buffer" assertion-violation? 
	      (buffered-port in (buffer-mode block) 
			     :buffer (make-bytevector 0)))
  (test-assert "buffered-port" (port? (buffered-port in (buffer-mode block))))
  (test-assert "source closed" (port-closed? in)))

(test-equal "buffered-port (read)" #vu8(1 2 3 4 5)
	    (call-with-port
	     (buffered-port (open-bytevector-input-port #vu8(1 2 3 4 5))
			    (buffer-mode block))
	     get-bytevector-all))

(test-equal "buffered-port (write)" #vu8(1 2 3 4 5)
	    (let-values (((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out (buffer-mode block))
	        (lambda (out) 
		  (put-bytevector out #vu8(1 2 3 4 5))
		  ;; it's buffered so flush it
		  (flush-output-port out)
		  (extract)))))

(test-equal "buffered-port write with buffer(1)" #vu8(1 2 3 4 5)
	    (let-values (((buf) (make-bytevector 5))
			 ((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out (buffer-mode block) 
					     :buffer buf)
	        (lambda (out) 
		  (put-bytevector out #vu8(1 2 3 4 5))))
	      buf))

;; This is rather unspecified behaviour but works like this
(test-equal "buffered-port write with buffer(2)" #vu8(5 2 3 4)
	    (let-values (((buf) (make-bytevector 4))
			 ((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out (buffer-mode block)
					     :buffer buf)
	        (lambda (out) 
		  (put-bytevector out #vu8(1 2 3 4 5))))
	      buf))

;; depending on default buffer size (8196)
(test-equal "buffered-port write with mode (1)" #vu8()
	    (let-values (((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out (buffer-mode block))
	        (lambda (out) 
		  (put-bytevector out #vu8(1 2 3 4 5))
		  (extract)))))
;; line mode should flush after EOL
(test-equal "buffered-port write with mode (2)" (string->utf8 "hello\n")
	    (let-values (((out extract) (open-bytevector-output-port)))
	      (call-with-port (buffered-port out (buffer-mode line))
	        (lambda (out) 
		  (put-bytevector out (string->utf8 "hello\n"))
		  (extract)))))

(define test-file "io-test")
(when (file-exists? test-file) (delete-file test-file))
;; create
(let ((out (open-file-output-port test-file (file-options no-fail)
				  (buffer-mode block) (native-transcoder))))
  (put-string out "hello")
  ;; no flush
  (close-port out)
  )
(let ((in (open-file-input-port test-file (file-options no-fail)
				(buffer-mode block) (native-transcoder))))
  (test-equal "buffered-port without flush" "hello" (get-string-all in))
  (close-port in))

(when (file-exists? test-file) (delete-file test-file))
;; seems R6RS doesn't specify which condition should be raised
;; but we raise &assertion
(test-error "buffered-port (pseudo closed)" assertion-violation?
	    (let* ((in (open-bytevector-input-port #vu8(1 2 3 4 5)))
		   (bin (buffered-port in)))
	      (get-bytevector-all in)))

;; NB: this is temporary decision to make my life easier
(test-error "buffered-port (error)" assertion-violation?
	    (buffered-port (open-string-input-port "abcde")
			   (buffer-mode block)))

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

(let ()
  (define-class <my-port> (<custom-binary-output-port>) 
    ((buffer :init-form (make-bytevector 5))))
  (let ((out (make <my-port>)))
    (slot-set! out 'write
	       (lambda (bv start count)
		 (bytevector-copy! bv start (slot-ref out 'buffer) 0 count)
		 count))
    (put-bytevector out #vu8(1 2 3 4 5))
    (test-equal "custom port sub class" #vu8(1 2 3 4 5)
		(slot-ref out 'buffer))))


(define (test-port-slots class)
  (define read #f)
  (define write #f)
  (define position #f)
  (define set-position #f)
  (define ready #f)
  (define flush #f)
  (define close #f)
  (let ((port (make class 
		:read (lambda (bs s c) (set! read #t) c)
		:write (lambda (bs s c) (set! write #t) c)
		:position (lambda () (set! position #t) 0)
		:set-position (lambda (o w) (set! set-position (list o w)))
		:ready (lambda () (set! ready #t) #f)
		:flush (lambda () (set! flush #t))
		:close (lambda () (set! close #t)))))
    (when (input-port? port)
      (if (binary-port? port)
	  (get-u8 port)
	  (get-char port))
      (test-assert (format "read ~a" class) read))
    (when (output-port? port)
      (if (binary-port? port)
	  (put-u8 port 1)
	  (put-char port #\a))
      (test-assert (format "write ~a" class) write))
    ;; position
    (test-equal (format "port-position: ~a(1)" class) 0 (port-position port))
    (test-assert (format "port-position: ~a(2)" class) position)

    (test-assert (format "set-port-position!: ~a (1)" class)
		 (set-port-position! port 0 'begin))
    (test-equal (format "set-port-position!: ~a (0 begin)" class)
		'(0 begin) set-position)
    (test-assert (format "set-port-position!: ~a (2)" class)
		 (set-port-position! port 1 'current))
    (test-equal (format "set-port-position!: ~a (1 current)" class)
		'(1 current) set-position)
    (test-assert (format "set-port-position!: ~a (3)" class)
		 (set-port-position! port -3 'end))
    (test-equal (format "set-port-position!: ~a (-3 end)" class)
		'(-3 end) set-position)
    
    (when (input-port? port)
      (test-assert (format "ready ~a" class) (not (port-ready? port)))
      (test-assert (format "ready ~a" class) ready))

    (when (output-port? port)
      (test-assert (format "flush ~a" class) (flush-output-port port))
      (test-assert (format "flush ~a" class) flush))

    (test-assert (format "close ~a" class) (close-port port))
    (test-assert (format "close ~a" class) close)
    ))

(test-port-slots <custom-binary-input-port>)
(test-port-slots <custom-textual-input-port>)
(test-port-slots <custom-binary-output-port>)
(test-port-slots <custom-textual-output-port>)
(test-port-slots <custom-binary-input/output-port>)
(test-port-slots <custom-textual-input/output-port>)

(let ((buffered-custom 
       (buffered-port (make <custom-binary-input/output-port>)
		      (buffer-mode block))))
  (test-assert "flush called set-port-position!"
	       (flush-output-port buffered-custom)))


(test-end)
