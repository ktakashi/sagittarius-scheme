(import (rnrs)
	(net mq mqtt packet)
	(srfi :64)
	(pp))

(test-begin "MQTT tests")

;; length thing
(define-syntax test-length-decode
  (syntax-rules ()
    ((_ expect v)
     (test-length-decode expect v test-equal))
    ((_ expect v cont)
     (cont (format "length ~X" expect) expect 
	   (let ((in (open-bytevector-input-port v)))
	     (let-values (((type flag len) (read-fixed-header in)))
	       len))))))

;; first byte doesn't matter
(test-length-decode 0          #vu8(0 #x00))
(test-length-decode 127        #vu8(0 #x7F))
(test-length-decode 128        #vu8(0 #x80 #x01))
(test-length-decode 16383      #vu8(0 #xFF #x7F))
(test-length-decode 16384      #vu8(0 #x80 #x80 #x01))
(test-length-decode 2097151    #vu8(0 #xFF #xFF #x7F))
(test-length-decode 2097152    #vu8(0 #x80 #x80 #x80 #x01))
(test-length-decode 268435455  #vu8(0 #xFF #xFF #xFF #x7F))
;; Too big
(test-length-decode condition? #vu8(0 #x80 #x80 #x80 #x80 #x01) test-error)

;; test chunked port
(let ()
  (define (->chunked-port bv :key (chunk-size 4096) (threshold #f))
    (make-chunked-binary-input-port (open-bytevector-input-port bv)
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
    (test-equal "port-position" 9 (port-position in))
    (test-assert "two chunks (eof)" (eof-object? (get-bytevector-all in)))
    (test-assert "set-port-position!" (set-port-position! in 3))
    (test-equal "port-position" 3 (port-position in)))

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

;; value *without* fixed header
(let* ((bv #vu8(1 0 2 97 98 1 2 1 2 3 4 5))
       (in (open-bytevector-input-port bv)))
  (let-values (((h p) (read-variable-header&payload in (bytevector-length bv)
						    :u8 :utf8 :pi)))
    (test-equal "variable header" '(1 "ab" #vu8(1 2)) h)
    (test-assert "input-port? (payload)" (input-port? p))
    (test-assert "binary-port? (payload)" (binary-port? p))
    (test-equal "payload" #vu8(1 2 3 4 5) (get-bytevector-all p))))

(let* ((bv #vu8(1 0 2 97 98 1 2 1 2 3 4 5))
       (in (open-bytevector-input-port bv)))
  (let-values (((h p) (read-variable-header&payload in 
						    (- (bytevector-length bv) 1)
						    :u8 :utf8 :pi)))
    (test-equal "variable header" '(1 "ab" #vu8(1 2)) h)
    (test-assert "input-port? (payload)" (input-port? p))
    (test-assert "binary-port? (payload)" (binary-port? p))
    (test-equal "payload" #vu8(1 2 3 4) (get-bytevector-all p))))

(let* ((bv #vu8(1 0 2 97 98 1 2 1 2 3 4 5))
       (in (open-bytevector-input-port bv)))
  (let-values (((h p) (read-variable-header&payload in 
						    (- (bytevector-length bv) 1)
						    :chunk-size 1
						    :u8 :utf8 :pi)))
    (test-equal "variable header" '(1 "ab" #vu8(1 2)) h)
    (test-assert "input-port? (payload)" (input-port? p))
    (test-assert "binary-port? (payload)" (binary-port? p))
    (test-equal "payload" #vu8(1 2 3 4) (get-bytevector-all p))))

(test-end)
