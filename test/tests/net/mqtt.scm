(import (rnrs)
	(net mq mqtt packet)
	(net mq mqtt topic)
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

;; value *without* fixed header
(let* ((bv #vu8(1 0 2 97 98 1 2 1 2 3 4 5))
       (in (open-bytevector-input-port bv)))
  (let-values (((h p) (read-variable-header&payload in (bytevector-length bv)
						    :u8 :utf8 :pi)))
    (test-equal "variable header" '(1 "ab" 258) h)
    (test-assert "input-port? (payload)" (input-port? p))
    (test-assert "binary-port? (payload)" (binary-port? p))
    (test-equal "payload" #vu8(1 2 3 4 5) (get-bytevector-all p))))

(let* ((bv #vu8(1 0 2 97 98 1 2 1 2 3 4 5))
       (in (open-bytevector-input-port bv)))
  (let-values (((h p) (read-variable-header&payload in 
						    (- (bytevector-length bv) 1)
						    :u8 :utf8 :pi)))
    (test-equal "variable header" '(1 "ab" 258) h)
    (test-assert "input-port? (payload)" (input-port? p))
    (test-assert "binary-port? (payload)" (binary-port? p))
    (test-equal "payload" #vu8(1 2 3 4) (get-bytevector-all p))))

(let* ((bv #vu8(1 0 2 97 98 1 2 1 2 3 4 5))
       (in (open-bytevector-input-port bv)))
  (let-values (((h p) (read-variable-header&payload in 
						    (- (bytevector-length bv) 1)
						    :chunk-size 1
						    :u8 :utf8 :pi)))
    (test-equal "variable header" '(1 "ab" 258) h)
    (test-assert "input-port? (payload)" (input-port? p))
    (test-assert "binary-port? (payload)" (binary-port? p))
    (test-equal "payload" #vu8(1 2 3 4) (get-bytevector-all p))))

;; no payload
(let* ((bv #vu8(1 0 2 97 98 1 2))
       (in (open-bytevector-input-port bv)))
  (let-values (((h p) (read-variable-header&payload in 
						    (bytevector-length bv)
						    :u8 :utf8 :pi)))
    (test-equal "variable header" '(1 "ab" 258) h)
    (test-assert "input-port? (payload)" (input-port? p))
    (test-assert "binary-port? (payload)" (binary-port? p))
    (test-assert "payload" (eof-object? (get-bytevector-all p)))))

;; error case
(let* ((bv #vu8(1 0 2 97 98 1 2))
       (in (open-bytevector-input-port bv)))
  (test-error "corrupted data" condition?
	      (read-variable-header&payload in 
					    (- (bytevector-length bv) 1)
					    :u8 :utf8 :pi)))

;; topic validation
(test-assert "#" (mqtt-valid-topic? "#"))
(test-assert "sport/tennis/#" (mqtt-valid-topic? "sport/tennis/#"))
(test-assert "sport/tennis#" (not (mqtt-valid-topic? "sport/tennis#")))
(test-assert "sport/tennis/#/ranking" 
	     (not (mqtt-valid-topic? "sport/tennis/#/ranking")))
(test-assert "sport/#/#" (not (mqtt-valid-topic? "sport/#/#")))

(test-assert "+" (mqtt-valid-topic? "+"))
(test-assert "+/tennis/#" (mqtt-valid-topic? "+/tennis/#"))
(test-assert "sport+" (not (mqtt-valid-topic? "sport+")))
(test-assert "sport/+/player1" (mqtt-valid-topic? "sport/+/player1"))

(test-assert "sport/tennis/player1/# (1)"
	     (mqtt-topic-match? "sport/tennis/player1/#"
				"sport/tennis/player1"))
(test-assert "sport/tennis/player1/# (2)"
	     (mqtt-topic-match? "sport/tennis/player1/#" 
				"sport/tennis/player1/ranking"))
(test-assert "sport/tennis/player1/# (3)"
	     (mqtt-topic-match? "sport/tennis/player1/#"
				"sport/tennis/player1/score/wimbledon"))

(test-assert "sport/tennis/+ (1)" 
	     (mqtt-topic-match? "sport/tennis/+" "sport/tennis/player1"))
(test-assert "sport/tennis/+ (2)" 
	     (mqtt-topic-match? "sport/tennis/+" "sport/tennis/player2"))
(test-assert "sport/tennis/+ (3)" 
	     (not (mqtt-topic-match? "sport/tennis/+"
				     "sport/tennis/player1/ranking")))

(test-assert "sport/+ (1)" (not (mqtt-topic-match? "sport/+" "sport")))
(test-assert "sport/+ (2)" (mqtt-topic-match? "sport/+" "sport/"))

(test-assert "/finance (1)" (mqtt-topic-match? "+/+" "/finance"))
(test-assert "/finance (2)" (not (mqtt-topic-match? "+" "/finance")))
(test-assert "/finance (3)" (mqtt-topic-match? "/finance" "/finance"))

(test-assert "# (1)" (mqtt-topic-match? "#" "sport/tennis/player1"))
(test-assert "# (2)" (not (mqtt-topic-match? "#" "$SYS")))
(test-assert "+/monitor/Clients" 
	     (not (mqtt-topic-match? "+/monitor/Clients"
				     "$SYS/monitor/Clients")))
(test-assert "$SYS/# (1)" (mqtt-topic-match? "$SYS/#" "$SYS/"))
(test-assert "$SYS/monitor/+" (mqtt-topic-match? "$SYS/monitor/+"
						 "$SYS/monitor/Clients"))


;; specific topic
(define-syntax test-topic-compare
  (syntax-rules ()
    ((_ a b expected)
     (test-equal (format "~a vs ~a" a b)
		 expected (mqtt-topic-compare a b)))))
(test-topic-compare "foo/bar" "buz/bla" 0)
(test-topic-compare "foo/bar" "buz/bla/brr" -1)
(test-topic-compare "foo/bar/brr" "buz/bla" 1)
(test-topic-compare "+/bar" "buz/bla" -1)
(test-topic-compare "foo/bar" "+/bla" 1)
(test-topic-compare "+/bar" "+/bla" 0)
(test-topic-compare "#" "buz/bla" -1)
(test-topic-compare "#" "buz/#" -1)
(test-topic-compare "foo/#" "buz/#" 0)
(test-topic-compare "foo/bar/#" "buz/#" 1)

(define-syntax test-topic*?
  (syntax-rules ()
    ((_ a b <=>?)
     (test-assert (format "~a vs ~a (~a)" a b <=>?) (<=>? a b)))))
(define-syntax test-topic<?
  (syntax-rules ()
    ((_ a b) (test-topic*? a b mqtt-topic<?))))
(define-syntax test-topic<=?
  (syntax-rules ()
    ((_ a b) (test-topic*? a b mqtt-topic<=?))))
(define-syntax test-topic>?
  (syntax-rules ()
    ((_ a b) (test-topic*? a b mqtt-topic>?))))
(define-syntax test-topic>=?
  (syntax-rules ()
    ((_ a b) (test-topic*? a b mqtt-topic>=?))))
(define-syntax test-topic=?
  (syntax-rules ()
    ((_ a b) (test-topic*? a b mqtt-topic=?))))
(test-topic<? "foo/bar" "buz/bla/brr")
(test-topic<? "+/bar" "buz/bla")
(test-topic<? "#" "buz/bla")
(test-topic<? "#" "buz/#")

(test-topic>? "foo/bar" "+/bla")
(test-topic>? "foo/bar/brr" "buz/bla")
(test-topic>? "foo/bar/#" "buz/#")

(test-topic>=? "foo/#" "buz/#")
(test-topic>=? "foo/bar" "buz/bla")
(test-topic>=? "+/bar" "+/bla")

(test-topic<=? "foo/#" "buz/#")
(test-topic<=? "foo/bar" "buz/bla")
(test-topic<=? "+/bar" "+/bla")

(test-topic=? "foo/#" "buz/#")
(test-topic=? "foo/bar" "buz/bla")
(test-topic=? "+/bar" "+/bla")

(test-end)
