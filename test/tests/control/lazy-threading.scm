(import (rnrs)
	(control lazy-threading)
	(srfi :64)
	(scheme lazy))

(test-begin "Lazy threading macro")

(test-assert "lazy-chain creates promise" (promise? (lazy-chain (+ 1 1))))

(test-equal "lazy-chain (1)" 4
	    (force (lazy-chain (+ 1 1) (* 4 _) (/ _ 2))))

(test-equal "lazy-chain (2)" 4
	    (force (lazy-chain (+ 1 1) (* _ _))))

(let ((t '()))
  (define (update! v n)
    (set! t (cons n t))
    v)
  (let ((p (lazy-chain (begin (set! t (cons 1 t)) (+ 1 1))
		       (update! _ 2)
		       (* _ _)
		       (update! _ 3))))
    (test-equal '() t)
    (test-equal 4 (force p))
    (test-equal '(3 2 1) t)
    (test-equal 4 (force p))
    (test-equal '(3 2 1 3 2 1) t)))

;; some tests from SRFI-197 tests
(define (exclamation x) (string-append x "!"))
(test-equal "lazy-chain" "bazbarfoo!"
  (force 
   (lazy-chain ""
	       (string-append "foo" _)
	       (string-append "bar" _)
	       (string-append "baz" _)
	       (exclamation _))))

(test-equal "lazy-chain with mixed _ position" "barfoobaz"
  (force
   (lazy-chain ""
	       (string-append _ "foo")
	       (string-append "bar" _)
	       (string-append _ "baz"))))

(test-equal "chain with _ in operator position" 3
  (force (lazy-chain + (_ 1 2))))

(test-equal "chain without _" "barbazqux"
  (force
   (lazy-chain ""
	       (string-append _ "foo")
	       (string-append "bar" "baz")
	       (string-append _ "qux"))))

(test-equal "chain with custom _" "bazbarfoo!"
  (force 
   (lazy-chain "" <>
	       (string-append "foo" <>)
	       (string-append "bar" <>)
	       (string-append "baz" <>)
	       (exclamation <>))))

(test-equal "chain-if (1)" 'ok
	    (force (lazy-chain-if (+ 1 1)
				  (> _ 2)
				  'nok
				  'ok)))
(test-equal "chain-if (2)" 4
	    (force (lazy-chain-if (+ 1 1)
				  (> _ 2)
				  'nok
				  (* _ _))))

(test-equal "chain-if (3)" 4
	    (force (lazy-chain-if (+ 1 1) <>
				  (> <> 2)
				  'nok
				  (* <> <>))))

(test-equal "chain-cond (1)" 'ok
	    (force (lazy-chain-cond (+ 1 1)
				    ((> _ 2) 'nok)
				    ((= _ 2) 'ok)
				    (else 'nok))))
(test-equal "chain-cond (2)" 4
	    (force (lazy-chain-cond (+ 1 1)
				    ((> _ 2) 'nok)
				    ((= _ 2) (* _ _))
				    (else 'nok))))

(test-equal "chain-cond (3)" 9
	    (force (lazy-chain-cond (+ 1 1) <>
				    ((> <> 2) 'nok)
				    ((= <> 2) (+ <> 1) (* <> <>))
				    (else 'nok))))

(test-equal "chain-cond (4)" '(3)
	    (force (lazy-chain-cond (+ 1 1) <>
				    ((> <> 2) 'nok)
				    ((memq <> '(1 2 3)) => cdr)
				    (else 'nok))))

(test-equal "chain-cond (5)" '(3)
	    (force (lazy-chain-cond (+ 1 1) <>
				    ((> <> 2) 'nok)
				    ((memq <> '(1 2 3)) => (cdr <>))
				    (else 'nok))))

(test-end)
