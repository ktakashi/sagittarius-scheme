(import (rnrs) 
	(cache lru)
	(cache apis)
	(clos user)
	(sagittarius regex)
	(sagittarius comparators)
	(srfi :64))

(test-begin "Cache LRU")

;; make-simple-lru-cache
(let ((count 0)
      (first #f))
  (define cache
    (make-simple-lru-cache 3
     ;; count it
     (lambda (name) (set! count (+ count 1)) (regex name))
     string-comparator
     ;;(lambda (pattern name) (string=? (regex-pattern pattern) name))
     ))

  (let ((o (cache "\\s+")))
    (test-assert "cache 1" (eq? o (cache "\\s+")))
    (test-equal "first one" 1 count)
    (set! first o))

  (let ((o (cache "\\d+")))
    (test-assert "cache 2" (eq? o (cache "\\d+")))
    (test-equal "second one" 2 count))

  (let ((o (cache "\\w+")))
    (test-assert "cache 3" (eq? o (cache "\\w+")))
    (test-equal "third one" 3 count))

  (let ((o (cache "\\d+")))
    (test-assert "cache 2 (cached)" (eq? o (cache "\\d+")))
    (test-equal "must be the same" 3 count))

  (let ((o (cache "[a-z]+")))
    (test-assert "cache 4" (eq? o (cache "[a-z]+")))
    (test-equal "incremented" 4 count))

  (let ((o (cache "\\s+")))
    (test-assert "recreated" (not (eq? first (cache "\\s+"))))
    (test-equal "first one" 5 count))
  )

(let ()
  (define cache (make <lru-cache> :max-size 1 :comparator eq-comparator))
  (define first (list 'ok))
  (define second (list 'ok))
  (test-assert (cache-put! cache first 'ok))
  (test-equal 'ok (cache-get cache first))

  (test-equal '(ok) (cache-values cache))
  (test-assert (cache-put! cache second '2nd))
  (test-equal 'fallback (cache-get cache first 'fallback))
  (test-equal '2nd (cache-get cache second))
  (test-equal 1 (cache-size cache))
  (test-equal '2nd (cache-evict! cache second))
  )

(let ((count 0))
  (define cache (make <lru-cache> :max-size 2 :comparator eq-comparator
		      :on-evict (lambda (o) (set! count (+ count 1)))))
  (define first (list 'ok))
  (define second (list 'ok))
  (define third (list 'ok))
  (cache-put! cache first '1st)
  (cache-put! cache second '2nd)
  (cache-put! cache third '3rd)
  (test-equal 1 count)
  (test-eq '2nd (cache-evict! cache second))
  (test-assert (not (cache-evict! cache second)))
  (test-equal 2 count)

  (cache-put! cache first '1st)
  (test-assert (cache-clear! cache))
  (test-equal 4 count)
  )

(test-end)
