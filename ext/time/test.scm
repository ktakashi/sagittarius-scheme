;; -*- scheme -*-

;; testing mutil thread
;; this file will be called from one upper
;; so load path must be like this
(add-load-path "./time")
(library (time test)
    (export run-time-test)
    (import (srfi :64 testing)
	    (rnrs)
	    (sagittarius time))

  (define (run-time-test)
    (let ((t (make-time time-utc 10 20)))
      (test-assert "time?" (time? t))
      (test-equal "time type" (time-type t) time-utc)
      (test-equal "time nanosecond" (time-nanosecond t) 10)
      (test-equal "time second" (time-second t) 20)
      (let ((ct (copy-time t)))
	(test-equal "copied time type" (time-type ct) time-utc)
	(test-equal "copied time nanosecond" (time-nanosecond ct) 10)
	(test-equal "copied time second" (time-second ct) 20)))
    (test-assert "time=?(1)" (time=? (make-time time-utc 20 10) (make-time time-utc 20 10)))
    (test-assert "time=?(2)" (not (time=? (make-time time-utc 20 10) (make-time time-utc 10 10))))
    (test-assert "time=?(3)" (not (time=? (make-time time-utc 10 10) (make-time time-utc 20 10))))

    (test-assert "time>=?(1)" (time>=? (make-time time-utc 20 10) (make-time time-utc 20 10)))
    (test-assert "time>=?(2)" (time>=? (make-time time-utc 30 10) (make-time time-utc 20 10)))
    (test-assert "time>=?(3)" (not (time>=? (make-time time-utc 10 10) (make-time time-utc 20 10))))

    (test-assert "time>?(1)" (not (time>? (make-time time-utc 20 10) (make-time time-utc 20 10))))
    (test-assert "time>?(2)" (time>? (make-time time-utc 30 10) (make-time time-utc 20 10)))
    (test-assert "time>?(3)" (not (time>? (make-time time-utc 10 10) (make-time time-utc 20 10))))

    (test-assert "time<=?(1)" (time<=? (make-time time-utc 20 10) (make-time time-utc 20 10)))
    (test-assert "time<=?(2)" (time<=? (make-time time-utc 10 10) (make-time time-utc 20 10)))
    (test-assert "time<=?(3)" (not (time<=? (make-time time-utc 30 10) (make-time time-utc 20 10))))

    (test-assert "time<?(1)" (not (time<? (make-time time-utc 20 10) (make-time time-utc 20 10))))
    (test-assert "time<?(2)" (time<? (make-time time-utc 10 10) (make-time time-utc 20 10)))
    (test-assert "time<?(3)" (not (time<? (make-time time-utc 30 10) (make-time time-utc 20 10))))

    (let ((diff (time-difference (make-time time-utc 20 10)
				 (make-time time-utc 10 10))))
      (test-assert "diff time?" (time? diff))
      (test-equal "diff type" (time-type diff) time-duration)
       ))
)