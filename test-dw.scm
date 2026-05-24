(import (rnrs) (sagittarius continuations))
;;#lang racket
;;(require racket/control)

(define saved #f)
(define count 0)
(call/prompt
 (lambda ()
   (dynamic-wind
       (lambda () (display "pre1\n"))
       (lambda ()
         (call/prompt
          (lambda ()
            (dynamic-wind
                (lambda () (display "pre2\n"))
                (lambda () #t)
                (lambda ()
                  (call/delim-cc (lambda (k) (set! saved k) ))
                  (display "post2\n"))))))
       (lambda () (display "post1\n")))
   (unless (= count 1)
     (set! count (+ count 1))
     (saved 'again))))
