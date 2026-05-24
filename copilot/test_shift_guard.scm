(import (rnrs)
        (rnrs mutable-pairs)
        (sagittarius)
        (sagittarius continuations))

(define call/cc call-with-current-continuation)

(define-syntax push!
  (syntax-rules ()
    ((push! loc val)
     (begin (set! loc (cons val loc)) loc))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! loc)
     (let ((v (car loc)))
       (set! loc (cdr loc))
       v))))

(display "reset/shift + guard 1 test\n")
(display "Output: ")
(let ()
  (define queue '())
  (define (yield) (shift k (push! queue k)))
  (push! queue (lambda ()
                 (guard (e (else (display (condition-message e))))
                   (yield)
                   (error "[E01]"))))
  (let loop ()
    (when (and (pair? queue))
      (let ((next (pop! queue)))
        (display "[W01]")
        (reset
         (dynamic-wind
           (lambda () (display "[D01]"))
           next
           (lambda () (display "[D02]")))))
      (loop))))
(newline)
(display "Expected: [W01][D01][D02][W01][D01][D01][E01][D02][D02]\n")
