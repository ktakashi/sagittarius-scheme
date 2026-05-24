(import (rnrs)
        (sagittarius continuations))

(display "Test 1: basic call/prompt")
(newline)
(let ((result (call/prompt (lambda () 'ok))))
  (display result)
  (newline))

(display "Test 2: call/prompt with call/delim-cc")
(newline)
(let ((result (call/prompt (lambda () (call/delim-cc values)))))
  (display (continuation? result))
  (newline))

(display "Done")
(newline)
