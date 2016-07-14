(import (rnrs)
	(sagittarius remote-repl)
	(sagittarius control)
	(sagittarius interactive)
	(srfi :18)
	(srfi :39)
	(srfi :64)
	(sagittarius socket))

(test-begin "Remote REPL")

(define-values (thunk server) (make-remote-repl "8501" :log #f))
(define t (thread-start! (make-thread thunk)))

(define commands "#!read-macro=sagittarius/regex #/aaa/")

(define (test-repl command)
  (let-values (((out extract) (open-string-output-port)))
    (parameterize ((current-input-port (open-string-input-port commands))
		   (current-prompter (lambda () #t))
		   (current-output-port out))
      (connect-remote-repl "localhost" "8501" :quiet #t))
    (extract)))

(test-equal "hashbang on remote REPL" "#/aaa/\n" (test-repl commands))

(socket-close server)
(guard (e (else #t)) (thread-join! t))

(test-end)
