(import (rnrs)
	(rnrs eval)
	(sagittarius remote-repl)
	(sagittarius control)
	(sagittarius interactive)
	(srfi :18)
	(srfi :39)
	(srfi :64)
	(sagittarius socket))

(test-begin "Remote REPL")

(define-values (thunk server)
  (make-remote-repl "10000" :log #f :environment (environment '(rnrs))))
(define t (thread-start! (make-thread thunk)))

(define (test-repl-port in)
  (let-values (((out extract) (open-string-output-port)))
    (parameterize ((current-input-port in)
		   (current-prompter (lambda () #t))
		   (current-output-port out))
      (connect-remote-repl "localhost" "10000" :quiet #t))
    (extract)))
(define (test-repl command) (test-repl-port (open-string-input-port command)))

(test-equal "basics (symbol)" "a\n" (test-repl "'a"))
(test-equal "basics (number)" "123\n" (test-repl "123"))
(test-equal "definition(1)" "#<unspecified>\n" (test-repl "(define a 'b)"))
(test-equal "definition(2)" "b\n" (test-repl "a"))

(test-equal "hashbang on remote REPL" "#/aaa/\n"
	    (test-repl "#!read-macro=sagittarius/regex #/aaa/"))
(test-equal "hashbang on remote REPL(2)" "#vu8(97 97 97)\n"
	    (test-repl "#!read-macro=sagittarius/bv-string #*\"aaa\""))

(socket-close server)
(guard (e (else #t)) (thread-join! t))

(test-end)
