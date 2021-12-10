(import (rnrs)
	(scribble)
	(srfi :64))

(test-begin "Scribble format")

(define (test-parse expected s)
  (test-equal s expected (scribble-parse (open-string-input-port s))))

(test-parse '(foo) "@foo")
(test-parse '((foo 1 2 3 4)) "@foo[1 2 3 4]")
(test-parse '((foo 1 2 3 4 "text " code)) "@foo[1 2 3 4]{text @code}")
(test-parse '((foo 1 2 3 4 "text " "\n" "\n" code))
	    "@foo[1 2 3 4]{text \n\n@code}")

(test-parse '((foo "blah \"blah\" (`blah'?)")) "@foo{blah \"blah\" (`blah'?)}")

(test-parse '((foo 1 2 "3 4")) "@foo[1 2]{3 4}")
(test-parse '((foo :width 2 "blah blah")) "@foo[:width 2]{blah blah}")

;; we don't do indentation thing
(test-parse '((foo "blah blah" "\n" "yada yada")) "@foo{blah blah\nyada yada}")

(test-parse '((foo "bar " (baz "3") "\n" "blah")) "@foo{bar @baz{3}\nblah}")
(test-parse '((foo (b (u 3) " " (u "4")) "\n" "blah"))
	    "@foo{@b{@u[3] @u{4}}\nblah}")

(test-parse '(("blah blah")) "@{blah blah}")
(test-parse '(("blah " (3))) "@{blah @[3]}")

;; This is diverse from the official Racket document, but for backward
;; compatibility
(test-parse '("'" ("foo" "\n" "bar" "\n" "baz")) "'@{foo\nbar\nbaz}")
(test-parse '("'" (foo)) "'@foo{}")

(test-parse '(("blah " foo " blah")) "@{blah @foo blah}")
(test-parse '(("blah " foo: " blah")) "@{blah @foo: blah}")
(test-parse '(("blah " foo ": blah")) "@{blah @|foo|: blah}")

(test-parse '((foo "(+ 1 2) -> " (+ 1 2) "!")) "@foo{(+ 1 2) -> @(+ 1 2)!}")
(test-parse '((foo "A string escape")) "@foo{A @\"string\" escape}")
(test-parse '("@") "@\"@\"")

(test-parse '((foo "eli@barzilay.org")) "@foo{eli@\"@\"barzilay.org}")
(test-parse '((foo "A { begins a block")) "@foo{A @\"{\" begins a block}")

(test-parse '((foo "bar}@{baz")) "@foo|{bar}@{baz}|")
(test-parse '((foo "bar}@{baz")) "@foo|--{bar}@{baz}--|")
(test-parse '((foo "bar " (x "X") " baz")) "@foo|{bar |@x{X} baz}|")
(test-parse '((foo "bar " (x "@") " baz")) "@foo|{bar |@x|{@}| baz}|")
(test-parse '((foo "bar}@|{baz")) "@foo|--{bar}@|{baz}--|")
(test-parse '((foo "bar}@|{baz")) "@foo|<<{bar}@|{baz}>>|")

(test-parse '((foo "bar " (baz 2 3) " {4 5}")) "@foo{bar @baz[2 3] {4 5}}")

(test-parse '(`',@(foo "blah")) "@`',@foo{blah}")
(test-parse '(((lambda (x) x) "blah")) "@(lambda (x) x){blah}")
(test-parse '(`(,foo  "blah")) "@`(unquote foo){blah}")

(test-parse '((fo@o)) "@fo@o{}")
(test-parse '(((+ 1 1) "bla")) "@(+ 1 1){bla}")
(test-parse '(foo "(+ 1 1)") "@foo(+ 1 1)")

(test-parse '(("foo bar" "\n" "baz")) "@{foo bar\nbaz}")
(test-parse '('("foo bar" "\n" "baz")) "@'{foo bar\nbaz}")

(test-parse '((foo "bar bazblah")) "@foo{bar @; comment\nbaz@;\nblah}")
(test-parse '((foo)) "@foo{@;{var;}}")
(test-parse '((foo)) "@;comment\n@foo{}")
(test-parse '((#f)) "@;comment\n@[#f]")

(test-parse '((foo "x " y " z")) "@foo{x @y z}")
(test-parse '((foo "x " (* y 2) " z")) "@foo{x @(* y 2) z}")
(test-parse '((foo " bar")) "@{@foo bar}")

(test-parse '((foo)) "@foo{\n}")

(test-parse '((foo 1 (* 2 3) "bar")) "@foo[1 (* 2 3)]{bar}")
(test-parse '((foo (bar "...") "blah")) "@foo[@bar{...}]{blah}")

(test-parse '((+ foo (bar 1 2))) "@(+ @foo @bar[1 2])")

(test-parse '((foo "bar")) "@foo[]{bar}")
(test-parse '((foo)) "@foo[]")
(test-parse '(foo) "@foo")
(test-parse '((foo)) "@foo{}")

(test-parse '((foo :style 'big  "bar")) "@foo[:style 'big]{bar}")

(test-parse '((foo "f{o}o")) "@foo{f{o}o}")
(test-parse '((foo "{{}}{}")) "@foo{{{}}{}}")

(test-parse '((foo "bar")) "@foo{bar}")
(test-parse '((foo " bar ")) "@foo{ bar }")
(test-parse '((foo 1 " bar ")) "@foo[1]{ bar }")

(test-parse '((foo "a " (bar "b") " c")) "@foo{a @bar{b} c}")
(test-parse '((foo "a " bar " c")) "@foo{a @bar c}")
(test-parse '((foo "a " (bar 2) " c")) "@foo{a @(bar 2) c}")

(test-parse '((foo "A } marks the end")) "@foo{A @\"}\" marks the end}")

(test-parse '((foo "The prefix: @.")) "@foo{The prefix: @\"@\".}")
(test-parse '((foo "@x{y} --> (x \"y\")")) "@foo{@\"@x{y}\" --> (x \"y\")}")

(test-parse '((foo "...")) "@foo|{...}|")
(test-parse '((foo "\"}\" follows \"{\"")) "@foo|{\"}\" follows \"{\"}|")
(test-parse '((foo "Nesting |{is}| ok")) "@foo|{Nesting |{is}| ok}|")

(test-parse '((foo "Maze" "\n" (bar "is") "\n" "Life!"))
	    "@foo|{Maze\n|@bar{is}\nLife!}|")

(test-parse '((t "In " (i "sub@s") " too")) "@t|{In |@i|{sub|@\"@\"s}| too}|")

(test-parse '((foo "@x{foo} |@{bar}|.")) "@foo|<<<{@x{foo} |@{bar}|.}>>>|")
(test-parse '((foo "X " (b "Y") "...")) "@foo|!!{X |!!@b{Y}...}!!|")

(test-parse '((foo "foo" bar.)) "@foo{foo@bar.}")
(test-parse '((foo "foo" bar ".")) "@foo{foo@|bar|.}")
(test-parse '((foo "foo" 3.0)) "@foo{foo@3.}")
(test-parse '((foo "foo" 3 ".")) "@foo{foo@|3|.}")

(test-parse '((foo "foo" (f 1) "{bar}")) "@foo{foo@|(f 1)|{bar}}")
(test-parse '((foo "foo" bar "[1]{baz}")) "@foo{foo@|bar|[1]{baz}}")

(test-parse '((foo "xyz")) "@foo{x@\"y\"z}")
(test-parse '((foo "x" "y" "z")) "@foo{x@|\"y\"|z}")

(test-parse '((foo "x" 1 (+ 2 3) 4 "y")) "@foo{x@|1 (+ 2 3) 4|y}")
(test-parse '((foo "x" * * "y")) "@foo{x@|*\n*|y}")

(test-parse '((foo "Alice" "Bob" "Carol")) "@foo{Alice@||Bob@|\n|Carol}")

;; Diverse from Racket, we don't read it as string
(test-parse '((blah)) "@|{blah}|")
;; we can't handle this for now
;; (test-parse '(bla bla) "@|bla bla|")

(test-parse '((foo "First line" "\n" "Second line"))
	    "@foo{First line@;{there is still a\nnewline here;}\nSecond line}")
(test-parse '((foo "A long single-string arg."))
	    "@foo{A long @;\nsingle-@;\nstring arg.}")

(test-parse '((foo "bar")) "@foo{bar}")
(test-parse '((foo " bar ")) "@foo{ bar }")
(test-parse '((foo " bar" "\n" "baz ")) "@foo{ bar\nbaz }")

(test-parse '((foo "bar" "\n")) "@foo{bar\n}")
(test-parse '((foo "bar" "\n")) "@foo{\nbar\n}")
;; We don't handle contextual space, so this can't be passed
;; (test-parse '((foo "\n" "bar" "\n")) "@foo{\n  bar\n}")

;; (test-parse '(((foo "bar") "baz")) "@@foo{bar}{baz}")

(test-parse '(("{" "\n" "}")) "@{{\n}}")

(test-end)
