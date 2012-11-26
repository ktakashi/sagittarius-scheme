;; -*- mode:scheme; coding: utf-8; -*-
(import (srfi :13 strings)
	(srfi :14 char-sets)
	(except (rnrs) string?) ;; to avoid confliction
	(srfi :64 testing))

(define (check-srfi-13-error e) e)

(test-begin "SRFI-13 tests")

(test-assert 'string? (string? "abc"))
(test-assert "not string" (not (string? 'abc)))
(test-assert 'string-null? (string-null? ""))
(test-assert 'string-null?-error (not (string-null? "abc")))
(test-assert 'string-every (string-every (lambda (c)
					   (<= 0 (char->integer c) 128))
					 "abcdefg"))
(test-assert 'string-any (string-any (lambda (c)
				       (> (char->integer c) 128))
				     "abcdefgλ"))

;; below test cases are from guile
(define exception:strict-infix-grammar
  (cons 'misc-error "^strict-infix"))

;; Create a string from integer char values, eg. (string-ints 65) => "A"
(define (string-ints . args)
  (apply string (map integer->char args)))

;; converted automatically
(test-assert
  "string-any null string"
  (not (string-any #\a "")))
(test-assert
  "string-any start index == end index"
  (not (string-any #\a "aaa" 1 1)))
(test-error
  "string-any bad char_pred integer"
  check-srfi-13-error
  (string-any 123 "abcde"))
(test-error
  "string-any bad char_pred string"
  check-srfi-13-error
  (string-any "zzz" "abcde"))
(test-assert
  "string-any char no match"
  (not (string-any #\C "abcde")))
(test-assert
  "string-any char one match"
  (string-any #\C "abCde"))
(test-assert
  "string-any char one match: BMP"
  (string-any (integer->char 256) "abĀde"))
(test-assert
  "string-any char one match: SMP"
  (string-any (integer->char 66304) "ab𐌀de"))
(test-assert
  "string-any char more than one match"
  (string-any #\X "abXXX"))
(test-assert
  "string-any char no match, start index"
  (not (string-any #\A "Abcde" 1)))
(test-assert
  "string-any char one match, start index"
  (string-any #\C "abCde" 1))
(test-assert
  "string-any char more than one match, start index"
  (string-any #\X "abXXX" 1))
(test-assert
  "string-any char no match, start and end index"
  (not (string-any #\X "XbcdX" 1 4)))
(test-assert
  "string-any char one match, start and end index"
  (string-any #\C "abCde" 1 4))
(test-assert
  "string-any char more than one match, start and end index"
  (string-any #\X "abXXX" 1 4))
(test-assert
  "string-any charset no match"
  (not (string-any char-set:upper-case "abcde")))
(test-assert
  "string-any charset one match"
  (string-any char-set:upper-case "abCde"))
(test-assert
  "string-any charset more than one match"
  (string-any char-set:upper-case "abCDE"))
(test-assert
  "string-any charset no match, start index"
  (not (string-any char-set:upper-case "Abcde" 1)))
(test-assert
  "string-any charset one match, start index"
  (string-any char-set:upper-case "abCde" 1))
(test-assert
  "string-any charset more than one match, start index"
  (string-any char-set:upper-case "abCDE" 1))
(test-assert
  "string-any charset no match, start and end index"
  (not (string-any char-set:upper-case "AbcdE" 1 4)))
(test-assert
  "string-any charset one match, start and end index"
  (string-any char-set:upper-case "abCde" 1 4))
(test-assert
  "string-any charset more than one match, start and end index"
  (string-any char-set:upper-case "abCDE" 1 4))
(test-assert
  "string-any pred no match"
  (not (string-any char-upper-case? "abcde")))
(test-assert
  "string-any pred one match"
  (string-any char-upper-case? "abCde"))
(test-assert
  "string-any pred more than one match"
  (string-any char-upper-case? "abCDE"))
(test-assert
  "string-any pred no match, start index"
  (not (string-any char-upper-case? "Abcde" 1)))
(test-assert
  "string-any pred one match, start index"
  (string-any char-upper-case? "abCde" 1))
(test-assert
  "string-any pred more than one match, start index"
  (string-any char-upper-case? "abCDE" 1))
(test-assert
  "string-any pred no match, start and end index"
  (not (string-any char-upper-case? "AbcdE" 1 4)))
(test-assert
  "string-any pred one match, start and end index"
  (string-any char-upper-case? "abCde" 1 4))
(test-assert
  "string-any pred more than one match, start and end index"
  (string-any char-upper-case? "abCDE" 1 4))
(test-assert
  "string-titlecase all-lower"
  (string=? "Foo" (string-titlecase "foo")))
(test-assert
  "string-titlecase all-upper"
  (string=? "Foo" (string-titlecase "FOO")))
(test-assert
  "string-titlecase two-words"
  (string=?
    "Hello, World!"
    (string-titlecase "hello, world!")))

(test-assert
  "string-append/shared no args"
  (string=? "" (string-append/shared)))
(test-assert
  "string-append/shared one arg empty"
  (string=? "" (string-append/shared "")))
(test-assert
  "string-append/shared one arg non-empty"
  (string=? "xyz" (string-append/shared "xyz")))
(test-assert
  "string-append/shared two args"
  (string=? "" (string-append/shared "" "")))
(test-assert
  "string-append/shared two args"
  (string=? "xyz" (string-append/shared "xyz" "")))
(test-assert
  "string-append/shared two args"
  (string=? "xyz" (string-append/shared "" "xyz")))
(test-assert
  "string-append/shared two args"
  (string=?
    "abcxyz"
    (string-append/shared "abc" "xyz")))
(test-assert
  "string-append/shared two args"
  (string=?
    "abc\x0100;\x0101;"
    (string-append/shared "abc" "\x0100;\x0101;")))
(test-assert
  "string-append/shared three args"
  (string=? "" (string-append/shared "" "" "")))
(test-assert
  "string-append/shared three args"
  (string=? "xy" (string-append/shared "xy" "" "")))
(test-assert
  "string-append/shared three args"
  (string=? "xy" (string-append/shared "" "xy" "")))
(test-assert
  "string-append/shared three args"
  (string=?
    "abxy"
    (string-append/shared "ab" "xy" "")))
(test-assert
  "string-append/shared three args"
  (string=? "ab" (string-append/shared "" "" "ab")))
(test-assert
  "string-append/shared three args"
  (string=?
    "xyab"
    (string-append/shared "xy" "" "ab")))

(test-assert
  "string-append/shared three args"
  (string=?
    "xyab"
    (string-append/shared "" "xy" "ab")))
(test-assert
  "string-append/shared three args"
  (string=?
    "ghxyab"
    (string-append/shared "gh" "xy" "ab")))
(test-assert
  "string-append/shared four args"
  (string=? "" (string-append/shared "" "" "" "")))
(test-assert
  "string-append/shared four args"
  (string=?
    "xy"
    (string-append/shared "xy" "" "" "")))
(test-assert
  "string-append/shared four args"
  (string=?
    "xy"
    (string-append/shared "" "xy" "" "")))
(test-assert
  "string-append/shared four args"
  (string=?
    "xy"
    (string-append/shared "" "" "xy" "")))

(test-assert
  "string-append/shared four args"
  (string=?
    "xy"
    (string-append/shared "" "" "" "xy")))
(test-assert
  "string-append/shared four args"
  (string=?
    "abxy"
    (string-append/shared "ab" "xy" "" "")))
(test-assert
  "string-append/shared four args"
  (string=?
    "abxy"
    (string-append/shared "ab" "" "xy" "")))
(test-assert
  "string-append/shared four args"
  (string=?
    "abxy"
    (string-append/shared "ab" "" "" "xy")))
(test-assert
  "string-append/shared four args"
  (string=?
    "abxy"
    (string-append/shared "" "ab" "" "xy")))
(test-assert
  "string-append/shared four args"
  (string=?
    "abxy"
    (string-append/shared "" "" "ab" "xy")))

(test-error
  "string-concatenate inum"
  check-srfi-13-error
  (string-concatenate 123))
(test-error
  "string-concatenate symbol"
  check-srfi-13-error
  (string-concatenate 'x))
(test-error
  "string-concatenate improper 1"
  check-srfi-13-error
  (string-concatenate '("a" . "b")))
(test-assert
  "string-concatenate"
  (equal?
    "abc"
    (string-concatenate '("a" "b" "c"))))
(test-assert
  "string-concatenate concatenate BMP"
  (equal? "aĀ" (string-concatenate '("a" "Ā"))))
(test-assert
  "string-compare same as char<?"
  (eq? (char<? (integer->char 0) (integer->char 255))
       (string-compare
         (string-ints 0)
         (string-ints 255)
         (lambda (pos) #t)
         (lambda (pos) #f)
         (lambda (pos) #f))))
(test-assert
  "string-compare-ci same as char-ci<?"
  (eq? (char-ci<? (integer->char 0) (integer->char 255))
       (string-compare-ci
         (string-ints 0)
         (string-ints 255)
         (lambda (pos) #t)
         (lambda (pos) #f)
         (lambda (pos) #f))))
(test-error
  "string-concatenate/shared inum"
  check-srfi-13-error
  (string-concatenate/shared 123))
(test-error
  "string-concatenate/shared symbol"
  check-srfi-13-error
  (string-concatenate/shared 'x))
(test-error
  "string-concatenate/shared improper 1"
  check-srfi-13-error
  (string-concatenate/shared '("a" . "b")))
(test-assert
  "string-concatenate/shared"
  (equal?
    "abc"
    (string-concatenate/shared '("a" "b" "c"))))
(test-assert
  "string-concatenate/shared BMP"
  (equal?
    "aĀc"
    (string-concatenate/shared '("a" "Ā" "c"))))
(test-assert
  "string-every null string"
  (string-every #\a ""))
(test-assert
  "string-every start index == end index"
  (string-every #\a "bbb" 1 1))
(test-error
  "string-every bad char_pred integer"
  check-srfi-13-error
  (string-every 123 "abcde"))
(test-error
  "string-every bad char_pred string"
  check-srfi-13-error
  (string-every "zzz" "abcde"))
(test-assert
  "string-every char empty string"
  (string-every #\X ""))
(test-assert
  "string-every char empty substring"
  (string-every #\X "abc" 1 1))
(test-assert
  "string-every char no match at all"
  (not (string-every #\X "abcde")))
(test-assert
  "string-every char not all match"
  (not (string-every #\X "abXXX")))
(test-assert
  "string-every char all match"
  (string-every #\X "XXXXX"))
(test-assert
  "string-every char no match at all, start index"
  (not (string-every #\X "Xbcde" 1)))
(test-assert
  "string-every char not all match, start index"
  (not (string-every #\X "XXcde" 1)))
(test-assert
  "string-every char all match, start index"
  (string-every #\X "aXXXX" 1))
(test-assert
  "string-every char no match at all, start and end index"
  (not (string-every #\X "XbcdX" 1 4)))
(test-assert
  "string-every char not all match, start and end index"
  (not (string-every #\X "XXcde" 1 4)))
(test-assert
  "string-every char all match, start and end index"
  (string-every #\X "aXXXe" 1 4))
(test-assert
  "string-every charset empty string"
  (string-every char-set:upper-case ""))
(test-assert
  "string-every charset empty substring"
  (string-every char-set:upper-case "abc" 1 1))
(test-assert
  "string-every charset no match at all"
  (not (string-every char-set:upper-case "abcde")))
(test-assert
  "string-every charset not all match"
  (not (string-every char-set:upper-case "abCDE")))
(test-assert
  "string-every charset all match"
  (string-every char-set:upper-case "ABCDE"))
(test-assert
  "string-every charset no match at all, start index"
  (not (string-every char-set:upper-case "Abcde" 1)))
(test-assert
  "string-every charset not all match, start index"
  (not (string-every char-set:upper-case "ABcde" 1)))
(test-assert
  "string-every charset all match, start index"
  (string-every char-set:upper-case "aBCDE" 1))
(test-assert
  "string-every charset no match at all, start and end index"
  (not (string-every char-set:upper-case "AbcdE" 1 4)))
(test-assert
  "string-every charset not all match, start and end index"
  (not (string-every char-set:upper-case "ABcde" 1 4)))
(test-assert
  "string-every charset all match, start and end index"
  (string-every char-set:upper-case "aBCDe" 1 4))
(test-assert
  "string-every pred empty string"
  (string-every char-upper-case? ""))
(test-assert
  "string-every pred empty substring"
  (string-every char-upper-case? "abc" 1 1))
(test-assert
  "string-every pred no match at all"
  (not (string-every char-upper-case? "abcde")))
(test-assert
  "string-every pred not all match"
  (not (string-every char-upper-case? "abCDE")))
(test-assert
  "string-every pred all match"
  (string-every char-upper-case? "ABCDE"))
(test-assert
  "string-every pred no match at all, start index"
  (not (string-every char-upper-case? "Abcde" 1)))
(test-assert
  "string-every pred not all match, start index"
  (not (string-every char-upper-case? "ABcde" 1)))
(test-assert
  "string-every pred all match, start index"
  (string-every char-upper-case? "aBCDE" 1))
(test-assert
  "string-every pred no match at all, start and end index"
  (not (string-every char-upper-case? "AbcdE" 1 4)))
(test-assert
  "string-every pred not all match, start and end index"
  (not (string-every char-upper-case? "ABcde" 1 4)))
(test-assert
  "string-every pred all match, start and end index"
  (string-every char-upper-case? "aBCDe" 1 4))
(test-error
  "string-tabulate bad proc integer"
  check-srfi-13-error
  (string-tabulate 123 10))
(test-error
  "string-tabulate bad proc string"
  check-srfi-13-error
  (string-tabulate "zzz" 10))
(test-assert
  "string-tabulate static fill-char"
  (string=?
    (string-tabulate (lambda (idx) #\!) 10)
    "!!!!!!!!!!"))
(test-assert
  "string-tabulate variable fill-char"
  (string=?
    (string-tabulate
      (lambda (idx) (integer->char (+ idx 32)))
      10)
    " !\"#$%&'()"))
(test-assert
  "string->list empty"
  (zero? (length (string->list ""))))
(test-assert
  "string->list nonempty"
  (= (length (string->list "foo")) 3))
(test-assert
  "string->list empty, start index"
  (zero? (length (string->list "foo" 3 3))))
(test-assert
  "string->list nonempty, start index"
  (= (length (string->list "foo" 1 3)) 2))
(test-assert
  "string->list nonempty, start index, BMP"
  (= (length (string->list "ÿĀ̀" 1 3)) 2))
(test-assert
  "reverse-list->string empty"
  (string-null? (reverse-list->string '())))
(test-assert
  "reverse-list->string nonempty"
  (string=?
    "foo"
    (reverse-list->string '(#\o #\o #\f))))
(test-assert
  "reverse-list->string nonempty, BMP"
  (string=?
    "\x0100;\x0101;\x0102;"
    (reverse-list->string '(#\x0102 #\x0101 #\x0100))))
(test-assert
  "string-join empty list, no delimiter, implicit infix, empty 1"
  (string=? "" (string-join '())))
(test-assert
  "string-join empty string, no delimiter, implicit infix, empty 2"
  (string=? "" (string-join '(""))))
(test-assert
  "string-join non-empty, no delimiter, implicit infix"
  (string=? "bla" (string-join '("bla"))))
(test-assert
  "string-join empty list, implicit infix, empty 1"
  (string=? "" (string-join '() "|delim|")))
(test-assert
  "string-join empty string, implicit infix, empty 2"
  (string=? "" (string-join '("") "|delim|")))
(test-assert
  "string-join non-empty, implicit infix"
  (string=? "bla" (string-join '("bla") "|delim|")))
(test-assert
  "string-join non-empty, implicit infix"
  (string=? "bla" (string-join '("bla") "|delim|")))
(test-assert
  "string-join two strings, implicit infix"
  (string=?
    "bla|delim|fasel"
    (string-join '("bla" "fasel") "|delim|")))
(test-assert
  "string-join empty, explicit infix"
  (string=?
    ""
    (string-join '("") "|delim|" 'infix)))
(test-assert
  "string-join empty list, explicit infix"
  (string=? "" (string-join '() "|delim|" 'infix)))
(test-assert
  "string-join non-empty, explicit infix"
  (string=?
    "bla"
    (string-join '("bla") "|delim|" 'infix)))
(test-assert
  "string-join two strings, explicit infix"
  (string=?
    "bla|delim|fasel"
    (string-join '("bla" "fasel") "|delim|" 'infix)))
(test-assert
  "string-join two strings, explicit infix, BMP"
  (string=?
    "Āā::Ăă"
    (string-join '("Āā" "Ăă") "::" 'infix)))
(test-error
  "string-join empty list, strict infix"
  check-srfi-13-error
  (string-join '() "|delim|" 'strict-infix))
(test-assert
  "string-join empty, strict infix"
  (string=?
    ""
    (string-join '("") "|delim|" 'strict-infix)))
(test-assert
  "string-join non-empty, strict infix"
  (string=?
    "foo"
    (string-join '("foo") "|delim|" 'strict-infix)))
(test-assert
  "string-join two strings, strict infix"
  (string=?
    "foo|delim|bar"
    (string-join
      '("foo" "bar")
      "|delim|"
      'strict-infix)))
(test-assert
  "string-join empty list, prefix"
  (string=? "" (string-join '() "|delim|" 'prefix)))
(test-assert
  "string-join empty, prefix"
  (string=?
    "|delim|"
    (string-join '("") "|delim|" 'prefix)))
(test-assert
  "string-join non-empty, prefix"
  (string=?
    "|delim|foo"
    (string-join '("foo") "|delim|" 'prefix)))
(test-assert
  "string-join two strings, prefix"
  (string=?
    "|delim|foo|delim|bar"
    (string-join '("foo" "bar") "|delim|" 'prefix)))
(test-assert
  "string-join empty list, suffix"
  (string=? "" (string-join '() "|delim|" 'suffix)))
(test-assert
  "string-join empty, suffix"
  (string=?
    "|delim|"
    (string-join '("") "|delim|" 'suffix)))
(test-assert
  "string-join non-empty, suffix"
  (string=?
    "foo|delim|"
    (string-join '("foo") "|delim|" 'suffix)))
(test-assert
  "string-join two strings, suffix"
  (string=?
    "foo|delim|bar|delim|"
    (string-join '("foo" "bar") "|delim|" 'suffix)))
(test-assert
  "string-copy empty string"
  (string=? "" (string-copy "")))
(test-assert
  "string-copy full string"
  (string=? "foo-bar" (string-copy "foo-bar")))
(test-assert
  "string-copy full string, BMP"
  (string=? "foo-Āā" (string-copy "foo-Āā")))
(test-assert
  "string-copy start index"
  (string=? "o-bar" (string-copy "foo-bar" 2)))
(test-assert
  "string-copy start index"
  (string=? "o-bar" (string-copy "Āāo-bar" 2)))
(test-assert
  "string-copy start and end index"
  (string=? "o-ba" (string-copy "foo-bar" 2 6)))
(test-assert
  "substring/shared empty string"
  (let ((s "")) (eq? s (substring/shared s 0))))
(test-assert
  "substring/shared non-empty string, not eq?"
  (string=? "foo" (substring/shared "foo-bar" 0 3)))
(test-assert
  "substring/shared shared copy of non-empty string is eq?"
  (let ((s "foo-bar"))
    (eq? s (substring/shared s 0 7))))
(test-assert
  "string-copy! non-empty string"
  (string=?
    "welld, oh yeah!"
    (let* ((s "hello") (t (string-copy "world, oh yeah!")))
      (string-copy! t 1 s 1 3)
      t)))
(test-assert
  "string-take empty string"
  (string=? "" (string-take "foo bar braz" 0)))
(test-assert
  "string-take non-empty string"
  (string=? "foo " (string-take "foo bar braz" 4)))
(test-assert
  "string-take non-empty string BMP"
  (string=? "Āoo " (string-take "Āoo āar braz" 4)))
(test-assert
  "string-take full string"
  (string=?
    "foo bar braz"
    (string-take "foo bar braz" 12)))
(test-assert
  "string-take-right empty string"
  (string=?
    ""
    (string-take-right "foo bar braz" 0)))
(test-assert
  "string-take-right non-empty string"
  (string=?
    "braz"
    (string-take-right "foo bar braz" 4)))
(test-assert
  "string-take-right non-empty string"
  (string=?
    "braz"
    (string-take-right "foo baĀ braz" 4)))
(test-assert
  "string-take-right full string"
  (string=?
    "foo bar braz"
    (string-take-right "foo bar braz" 12)))
(test-assert
  "string-drop empty string"
  (string=? "" (string-drop "foo bar braz" 12)))
(test-assert
  "string-drop non-empty string"
  (string=? "braz" (string-drop "foo bar braz" 8)))
(test-assert
  "string-drop non-empty string BMP"
  (string=? "braz" (string-drop "foo ĀāĂ braz" 8)))
(test-assert
  "string-drop full string"
  (string=?
    "foo bar braz"
    (string-drop "foo bar braz" 0)))
(test-assert
  "string-drop-right empty string"
  (string=?
    ""
    (string-drop-right "foo bar braz" 12)))
(test-assert
  "string-drop-right non-empty string"
  (string=?
    "foo "
    (string-drop-right "foo bar braz" 8)))
(test-assert
  "string-drop-right non-empty string BMP"
  (string=?
    "foo "
    (string-drop-right "foo ĀāĂ braz" 8)))
(test-assert
  "string-drop-right full string"
  (string=?
    "foo bar braz"
    (string-drop-right "foo bar braz" 0)))
(test-assert
  "string-pad empty string, zero pad"
  (string=? "" (string-pad "" 0)))
(test-assert
  "string-pad empty string, zero pad, pad char"
  (string=? "" (string-pad "" 0)))
(test-assert
  "string-pad empty pad string, 2 pad "
  (string=? "  " (string-pad "" 2)))
(test-assert
  "string-pad empty pad string, 2 pad, pad char"
  (string=? "!!" (string-pad "" 2 #\!)))
(test-assert
  "string-pad empty pad string, 2 pad, pad char, start index"
  (string=? "!c" (string-pad "abc" 2 #\! 2)))
(test-assert
  "string-pad empty pad string, 2 pad, pad char, start and end index"
  (string=? "!c" (string-pad "abcd" 2 #\! 2 3)))
(test-assert
  "string-pad freestyle 1"
  (string=?
    "32"
    (string-pad (number->string 532) 2 #\!)))
(test-assert
  "string-pad freestyle 2"
  (string=?
    "!532"
    (string-pad (number->string 532) 4 #\!)))
(test-assert
  "string-pad-right empty string, zero pad"
  (string=? "" (string-pad-right "" 0)))
(test-assert
  "string-pad-right empty string, zero pad, pad char"
  (string=? "" (string-pad-right "" 0)))
(test-assert
  "string-pad-right empty pad string, 2 pad "
  (string=? "  " (string-pad-right "" 2)))
(test-assert
  "string-pad-right empty pad string, 2 pad, pad char"
  (string=? "!!" (string-pad-right "" 2 #\!)))
(test-assert
  "string-pad-right empty pad string, 2 pad, pad char, start index"
  (string=? "c!" (string-pad-right "abc" 2 #\! 2)))
(test-assert
  "string-pad-right empty pad string, 2 pad, pad char, start and end index"
  (string=?
    "c!"
    (string-pad-right "abcd" 2 #\! 2 3)))
(test-assert
  "string-pad-right freestyle 1"
  (string=?
    "53"
    (string-pad-right (number->string 532) 2 #\!)))
(test-assert
  "string-pad-right freestyle 2"
  (string=?
    "532!"
    (string-pad-right (number->string 532) 4 #\!)))
(test-error
  "string-trim bad char_pred integer"
  check-srfi-13-error
  (string-trim "abcde" 123))
(test-error
  "string-trim bad char_pred string"
  check-srfi-13-error
  (string-trim "abcde" "zzz"))
(test-assert
  "string-trim empty string"
  (string=? "" (string-trim "")))
(test-assert
  "string-trim no char/pred"
  (string=? "foo " (string-trim " 	foo ")))
(test-assert
  "string-trim start index, pred"
  (string=?
    "foo "
    (string-trim " 	foo " char-whitespace? 1)))
(test-assert
  "string-trim start and end index, pred"
  (string=?
    "f"
    (string-trim " 	foo " char-whitespace? 1 3)))
(test-assert
  "string-trim start index, char"
  (string=?
    "	foo "
    (string-trim " 	foo " #\space 1)))
(test-assert
  "string-trim start and end index, char"
  (string=?
    "	f"
    (string-trim " 	foo " #\space 1 3)))
(test-assert
  "string-trim start index, charset"
  (string=?
    "foo "
    (string-trim " 	foo " char-set:whitespace 1)))
(test-assert
  "string-trim start and end index, charset"
  (string=?
    "f"
    (string-trim " 	foo " char-set:whitespace 1 3)))
(test-error
  "string-trim-right bad char_pred integer"
  check-srfi-13-error
  (string-trim-right "abcde" 123))
(test-error
  "string-trim-right bad char_pred string"
  check-srfi-13-error
  (string-trim-right "abcde" "zzz"))
(test-assert
  "string-trim-right empty string"
  (string=? "" (string-trim-right "")))
(test-assert
  "string-trim-right no char/pred"
  (string=? " 	foo" (string-trim-right " 	foo ")))
(test-assert
  "string-trim-right start index, pred"
  (string=?
    "\tfoo"
    (string-trim-right " \tfoo " char-whitespace? 1)))
(test-assert
  "string-trim-right start and end index, pred"
  (string=?
    "\tf"
    (string-trim-right " \tfoo " char-whitespace? 1 3)))
(test-assert
  "string-trim-right start index, char"
  (string=?
    "	foo"
    (string-trim-right " 	foo " #\space 1)))
(test-assert
  "string-trim-right start and end index, char"
  (string=?
    "	f"
    (string-trim-right " 	foo " #\space 1 3)))
(test-assert
  "string-trim-right start index, charset"
  (string=?
    "	foo"
    (string-trim-right
      " 	foo "
      char-set:whitespace
      1)))
(test-assert
  "string-trim-right start and end index, charset"
  (string=?
    "	f"
    (string-trim-right
      " 	foo "
      char-set:whitespace
      1
      3)))
(test-error
  "string-trim-both bad char_pred integer"
  check-srfi-13-error
  (string-trim-both "abcde" 123))
(test-error
  "string-trim-both bad char_pred string"
  check-srfi-13-error
  (string-trim-both "abcde" "zzz"))
(test-assert
  "string-trim-both empty string"
  (string=? "" (string-trim-both "")))
(test-assert
  "string-trim-both no char/pred"
  (string=? "foo" (string-trim-both " 	foo ")))
(test-assert
  "string-trim-both start index, pred"
  (string=?
    "foo"
    (string-trim-both " 	foo " char-whitespace? 1)))
(test-assert
  "string-trim-both start and end index, pred"
  (string=?
    "f"
    (string-trim-both " 	foo " char-whitespace? 1 3)))
(test-assert
  "string-trim-both start index, char"
  (string=?
    "	foo"
    (string-trim-both " 	foo " #\space 1)))
(test-assert
  "string-trim-both start and end index, char"
  (string=?
    "	f"
    (string-trim-both " 	foo " #\space 1 3)))
(test-assert
  "string-trim-both start index, charset"
  (string=?
    "foo"
    (string-trim-both " 	foo " char-set:whitespace 1)))
(test-assert
  "string-trim-both start and end index, charset"
  (string=?
    "f"
    (string-trim-both
      " 	foo "
      char-set:whitespace
      1
      3)))
(define s0 (make-string 200 #\!))
(define s1 (make-string 0 #\!))

(string-fill! s1 #\*)
(test-assert
  "string-fill! empty string, no indices"
  (= (string-length s1) 0))

(string-fill! s1 #\* 0)
(test-assert
  "string-fill! empty string, start index"
  (= (string-length s1) 0))

(string-fill! s1 #\* 0 0)
(test-assert
  "string-fill! empty string, start and end index"
  (= (string-length s1) 0))

(string-fill! s0 #\*)
(test-assert
  "string-fill! no indices"
  (char=? (string-ref s0 0) #\*))

(string-fill! s0 #\+ 10)
(test-assert
  "string-fill! start index"
  (char=? (string-ref s0 11) #\+))

(string-fill! s0 #\| 12 20)
(test-assert
  "string-fill! start and end index"
  (char=? (string-ref s0 13) #\|))

(test-assert
  "string-prefix-length empty prefix"
  (= 0 (string-prefix-length "" "foo bar")))
(test-assert
  "string-prefix-length non-empty prefix - match"
  (= 3 (string-prefix-length "foo" "foo bar")))
(test-assert
  "string-prefix-length non-empty prefix - no match"
  (= 0 (string-prefix-length "bar" "foo bar")))
(test-assert
  "string-prefix-length-ci empty prefix"
  (= 0 (string-prefix-length-ci "" "foo bar")))
(test-assert
  "string-prefix-length-ci non-empty prefix - match"
  (= 3 (string-prefix-length-ci "fOo" "foo bar")))
(test-assert
  "string-prefix-length-ci non-empty prefix - no match"
  (= 0 (string-prefix-length-ci "bAr" "foo bar")))
(test-assert
  "string-suffix-length empty suffix"
  (= 0 (string-suffix-length "" "foo bar")))
(test-assert
  "string-suffix-length non-empty suffix - match"
  (= 3 (string-suffix-length "bar" "foo bar")))
(test-assert
  "string-suffix-length non-empty suffix - no match"
  (= 0 (string-suffix-length "foo" "foo bar")))
(test-assert
  "string-suffix-length-ci empty suffix"
  (= 0 (string-suffix-length-ci "" "foo bar")))
(test-assert
  "string-suffix-length-ci non-empty suffix - match"
  (= 3 (string-suffix-length-ci "bAr" "foo bar")))
(test-assert
  "string-suffix-length-ci non-empty suffix - no match"
  (= 0 (string-suffix-length-ci "fOo" "foo bar")))
(test-assert
  "string-prefix? empty prefix"
  (string-prefix? "" "foo bar"))
(test-assert
  "string-prefix? non-empty prefix - match"
  (string-prefix? "foo" "foo bar"))
(test-assert
  "string-prefix? non-empty prefix - no match"
  (not (string-prefix? "bar" "foo bar")))
(test-assert
  "string-prefix-ci? empty prefix"
  (string-prefix-ci? "" "foo bar"))
(test-assert
  "string-prefix-ci? non-empty prefix - match"
  (string-prefix-ci? "fOo" "foo bar"))
(test-assert
  "string-prefix-ci? non-empty prefix - no match"
  (not (string-prefix-ci? "bAr" "foo bar")))
(test-assert
  "string-suffix? empty suffix"
  (string-suffix? "" "foo bar"))
(test-assert
  "string-suffix? non-empty suffix - match"
  (string-suffix? "bar" "foo bar"))
(test-assert
  "string-suffix? non-empty suffix - no match"
  (not (string-suffix? "foo" "foo bar")))
(test-assert
  "string-suffix-ci? empty suffix"
  (string-suffix-ci? "" "foo bar"))
(test-assert
  "string-suffix-ci? non-empty suffix - match"
  (string-suffix-ci? "bAr" "foo bar"))
(test-assert
  "string-suffix-ci? non-empty suffix - no match"
  (not (string-suffix-ci? "fOo" "foo bar")))
(test-error
  "string-index bad char_pred integer"
  check-srfi-13-error
  (string-index "abcde" 123))
(test-error
  "string-index bad char_pred string"
  check-srfi-13-error
  (string-index "abcde" "zzz"))
(test-assert
  "string-index empty string - char"
  (not (string-index "" #\a)))
(test-assert
  "string-index non-empty - char - match"
  (= 5 (string-index "foo bar" #\a)))
(test-assert
  "string-index non-empty - char - no match"
  (not (string-index "frobnicate" #\x)))
(test-assert
  "string-index empty string - char - start index"
  (not (string-index "" #\a 0)))
(test-assert
  "string-index non-empty - char - match - start index"
  (= 5 (string-index "foo bar" #\a 1)))
(test-assert
  "string-index non-empty - char - no match - start index"
  (not (string-index "frobnicate" #\x 2)))
(test-assert
  "string-index empty string - char - start and end index"
  (not (string-index "" #\a 0 0)))
(test-assert
  "string-index non-empty - char - match - start and end index"
  (= 5 (string-index "foo bar" #\a 1 6)))
(test-assert
  "string-index non-empty - char - no match - start and end index"
  (not (string-index "frobnicate" #\a 2 5)))
(test-assert
  "string-index empty string - charset"
  (not (string-index "" char-set:letter)))
(test-assert
  "string-index non-empty - charset - match"
  (= 0 (string-index "foo bar" char-set:letter)))
(test-assert
  "string-index non-empty - charset - no match"
  (not (string-index "frobnicate" char-set:digit)))
(test-assert
  "string-index empty string - charset - start index"
  (not (string-index "" char-set:letter 0)))
(test-assert
  "string-index non-empty - charset - match - start index"
  (= 1 (string-index "foo bar" char-set:letter 1)))
(test-assert
  "string-index non-empty - charset - no match - start index"
  (not (string-index "frobnicate" char-set:digit 2)))
(test-assert
  "string-index empty string - charset - start and end index"
  (not (string-index "" char-set:letter 0 0)))
(test-assert
  "string-index non-empty - charset - match - start and end index"
  (= 1
     (string-index "foo bar" char-set:letter 1 6)))
(test-assert
  "string-index non-empty - charset - no match - start and end index"
  (not (string-index "frobnicate" char-set:digit 2 5)))
(test-assert
  "string-index empty string - pred"
  (not (string-index "" char-alphabetic?)))
(test-assert
  "string-index non-empty - pred - match"
  (= 0 (string-index "foo bar" char-alphabetic?)))
(test-assert
  "string-index non-empty - pred - no match"
  (not (string-index "frobnicate" char-numeric?)))
(test-assert
  "string-index empty string - pred - start index"
  (not (string-index "" char-alphabetic? 0)))
(test-assert
  "string-index non-empty - pred - match - start index"
  (= 1 (string-index "foo bar" char-alphabetic? 1)))
(test-assert
  "string-index non-empty - pred - no match - start index"
  (not (string-index "frobnicate" char-numeric? 2)))
(test-assert
  "string-index empty string - pred - start and end index"
  (not (string-index "" char-alphabetic? 0 0)))
(test-assert
  "string-index non-empty - pred - match - start and end index"
  (= 1
     (string-index "foo bar" char-alphabetic? 1 6)))
(test-assert
  "string-index non-empty - pred - no match - start and end index"
  (not (string-index "frobnicate" char-numeric? 2 5)))
(test-assert
  "string-index 8-bit char in string"
  (begin
    (string-index
      (string (integer->char 200))
      char-numeric?)
    #t))
(test-error
  "string-index-right bad char_pred integer"
  check-srfi-13-error
  (string-index-right "abcde" 123))
(test-error
  "string-index-right bad char_pred string"
  check-srfi-13-error
  (string-index-right "abcde" "zzz"))
(test-assert
  "string-index-right empty string - char"
  (not (string-index-right "" #\a)))
(test-assert
  "string-index-right non-empty - char - match"
  (= 5 (string-index-right "foo bar" #\a)))
(test-assert
  "string-index-right non-empty - char - no match"
  (not (string-index-right "frobnicate" #\x)))
(test-assert
  "string-index-right empty string - char - start index-right"
  (not (string-index-right "" #\a 0)))
(test-assert
  "string-index-right non-empty - char - match - start index"
  (= 5 (string-index-right "foo bar" #\a 1)))
(test-assert
  "string-index-right non-empty - char - no match - start index"
  (not (string-index-right "frobnicate" #\x 2)))
(test-assert
  "string-index-right empty string - char - start and end index"
  (not (string-index-right "" #\a 0 0)))
(test-assert
  "string-index-right non-empty - char - match - start and end index"
  (= 5 (string-index-right "foo bar" #\a 1 6)))
(test-assert
  "string-index-right non-empty - char - no match - start and end index"
  (not (string-index-right "frobnicate" #\a 2 5)))
(test-assert
  "string-index-right empty string - charset"
  (not (string-index-right "" char-set:letter)))
(test-assert
  "string-index-right non-empty - charset - match"
  (= 6
     (string-index-right "foo bar" char-set:letter)))
(test-assert
  "string-index-right non-empty - charset - no match"
  (not (string-index-right "frobnicate" char-set:digit)))
(test-assert
  "string-index-right empty string - charset - start index"
  (not (string-index-right "" char-set:letter 0)))
(test-assert
  "string-index-right non-empty - charset - match - start index"
  (= 6
     (string-index-right "foo bar" char-set:letter 1)))
(test-assert
  "string-index-right non-empty - charset - no match - start index"
  (not (string-index-right
         "frobnicate"
         char-set:digit
         2)))
(test-assert
  "string-index-right empty string - charset - start and end index"
  (not (string-index-right "" char-set:letter 0 0)))
(test-assert
  "string-index-right non-empty - charset - match - start and end index"
  (= 5
     (string-index-right
       "foo bar"
       char-set:letter
       1
       6)))
(test-assert
  "string-index-right non-empty - charset - no match - start and end index"
  (not (string-index-right
         "frobnicate"
         char-set:digit
         2
         5)))
(test-assert
  "string-index-right empty string - pred"
  (not (string-index-right "" char-alphabetic?)))
(test-assert
  "string-index-right non-empty - pred - match"
  (= 6
     (string-index-right "foo bar" char-alphabetic?)))
(test-assert
  "string-index-right non-empty - pred - no match"
  (not (string-index-right "frobnicate" char-numeric?)))
(test-assert
  "string-index-right empty string - pred - start index"
  (not (string-index-right "" char-alphabetic? 0)))
(test-assert
  "string-index-right non-empty - pred - match - start index"
  (= 6
     (string-index-right "foo bar" char-alphabetic? 1)))
(test-assert
  "string-index-right non-empty - pred - no match - start index"
  (not (string-index-right "frobnicate" char-numeric? 2)))
(test-assert
  "string-index-right empty string - pred - start and end index"
  (not (string-index-right "" char-alphabetic? 0 0)))
(test-assert
  "string-index-right non-empty - pred - match - start and end index"
  (= 5
     (string-index-right
       "foo bar"
       char-alphabetic?
       1
       6)))
(test-assert
  "string-index-right non-empty - pred - no match - start and end index"
  (not (string-index-right
         "frobnicate"
         char-numeric?
         2
         5)))
(test-error
  "string-skip bad char_pred integer"
  check-srfi-13-error
  (string-skip "abcde" 123))
(test-error
  "string-skip bad char_pred string"
  check-srfi-13-error
  (string-skip "abcde" "zzz"))
(test-assert
  "string-skip empty string - char"
  (not (string-skip "" #\a)))
(test-assert
  "string-skip non-empty - char - match"
  (= 0 (string-skip "foo bar" #\a)))
(test-assert
  "string-skip non-empty - char - no match"
  (= 0 (string-skip "frobnicate" #\x)))
(test-assert
  "string-skip empty string - char - start index"
  (not (string-skip "" #\a 0)))
(test-assert
  "string-skip non-empty - char - match - start index"
  (= 1 (string-skip "foo bar" #\a 1)))
(test-assert
  "string-skip non-empty - char - no match - start index"
  (= 2 (string-skip "frobnicate" #\x 2)))
(test-assert
  "string-skip empty string - char - start and end index"
  (not (string-skip "" #\a 0 0)))
(test-assert
  "string-skip non-empty - char - match - start and end index"
  (= 1 (string-skip "foo bar" #\a 1 6)))
(test-assert
  "string-skip non-empty - char - no match - start and end index"
  (= 2 (string-skip "frobnicate" #\a 2 5)))
(test-assert
  "string-skip empty string - charset"
  (not (string-skip "" char-set:letter)))
(test-assert
  "string-skip non-empty - charset - match"
  (= 3 (string-skip "foo bar" char-set:letter)))
(test-assert
  "string-skip non-empty - charset - no match"
  (= 0 (string-skip "frobnicate" char-set:digit)))
(test-assert
  "string-skip empty string - charset - start index"
  (not (string-skip "" char-set:letter 0)))
(test-assert
  "string-skip non-empty - charset - match - start index"
  (= 3 (string-skip "foo bar" char-set:letter 1)))
(test-assert
  "string-skip non-empty - charset - no match - start index"
  (= 2 (string-skip "frobnicate" char-set:digit 2)))
(test-assert
  "string-skip empty string - charset - start and end index"
  (not (string-skip "" char-set:letter 0 0)))
(test-assert
  "string-skip non-empty - charset - match - start and end index"
  (= 3 (string-skip "foo bar" char-set:letter 1 6)))
(test-assert
  "string-skip non-empty - charset - no match - start and end index"
  (= 2
     (string-skip "frobnicate" char-set:digit 2 5)))
(test-assert
  "string-skip empty string - pred"
  (not (string-skip "" char-alphabetic?)))
(test-assert
  "string-skip non-empty - pred - match"
  (= 3 (string-skip "foo bar" char-alphabetic?)))
(test-assert
  "string-skip non-empty - pred - no match"
  (= 0 (string-skip "frobnicate" char-numeric?)))
(test-assert
  "string-skip empty string - pred - start index"
  (not (string-skip "" char-alphabetic? 0)))
(test-assert
  "string-skip non-empty - pred - match - start index"
  (= 3 (string-skip "foo bar" char-alphabetic? 1)))
(test-assert
  "string-skip non-empty - pred - no match - start index"
  (= 2 (string-skip "frobnicate" char-numeric? 2)))
(test-assert
  "string-skip empty string - pred - start and end index"
  (not (string-skip "" char-alphabetic? 0 0)))
(test-assert
  "string-skip non-empty - pred - match - start and end index"
  (= 3
     (string-skip "foo bar" char-alphabetic? 1 6)))
(test-assert
  "string-skip non-empty - pred - no match - start and end index"
  (= 2
     (string-skip "frobnicate" char-numeric? 2 5)))
(test-error
  "string-skip-right bad char_pred integer"
  check-srfi-13-error
  (string-skip-right "abcde" 123))
(test-error
  "string-skip-right bad char_pred string"
  check-srfi-13-error
  (string-skip-right "abcde" "zzz"))
(test-assert
  "string-skip-right empty string - char"
  (not (string-skip-right "" #\a)))
(test-assert
  "string-skip-right non-empty - char - match"
  (= 6 (string-skip-right "foo bar" #\a)))
(test-assert
  "string-skip-right non-empty - char - no match"
  (= 9 (string-skip-right "frobnicate" #\x)))
(test-assert
  "string-skip-right empty string - char - start index-right"
  (not (string-skip-right "" #\a 0)))
(test-assert
  "string-skip-right non-empty - char - match - start index"
  (= 6 (string-skip-right "foo bar" #\a 1)))
(test-assert
  "string-skip-right non-empty - char - no match - start index"
  (= 9 (string-skip-right "frobnicate" #\x 2)))
(test-assert
  "string-skip-right empty string - char - start and end index"
  (not (string-skip-right "" #\a 0 0)))
(test-assert
  "string-skip-right non-empty - char - match - start and end index"
  (= 4 (string-skip-right "foo bar" #\a 1 6)))
(test-assert
  "string-skip-right non-empty - char - no match - start and end index"
  (= 4 (string-skip-right "frobnicate" #\a 2 5)))
(test-assert
  "string-skip-right empty string - charset"
  (not (string-skip-right "" char-set:letter)))
(test-assert
  "string-skip-right non-empty - charset - match"
  (= 3
     (string-skip-right "foo bar" char-set:letter)))
(test-assert
  "string-skip-right non-empty - charset - no match"
  (= 9
     (string-skip-right "frobnicate" char-set:digit)))
(test-assert
  "string-skip-right empty string - charset - start index"
  (not (string-skip-right "" char-set:letter 0)))
(test-assert
  "string-skip-right non-empty - charset - match - start index"
  (= 3
     (string-skip-right "foo bar" char-set:letter 1)))
(test-assert
  "string-skip-right non-empty - charset - no match - start index"
  (= 9
     (string-skip-right "frobnicate" char-set:digit 2)))
(test-assert
  "string-skip-right empty string - charset - start and end index"
  (not (string-skip-right "" char-set:letter 0 0)))
(test-assert
  "string-skip-right non-empty - charset - match - start and end index"
  (= 3
     (string-skip-right "foo bar" char-set:letter 1 6)))
(test-assert
  "string-skip-right non-empty - charset - no match - start and end index"
  (= 4
     (string-skip-right
       "frobnicate"
       char-set:digit
       2
       5)))
(test-assert
  "string-skip-right empty string - pred"
  (not (string-skip-right "" char-alphabetic?)))
(test-assert
  "string-skip-right non-empty - pred - match"
  (= 3
     (string-skip-right "foo bar" char-alphabetic?)))
(test-assert
  "string-skip-right non-empty - pred - no match"
  (= 9
     (string-skip-right "frobnicate" char-numeric?)))
(test-assert
  "string-skip-right empty string - pred - start index"
  (not (string-skip-right "" char-alphabetic? 0)))
(test-assert
  "string-skip-right non-empty - pred - match - start index"
  (= 3
     (string-skip-right "foo bar" char-alphabetic? 1)))
(test-assert
  "string-skip-right non-empty - pred - no match - start index"
  (= 9
     (string-skip-right "frobnicate" char-numeric? 2)))
(test-assert
  "string-skip-right empty string - pred - start and end index"
  (not (string-skip-right "" char-alphabetic? 0 0)))
(test-assert
  "string-skip-right non-empty - pred - match - start and end index"
  (= 3
     (string-skip-right
       "foo bar"
       char-alphabetic?
       1
       6)))
(test-assert
  "string-skip-right non-empty - pred - no match - start and end index"
  (= 4
     (string-skip-right
       "frobnicate"
       char-numeric?
       2
       5)))
(test-error
  "string-count bad char_pred integer"
  check-srfi-13-error
  (string-count "abcde" 123))
(test-error
  "string-count bad char_pred string"
  check-srfi-13-error
  (string-count "abcde" "zzz"))
(test-assert
  "string-count char"
  (eqv? 0 (string-count "" #\a)))
(test-assert
  "string-count char"
  (eqv? 0 (string-count "-" #\a)))
(test-assert
  "string-count char"
  (eqv? 1 (string-count "a" #\a)))
(test-assert
  "string-count char"
  (eqv? 0 (string-count "--" #\a)))
(test-assert
  "string-count char"
  (eqv? 1 (string-count "a-" #\a)))
(test-assert
  "string-count char"
  (eqv? 1 (string-count "-a" #\a)))
(test-assert
  "string-count char"
  (eqv? 2 (string-count "aa" #\a)))
(test-assert
  "string-count char"
  (eqv? 0 (string-count "---" #\a)))
(test-assert
  "string-count char"
  (eqv? 1 (string-count "-a-" #\a)))
(test-assert
  "string-count char"
  (eqv? 1 (string-count "a--" #\a)))
(test-assert
  "string-count char"
  (eqv? 2 (string-count "aa-" #\a)))
(test-assert
  "string-count char"
  (eqv? 2 (string-count "a-a" #\a)))
(test-assert
  "string-count char"
  (eqv? 3 (string-count "aaa" #\a)))
(test-assert
  "string-count char"
  (eqv? 1 (string-count "--a" #\a)))
(test-assert
  "string-count char"
  (eqv? 2 (string-count "-aa" #\a)))
(test-assert
  "string-count charset"
  (eqv? 0 (string-count "" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 0 (string-count "-" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 1 (string-count "a" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 0 (string-count "--" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 1 (string-count "a-" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 1 (string-count "-a" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 2 (string-count "aa" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 0 (string-count "---" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 1 (string-count "-a-" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 1 (string-count "a--" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 2 (string-count "aa-" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 2 (string-count "a-a" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 3 (string-count "aaa" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 1 (string-count "--a" char-set:letter)))
(test-assert
  "string-count charset"
  (eqv? 2 (string-count "-aa" char-set:letter)))
(test-assert
  "string-count proc"
  (eqv? 0 (string-count "" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 0 (string-count "-" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 1 (string-count "a" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 0 (string-count "--" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 1 (string-count "a-" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 1 (string-count "-a" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 2 (string-count "aa" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 0 (string-count "---" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 1 (string-count "-a-" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 1 (string-count "a--" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 2 (string-count "aa-" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 2 (string-count "a-a" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 3 (string-count "aaa" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 1 (string-count "--a" char-alphabetic?)))
(test-assert
  "string-count proc"
  (eqv? 2 (string-count "-aa" char-alphabetic?)))


(test-assert
  "string-replace empty string(s), 2 indices"
  (string=? "" (string-replace "" "" 0 0)))
(test-assert
  "string-replace empty string(s), 3 indices"
  (string=? "" (string-replace "" "" 0 0 0)))
(test-assert
  "string-replace empty string(s), 4 indices"
  (string=? "" (string-replace "" "" 0 0 0 0)))

(test-assert
  "string-replace two indices"
  (string=?
    "fuuar"
    (string-replace "foo bar" "uu" 1 5)))
(test-assert
  "string-replace three indices"
  (string=?
    "fuar"
    (string-replace "foo bar" "uu" 1 5 1)))
(test-assert
  "string-replace four indices"
  (string=?
    "fuar"
    (string-replace "foo bar" "uu" 1 5 1 2)))
(test-assert
  "string-tokenize empty string, no char/pred"
  (zero? (length (string-tokenize ""))))
(test-assert
  "string-tokenize empty string, charset"
  (zero? (length
           (string-tokenize "" char-set:punctuation))))
(test-assert
  "string-tokenize no char/pred"
  (equal?
    '("foo" "bar" "!a")
    (string-tokenize "foo	bar !a")))
(test-assert
  "string-tokenize charset"
  (equal?
    '("foo" "bar" "!a")
    (string-tokenize "foo	bar !a" char-set:graphic)))
(test-assert
  "string-tokenize charset, start index"
  (equal?
    '("oo" "bar" "!a")
    (string-tokenize "foo	bar !a" char-set:graphic 1)))
(test-assert
  "string-tokenize charset, start and end index"
  (equal?
    '("oo" "bar" "!")
    (string-tokenize
      "foo	bar !a"
      char-set:graphic
      1
      9)))
(test-error
  "string-filter bad char_pred integer"
  check-srfi-13-error
  (string-filter 123 "abcde"))
(test-assert
  "string-filter empty string, char"
  (string=? "" (string-filter #\. "")))
(test-assert
  "string-filter empty string, charset"
  (string=?
    ""
    (string-filter char-set:punctuation "")))
(test-assert
  "string-filter empty string, pred"
  (string=? "" (string-filter char-alphabetic? "")))
(test-assert
  "string-filter char"
  (string=? "..." (string-filter #\. ".foo.bar.")))
(test-assert
  "string-filter charset"
  (string=?
    "..."
    (string-filter char-set:punctuation ".foo.bar.")))
(test-assert
  "string-filter pred"
  (string=?
    "foobar"
    (string-filter char-alphabetic? ".foo.bar.")))
(test-assert
  "string-filter char, start index"
  (string=? ".." (string-filter #\. ".foo.bar." 2)))
(test-assert
  "string-filter charset, start index"
  (string=?
    ".."
    (string-filter
      char-set:punctuation
      ".foo.bar."
      2)))
(test-assert
  "string-filter pred, start index"
  (string=?
    "oobar"
    (string-filter char-alphabetic? ".foo.bar." 2)))
(test-assert
  "string-filter char, start and end index"
  (string=? "" (string-filter #\. ".foo.bar." 2 4)))
(test-assert
  "string-filter charset, start and end index"
  (string=?
    ""
    (string-filter
      char-set:punctuation
      ".foo.bar."
      2
      4)))
(test-assert
  "string-filter pred, start and end index"
  (string=?
    "oo"
    (string-filter char-alphabetic? ".foo.bar." 2 4)))
(test-assert
  "string-filter char"
  (equal? "x" (string-filter #\x "x")))
(test-assert
  "string-filter char"
  (equal? "xx" (string-filter #\x "xx")))
(test-assert
  "string-filter char"
  (equal? "xx" (string-filter #\x "xyx")))
(test-assert
  "string-filter char"
  (equal? "x" (string-filter #\x "xyyy")))
(test-assert
  "string-filter char"
  (equal? "x" (string-filter #\x "yyyx")))
(test-assert
  "string-filter char"
  (equal? "xx" (string-filter #\x "xxx" 1)))
(test-assert
  "string-filter char"
  (equal? "xx" (string-filter #\x "xxx" 0 2)))
(test-assert
  "string-filter char"
  (equal? "x" (string-filter #\x "xyx" 1)))
(test-assert
  "string-filter char"
  (equal? "x" (string-filter #\x "yxx" 0 2)))
(test-assert
  "string-filter char"
  (string=? "" (string-filter #\x ".")))
(test-assert
  "string-filter char"
  (string=? "" (string-filter #\x "..")))
(test-assert
  "string-filter char"
  (string=? "" (string-filter #\x "...")))
(test-assert
  "string-filter char"
  (string=? "x" (string-filter #\x ".x")))
(test-assert
  "string-filter char"
  (string=? "x" (string-filter #\x "..x")))
(test-assert
  "string-filter char"
  (string=? "x" (string-filter #\x "...x")))
(test-assert
  "string-filter char"
  (string=? "x" (string-filter #\x "x.")))
(test-assert
  "string-filter char"
  (string=? "x" (string-filter #\x "x..")))
(test-assert
  "string-filter char"
  (string=? "x" (string-filter #\x "x...")))
(test-assert
  "string-filter char"
  (string=? "x" (string-filter #\x "...x...")))
(let ((charset (char-set #\x #\y)))
  (test-assert
    (equal? "x" (string-filter charset "x")))
  (test-assert
    (equal? "xx" (string-filter charset "xx")))
  (test-assert
    (equal? "xy" (string-filter charset "xy")))
  (test-assert
    (equal? "x" (string-filter charset "xaaa")))
  (test-assert
    (equal? "y" (string-filter charset "aaay")))
  (test-assert
    (equal? "yx" (string-filter charset "xyx" 1)))
  (test-assert
    (equal? "xy" (string-filter charset "xyx" 0 2)))
  (test-assert
    (equal? "x" (string-filter charset "xax" 1)))
  (test-assert
    (equal? "x" (string-filter charset "axx" 0 2))))
(test-assert
  "string-filter charset"
  (string=? "" (string-filter char-set:letter ".")))
(test-assert
  "string-filter charset"
  (string=?
    ""
    (string-filter char-set:letter "..")))
(test-assert
  "string-filter charset"
  (string=?
    ""
    (string-filter char-set:letter "...")))
(test-assert
  "string-filter charset"
  (string=?
    "x"
    (string-filter char-set:letter ".x")))
(test-assert
  "string-filter charset"
  (string=?
    "x"
    (string-filter char-set:letter "..x")))
(test-assert
  "string-filter charset"
  (string=?
    "x"
    (string-filter char-set:letter "...x")))
(test-assert
  "string-filter charset"
  (string=?
    "x"
    (string-filter char-set:letter "x.")))
(test-assert
  "string-filter charset"
  (string=?
    "x"
    (string-filter char-set:letter "x..")))
(test-assert
  "string-filter charset"
  (string=?
    "x"
    (string-filter char-set:letter "x...")))
(test-assert
  "string-filter charset"
  (string=?
    "x"
    (string-filter char-set:letter "...x...")))
(test-error
  "string-delete bad char_pred integer"
  check-srfi-13-error
  (string-delete 123 "abcde"))
(test-assert
  "string-delete empty string, char"
  (string=? "" (string-delete #\. "")))
(test-assert
  "string-delete empty string, charset"
  (string=?
    ""
    (string-delete char-set:punctuation "")))
(test-assert
  "string-delete empty string, pred"
  (string=? "" (string-delete char-alphabetic? "")))
(test-assert
  "string-delete char"
  (string=?
    "foobar"
    (string-delete #\. ".foo.bar.")))
(test-assert
  "string-delete charset"
  (string=?
    "foobar"
    (string-delete char-set:punctuation ".foo.bar.")))
(test-assert
  "string-delete pred"
  (string=?
    "..."
    (string-delete char-alphabetic? ".foo.bar.")))
(test-assert
  "string-delete char, start index"
  (string=?
    "oobar"
    (string-delete #\. ".foo.bar." 2)))
(test-assert
  "string-delete charset, start index"
  (string=?
    "oobar"
    (string-delete
      char-set:punctuation
      ".foo.bar."
      2)))
(test-assert
  "string-delete pred, start index"
  (string=?
    ".."
    (string-delete char-alphabetic? ".foo.bar." 2)))
(test-assert
  "string-delete char, start and end index"
  (string=?
    "oo"
    (string-delete #\. ".foo.bar." 2 4)))
(test-assert
  "string-delete charset, start and end index"
  (string=?
    "oo"
    (string-delete
      char-set:punctuation
      ".foo.bar."
      2
      4)))
(test-assert
  "string-delete pred, start and end index"
  (string=?
    ""
    (string-delete char-alphabetic? ".foo.bar." 2 4)))
(test-assert
  "string-delete"
  (string=? "" (string-delete #\. ".")))
(test-assert
  "string-delete"
  (string=? "" (string-delete #\. "..")))
(test-assert
  "string-delete"
  (string=? "" (string-delete #\. "...")))
(test-assert
  "string-delete"
  (string=? "x" (string-delete #\. ".x")))
(test-assert
  "string-delete"
  (string=? "x" (string-delete #\. "..x")))
(test-assert
  "string-delete"
  (string=? "x" (string-delete #\. "...x")))
(test-assert
  "string-delete"
  (string=? "x" (string-delete #\. "x.")))
(test-assert
  "string-delete"
  (string=? "x" (string-delete #\. "x..")))
(test-assert
  "string-delete"
  (string=? "x" (string-delete #\. "x...")))
(test-assert
  "string-delete"
  (string=? "x" (string-delete #\. "...x...")))
(test-assert
  "string-delete"
  (string=?
    ""
    (string-delete char-set:punctuation ".")))
(test-assert
  "string-delete"
  (string=?
    ""
    (string-delete char-set:punctuation "..")))
(test-assert
  "string-delete"
  (string=?
    ""
    (string-delete char-set:punctuation "...")))
(test-assert
  "string-delete"
  (string=?
    "x"
    (string-delete char-set:punctuation ".x")))
(test-assert
  "string-delete"
  (string=?
    "x"
    (string-delete char-set:punctuation "..x")))
(test-assert
  "string-delete"
  (string=?
    "x"
    (string-delete char-set:punctuation "...x")))
(test-assert
  "string-delete"
  (string=?
    "x"
    (string-delete char-set:punctuation "x.")))
(test-assert
  "string-delete"
  (string=?
    "x"
    (string-delete char-set:punctuation "x..")))
(test-assert
  "string-delete"
  (string=?
    "x"
    (string-delete char-set:punctuation "x...")))
(test-assert
  "string-delete"
  (string=?
    "x"
    (string-delete char-set:punctuation "...x...")))
(test-error
  "string-map bad proc integer"
  check-srfi-13-error
  (string-map 123 "abcde"))
(test-error
  "string-map bad proc string"
  check-srfi-13-error
  (string-map "zzz" "abcde"))
(test-assert
  "string-map constant"
  (string=?
    "xxx"
    (string-map (lambda (c) #\x) "foo")))

(define (identity x) x)

(test-assert
  "string-map identity"
  (string=? "foo" (string-map identity "foo")))
(test-assert
  "string-map upcase"
  (string=? "FOO" (string-map char-upcase "foo")))
(test-error
  "string-map! bad proc integer"
  check-srfi-13-error
  (string-map 123 "abcde"))
(test-error
  "string-map! bad proc string"
  check-srfi-13-error
  (string-map "zzz" "abcde"))
(test-assert
  "string-map! constant"
  (let ((str (string-copy "foo")))
    (string-map! (lambda (c) #\x) str)
    (string=? str "xxx")))
(test-assert
  "string-map! identity"
  (let ((str (string-copy "foo")))
    (string-map! identity str)
    (string=? str "foo")))
(test-assert
  "string-map! upcase"
  (let ((str (string-copy "foo")))
    (string-map! char-upcase str)
    (string=? str "FOO")))
(test-error
  "string-for-each bad proc integer"
  check-srfi-13-error
  (string-for-each 123 "abcde"))
(test-error
  "string-for-each bad proc string"
  check-srfi-13-error
  (string-for-each "zzz" "abcde"))
(test-assert
  "string-for-each copy"
  (let* ((foo "foo")
         (bar (make-string (string-length foo)))
         (i 0))
    (string-for-each
      (lambda (c)
        (string-set! bar i c)
        (set! i (+ i 1)))
      foo)
    (string=? foo bar)))
(test-error
  "string-for-each-index bad proc integer"
  check-srfi-13-error
  (string-for-each-index 123 "abcde"))
(test-error
  "string-for-each-index bad proc string"
  check-srfi-13-error
  (string-for-each-index "zzz" "abcde"))
(test-assert
  "string-for-each-index index"
  (let* ((foo "foo")
         (bar (make-string (string-length foo))))
    (string-for-each-index
      (lambda (i)
        (string-set! bar i (string-ref foo i)))
      foo)
    (string=? foo bar)))

(test-end)