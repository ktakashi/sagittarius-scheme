(import (rnrs)
	(util bytevector)
	(srfi :1)
	(srfi :14)
	(srfi :64 testing))

(test-begin "Extra bytevector utilities tests")

;; TODO more tests
(test-equal "bytevector-xor" #vu8(#x66 #x66)
	    (bytevector-xor #vu8(#x35 #x35) #vu8(#x53 #x53)))

(test-equal "bytevector-ior" #vu8(#x53 #x53)
	    (bytevector-ior #vu8(#x00 #x00) #vu8(#x53 #x53)))

(test-equal "bytevector-and" #vu8(#xF0 #x0F)
	    (bytevector-and #vu8(#xFa #x6F) #vu8(#xF0 #x0F)))

;; comparisons
(let ((runner (lambda (proc s1 s2)
		(proc (string->utf8 s1) (string->utf8 s2)))))
  (define-syntax test-cmp
    (syntax-rules ()
      ((_ expr expected)
       (test-equal (format "~s" 'expr) expected expr))))
  (test-cmp (runner bytevector<? "z" "z") #f)
  (test-cmp (runner bytevector<? "z" "\xDF;") #t)
  (test-cmp (runner bytevector<? "\xDF;" "z") #f)
  (test-cmp (runner bytevector<? "z" "zz") #t)
  (test-cmp (runner bytevector<? "z" "Z") #f)
  (test-cmp (runner bytevector<=? "z" "\xDF;") #t)
  (test-cmp (runner bytevector<=? "\xDF;" "z") #f)
  (test-cmp (runner bytevector<=? "z" "zz") #t)
  (test-cmp (runner bytevector<=? "z" "Z") #f)
  (test-cmp (runner bytevector<=? "z" "z") #t)
  (test-cmp (runner bytevector<? "z" "z") #f)
  (test-cmp (runner bytevector>? "z" "\xDF;") #f)
  (test-cmp (runner bytevector>? "\xDF;" "z") #t)
  (test-cmp (runner bytevector>? "z" "zz") #f)
  (test-cmp (runner bytevector>? "z" "Z") #t)
  (test-cmp (runner bytevector>=? "z" "\xDF;") #f)
  (test-cmp (runner bytevector>=? "\xDF;" "z") #t)
  (test-cmp (runner bytevector>=? "z" "zz") #f)
  (test-cmp (runner bytevector>=? "z" "Z") #t)
  (test-cmp (runner bytevector>=? "z" "z") #t)
  
  (test-cmp (runner bytevector<=? "c2" "c%40") #f)
  (test-cmp (runner bytevector<? "c2" "c%40") #f)
  (test-cmp (runner bytevector>=? "c2" "c%40") #t)
  (test-cmp (runner bytevector>? "c2" "c%40") #t))

(test-equal "bytevector-slices (normal)"
	    '(#vu8(0 1 2 3) #vu8(4 5 6 7) #vu8(8 9 10 11) #vu8(12 13 14 15))
	    (bytevector-slices (u8-list->bytevector (iota 16)) 4))
(test-equal "bytevector-slices (boundary)" '() (bytevector-slices #vu8() 4))
(test-equal "bytevector-slices (short)"
	    '(#vu8(0 1 2 3) #vu8(4 5 6 7) #vu8(8 9 10 11) #vu8(12))
	    (bytevector-slices (u8-list->bytevector (iota 13)) 4))
(test-equal "bytevector-slices (short)" '(#vu8(0 1))
	    (bytevector-slices (u8-list->bytevector (iota 2)) 4))
(test-equal "bytevector-slices (padding)" 
	    '(#vu8(0 1 2 3) #vu8(4 5 6 7) #vu8(8 9 10 11) #vu8(12 0 0 0))
	    (bytevector-slices (u8-list->bytevector (iota 13)) 4 
			       :padding
			       (lambda (bv) 
				 ;; lazy
				 (bytevector-append bv #vu8(0 0 0)))))
(test-error "bytevector-slices (error)" assertion-violation? 
	    (bytevector-slices (u8-list->bytevector (iota 13)) 0))
(test-error "bytevector-slices (error)" assertion-violation? 
	    (bytevector-slices (u8-list->bytevector (iota 13)) -1))

(test-equal "bytevector-split-at* (normal)" '(#vu8(1 2 3) #vu8(4)) 
	    (receive r (bytevector-split-at* #vu8(1 2 3 4) 3) r))
(test-equal "bytevector-split-at* (boundary)" '(#vu8() #vu8(1 2 3 4)) 
	    (receive r (bytevector-split-at* #vu8(1 2 3 4) 0) r))
(test-equal "bytevector-split-at* (boundary)" '(#vu8(1 2 3 4) #vu8())
	    (receive r (bytevector-split-at* '#vu8(1 2 3 4) 4) r))
(test-error "bytevector-split-at* (error)" assertion-violation?
	    (bytevector-split-at* #vu8(1 2 3 4) -1))
(test-equal "bytevector-split-at* (shorten)" '(#vu8(1 2 3 4) #vu8())
	    (receive r (bytevector-split-at* #vu8(1 2 3 4) 5) r))
(test-equal "bytevector-split-at* (padding)" '(#vu8(1 2 3 4 0 0) #vu8())
	    (receive r (bytevector-split-at* #vu8(1 2 3 4) 6 
			:padding (lambda (bv) 
				   (bytevector-append bv #vu8(0 0)))) r))

(test-equal "bytevector->hex-string" "12345678" 
	    (bytevector->hex-string #vu8(#x12 #x34 #x56 #x78)))
(test-equal "bytevector->hex-string" "0ABC" 
	    (bytevector->hex-string #vu8(#x0A #xBC)))
(test-equal "bytevector->hex-string" "0abc" 
	    (bytevector->hex-string #vu8(#x0A #xBC) :upper? #f))
(test-equal "hex-string->bytevector" #vu8(#x12 #x34 #x56 #x78)
	    (hex-string->bytevector "12345678"))
(test-equal "hex-string->bytevector" #vu8(#x1 #x23)
	    (hex-string->bytevector "123"))
(test-equal "hex-string->bytevector" #vu8(#x0A #xBC)
	    (hex-string->bytevector "abc"))


(define ->bv string->utf8)
(define u8-set:whitespace  (char-set->u8-set char-set:whitespace))
(define u8-set:letter      (char-set->u8-set char-set:letter))
(define u8-set:digit       (char-set->u8-set char-set:digit))
(define u8-set:graphic     (char-set->u8-set char-set:graphic))
(define u8-set:punctuation (char-set->u8-set char-set:punctuation))

(define (u8-whitespace? u8) (char-whitespace? (integer->char u8)))
(define (u8-alphabetic? u8) (char-alphabetic? (integer->char u8)))
(define (u8-numeric? u8)    (char-numeric? (integer->char u8)))
 
(test-assert
  "bytevector-take empty bytevector"
  (bytevector=? #vu8() (bytevector-take (->bv "foo bar braz") 0)))
(test-assert
  "bytevector-take non-empty bytevector"
  (bytevector=? (->bv "foo ") (bytevector-take (->bv "foo bar braz") 4)))
(test-assert
  "bytevector-take full bytevector"
  (bytevector=?
    (->bv "foo bar braz")
    (bytevector-take (->bv "foo bar braz") 12)))
(test-assert
  "bytevector-take-right empty bytevector"
  (bytevector=?
    #vu8()
    (bytevector-take-right (->bv "foo bar braz") 0)))
(test-assert
  "bytevector-take-right non-empty bytevector"
  (bytevector=?
   (->bv "braz")
    (bytevector-take-right (->bv "foo bar braz") 4)))
(test-assert
  "bytevector-take-right full bytevector"
  (bytevector=?
    (->bv "foo bar braz")
    (bytevector-take-right (->bv "foo bar braz") 12)))
(test-assert
  "bytevector-drop empty bytevector"
  (bytevector=? #vu8() (bytevector-drop (->bv "foo bar braz") 12)))
(test-assert
  "bytevector-drop non-empty bytevector"
  (bytevector=? (->bv "braz") (bytevector-drop (->bv "foo bar braz") 8)))
(test-assert
  "bytevector-drop full bytevector"
  (bytevector=?
    (->bv "foo bar braz")
    (bytevector-drop (->bv "foo bar braz") 0)))
(test-assert
  "bytevector-drop-right empty bytevector"
  (bytevector=?
    #vu8()
    (bytevector-drop-right (->bv "foo bar braz") 12)))
(test-assert
  "bytevector-drop-right non-empty bytevector"
  (bytevector=?
   (->bv "foo ")
    (bytevector-drop-right (->bv "foo bar braz") 8)))
(test-assert
  "bytevector-drop-right full bytevector"
  (bytevector=?
    (->bv "foo bar braz")
    (bytevector-drop-right (->bv "foo bar braz") 0)))
(test-assert
  "bytevector-pad empty bytevector, zero pad"
  (bytevector=? #vu8() (bytevector-pad #vu8() 0)))
(test-assert
  "bytevector-pad empty bytevector, zero pad, pad char"
  (bytevector=? #vu8() (bytevector-pad #vu8() 0)))
(test-assert
  "bytevector-pad empty pad bytevector, 2 pad "
  (bytevector=? #vu8(0 0) (bytevector-pad #vu8() 2)))
(test-assert
  "bytevector-pad empty pad bytevector, 2 pad, pad char"
  (bytevector=? (->bv "!!") (bytevector-pad #vu8() 2 (char->integer #\!))))
(test-assert
  "bytevector-pad empty pad bytevector, 2 pad, pad char, start index"
  (bytevector=? (->bv "!c")
		(bytevector-pad (->bv "abc") 2 (char->integer #\!) 2)))
(test-assert
  "bytevector-pad empty pad bytevector, 2 pad, pad char, start and end index"
  (bytevector=? (->bv "!c")
		(bytevector-pad (->bv "abcd") 2 (char->integer #\!) 2 3)))
(test-assert
  "bytevector-pad freestyle 1"
  (bytevector=?
   (->bv "32")
   (bytevector-pad (->bv (number->string 532)) 2 (char->integer #\!))))
(test-assert
  "bytevector-pad freestyle 2"
  (bytevector=?
   (->bv "!532")
   (bytevector-pad (->bv (number->string 532)) 4 (char->integer #\!))))
(test-assert
  "bytevector-pad-right empty bytevector, zero pad"
  (bytevector=? #vu8() (bytevector-pad-right #vu8() 0)))
(test-assert
  "bytevector-pad-right empty bytevector, zero pad, pad char"
  (bytevector=? #vu8() (bytevector-pad-right #vu8() 0)))
(test-assert
  "bytevector-pad-right empty pad bytevector, 2 pad "
  (bytevector=? #vu8(0 0) (bytevector-pad-right #vu8() 2)))
(test-assert
  "bytevector-pad-right empty pad bytevector, 2 pad, pad char"
  (bytevector=? (->bv "!!") 
		(bytevector-pad-right #vu8() 2 (char->integer #\!))))
(test-assert
  "bytevector-pad-right empty pad bytevector, 2 pad, pad char, start index"
  (bytevector=? (->bv "c!")
		(bytevector-pad-right (->bv "abc") 2 (char->integer #\!) 2)))
(test-assert
  "bytevector-pad-right empty pad bytevector, 2 pad, pad char, start and end index"
  (bytevector=?
   (->bv "c!")
    (bytevector-pad-right (->bv "abcd") 2 (char->integer #\!) 2 3)))
(test-assert
  "bytevector-pad-right freestyle 1"
  (bytevector=?
   (->bv "53")
    (bytevector-pad-right (->bv (number->string 532)) 2 (char->integer #\!))))
(test-assert
  "bytevector-pad-right freestyle 2"
  (bytevector=?
   (->bv "532!")
    (bytevector-pad-right (->bv (number->string 532)) 4 (char->integer #\!))))
(test-error
  "bytevector-trim bad u8_pred char"
  condition?
  (bytevector-trim (->bv "abcde") #\c))
(test-error
  "bytevector-trim bad char_pred bytevector"
  condition?
  (bytevector-trim (->bv "abcde") (->bv "zzz")))
(test-assert
  "bytevector-trim empty bytevector"
  (bytevector=? #vu8() (bytevector-trim #vu8())))
(test-assert
  "bytevector-trim no char/pred"
  (bytevector=? (->bv "foo ") (bytevector-trim (->bv " 	foo "))))
(test-assert
  "bytevector-trim start index, pred"
  (bytevector=?
    (->bv "foo ")
    (bytevector-trim (->bv " 	foo ") u8-whitespace? 1)))
(test-assert
  "bytevector-trim start and end index, pred"
  (bytevector=?
    (->bv "f")
    (bytevector-trim (->bv " 	foo ") u8-whitespace? 1 3)))
(test-assert
  "bytevector-trim start index, char"
  (bytevector=?
   (->bv "	foo ")
    (bytevector-trim (->bv " 	foo ") (char->integer #\space) 1)))
(test-assert
  "bytevector-trim start and end index, char"
  (bytevector=?
   (->bv "	f")
    (bytevector-trim (->bv " 	foo ") (char->integer #\space) 1 3)))
(test-assert
  "bytevector-trim start index, charset"
  (bytevector=?
    (->bv "foo ")
    (bytevector-trim (->bv " 	foo ") u8-set:whitespace 1)))
(test-assert
  "bytevector-trim start and end index, charset"
  (bytevector=?
    (->bv "f")
    (bytevector-trim (->bv " 	foo ") u8-set:whitespace 1 3)))
(test-error
  "bytevector-trim-right bad char_pred integer"
  condition?
  (bytevector-trim-right "abcde" 123))
(test-error
  "bytevector-trim-right bad char_pred bytevector"
  condition?
  (bytevector-trim-right "abcde" "zzz"))
(test-assert
  "bytevector-trim-right empty bytevector"
  (bytevector=? #vu8() (bytevector-trim-right #vu8())))
(test-assert
  "bytevector-trim-right no char/pred"
  (bytevector=? (->bv " 	foo")
		(bytevector-trim-right (->bv " 	foo "))))
(test-assert
  "bytevector-trim-right start index, pred"
  (bytevector=?
   (->bv "\tfoo")
    (bytevector-trim-right (->bv " \tfoo ") u8-whitespace? 1)))
(test-assert
  "bytevector-trim-right start and end index, pred"
  (bytevector=?
   (->bv "\tf")
    (bytevector-trim-right (->bv " \tfoo ") u8-whitespace? 1 3)))
(test-assert
  "bytevector-trim-right start index, char"
  (bytevector=?
   (->bv "	foo")
    (bytevector-trim-right (->bv " 	foo ") (char->integer #\space) 1)))
(test-assert
  "bytevector-trim-right start and end index, char"
  (bytevector=?
    (->bv "	f")
    (bytevector-trim-right (->bv " 	foo ") (char->integer #\space) 1 3)))
(test-assert
  "bytevector-trim-right start index, charset"
  (bytevector=?
   (->bv "	foo")
    (bytevector-trim-right
      (->bv " 	foo ")
      u8-set:whitespace
      1)))
(test-assert
  "bytevector-trim-right start and end index, charset"
  (bytevector=?
    (->bv "	f")
    (bytevector-trim-right
      (->bv " 	foo ")
      u8-set:whitespace
      1
      3)))
(test-error
  "bytevector-trim-both bad char_pred integer"
  condition?
  (bytevector-trim-both (->bv "abcde") #\c))
(test-error
  "bytevector-trim-both bad char_pred bytevector"
  condition?
  (bytevector-trim-both (->bv "abcde") (->bv "zzz")))
(test-assert
  "bytevector-trim-both empty bytevector"
  (bytevector=? #vu8() (bytevector-trim-both #vu8())))
(test-assert
  "bytevector-trim-both no char/pred"
  (bytevector=? (->bv "foo") (bytevector-trim-both (->bv " 	foo "))))
(test-assert
  "bytevector-trim-both start index, pred"
  (bytevector=?
   (->bv "foo")
   (bytevector-trim-both (->bv " 	foo ") u8-whitespace? 1)))
(test-assert
  "bytevector-trim-both start and end index, pred"
  (bytevector=?
    (->bv "f")
    (bytevector-trim-both (->bv " 	foo ") u8-whitespace? 1 3)))
(test-assert
  "bytevector-trim-both start index, char"
  (bytevector=?
   (->bv "	foo")
   (bytevector-trim-both (->bv " 	foo ") (char->integer #\space) 1)))

(test-assert
  "bytevector-trim-both start and end index, charset"
  (bytevector=?
    (->bv "f")
    (bytevector-trim-both
      (->bv " 	foo ")
      u8-set:whitespace
      1
      3)))

;; prefix & suffix
(test-assert
  "bytevector-prefix-length empty prefix"
  (= 0 (bytevector-prefix-length (->bv "") (->bv "foo bar"))))
(test-assert
  "bytevector-prefix-length non-empty prefix - match"
  (= 3 (bytevector-prefix-length (->bv "foo") (->bv "foo bar"))))
(test-assert
  "bytevector-prefix-length non-empty prefix - no match"
  (= 0 (bytevector-prefix-length (->bv "bar") (->bv "foo bar"))))

(test-assert
  "bytevector-suffix-length empty suffix"
  (= 0 (bytevector-suffix-length (->bv "") (->bv "foo bar"))))
(test-assert
  "bytevector-suffix-length non-empty suffix - match"
  (= 3 (bytevector-suffix-length (->bv "bar") (->bv "foo bar"))))
(test-assert
  "bytevector-suffix-length non-empty suffix - no match"
  (= 0 (bytevector-suffix-length (->bv "foo") (->bv "foo bar"))))

(test-assert
  "bytevector-prefix? empty prefix"
  (bytevector-prefix? (->bv "") (->bv "foo bar")))
(test-assert
  "bytevector-prefix? non-empty prefix - match"
  (bytevector-prefix? (->bv "foo") (->bv "foo bar")))
(test-assert
  "bytevector-prefix? non-empty prefix - no match"
  (not (bytevector-prefix? (->bv "bar") (->bv "foo bar"))))

(test-assert
  "bytevector-suffix? empty suffix"
  (bytevector-suffix? (->bv "") (->bv "foo bar")))
(test-assert
  "bytevector-suffix? non-empty suffix - match"
  (bytevector-suffix? (->bv "bar") (->bv "foo bar")))
(test-assert
  "bytevector-suffix? non-empty suffix - no match"
  (not (bytevector-suffix? (->bv "foo") (->bv "foo bar"))))

;; search
(test-error
  "bytevector-index bad char_pred char"
  condition?
  (bytevector-index (->bv "abcde") #\c))
(test-error
  "bytevector-index bad char_pred string"
  condition?
  (bytevector-index (->bv "abcde") "zzz"))
(test-assert
  "bytevector-index empty bytevector - char"
  (not (bytevector-index #vu8() (char->integer #\a))))
(test-assert
  "bytevector-index non-empty - char - match"
  (= 5 (bytevector-index (->bv "foo bar") (char->integer #\a))))
(test-assert
  "bytevector-index non-empty - char - no match"
  (not (bytevector-index (->bv "frobnicate") (char->integer #\x))))
(test-assert
  "bytevector-index empty bytevector - char - start index"
  (not (bytevector-index #vu8() (char->integer #\a) 0)))
(test-assert
  "bytevector-index non-empty - char - match - start index"
  (= 5 (bytevector-index (->bv "foo bar") (char->integer #\a) 1)))
(test-assert
  "bytevector-index non-empty - char - no match - start index"
  (not (bytevector-index (->bv "frobnicate") (char->integer #\x) 2)))
(test-assert
  "bytevector-index empty bytevector - char - start and end index"
  (not (bytevector-index #vu8() (char->integer #\a) 0 0)))
(test-assert
  "bytevector-index non-empty - char - match - start and end index"
  (= 5 (bytevector-index (->bv "foo bar") (char->integer #\a) 1 6)))
(test-assert
  "bytevector-index non-empty - char - no match - start and end index"
  (not (bytevector-index (->bv "frobnicate") (char->integer #\a) 2 5)))
(test-assert
  "bytevector-index empty bytevector - u8-set"
  (not (bytevector-index #vu8() u8-set:letter)))
(test-assert
  "bytevector-index non-empty - u8-set - match"
  (= 0 (bytevector-index (->bv "foo bar") u8-set:letter)))
(test-assert
  "bytevector-index non-empty - u8-set - no match"
  (not (bytevector-index (->bv "frobnicate") u8-set:digit)))
(test-assert
  "bytevector-index empty bytevector - u8-set - start index"
  (not (bytevector-index #vu8() u8-set:letter 0)))
(test-assert
  "bytevector-index non-empty - u8-set - match - start index"
  (= 1 (bytevector-index (->bv "foo bar") u8-set:letter 1)))
(test-assert
  "bytevector-index non-empty - u8-set - no match - start index"
  (not (bytevector-index (->bv "frobnicate") u8-set:digit 2)))
(test-assert
  "bytevector-index empty bytevector - u8-set - start and end index"
  (not (bytevector-index #vu8() u8-set:letter 0 0)))
(test-assert
  "bytevector-index non-empty - u8-set - match - start and end index"
  (= 1
     (bytevector-index (->bv "foo bar") u8-set:letter 1 6)))
(test-assert
  "bytevector-index non-empty - u8-set - no match - start and end index"
  (not (bytevector-index (->bv "frobnicate") u8-set:digit 2 5)))
(test-assert
  "bytevector-index empty bytevector - pred"
  (not (bytevector-index #vu8() u8-alphabetic?)))
(test-assert
  "bytevector-index non-empty - pred - match"
  (= 0 (bytevector-index (->bv "foo bar") u8-alphabetic?)))
(test-assert
  "bytevector-index non-empty - pred - no match"
  (not (bytevector-index (->bv "frobnicate") u8-numeric?)))
(test-assert
  "bytevector-index empty bytevector - pred - start index"
  (not (bytevector-index #vu8() u8-alphabetic? 0)))
(test-assert
  "bytevector-index non-empty - pred - match - start index"
  (= 1 (bytevector-index (->bv "foo bar") u8-alphabetic? 1)))
(test-assert
  "bytevector-index non-empty - pred - no match - start index"
  (not (bytevector-index (->bv "frobnicate") u8-numeric? 2)))
(test-assert
  "bytevector-index empty bytevector - pred - start and end index"
  (not (bytevector-index #vu8() u8-alphabetic? 0 0)))
(test-assert
  "bytevector-index non-empty - pred - match - start and end index"
  (= 1
     (bytevector-index (->bv "foo bar") u8-alphabetic? 1 6)))
(test-assert
  "bytevector-index non-empty - pred - no match - start and end index"
  (not (bytevector-index (->bv "frobnicate") u8-numeric? 2 5)))
(test-assert
  "bytevector-index 8-bit char in bytevector"
  (begin
    (bytevector-index #vu8(200) u8-numeric?)
    #t))
(test-error
  "bytevector-index-right bad char_pred char"
  condition?
  (bytevector-index-right (->bv "abcde") #\c))
(test-error
  "bytevector-index-right bad char_pred bytevector"
  condition?
  (bytevector-index-right (->bv "abcde") "zzz"))
(test-assert
  "bytevector-index-right empty bytevector - char"
  (not (bytevector-index-right #vu8() (char->integer #\a))))
(test-assert
  "bytevector-index-right non-empty - char - match"
  (= 5 (bytevector-index-right (->bv "foo bar") (char->integer #\a))))
(test-assert
  "bytevector-index-right non-empty - char - no match"
  (not (bytevector-index-right (->bv "frobnicate") (char->integer #\x))))
(test-assert
  "bytevector-index-right empty bytevector - char - start index-right"
  (not (bytevector-index-right #vu8() (char->integer #\a) 0)))
(test-assert
  "bytevector-index-right non-empty - char - match - start index"
  (= 5 (bytevector-index-right (->bv "foo bar") (char->integer #\a) 1)))
(test-assert
  "bytevector-index-right non-empty - char - no match - start index"
  (not (bytevector-index-right (->bv "frobnicate") (char->integer #\x) 2)))
(test-assert
  "bytevector-index-right empty bytevector - char - start and end index"
  (not (bytevector-index-right #vu8() (char->integer #\a) 0 0)))
(test-assert
  "bytevector-index-right non-empty - char - match - start and end index"
  (= 5 (bytevector-index-right (->bv "foo bar") (char->integer #\a) 1 6)))
(test-assert
  "bytevector-index-right non-empty - char - no match - start and end index"
  (not (bytevector-index-right (->bv "frobnicate") (char->integer #\a) 2 5)))
(test-assert
  "bytevector-index-right empty bytevector - u8-set"
  (not (bytevector-index-right #vu8() u8-set:letter)))
(test-assert
  "bytevector-index-right non-empty - u8-set - match"
  (= 6
     (bytevector-index-right (->bv "foo bar") u8-set:letter)))
(test-assert
  "bytevector-index-right non-empty - u8-set - no match"
  (not (bytevector-index-right (->bv "frobnicate") u8-set:digit)))
(test-assert
  "bytevector-index-right empty bytevector - u8-set - start index"
  (not (bytevector-index-right #vu8() u8-set:letter 0)))
(test-assert
  "bytevector-index-right non-empty - u8-set - match - start index"
  (= 6
     (bytevector-index-right (->bv "foo bar") u8-set:letter 1)))
(test-assert
  "bytevector-index-right non-empty - u8-set - no match - start index"
  (not (bytevector-index-right
         (->bv "frobnicate")
         u8-set:digit
         2)))
(test-assert
  "bytevector-index-right empty bytevector - u8-set - start and end index"
  (not (bytevector-index-right #vu8() u8-set:letter 0 0)))
(test-assert
  "bytevector-index-right non-empty - u8-set - match - start and end index"
  (= 5
     (bytevector-index-right
       (->bv "foo bar")
       u8-set:letter
       1
       6)))
(test-assert
  "bytevector-index-right non-empty - u8-set - no match - start and end index"
  (not (bytevector-index-right
         (->bv "frobnicate")
         u8-set:digit
         2
         5)))
(test-assert
  "bytevector-index-right empty bytevector - pred"
  (not (bytevector-index-right #vu8() u8-alphabetic?)))
(test-assert
  "bytevector-index-right non-empty - pred - match"
  (= 6
     (bytevector-index-right (->bv "foo bar") u8-alphabetic?)))
(test-assert
  "bytevector-index-right non-empty - pred - no match"
  (not (bytevector-index-right (->bv "frobnicate") u8-numeric?)))
(test-assert
  "bytevector-index-right empty bytevector - pred - start index"
  (not (bytevector-index-right #vu8() u8-alphabetic? 0)))
(test-assert
  "bytevector-index-right non-empty - pred - match - start index"
  (= 6
     (bytevector-index-right (->bv "foo bar") u8-alphabetic? 1)))
(test-assert
  "bytevector-index-right non-empty - pred - no match - start index"
  (not (bytevector-index-right (->bv "frobnicate") u8-numeric? 2)))
(test-assert
  "bytevector-index-right empty bytevector - pred - start and end index"
  (not (bytevector-index-right #vu8() u8-alphabetic? 0 0)))
(test-assert
  "bytevector-index-right non-empty - pred - match - start and end index"
  (= 5
     (bytevector-index-right
       (->bv "foo bar")
       u8-alphabetic?
       1
       6)))
(test-assert
  "bytevector-index-right non-empty - pred - no match - start and end index"
  (not (bytevector-index-right
         (->bv "frobnicate")
         u8-numeric?
         2
         5)))

;; skip
(test-error
  "bytevector-skip bad char_pred integer"
  condition?
  (bytevector-skip (->bv "abcde") #\c))
(test-error
  "bytevector-skip bad char_pred string"
  condition?
  (bytevector-skip (->bv "abcde") "zzz"))
(test-assert
  "bytevector-skip empty string - char"
  (not (bytevector-skip #vu8() (char->integer #\a))))
(test-assert
  "bytevector-skip non-empty - char - match"
  (= 0 (bytevector-skip (->bv "foo bar") (char->integer #\a))))
(test-assert
  "bytevector-skip non-empty - char - no match"
  (= 0 (bytevector-skip (->bv "frobnicate") (char->integer #\x))))
(test-assert
  "bytevector-skip empty string - char - start index"
  (not (bytevector-skip #vu8() (char->integer #\a) 0)))
(test-assert
  "bytevector-skip non-empty - char - match - start index"
  (= 1 (bytevector-skip (->bv "foo bar") (char->integer #\a) 1)))
(test-assert
  "bytevector-skip non-empty - char - no match - start index"
  (= 2 (bytevector-skip (->bv "frobnicate") (char->integer #\x) 2)))
(test-assert
  "bytevector-skip empty string - char - start and end index"
  (not (bytevector-skip #vu8() (char->integer #\a) 0 0)))
(test-assert
  "bytevector-skip non-empty - char - match - start and end index"
  (= 1 (bytevector-skip (->bv "foo bar") (char->integer #\a) 1 6)))
(test-assert
  "bytevector-skip non-empty - char - no match - start and end index"
  (= 2 (bytevector-skip (->bv "frobnicate") (char->integer #\a) 2 5)))
(test-assert
  "bytevector-skip empty string - charset"
  (not (bytevector-skip #vu8() u8-set:letter)))
(test-assert
  "bytevector-skip non-empty - charset - match"
  (= 3 (bytevector-skip (->bv "foo bar") u8-set:letter)))
(test-assert
  "bytevector-skip non-empty - charset - no match"
  (= 0 (bytevector-skip (->bv "frobnicate") u8-set:digit)))
(test-assert
  "bytevector-skip empty string - charset - start index"
  (not (bytevector-skip #vu8() u8-set:letter 0)))
(test-assert
  "bytevector-skip non-empty - charset - match - start index"
  (= 3 (bytevector-skip (->bv "foo bar") u8-set:letter 1)))
(test-assert
  "bytevector-skip non-empty - charset - no match - start index"
  (= 2 (bytevector-skip (->bv "frobnicate") u8-set:digit 2)))
(test-assert
  "bytevector-skip empty string - charset - start and end index"
  (not (bytevector-skip #vu8() u8-set:letter 0 0)))
(test-assert
  "bytevector-skip non-empty - charset - match - start and end index"
  (= 3 (bytevector-skip (->bv "foo bar") u8-set:letter 1 6)))
(test-assert
  "bytevector-skip non-empty - charset - no match - start and end index"
  (= 2
     (bytevector-skip (->bv "frobnicate") u8-set:digit 2 5)))
(test-assert
  "bytevector-skip empty string - pred"
  (not (bytevector-skip #vu8() u8-alphabetic?)))
(test-assert
  "bytevector-skip non-empty - pred - match"
  (= 3 (bytevector-skip (->bv "foo bar") u8-alphabetic?)))
(test-assert
  "bytevector-skip non-empty - pred - no match"
  (= 0 (bytevector-skip (->bv "frobnicate") u8-numeric?)))
(test-assert
  "bytevector-skip empty string - pred - start index"
  (not (bytevector-skip #vu8() u8-alphabetic? 0)))
(test-assert
  "bytevector-skip non-empty - pred - match - start index"
  (= 3 (bytevector-skip (->bv "foo bar") u8-alphabetic? 1)))
(test-assert
  "bytevector-skip non-empty - pred - no match - start index"
  (= 2 (bytevector-skip (->bv "frobnicate") u8-numeric? 2)))
(test-assert
  "bytevector-skip empty string - pred - start and end index"
  (not (bytevector-skip #vu8() u8-alphabetic? 0 0)))
(test-assert
  "bytevector-skip non-empty - pred - match - start and end index"
  (= 3
     (bytevector-skip (->bv "foo bar") u8-alphabetic? 1 6)))
(test-assert
  "bytevector-skip non-empty - pred - no match - start and end index"
  (= 2
     (bytevector-skip (->bv "frobnicate") u8-numeric? 2 5)))
(test-error
  "bytevector-skip-right bad char_pred integer"
  condition?
  (bytevector-skip-right (->bv "abcde") #\c))
(test-error
  "bytevector-skip-right bad char_pred string"
  condition?
  (bytevector-skip-right (->bv "abcde") "zzz"))
(test-assert
  "bytevector-skip-right empty string - char"
  (not (bytevector-skip-right #vu8() (char->integer #\a))))
(test-assert
  "bytevector-skip-right non-empty - char - match"
  (= 6 (bytevector-skip-right (->bv "foo bar") (char->integer #\a))))
(test-assert
  "bytevector-skip-right non-empty - char - no match"
  (= 9 (bytevector-skip-right (->bv "frobnicate") (char->integer #\x))))
(test-assert
  "bytevector-skip-right empty string - char - start index-right"
  (not (bytevector-skip-right #vu8() (char->integer #\a) 0)))
(test-assert
  "bytevector-skip-right non-empty - char - match - start index"
  (= 6 (bytevector-skip-right (->bv "foo bar") (char->integer #\a) 1)))
(test-assert
  "bytevector-skip-right non-empty - char - no match - start index"
  (= 9 (bytevector-skip-right (->bv "frobnicate") (char->integer #\x) 2)))
(test-assert
  "bytevector-skip-right empty string - char - start and end index"
  (not (bytevector-skip-right #vu8() (char->integer #\a) 0 0)))
(test-assert
  "bytevector-skip-right non-empty - char - match - start and end index"
  (= 4 (bytevector-skip-right (->bv "foo bar") (char->integer #\a) 1 6)))
(test-assert
  "bytevector-skip-right non-empty - char - no match - start and end index"
  (= 4 (bytevector-skip-right (->bv "frobnicate") (char->integer #\a) 2 5)))
(test-assert
  "bytevector-skip-right empty string - charset"
  (not (bytevector-skip-right #vu8() u8-set:letter)))
(test-assert
  "bytevector-skip-right non-empty - charset - match"
  (= 3
     (bytevector-skip-right (->bv "foo bar") u8-set:letter)))
(test-assert
  "bytevector-skip-right non-empty - charset - no match"
  (= 9
     (bytevector-skip-right (->bv "frobnicate") u8-set:digit)))
(test-assert
  "bytevector-skip-right empty string - charset - start index"
  (not (bytevector-skip-right #vu8() u8-set:letter 0)))
(test-assert
  "bytevector-skip-right non-empty - charset - match - start index"
  (= 3
     (bytevector-skip-right (->bv "foo bar") u8-set:letter 1)))
(test-assert
  "bytevector-skip-right non-empty - charset - no match - start index"
  (= 9
     (bytevector-skip-right (->bv "frobnicate") u8-set:digit 2)))
(test-assert
  "bytevector-skip-right empty string - charset - start and end index"
  (not (bytevector-skip-right #vu8() u8-set:letter 0 0)))
(test-assert
  "bytevector-skip-right non-empty - charset - match - start and end index"
  (= 3
     (bytevector-skip-right (->bv "foo bar") u8-set:letter 1 6)))
(test-assert
  "bytevector-skip-right non-empty - charset - no match - start and end index"
  (= 4
     (bytevector-skip-right
       (->bv "frobnicate")
       u8-set:digit
       2
       5)))
(test-assert
  "bytevector-skip-right empty string - pred"
  (not (bytevector-skip-right #vu8() u8-alphabetic?)))
(test-assert
  "bytevector-skip-right non-empty - pred - match"
  (= 3
     (bytevector-skip-right (->bv "foo bar") u8-alphabetic?)))
(test-assert
  "bytevector-skip-right non-empty - pred - no match"
  (= 9
     (bytevector-skip-right (->bv "frobnicate") u8-numeric?)))
(test-assert
  "bytevector-skip-right empty string - pred - start index"
  (not (bytevector-skip-right #vu8() u8-alphabetic? 0)))
(test-assert
  "bytevector-skip-right non-empty - pred - match - start index"
  (= 3
     (bytevector-skip-right (->bv "foo bar") u8-alphabetic? 1)))
(test-assert
  "bytevector-skip-right non-empty - pred - no match - start index"
  (= 9
     (bytevector-skip-right (->bv "frobnicate") u8-numeric? 2)))
(test-assert
  "bytevector-skip-right empty string - pred - start and end index"
  (not (bytevector-skip-right #vu8() u8-alphabetic? 0 0)))
(test-assert
  "bytevector-skip-right non-empty - pred - match - start and end index"
  (= 3
     (bytevector-skip-right
       (->bv "foo bar")
       u8-alphabetic?
       1
       6)))
(test-assert
  "bytevector-skip-right non-empty - pred - no match - start and end index"
  (= 4
     (bytevector-skip-right
      (->bv "frobnicate")
       u8-numeric?
       2
       5)))

(test-assert
  "bytevector-replace empty bytevector(s), 2 indices"
  (bytevector=? #vu8() (bytevector-replace #vu8() #vu8() 0 0)))
(test-assert
  "bytevector-replace empty bytevector(s), 3 indices"
  (bytevector=? #vu8() (bytevector-replace #vu8() #vu8() 0 0 0)))
(test-assert
  "bytevector-replace empty bytevector(s), 4 indices"
  (bytevector=? #vu8() (bytevector-replace #vu8() #vu8() 0 0 0 0)))

(test-assert
  "bytevector-replace two indices"
  (bytevector=?
    (->bv "fuuar")
    (bytevector-replace (->bv "foo bar") (->bv "uu") 1 5)))
(test-assert
  "bytevector-replace three indices"
  (bytevector=?
    (->bv "fuar")
    (bytevector-replace (->bv "foo bar") (->bv "uu") 1 5 1)))
(test-assert
  "bytevector-replace four indices"
  (bytevector=?
    (->bv "fuar")
    (bytevector-replace (->bv "foo bar") (->bv "uu") 1 5 1 2)))


(test-assert
  "bytevector-tokenize empty bytevector, no char/pred"
  (zero? (length (bytevector-tokenize #vu8()))))
(test-assert
  "bytevector-tokenize empty bytevector, charset"
  (zero? (length
           (bytevector-tokenize #vu8() u8-set:punctuation))))
(test-assert
  "bytevector-tokenize no char/pred"
  (equal?
   '("foo" "bar" "!a")
   (map utf8->string (bytevector-tokenize (->bv "foo	bar !a")))))

(test-assert
  "bytevector-tokenize charset"
  (equal?
    '("foo" "bar" "!a")
    (map utf8->string 
	 (bytevector-tokenize (->bv "foo	bar !a") u8-set:graphic))))
(test-assert
  "bytevector-tokenize charset, start index"
  (equal?
    '("oo" "bar" "!a")
    (map utf8->string 
	 (bytevector-tokenize (->bv "foo	bar !a") u8-set:graphic 1))))
(test-assert
  "bytevector-tokenize charset, start and end index"
  (equal?
    '("oo" "bar" "!")
    (map utf8->string
	 (bytevector-tokenize
	  (->bv "foo	bar !a")
	  u8-set:graphic
	  1
	  9))))

;; filter & delete
(test-error
  "bytevector-filter bad char_pred integer"
  condition?
  (bytevector-filter #\c (->bv "abcde")))
(test-assert
  "bytevector-filter empty bytevector, char"
  (bytevector=? #vu8() (bytevector-filter (char->integer #\.) #vu8())))
(test-assert
  "bytevector-filter empty bytevector, charset"
  (bytevector=?
    #vu8()
    (bytevector-filter u8-set:punctuation #vu8())))
(test-assert
  "bytevector-filter empty bytevector, pred"
  (bytevector=? #vu8() (bytevector-filter u8-alphabetic? #vu8())))
(test-assert
  "bytevector-filter char"
  (bytevector=? (->bv "...") (bytevector-filter (char->integer #\.) (->bv ".foo.bar."))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    (->bv "...")
    (bytevector-filter u8-set:punctuation (->bv ".foo.bar."))))
(test-assert
  "bytevector-filter pred"
  (bytevector=?
    (->bv "foobar")
    (bytevector-filter u8-alphabetic? (->bv ".foo.bar."))))
(test-assert
  "bytevector-filter char, start index"
  (bytevector=? (->bv "..") (bytevector-filter (char->integer #\.) (->bv ".foo.bar.") 2)))
(test-assert
  "bytevector-filter charset, start index"
  (bytevector=?
    (->bv "..")
    (bytevector-filter
      u8-set:punctuation
      (->bv ".foo.bar.")
      2)))
(test-assert
  "bytevector-filter pred, start index"
  (bytevector=?
    (->bv "oobar")
    (bytevector-filter u8-alphabetic? (->bv ".foo.bar.") 2)))
(test-assert
  "bytevector-filter char, start and end index"
  (bytevector=? #vu8() (bytevector-filter (char->integer #\.) (->bv ".foo.bar.") 2 4)))
(test-assert
  "bytevector-filter charset, start and end index"
  (bytevector=?
    #vu8()
    (bytevector-filter
      u8-set:punctuation
      (->bv ".foo.bar.")
      2
      4)))
(test-assert
  "bytevector-filter pred, start and end index"
  (bytevector=?
    (->bv "oo")
    (bytevector-filter u8-alphabetic? (->bv ".foo.bar.") 2 4)))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "x"))))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "xx") (bytevector-filter (char->integer #\x) (->bv "xx"))))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "xx") (bytevector-filter (char->integer #\x) (->bv "xyx"))))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "xyyy"))))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "yyyx"))))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "xx") (bytevector-filter (char->integer #\x) (->bv "xxx") 1)))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "xx") (bytevector-filter (char->integer #\x) (->bv "xxx") 0 2)))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "xyx") 1)))
(test-assert
  "bytevector-filter char"
  (equal? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "yxx") 0 2)))
(test-assert
  "bytevector-filter char"
  (bytevector=? #vu8() (bytevector-filter (char->integer #\x) (->bv "."))))
(test-assert
  "bytevector-filter char"
  (bytevector=? #vu8() (bytevector-filter (char->integer #\x) (->bv ".."))))
(test-assert
  "bytevector-filter char"
  (bytevector=? #vu8() (bytevector-filter (char->integer #\x) (->bv "..."))))
(test-assert
  "bytevector-filter char"
  (bytevector=? (->bv "x") (bytevector-filter (char->integer #\x) (->bv ".x"))))
(test-assert
  "bytevector-filter char"
  (bytevector=? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "..x"))))
(test-assert
  "bytevector-filter char"
  (bytevector=? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "...x"))))
(test-assert
  "bytevector-filter char"
  (bytevector=? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "x."))))
(test-assert
  "bytevector-filter char"
  (bytevector=? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "x.."))))
(test-assert
  "bytevector-filter char"
  (bytevector=? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "x..."))))
(test-assert
  "bytevector-filter char"
  (bytevector=? (->bv "x") (bytevector-filter (char->integer #\x) (->bv "...x..."))))
(let ((charset (list (char->integer #\x) (char->integer #\y))))
  (test-assert
    (equal? (->bv "x") (bytevector-filter charset (->bv "x"))))
  (test-assert
    (equal? (->bv "xx") (bytevector-filter charset (->bv "xx"))))
  (test-assert
    (equal? (->bv "xy") (bytevector-filter charset (->bv "xy"))))
  (test-assert
    (equal? (->bv "x") (bytevector-filter charset (->bv "xaaa"))))
  (test-assert
    (equal? (->bv "y") (bytevector-filter charset (->bv "aaay"))))
  (test-assert
    (equal? (->bv "yx") (bytevector-filter charset (->bv "xyx") 1)))
  (test-assert
    (equal? (->bv "xy") (bytevector-filter charset (->bv "xyx") 0 2)))
  (test-assert
    (equal? (->bv "x") (bytevector-filter charset (->bv "xax") 1)))
  (test-assert
    (equal? (->bv "x") (bytevector-filter charset (->bv "axx") 0 2))))
(test-assert
  "bytevector-filter charset"
  (bytevector=? #vu8() (bytevector-filter u8-set:letter (->bv "."))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    #vu8()
    (bytevector-filter u8-set:letter (->bv ".."))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    #vu8()
    (bytevector-filter u8-set:letter (->bv "..."))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    (->bv "x")
    (bytevector-filter u8-set:letter (->bv ".x"))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    (->bv "x")
    (bytevector-filter u8-set:letter (->bv "..x"))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    (->bv "x")
    (bytevector-filter u8-set:letter (->bv "...x"))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    (->bv "x")
    (bytevector-filter u8-set:letter (->bv "x."))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    (->bv "x")
    (bytevector-filter u8-set:letter (->bv "x.."))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    (->bv "x")
    (bytevector-filter u8-set:letter (->bv "x..."))))
(test-assert
  "bytevector-filter charset"
  (bytevector=?
    (->bv "x")
    (bytevector-filter u8-set:letter (->bv "...x..."))))
(test-error
  "bytevector-delete bad char_pred integer"
  condition?
  (bytevector-delete #\c (->bv "abcde")))
(test-assert
  "bytevector-delete empty bytevector, char"
  (bytevector=? #vu8() (bytevector-delete (char->integer #\.) #vu8())))
(test-assert
  "bytevector-delete empty bytevector, charset"
  (bytevector=?
    #vu8()
    (bytevector-delete u8-set:punctuation #vu8())))
(test-assert
  "bytevector-delete empty bytevector, pred"
  (bytevector=? #vu8() (bytevector-delete u8-alphabetic? #vu8())))
(test-assert
  "bytevector-delete char"
  (bytevector=?
    (->bv "foobar")
    (bytevector-delete (char->integer #\.) (->bv ".foo.bar."))))
(test-assert
  "bytevector-delete charset"
  (bytevector=?
    (->bv "foobar")
    (bytevector-delete u8-set:punctuation (->bv ".foo.bar."))))
(test-assert
  "bytevector-delete pred"
  (bytevector=?
    (->bv "...")
    (bytevector-delete u8-alphabetic? (->bv ".foo.bar."))))
(test-assert
  "bytevector-delete char, start index"
  (bytevector=?
    (->bv "oobar")
    (bytevector-delete (char->integer #\.) (->bv ".foo.bar.") 2)))
(test-assert
  "bytevector-delete charset, start index"
  (bytevector=?
    (->bv "oobar")
    (bytevector-delete
      u8-set:punctuation
      (->bv ".foo.bar.")
      2)))
(test-assert
  "bytevector-delete pred, start index"
  (bytevector=?
    (->bv "..")
    (bytevector-delete u8-alphabetic? (->bv ".foo.bar.") 2)))
(test-assert
  "bytevector-delete char, start and end index"
  (bytevector=?
    (->bv "oo")
    (bytevector-delete (char->integer #\.) (->bv ".foo.bar.") 2 4)))
(test-assert
  "bytevector-delete charset, start and end index"
  (bytevector=?
    (->bv "oo")
    (bytevector-delete
      u8-set:punctuation
      (->bv ".foo.bar.")
      2
      4)))
(test-assert
  "bytevector-delete pred, start and end index"
  (bytevector=?
    #vu8()
    (bytevector-delete u8-alphabetic? (->bv ".foo.bar.") 2 4)))
(test-assert
  "bytevector-delete"
  (bytevector=? #vu8() (bytevector-delete (char->integer #\.) (->bv "."))))
(test-assert
  "bytevector-delete"
  (bytevector=? #vu8() (bytevector-delete (char->integer #\.) (->bv ".."))))
(test-assert
  "bytevector-delete"
  (bytevector=? #vu8() (bytevector-delete (char->integer #\.) (->bv "..."))))
(test-assert
  "bytevector-delete"
  (bytevector=? (->bv "x") (bytevector-delete (char->integer #\.) (->bv ".x"))))
(test-assert
  "bytevector-delete"
  (bytevector=? (->bv "x") (bytevector-delete (char->integer #\.) (->bv "..x"))))
(test-assert
  "bytevector-delete"
  (bytevector=? (->bv "x") (bytevector-delete (char->integer #\.) (->bv "...x"))))
(test-assert
  "bytevector-delete"
  (bytevector=? (->bv "x") (bytevector-delete (char->integer #\.) (->bv "x."))))
(test-assert
  "bytevector-delete"
  (bytevector=? (->bv "x") (bytevector-delete (char->integer #\.) (->bv "x.."))))
(test-assert
  "bytevector-delete"
  (bytevector=? (->bv "x") (bytevector-delete (char->integer #\.) (->bv "x..."))))
(test-assert
  "bytevector-delete"
  (bytevector=? (->bv "x") (bytevector-delete (char->integer #\.) (->bv "...x..."))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    #vu8()
    (bytevector-delete u8-set:punctuation (->bv "."))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    #vu8()
    (bytevector-delete u8-set:punctuation (->bv ".."))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    #vu8()
    (bytevector-delete u8-set:punctuation (->bv "..."))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    (->bv "x")
    (bytevector-delete u8-set:punctuation (->bv ".x"))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    (->bv "x")
    (bytevector-delete u8-set:punctuation (->bv "..x"))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    (->bv "x")
    (bytevector-delete u8-set:punctuation (->bv "...x"))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    (->bv "x")
    (bytevector-delete u8-set:punctuation (->bv "x."))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    (->bv "x")
    (bytevector-delete u8-set:punctuation (->bv "x.."))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    (->bv "x")
    (bytevector-delete u8-set:punctuation (->bv "x..."))))
(test-assert
  "bytevector-delete"
  (bytevector=?
    (->bv "x")
    (bytevector-delete u8-set:punctuation (->bv "...x..."))))

;; contains
(test-assert "bytevector-contains"
	     (bytevector-contains (->bv "The brown fox jumped...") 
				  (->bv "e b")))
(test-assert "bytevector-contains index start"
	     (bytevector-contains (->bv "The brown fox jumped...") 
				  (->bv "e b") 2))
(test-assert "bytevector-contains index start end"
	     (not (bytevector-contains (->bv "The brown fox jumped...") 
				       (->bv "e b") 2 3)))
(test-assert "bytevector-contains index start end (2)"
	     (bytevector-contains (->bv "The brown fox jumped...") 
				  (->bv "e b") 2 5))
(test-assert "bytevector-contains index start end start"
	     (bytevector-contains (->bv "The brown fox jumped...") 
				  (->bv "e b") 2 5 0))
(test-assert "bytevector-contains index start end start end"
	     (bytevector-contains (->bv "The brown fox jumped...") 
				  (->bv "e b") 2 5 0 2))

;; others
(test-equal "align-bytevectors (1)" '(#vu8(1 2 3 4 5) #vu8(6 7 8 9 3) #vu8(4))
	    (align-bytevectors '(#vu8(1 2 3) #vu8(4 5 6 7 8 9) #vu8(3 4)) 5))
(test-equal "align-bytevectors (2)" '(#vu8(1 2 3 4 5) #vu8(6 7 8 9 3) #vu8(4))
	    (align-bytevectors '(#vu8(1 2 3 4 5) #vu8(6 7 8 9) #vu8(3 4)) 5))


(test-equal "\x1;\x2;\x3;\x4;\x5;" (bytevector->escaped-string #vu8(1 2 3 4 5)))
(test-end)
