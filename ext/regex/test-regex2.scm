;; -*- coding: utf-8 -*-
(add-dynamic-load-path "./build")
(add-load-path "./sitelib")
(load-dynamic-library "sagittarius--regex2")

(library (regex test)
    (export run-regex-test)
    (import (srfi :64 testing)
	    (srfi :13 strings)
	    (srfi :1 lists)
	    (rnrs)
	    (sagittarius regex2 impl))

  (define (generic-match&list match-func pat text flags)
    (let* ((rx (compile-regex pat (if (null? flags) 0 (car flags))))
	   (m  (regex-matcher rx text)))
      (cond ((match-func m)
	     (map (lambda (n)
		    (regex-group m n))
		  (iota (regex-capture-count m))))
	    (else #f))))
      

  (define (run-looking-at-test)
    (let ((match&list (lambda (pat text . flags)
			(generic-match&list regex-looking-at pat text flags))))
      ;; basics
      (test-equal "a" '("a")
		  (match&list "a" "a"))
      (test-equal "A" #f
		  (match&list "a" "A"))
      (test-equal "Aa" '("a")
		  (match&list "a" "Aa"))
      (test-equal "a(null)" #f ;; input null str
		  (match&list "a" ""))
      (test-equal "a(contains null)" #f ;; input includes NUL character
		  (match&list "a" (string (integer->char 0) #\a #\b)))

      (test-equal "abc" '("abc")
		  (match&list "abc" "abc"))
      (test-equal "abc" #f
		  (match&list "abc" "abbc"))
      (test-equal "abc" '("abc")
		  (match&list "abc" "babcd"))

      (test-equal "abc|de" '("abc")
		  (match&list "abc|de" "babce"))
      (test-equal "abc|de" '("de")
		  (match&list "abc|de" "abdec"))

      (test-equal "a|b|c" '("a")
		  (match&list "a|b|c" "abc"))
      (test-equal "a|b|c" '("b")
		  (match&list "a|b|c" "bac"))
      (test-equal "a|b|c" #f
		  (match&list "a|b|c" "def"))

      (test-equal "|abc" '("")
		  (match&list "|abc" "abc"))
      (test-equal "abc|" '("abc")
		  (match&list "abc|" "abc"))
      (test-equal "abc|" '("")
		  (match&list "abc|" "abd"))
      ;; parens
      (test-equal "a(b)c" '("abc" "b")
		  (match&list "a(b)c" "abc"))
      (test-equal "a((b)(c))" '("abc" "bc" "b" "c")
		  (match&list "a((b)(c))" "abc"))
      (test-equal "a((((b))))c" '("abc" "b" "b" "b" "b")
		  (match&list "a((((b))))c" "abc"))
      (test-equal "a((((b))))c" '#f
		  (match&list "a((((b))))c" "a(b)c"))
      (test-equal "a\\(" '("a(")
		  (match&list "a\\(" "a("))
      (test-equal "a()b" '("ab" "")
		  (match&list "a()b" "ab"))
      (test-equal "a()()b" '("ab" "" "")
		  (match&list "a()()b" "ab"))
      (test-equal "(we|wee|week|frob)(knights|night|day)"
		  '("weeknights" "wee" "knights")
		  (match&list "(we|wee|week|frob)(knights|night|day)"
			      "weeknights"))
      (test-equal "aa|(bb)|cc" '("aa" #f)
		  (match&list "aa|(bb)|cc" "aabb"))
      (test-equal "aa|(bb)|cc" '("bb" "bb")
		  (match&list "aa|(bb)|cc" "abbaa"))
      (test-equal "aa|(bb)|cc" '("cc" #f)
		  (match&list "aa|(bb)|cc" "bccaa"))
      (test-equal "aa|a(b)|cc" '("ab" "b")
		  (match&list "aa|a(b)|cc" "abaab"))
      (test-equal "aa|a(b)" '("ab" "b")
		  (match&list "aa|a(b)" "abaab"))
      (test-equal "aa|(a(b))|ac" '("ab" "ab" "b")
		  (match&list "aa|(a(b))|cc" "abaabcc"))
      (test-equal "(ab)|ac" '("ab" "ab")
		  (match&list "(ab)|ac" "aaaabcc"))
      (test-equal "(a(b))|ac" '("ab" "ab" "b")
		  (match&list "(a(b))|ac" "abaabcc"))
      (test-equal "ab|(ac)" '("ab" #f)
		  (match&list "ab|(ac)" "aaaabcc"))
      (test-equal "ab|(ac)" '("ac" "ac")
		  (match&list "ab|(ac)" "aaaacbc"))
      (test-equal "aa|(ab|(ac))|ad" '("ac" "ac" "ac")
		  (match&list "aa|(ab|(ac))|ad" "cac"))
      (test-equal "(aa|(a(b)|a(c))|ad)" '("ac" "ac" "ac" #f "c")
		  (match&list "(aa|(a(b)|a(c))|ad)" "cac"))
      (test-equal "(.)*" '("abc" "c")
		  (match&list "(.)*" "abc"))
      (test-equal "(a([^a])*)*" '("abcaBC" "aBC" "C")
		  (match&list "(a([^a])*)*" "abcaBC"))
      (test-equal "b|()|a" '("" "")
		  (match&list "b|()|a" "cac"))
      ;; simple meta      
      (test-equal "a.c" '("abc")
		  (match&list "a.c" "abc"))
      (test-equal "a.." '("abc")
		  (match&list "a.." "abc"))
      (test-equal "a.." #f
		  (match&list "a.." "ab"))
      (test-equal "..." #f
		  (match&list "..." "ab"))
      (test-equal "." '("a")
		  (match&list "." "abc"))
      (test-equal "." #f
		  (match&list "." ""))
      ;; anchors
      (test-equal "^abc" '("abc")
	     (match&list "^abc" "abcd"))
      (test-equal "^abc" #f
	     (match&list "^abc" "aabcd"))
      ;; Gauche accept ^^ as start-anchor and #\^, however Perl and CL-PPCRE
      ;; parse it as two start-anchors. we do it defact standard (Perl) way.
      (test-equal "^^" '("")
	     (match&list "^^" "^^abc"))
      ;; so this test matches the first zero-width as well.
      (test-equal "^^" '("")
	     (match&list "^^" "a^^c"))
      (test-equal "^abc|def" '("abc")
	     (match&list "^abc|def" "abc"))
      (test-equal "^abc|def" #f
	     (match&list "^abc|def" "zabc"))
      (test-equal "^abc|def" '("def")
	     (match&list "^abc|def" "zabcdef"))
      (test-equal "abc|^def" '("def")
	     (match&list "abc|^def" "defabc"))
      (test-equal "abc|^def" '("abc")
	     (match&list "abc|^def" "abcdef"))
      (test-equal "abc|^def" '("def")
	     (match&list "abc|^def" "defabbc"))
      (test-equal "abc|^def" #f
	     (match&list "abc|^def" "adefbc"))
      (test-equal "^(abc|def)" '("abc" "abc")
	     (match&list "^(abc|def)" "abc"))
      (test-equal "^(abc|def)" #f
	     (match&list "^(abc|def)" "aabc"))
      (test-equal "(^abc|def)" '("abc" "abc")
	     (match&list "(^abc|def)" "abcdef"))
      (test-equal "(^abc|def)" '("def" "def")
	     (match&list "(^abc|def)" "^abcdef"))
      ;; the same as above comment. ^ is always start-anchor
      (test-equal "a(^bc|def)" #f
	     (match&list "a(^bc|def)" "a^bcdef"))
      (test-equal "a(^bc|def)" #f
	     (match&list "a(^bc|def)" "abcdef"))
      (test-equal "^" '("")
	     (match&list "^" "hoge"))
      (test-equal "$" '("")
	     (match&list "$" "hoge"))
      (test-equal "abc$" '("abc")
	     (match&list "abc$" "bcabc"))
      (test-equal "abc$" #f
	     (match&list "abc$" "abcab"))
      (test-equal "^abc$" '("abc")
	     (match&list "^abc$" "abc"))
      ;; $ is always end-anchor as well.
      (test-equal "abc$$" '("abc")
	     (match&list "abc$$" "abc"))
      (test-equal "abc$$" #f
	     (match&list "abc$$" "abc$"))
      (test-equal "$$" '("")
	     (match&list "$$" "abc$"))
      (test-equal "^$" '("")
	     (match&list "^$" ""))
      (test-equal "^$" #f
	     (match&list "^$" "a"))
      ;; the same as comment above
      (test-equal "^^$$" #f
	     (match&list "^^$$" "^$"))
      (test-equal "abc$|def" '("abc")
	     (match&list "abc$|def" "abc"))
      (test-equal "abc$|def" '("def")
	     (match&list "abc$|def" "defabc"))
      (test-equal "^abc|def$" '("abc")
	     (match&list "^abc|def$" "abcdef"))
      (test-equal "^abc|def$" #f
	     (match&list "^abc|def$" "defabc"))
      (test-equal "^abc|def$" #f
	     (match&list "^abc|def$" "defabc"))
      (test-equal "(^abc|def$)" '("def" "def")
	     (match&list "(^abc|def$)" "aaadef"))
      (test-equal "(^abc|def$)$" '("def" "def")
	     (match&list "(^abc|def$)$" "aaadef"))
      (test-equal "(^abc|def$)$" #f
	     (match&list "(^abc|def$)$" "aaadef$"))
      (test-equal "(abc$|def)$" '("abc" "abc")
	     (match&list "(abc$|def)$" "aaabc"))
      (test-equal "(abc$|def)$" #f
	     (match&list "(abc$|def)$" "aaabc$"))
      (test-equal "a$b" #f
	     (match&list "a$b" "aa$bb"))
      (test-equal "ab\\$" '("ab$")
	     (match&list "ab\\$" "ab$cd"))
      ;; backslash escape
      (test-equal "a\\*c" '("a*c")
		  (match&list "a\\*c" "a*c"))
      (test-equal "a\\.c" '("a.c")
		  (match&list "a\\.c" "a.c"))
      (test-equal "a\\.c" #f
		  (match&list "a\\.c" "abc"))
      (test-equal "a\\\\b" '("a\\b")
		  (match&list "a\\\\b" "a\\b"))
      (test-equal "a\\\\\\*b" '("a\\*b")
		  (match&list "a\\\\\\*b" "a\\*b"))
      (test-equal "a\\jc" '("ajc")
		  (match&list "a\\jc" "ajc"))
      (test-equal "a\\\\bc" '("a\\bc")
		  (match&list "a\\\\bc" "a\\bc"))
      (test-equal "a\\[b" '("a[b")
		  (match&list "a\\[b" "a[b"))
      ;; word boundary      
      (test-equal ".z\\b" '("oz")
		  (match&list ".z\\b" "bzbazoz ize"))
      (test-equal "\\b.z" '("iz")
		  (match&list "\\b.z" "brzbazoz ize"))
      (test-equal ".z\\B" '("iz")
		  (match&list ".z\\B" "bz baz oz ize"))
      (test-equal "\\B.z" '("az")
		  (match&list "\\B.z" "bz baz oz ize"))
      ;; repetitions      
      (test-equal "ab*c" '("abc")
		  (match&list "ab*c" "abc"))
      (test-equal "ab*c" '("ac")
		  (match&list "ab*c" "ac"))
      (test-equal "ab*c" '("abbbc")
		  (match&list "ab*c" "abbbc"))
      (test-equal "ab*c" '("abbc")
		  (match&list "ab*c" "abbabaabbc"))
      (test-equal "ab+c" '("abc")
		  (match&list "ab+c" "abc"))
      (test-equal "ab+c" '("abbc")
		  (match&list "ab+c" "abbc"))
      (test-equal "ab+c" '("abbc")
		  (match&list "ab+c" "abbabaabbc"))
      (test-equal "ab?c" '("abc")
		  (match&list "ab?c" "abc"))
      (test-equal "ab?c" '("ac")
		  (match&list "ab?c" "abbaac"))
      (test-equal "a.*c" '("abc")
		  (match&list "a.*c" "abc"))
      (test-equal "a.*c" '("aabcabcabcabcc")
		  (match&list "a.*c" "zaabcabcabcabcczab"))
      (test-equal "a(b*|c)d" '("abbd" "bb")
		  (match&list "a(b*|c)d" "abbd"))
      (test-equal "a(b*|c)d" '("ad" "")
		  (match&list "a(b*|c)d" "ad"))
      (test-equal "a(b*|c)d" '("acd" "c")
		  (match&list "a(b*|c)d" "acd"))
      (test-equal "a(b*|c)d" #f
		  (match&list "a(b*|c)d" "abcd"))
      (test-equal "a.*c" '("ac")
		  (match&list "a.*c" "bacbababbbbadbaba"))
      (test-equal "a.*c" #f
		  (match&list "a.*c" "abaaaabababbadbabdba"))
      ;; repetitions (non-greedy)      
      (test-equal "ab*?." '("ab")
		  (match&list "ab*?." "abc"))
      (test-equal "ab*?." '("ac")
		  (match&list "ab*?." "ac"))
      (test-equal "a.*?c" '("abbbc")
		  (match&list "a.*?c" "abbbc"))
      (test-equal "a.*?a" '("abba")
		  (match&list "a.*?a" "abbabaabbc"))
      (test-equal "<.*?>" '("<tag1>")
		  (match&list "<.*?>" "<tag1><tag2><tag3>"))

      (test-equal "ab+?." '("abc")
		  (match&list "ab+?." "abc"))
      (test-equal "ab+?." '("abb")
		  (match&list "ab+?." "abbc"))
      (test-equal "a.+?a" '("abba")
		  (match&list "a.+?a" "abbabaabbc"))
      (test-equal "<.+?>" '("<><tag1>")
		  (match&list "<.+?>" " <><tag1><tag2>"))

      (test-equal "ab??c" '("abc")
		  (match&list "ab??c" "abc"))
      (test-equal "ab??c" '("ac")
		  (match&list "ab??c" "abbaac"))
      (test-equal "ab??." '("ab")
		  (match&list "ab??." "abbaac"))
      (test-equal "a(hoge)??hoge" '("ahoge" #f)
		  (match&list "a(hoge)??hoge" "ahogehoge"))
      (test-equal "(foo)??bar" '("foobar" "foo")
		  (match&list "(foo)??bar" "foobar"))
      (test-equal "(foo)??bar" '("foobar" "foo")
		  (match&list "(foo)??bar" "foofoobar"))
      (test-equal "(foo)*?bar" '("foofoobar" "foo")
		  (match&list "(foo)*?bar" "foofoobar"))
      ;; character class      
      (test-equal "a[bc]d" '("abd")
		  (match&list "a[bc]d" "abd"))
      (test-equal "a[bc]d" '("acd")
		  (match&list "a[bc]d" "acd"))
      (test-equal "a[bc]d" #f
		  (match&list "a[bc]d" "aed"))
      (test-equal "a[a-z]d" '("aed")
		  (match&list "a[a-z]d" "aed"))
      (test-equal "a[a-z]d" #f
		  (match&list "a[a-z]d" "aEd"))
      (test-equal "a[]]d" '("a]d")
		  (match&list "a[]]d" "a]d"))
      (test-equal "a[]-]d" '("a-d")
		  (match&list "a[]-]d" "a-d"))
      (test-equal "a[]-^]d" #f
		  (match&list "a[]-^]d" "a-d"))
      (test-equal "a[]-^]d" '("a]d")
		  (match&list "a[]-^]d" "a]d"))
      (test-equal "a[a-z-]d" '("a-d")
		  (match&list "a[a-z-]d" "a-d"))
      (test-equal "a[a-z-]d" '("afd")
		  (match&list "a[a-z-]d" "afd"))
      (test-equal "a[az-]d" '("a-d")
		  (match&list "a[az-]d" "a-d"))
      (test-equal "a[a-]d" '("a-d")
		  (match&list "a[a-]d" "a-d"))
      (test-equal "a[az-]d" #f
		  (match&list "a[az-]d" "afd"))
      (test-equal "a[az-]d" '("azd")
		  (match&list "a[az-]d" "azd"))
      (test-equal "a[^ab]c" '("acc")
		  (match&list "a[^ab]c" "abacc"))
      (test-equal "a[^]]c" '("abc")
		  (match&list "a[^]]c" "abc"))
      (test-equal "a[^]]c" #f
		  (match&list "a[^]]c" "a]c"))
      (test-equal "a[^^]c" '("abc")
		  (match&list "a[^^]c" "abc"))
      (test-equal "a[^^]c" #f
		  (match&list "a[^^]c" "a^c"))
      (test-equal "a[Bc]*d" '("aBccBd")
		  (match&list "a[Bc]*d" "aBccBd"))
      (test-equal "[a]b[c]" '("abc")
		  (match&list "[a]b[c]" "abc"))
      (test-equal "[abc]b[abc]" '("abc")
		  (match&list "[abc]b[abc]" "abc"))
      (test-equal "a[bc]d" '("abd")
		  (match&list "a[bc]d" "xyzaaabcaababdacd"))
      (test-equal "a[ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab](wee|week)(knights|night)"
		  '("aaaaabaaaabaaaabaaaabweeknights" "wee" "knights")
		  (match&list "a[ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab][ab](wee|week)(knights|night)"
			      "aaaaabaaaabaaaabaaaabweeknights"))
      (test-equal "[ab][cd][ef][gh][ij][kl][mn]"
		  '("acegikm")
		  (match&list "[ab][cd][ef][gh][ij][kl][mn]"
			      "xacegikmoq"))
      (test-equal "[ab][cd][ef][gh][ij][kl][mn][op]"
		  '("acegikmo")
		  (match&list "[ab][cd][ef][gh][ij][kl][mn][op]"
			      "xacegikmoq"))
      (test-equal "[ab][cd][ef][gh][ij][kl][mn][op][qr]"
		  '("acegikmoq")
		  (match&list "[ab][cd][ef][gh][ij][kl][mn][op][qr]"
			      "xacegikmoqy"))
      (test-equal "[ab][cd][ef][gh][ij][kl][mn][op][q]"
		  '("acegikmoq")
		  (match&list "[ab][cd][ef][gh][ij][kl][mn][op][q]"
			      "xacegikmoqy"))
      ;; {n,m}
      (test-equal "(\\d{2})(\\d{2})" '("1234" "12" "34")
		  (match&list "(\\d{2})(\\d{2})" "a12345b"))
      (test-equal "(\\d{2,})(\\d{2,})" '("12345" "123" "45")
		  (match&list "(\\d{2,})(\\d{2,})" "a12345b"))
      (test-equal "(\\d{2})(\\d{2,})" '("12345" "12" "345")
		  (match&list "(\\d{2})(\\d{2,})" "a12345b"))
      (test-equal "(\\d{1,3})(\\d{2,})" '("1234" "12" "34")
		  (match&list "(\\d{1,3})(\\d{2,})" "a1234b"))
      (test-equal "(\\d{1,3})(\\d{0,2})" '("1234" "123" "4")
		  (match&list "(\\d{1,3})(\\d{0,2})" "a1234b"))
      (test-equal "(\\d{2}){2}" '("1234" "34")
		  (match&list "(\\d{2}){2}" "a12345b"))
      ;; {n,m} (non-greedy)
      (test-equal "(\\d{2,}?)(\\d{2,}?)" '("1234" "12" "34")
		  (match&list "(\\d{2,}?)(\\d{2,}?)" "a12345b"))
      (test-equal "(\\d{2,})(\\d{2,}?)" '("12345" "123" "45")
		  (match&list "(\\d{2,})(\\d{2,}?)" "a12345b"))
      (test-equal "(\\d{2,}?)(\\d{2,})" '("12345" "12" "345")
		  (match&list "(\\d{2,}?)(\\d{2,})" "a12345b"))
      (test-equal "(\\d{1,3}?)(\\d{2,}?)" '("123" "1" "23")
		  (match&list "(\\d{1,3}?)(\\d{2,}?)" "a1234b"))
      (test-equal "(\\d{1,3}?)(\\d{0,2}?)" '("1" "1" "")
		  (match&list "(\\d{1,3}?)(\\d{0,2}?)" "a1234b"))
      ;; uncapturing group      
      (test-equal "a(?:b)*c(d)" '("abcd" "d")
		  (match&list "a(?:b)*c(d)" "abcdbcdefg"))
      (test-equal "a(?:bcd)*e(f)"  '("abcdbcdef" "f")
		  (match&list "a(?:bcd)*e(f)" "abcdbcdefg"))
      (test-equal "a(?:bcd)*e(f)" '("aef" "f")
		  (match&list "a(?:bcd)*e(f)" "aefg"))
      (test-equal "a(?:bcd)+e(f)" #f
		  (match&list "a(?:bcd)+e(f)" "aefg"))
      (test-equal "a(?:bc(de(?:fg)?hi)jk)?l" '("abcdefghijkl" "defghi")
		  (match&list "a(?:bc(de(?:fg)?hi)jk)?l" "abcdefghijkl"))
      (test-equal "a(?:bc(de(?:fg)?hi)jk)?l" '("abcdehijkl" "dehi")
		  (match&list "a(?:bc(de(?:fg)?hi)jk)?l" "abcdehijkl"))

      (test-equal "a(?i:bc)d" '("aBCd")
	     (match&list "a(?i:bc)d" "!aBCd!"))
      (test-equal "a(?i:bc)d" #f
	     (match&list "a(?i:bc)d" "!aBCD!"))
      (test-equal "a(?i:[a-z]+)d" '("aBcd")
	     (match&list "a(?i:[a-z]+)d" "!aBcd!"))
      (test-equal "a(?i:[a-z]+)d" #f
	     (match&list "a(?i:[a-z]+)d" "!ABcd!"))

      ;; uncapturing case insensitive
      (test-equal "A(?-i:Bc)D" '("ABcD")
		  (match&list "A(?-i:Bc)D" "!ABcD!"))
      (test-equal "A(?-i:Bc)D" #f
		  (match&list "A(?-i:Bc)D" "!ABcd!"))
      (test-equal "A(?-i:[A-Z]+)D" '("ABCD")
		  (match&list "A(?-i:[A-Z]+)D" "!ABCD!"))
      (test-equal "A(?-i:[A-Z]+)D" #f
		  (match&list "A(?-i:[A-Z]+)D" "!abCD!"))

      (test-equal "A(?-i:Bc)D" '("aBcd")
		  (match&list "A(?-i:Bc)D" "!aBcd!" CASE-INSENSITIVE))
      (test-equal "A(?-i:Bc)D" #f
		  (match&list "A(?-i:Bc)D" "!abCd!" CASE-INSENSITIVE))
      (test-equal "A(?-i:[A-Z]+)D" '("aBCd")
		  (match&list "A(?-i:[A-Z]+)D" "!aBCd!" CASE-INSENSITIVE))
      (test-equal "A(?-i:[A-Z]+)D" #f
		  (match&list "A(?-i:[A-Z]+)D" "!abcd!" CASE-INSENSITIVE))

      ;; multiline
      (test-equal "(?m:^[d-z]+)" '("def")
		  (match&list "(?m:^[d-z]+)" "abc\ndef"))
      (test-equal "(?-m:^[d-z]+)" #f
		  (match&list "(?-m:^[d-z]+)" "abc\ndef" MULTILINE))

      ;; dot all
      (test-equal "(?s:.*)" '("abc\ndef")
		  (match&list "(?s:.*)" "abc\ndef"))
      (test-equal "(?-s:.*)" '("abc")
		  (match&list "(?-s:.*)" "abc\ndef" DOTALL))

      ;; backreference
      (test-equal "^(.)\\1$" '("aa" "a")
		  (match&list "^(.)\\1$" "aa"))
      (test-equal "^(.)\\1$" #f
		  (match&list "^(.)\\1$" "ab"))
      (test-equal "(.+)\\1" '("123123" "123")
		  (match&list "(.+)\\1" "a123123j"))
      (test-equal "/(.+)\\1/i" #f
		  (match&list "(.+)\\1" "AbCAb1"))
      ;; possessive match
      (test-equal "(?>.*/)foo" #f
		  (match&list "(?>.*/)foo" "/this/is/a/long/line/"))
      (test-equal "(?>.*/)foo" '("/this/is/a/long/line/foo")
		  (match&list "(?>.*/)foo" "/this/is/a/long/line/foo"))
      (test-equal "(?>(\\.\\d\\d[1-9]?))\\d+" '(".230003938" ".23")
		  (match&list "(?>(\\.\\d\\d[1-9]?))\\d+" "1.230003938"))
      (test-equal "(?>(\\.\\d\\d[1-9]?))\\d+" '(".875000282" ".875")
		  (match&list "(?>(\\.\\d\\d[1-9]?))\\d+" "1.875000282"))
      (test-equal "(?>(\\.\\d\\d[1-9]?))\\d+" #f
		  (match&list "(?>(\\.\\d\\d[1-9]?))\\d+" "1.235"))
      (test-equal "^((?>\\w+)|(?>\\s+))*$" '("foo bar" "bar")
		  (match&list "^((?>\\w+)|(?>\\s+))*$" "foo bar"))
      (test-equal "a*+a" #f
		  (match&list "a*+a" "aaa"))
      (test-equal "a*+b" '("aab")
		  (match&list "a*+b" "aab"))
      (test-equal "a++a" #f
		  (match&list "a++a" "aaa"))
      (test-equal "a++b" '("aab")
		  (match&list "a++b" "aab"))
      (test-equal "(a?+)a" #f
		  (match&list "a?+a" "a"))
      (test-equal "(a?+)b" '("ab")
		  (match&list "a?+b" "ab"))
      ;; lookahead assertion
      (test-equal "^(?=ab(de))(abd)(e)" '("abde" "de" "abd" "e")
		  (match&list "^(?=ab(de))(abd)(e)" "abde"))
      (test-equal "^(?!(ab)de|x)(abd)(f)" '("abdf" #f "abd" "f")
		  (match&list "^(?!(ab)de|x)(abd)(f)" "abdf"))
      (test-equal "^(?=(ab(cd)))(ab)" '("ab" "abcd" "cd" "ab")
		  (match&list "^(?=(ab(cd)))(ab)" "abcd"))
      (test-equal "\\w+(?=\\t)" '("brown")
		  (match&list "\\w+(?=\\t)" "the quick brown\t fox"))
      (test-equal "foo(?!bar)(.*)" '("foolish see?" "lish see?")
		  (match&list "foo(?!bar)(.*)" "foobar is foolish see?"))
      (test-equal "(?:(?!foo)...|^.{0,2})bar(.*)" '("rowbar etc" " etc")
		  (match&list "(?:(?!foo)...|^.{0,2})bar(.*)"
			      "foobar crowbar etc"))
      (test-equal "(?:(?!foo)...|^.{0,2})bar(.*)" '("barrel" "rel")
		  (match&list "(?:(?!foo)...|^.{0,2})bar(.*)" "barrel"))
      (test-equal "(?:(?!foo)...|^.{0,2})bar(.*)" '("2barrel" "rel")
		  (match&list "(?:(?!foo)...|^.{0,2})bar(.*)" "2barrel"))
      (test-equal "(?:(?!foo)...|^.{0,2})bar(.*)" '("A barrel" "rel")
		  (match&list "(?:(?!foo)...|^.{0,2})bar(.*)" "A barrel"))
      (test-equal "^(\\D*)(?=\\d)(?!123)" '("abc" "abc")
		  (match&list "^(\\D*)(?=\\d)(?!123)" "abc456"))
      (test-equal "^(\\D*)(?=\\d)(?!123)" #f
		  (match&list "^(\\D*)(?=\\d)(?!123)" "abc123"))
      (test-equal "(?!^)abc" '("abc")
		  (match&list "(?!^)abc" "the abc"))
      (test-equal "(?!^)abc" #f
		  (match&list "(?!^)abc" "abc"))
      (test-equal "(?=^)abc" '("abc")
		  (match&list "(?=^)abc" "abc"))
      (test-equal "(?=^)abc" #f
		  (match&list "(?=^)abc" "the abc"))
      (test-equal "(\\.\\d\\d((?=0)|\\d(?=\\d)))" '(".23" ".23" "")
		  (match&list "(\\.\\d\\d((?=0)|\\d(?=\\d)))" "1.230003938"))
      (test-equal "(\\.\\d\\d((?=0)|\\d(?=\\d)))" '(".875" ".875" "5")
		  (match&list "(\\.\\d\\d((?=0)|\\d(?=\\d)))" "1.875000282"))
      (test-equal "(\\.\\d\\d((?=0)|\\d(?=\\d)))" #f
		  (match&list "(\\.\\d\\d((?=0)|\\d(?=\\d)))" "1.235"))
      (test-equal "^\\D*(?!123)" '("AB")
		  (match&list "^\\D*(?!123)" "ABC123"))
      (test-equal "^(\\D*)(?=\\d)(?!123)" '("ABC" "ABC")
		  (match&list "^(\\D*)(?=\\d)(?!123)" "ABC445"))
      (test-equal "^(\\D*)(?=\\d)(?!123)" #f
		  (match&list "^(\\D*)(?=\\d)(?!123)" "ABC123"))
      (test-equal "a(?!b)." '("ad")
		  (match&list "a(?!b)." "abad"))
      (test-equal "a(?!b)" '("a")
		  (match&list "a(?!b)" "abad"))
      (test-equal "a(?=d)." '("ad")
		  (match&list "a(?=d)." "abad"))
      (test-equal "a(?=c|d)." '("ad")
		  (match&list "a(?=c|d)." "abad"))
      ;; lookbehind
      (test-equal "(?<=a)b" #f
		  (match&list "(?<=a)b" "b"))
      (test-equal "(?<=a)b" '("b")
		  (match&list "(?<=a)b" "ab"))
      (test-equal "(?<=a+)b" '("b")
		  (match&list "(?<=a+)b" "aab"))
      (test-equal "(?<=x[yz])b" '("b")
		  (match&list "(?<=x[yz])b" "xzb"))
      (test-equal "(?<=zyx)b" #f
		  (match&list "(?<=zyx)b" "xyzb"))
      (test-equal "(?<=[ab]+)c" '("c")
		  (match&list "(?<=[ab]+)c" "abc"))
      (test-equal "(?<!<[^>]+)foo" #f
		  (match&list "(?<!<[^>]*)foo" "<foo>"))
      (test-equal "(?<!<[^>]+)foo" '("foo")
		  (match&list "(?<!<[^>]*)foo" "<bar>foo"))
      (test-equal "(?<=^a)b" '("b")
		  (match&list "(?<=^a)b" "ab"))
      (test-equal "(?<=^)b" #f
		  (match&list "(?<=^)b" "ab"))
      (test-equal "(?<=^)b" '("b")
		  (match&list "(?<=^)b" "b"))
      (test-equal ".(?<=^)b" #f
		  (match&list ".(?<=^)b" "a^b"))
      (test-equal "(?<=^a$)" '("")
		  (match&list "(?<=^a$)" "a"))
      (test-equal "(?<=^a$)b" #f
		  (match&list "(?<=^a$)b" "a$b"))
      (test-equal "(?<=(a))b" '("b" "a")
		  (match&list "(?<=(a))b" "ab"))
      (test-equal "(?<=(a)(b))c" '("c" "a" "b")
		  (match&list "(?<=(a)(b))c" "abc"))
      (test-equal "(?<=(a)|(b))c" '("c" #f "b")
		  (match&list "(?<=(a)|(b))c" "bc"))
      (test-equal "(?<=(?<!foo)bar)baz" '("baz")
		  (match&list "(?<=(?<!foo)bar)baz" "abarbaz"))
      (test-equal "(?<=(?<!foo)bar)baz" #f
		  (match&list "(?<=(?<!foo)bar)baz" "foobarbaz"))
      (test-equal "(?<=\\d{3})(?<!999)foo" '("foo")
		  (match&list "(?<=\\d{3})(?<!999)foo" "865foo"))
      (test-equal "(?<=\\d{3})(?<!999)foo" #f
		  (match&list "(?<=\\d{3})(?<!999)foo" "999foo"))
      ;; Gauche does not support this, Perl is not allow to use veriable length
      ;; lookbehind, however Perl returned "" with #/(?<=(?>aaaa))/ expression
      ;; which input was "aaaa". So we return "" in this expression.
      (test-equal "(?<=(?>a*))" '("")
		  (match&list "(?<=(?>a*))" "aaaa"))
      (test-equal "(abc)...(?<=\\1)" '("abcabc" "abc")
		  (match&list "(abc)...(?<=\\1)" "abcabc"))
      (test-equal "(abC)...(?<=\\1)" '("abCAbc" "abC")
		  (match&list "(abC)...(?<=\\1)" "abCAbc" CASE-INSENSITIVE))

      ;; named group
      (test-equal "(?<foo>a)" '("a" "a")
		  (match&list "(?<foo>a)" "a"))
      (test-equal "(?<foo>a)(?<bar>.*)" '("abcd" "a" "bcd")
		  (match&list "(?<foo>a)(?<bar>.*)" "abcd"))
      (test-equal "(?<foo>^a$)" '("a" "a")
		  (match&list "(?<foo>^a$)" "a"))
      (test-equal "(?<foo>^a$)" #f
		  (match&list "(?<foo>^a$)" "ab"))
      (test-equal "(?<name-with-hyphen>a)" '("a" "a")
		  (match&list "(?<name-with-hyphen>a)" "a"))
      (test-equal "(?<foo>.+)\\k<foo>" '("abcabc" "abc")
		  (match&list "(?<foo>.+)\\k<foo>" "abcabc"))
      (test-equal "(?<foo>.+)\\k<foo>" #f
		  (match&list "(?<foo>.+)\\k<foo>" "abcdef"))

      (let ((regex (lambda (p t)
		     (let ((m (regex-matcher (compile-regex p 0) t)))
		       (regex-looking-at m)
		       m))))
	(test-equal "regex-before" "abc"
		    (regex-before (regex "(?<foo>def)" "abcdefghi")))
	(test-equal "regex-after" "ghi"
		    (regex-after (regex "(?<foo>def)" "abcdefghi")))
	(test-equal "regex-first" 3
		    (regex-first (regex "(?<foo>def)" "abcdefghi")))
	(test-equal "regex-last" 6
		    (regex-last (regex "(?<foo>def)" "abcdefghi"))))


      ;; conditional subexpression
      (test-equal "(a)(?(1)b)" '("ab" "a")
	     (match&list "(a)(?(1)b)" "ab"))
      (test-equal "(a)(?(1)b)" #f
	     (match&list "(a)(?(1)b)" "aa"))
      (test-equal "(a)(?(1)b|c)" #f
	     (match&list "(a)(?(1)b)" "ac"))
      (test-equal "(a)?(?(1)b|c)" #f
	     (match&list "(a)?(?(1)b|c)" "xb"))
      (test-equal "(a)?(?(1)b|c)" '("c" #f)
		  (match&list "(a)?(?(1)b|c)" "xc"))
      (test-equal "(?(?<=a)b)" '("b")
		  (match&list "(?(?<=a)b)" "ab"))
      (test-equal "(?(?<=a)b)" #f
		  (match&list "(?(?<=a)b)" "ac"))
      (test-equal "(?(?<=a)b)" #f
		  (match&list "(?(?<=a)b)" "xb"))
      (test-equal "(?(?<=a)b|c)" '("b")
		  (match&list "(?(?<=a)b)" "ab"))
      (test-equal "(?(?<=a)b|c)" #f
		  (match&list "(?(?<=a)b)" "ac"))
      (test-error "(?(?a)b|c)"
		  (compile-regex "(?(?a)b|c)" 0))
      (test-equal "()(?(1))" '("" "")
		  (match&list "()(?(1))" ""))

      )
    )

  ;; I'm so lazy to write this test, so just a few
  (define (run-matches-test)
    (let ((match&list (lambda (pat text . flags)
			(generic-match&list regex-matches pat text flags))))
      ;; only basics
      (test-equal "abcde" '("abcde")
		  (match&list "abcde" "abcde"))
      ;; regex-matches only matches whole input string
      (test-equal "abcde" #f
		  (match&list "abcde" "aabcde"))

      (test-equal "abcde" #f
		  (match&list "abcde" "abcdee"))
      ;; for extended pattern
      (test-equal "(abc)\\1" '("abcabc" "abc")
		  (match&list "(abc)\\1" "abcabc"))
      (test-equal "(abc)\\1" #f
		  (match&list "(abc)\\1" "aabcabc"))
      (test-equal "(abc)\\1" #f
		  (match&list "(abc)\\1" "abcabcd"))
      )
    )

  (define (run-replace-test)
    (let ((regex (lambda (pat text)
		   (regex-matcher (compile-regex pat 0) text))))
      (test-equal "regex-replace-first" "abc|def|ghi"
		  (regex-replace-first (regex "def|DEF" "abcdefghi") "|$0|"))

      (test-equal "regex-replace-first" "abc|$0|ghi"
		  (regex-replace-first (regex "def|DEF" "abcdefghi") "|\\$0|"))
      (test-equal "regex-replace-first"
		  "abraabra**brabra**brabrabracadabrabrabra"
		  (regex-replace-first
		   (regex "a((bra)+)cadabra"
			  "abraabraabrabracadabrabrabrabracadabrabrabra")
		   "**$1**"))

      (test-equal "regex-replace-all" "abraabra**brabra**br**brabra**brabra"
		  (regex-replace-all
		   (regex "a((bra)+)cadabra"
			  "abraabraabrabracadabrabrabrabracadabrabrabra")
		   "**$1**"))

      )
    )

  (define (run-regex-test)
    (run-looking-at-test)
    (run-matches-test)
    (run-replace-test)
    )

)

(import (regex test)
	(srfi :64))

;; srfi-64 default implementation does not report detail error.
;; so creates own test-runner
(define (test-on-test-end-detail runner)
  (define (%test-write-result1 pair port)
    (display "  " port)
    (display (car pair) port)
    (display ": " port)
    (write (cdr pair) port)
    (newline port))
  (let ((log  (test-runner-aux-value runner))
	(kind (test-result-ref runner 'result-kind)))
    (when (memq kind '(xpass fail))
      (let* ((results (test-result-alist runner))
	     (source-file (assq 'source-file results))
	     (source-line (assq 'source-line results))
	     (test-name (assq 'test-name results)))
	(when (or source-file source-line)
	  (if source-file (display (cdr source-file)))
	  (display ":")
	  (if source-line (display (cdr source-line)))
	  (display ":"))
	(display (if (eq? kind 'xpass) "XPASS" "FAIL"))
	(when test-name
	  (display " ")(display (cdr test-name)))
	(newline))
      (let ((expected (test-result-ref runner 'expected-value))
	    (actual   (test-result-ref runner 'actual-value)))
	(display #\tab)(display "expected value: ")(write expected)(newline)
	(display #\tab)(display "  actual value: ")(write actual)(newline)))
    (when (output-port? log)
      (display "Test end:" log)
      (newline log)
      (let loop ((list (test-result-alist runner)))
	(if (pair? list)
	    (let ((pair (car list)))
	      ;; Write out properties not written out by on-test-begin.
	      (if (not (memq (car pair)
			     '(test-name source-file source-line source-form)))
		  (%test-write-result1 pair log))
	      (loop (cdr list))))))))

(define (test-runner-detail)
  (let ((runner (test-runner-simple)))
      (test-runner-on-test-end! runner test-on-test-end-detail)
      runner))

(test-runner-factory test-runner-detail)

(test-begin "sagittarius functionality tests start")
(run-regex-test)
(test-end)