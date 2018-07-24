;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/parser.scm - YAML parser
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(library (text yaml parser)
    (export parse-yaml
	    ;; for testing
	    l-comment s-l-comments
	    l-directive

	    *parsing-context*
	    make-parsing-context
	    )
    (import (rnrs)
	    (peg)
	    (sagittarius generators)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :127 lseqs))

;; 4.1 Production Parameters
;; Indentation: n or m
(define *n* (make-parameter 0))
(define *m* (make-parameter 0))

;; Context: c
;; block styles or flow styles
(define *c* (make-parameter 'block-in))

;; (Block) Chomping: t
;; strip, clip or keep
(define *t* (make-parameter 'strip))

;; need this for start of line (sol)
(define-record-type parsing-context
  (fields input))
(define *parsing-context* (make-parameter #f))

(define ($in-cset s) ($satisfy (lambda (c) (char-set-contains? s c))))
(define ($token s) (apply $seq (map $eqv? (string->list s))))

;; [1] c-printable ::= #x9 | #xA | #xD | [#x20-#x7E]          /* 8 bit */
;;                   | #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD] /* 16 bit */
;;                   | [#x10000-#x10FFFF]                     /* 32 bit */
(define +printable-char-set+
  (char-set-union
   (char-set #\x9)
   (char-set #\xA)
   (char-set #\xD)
   (ucs-range->char-set #x20 #x7F)
   (char-set #\x85)
   (ucs-range->char-set #xA0 #xD800)
   (ucs-range->char-set #xE000 #xFFFE)
   (ucs-range->char-set #x10000 #x110000)))
(define c-printable ($in-cset +printable-char-set+))

;; [2] nb-json ::= #x9 | [#x20-#x10FFFF]
(define +json-char-set+
  (char-set-union
   (char-set #\x9)
   (ucs-range->char-set #x20 #x110000)))
(define nb-json ($in-cset +json-char-set+))

;; [3] c-byte-order-mark ::= #xFEFF
;; we don care byte order mark unless it's absolutely needed

;; [4] c-sequence-entry ::= “-”
(define c-sequence-entry ($eqv? #\-))
;; [5] c-mapping-key ::= “?”
(define c-mapping-key ($eqv? #\?))
;; [6] c-mapping-value ::= “:”
(define c-mapping-value ($eqv? #\:))
;; [7] c-collect-entry  ::= “,”
(define c-collect-entry ($eqv? #\,))
;; [8] c-sequence-start ::= “[”
(define c-sequence-start ($eqv? #\[))
;; [9] c-sequence-end   ::= “]”
(define c-sequence-end ($eqv? #\]))
;; [10] c-mapping-start ::= “{”
(define c-mapping-start ($eqv? #\{))
;; [11] c-mapping-end   ::= “}”
(define c-mapping-end ($eqv? #\}))
;; [12] c-comment ::= "#"
(define c-comment ($eqv? #\#))
;; [13] c-anchor ::= “&”
(define c-anchor ($eqv? #\&))
;; [14] c-alias  ::= “*”
(define c-alias ($eqv? #\^))
;; [15] c-tag    ::= “!”
(define c-tag ($eqv? #\!))
;; [16] c-literal ::= “|”
(define c-literal ($eqv? #\|))
;; [17] c-folded  ::= “>”
(define c-folded ($eqv? #\>))
;; [18] c-single-quote ::= “'”
(define c-single-quote ($eqv? #\'))
;; [19] c-double-quote ::= “"”
(define c-double-quote ($eqv? #\"))
;; [20] c-directive ::= “%”
(define c-directive ($eqv? #\%))
;; [21] c-reserved ::= “@” | “`”
(define c-reserved ($satisfy (lambda (c) (memv c '(#\@ #\`)))))
;; [22] c-indicator ::= “-” | “?” | “:” | “,” | “[” | “]” | “{” | “}”
;;                    | “#” | “&” | “*” | “!” | “|” | “>” | “'” | “"”
;;                    | “%” | “@” | “`”
(define +indicator-set+ (string->char-set "-?:,[]{}#&*!|>'\"%@`"))
(define c-indicator ($in-cset +indicator-set+))
;; [23] c-flow-indicator ::= “,” | “[” | “]” | “{” | “}”
(define c-flow-indicator
  ($satisfy (lambda (c) (memv c '(#\, #\[ #\] #\{ #\})))))

;; [24] b-line-feed ::= #xA    /* LF */
(define b-line-feed ($eqv? #\xA))
;; [25] b-carriage-return ::= #xD    /* CR */
(define b-carriage-return ($eqv? #\xD))
;; [26] b-char ::= b-line-feed | b-carriage-return
(define b-char ($satisfy (lambda (c) (memv c '(#\xA #\xB)))))

;; [27] nb-char ::= c-printable - b-char - c-byte-order-mark
(define +nb-char-set+
  (char-set-difference +printable-char-set+ (char-set #\xA #\xD)))
(define nb-char ($in-cset +nb-char-set+))

;; [28] b-break ::= ( b-carriage-return b-line-feed ) /* DOS, Windows */
;;                | b-carriage-return                 /* MacOS upto 9.x */
;;                | b-line-feed                       /* UNIX, MacOS X */
(define b-break
  ($or ($seq b-carriage-return b-line-feed)
       b-carriage-return
       b-line-feed))
;; [29] b-as-line-feed ::= b-break
(define b-as-line-feed b-break)
;; [30] b-non-content ::= b-break
(define b-non-content b-break)

;; [31] s-space ::= #x20 /* SP */
(define s-space ($eqv? #\x20))
;; [32] s-tab ::= #x9  /* TAB */
(define s-tab ($eqv? #\x9))
;; [33] s-white ::= s-space | s-tab
(define s-white ($satisfy (lambda (c) (memv c '(#\x20 #\9)))))
;; [34] ns-char ::= nb-char - s-white
(define +ns-char-set+ (char-set-difference +nb-char-set+ (char-set #\x20 #\9)))
(define ns-char ($in-cset +ns-char-set+))

;; [35] ns-dec-digit ::= [#x30-#x39] /* 0-9 */
(define ns-dec-digit
  ($in-cset (char-set-intersection char-set:ascii char-set:digit)))
;; [36] ns-hex-digit ::=   ns-dec-digit
;;                     | [#x41-#x46] /* A-F */ | [#x61-#x66] /* a-f */
(define ns-hex-digit
  ($in-cset (char-set-intersection char-set:ascii char-set:hex-digit)))
;; [37] ns-ascii-letter ::= [#x41-#x5A] /* A-Z */ | [#x61-#x7A] /* a-z */
(define ns-ascii-letter
  ($in-cset (char-set-intersection char-set:ascii char-set:letter)))
;; [38] ns-word-char ::= ns-dec-digit | ns-ascii-letter | “-”
(define ns-word-char
  ($in-cset
   (char-set-intersection
    char-set:ascii
    (char-set-union char-set:letter+digit (char-set #\-)))))


;; [39] ns-uri-char ::= “%” ns-hex-digit ns-hex-digit | ns-word-char | “#”
;;                    | “;” | “/” | “?” | “:” | “@” | “&” | “=” | “+” | “$”
;;                    | “,” | “_” | “.” | “!” | “~” | “*” | “'” | “(” | “)”
;;                    | “[” | “]”
(define +misc-uri-char-set+ (string->char-set "#;/?:@&=+$,_.!~*'()[]"))
(define +flow-indicator-set+ (string->char-set "!,[]{}"))
(define percent-hex2 ($seq ($eqv? #\%) ns-hex-digit ns-hex-digit))
(define ns-uri-char
  ($or percent-hex2
       ns-word-char
       ($in-cset +misc-uri-char-set+)))
;; [40] ns-tag-char ::= ns-uri-char - “!” - c-flow-indicator
(define ns-tag-char
  ($or percent-hex2
       ns-word-char
       ($in-cset (char-set-difference +misc-uri-char-set+
				      +flow-indicator-set+))))
;; 5.7 Escaped Characters
;; I'm lazy to write all the rules here.
(define c-ns-esc-char
  ($seq ($eqv? #\\)
	($or ($do (($eqv? #\0)) ($return #\null))
	     ($do (($eqv? #\a)) ($return #\alarm))
	     ($do (($eqv? #\b)) ($return #\backspace))
	     ($do (($or ($eqv? #\t) ($eqv? #\x9))) ($return #\tab))
	     ($do (($eqv? #\n)) ($return #\newline))
	     ($do (($eqv? #\v)) ($return #\vtab))
	     ($do (($eqv? #\f)) ($return #\page))
	     ($do (($eqv? #\r)) ($return #\return))
	     ($do (($eqv? #\e)) ($return #\esc))
	     ($do (($eqv? #\x20)) ($return #\space))
	     ($do (($eqv? #\")) ($return #\"))
	     ($do (($eqv? #\/)) ($return #\/))
	     ($do (($eqv? #\\)) ($return #\\))
	     ($do (($eqv? #\N)) ($return #\x85))
	     ($do (($eqv? #\_)) ($return #\xA0))
	     ($do (($eqv? #\L)) ($return #\x2028))
	     ($do (($eqv? #\P)) ($return #\x2029))
	     ($do (($eqv? #\x))
		  (n1 ns-hex-digit)
		  (n2 ns-hex-digit)
		  ($return (integer->char
			    (string->number (string-append n1 n2) 16))))
	     ($do (($eqv? #\u))
		  (n1 ns-hex-digit)
		  (n2 ns-hex-digit)
		  (n3 ns-hex-digit)
		  (n4 ns-hex-digit)
		  ($return (integer->char
			    (string->number
			     (string-append n1 n2 n3 n4) 16))))
	     ($do (($eqv? #\u))
		  (n1 ns-hex-digit)
		  (n2 ns-hex-digit)
		  (n3 ns-hex-digit)
		  (n4 ns-hex-digit)
		  (n5 ns-hex-digit)
		  (n6 ns-hex-digit)
		  (n7 ns-hex-digit)
		  (n8 ns-hex-digit)
		  ($return (integer->char
			    (string->number
			     (string-append n1 n2 n3 n4 n5 n6 n7 n8) 16)))))))

;; starting here we need to consider context (for start of line... fxxk!!!)
(define (s-indent n) ($many s-space n n))
(define (s-indent< n) ($many s-space 0 (- n 1)))
(define (s-indent<= n) ($many s-space 0 n))

(define sol
  (let ()
    (define (sol? current original)
      (let loop ((prev #f) (now original))
	(cond ((null? now) #f)
	      ((eq? current now)
	       (case prev
		 ((#\return) (not (eq? (lseq-car current) #\newline)))
		 ((#\newline) #t)
		 (else #f)))
	      (else (loop (car now) (cdr now))))))
    (lambda (input)
      (let ((context (*parsing-context*)))
	(cond ((not (parsing-context? context))
	       (return-failure "No parsing context!" input))
	      ((eq? input (parsing-context-input context))
	       (return-result #t input))
	      ;; FIXME this is extreamly heavy...
	      ((sol? input (parsing-context-input context))
	       (return-result #t input))
	      (else
	       (return-failure "Not a start of line" input)))))))

;; [66] s-separate-in-line ::= s-white+ | /* Start of line */
(define s-separate-in-line ($or ($many s-white 1) sol))

;; [67] s-line-prefix(n,c) ::= c = block-out ⇒ s-block-line-prefix(n)
;;                             c = block-in  ⇒ s-block-line-prefix(n)
;;                             c = flow-out  ⇒ s-flow-line-prefix(n)
;;                             c = flow-in   ⇒ s-flow-line-prefix(n)
(define (s-line-prefix n c)
  ($cond ((eq? c 'block-out) (s-block-line-prefix n))
	 ((eq? c 'block-in)  (s-block-line-prefix n))
	 ((eq? c 'flow-out)  (s-flow-line-prefix n))
	 ((eq? c 'flow-in)   (s-flow-line-prefix n))
	 (else (assertion-violation 's-line-prefix "Unknown context" c))))

;; [68] s-block-line-prefix(n) ::= s-indent(n)
(define (s-block-line-prefix n) (s-indent n))
;; [69] s-flow-line-prefix(n) ::= s-indent(n) s-separate-in-line?
(define (s-flow-line-prefix n)
  ($seq (s-indent n) ($optional s-separate-in-line)))


;; [70] l-empty(n,c) ::= ( s-line-prefix(n,c) | s-indent(<n) )
;;                       b-as-line-feed
(define (l-empty n c)
  ($do (c ($or (s-line-prefix n c) (s-indent< n))) b-as-line-feed))

;; [71] b-l-trimmed(n,c) ::= b-non-content l-empty(n,c)+
(define (b-l-trimmed n c) ($seq b-non-content ($many (l-empty n c) 1)))

;; [72] b-as-space ::= b-break
(define b-as-space b-break)

;; [73] b-l-folded(n,c) ::= b-l-trimmed(n,c) | b-as-space
(define (b-l-folded n c) ($or (b-l-trimmed n c) b-as-space))


;; [74] s-flow-folded(n) ::= s-separate-in-line? b-l-folded(n,flow-in)
;;                           s-flow-line-prefix(n)
(define (s-flow-folded n)
  ($seq ($optional s-separate-in-line?)
	(b-l-folded n 'flow-in)
	(s-flow-line-prefix n)))


;; [75] c-nb-comment-text ::= “#” nb-char*
(define c-nb-comment-text
  ($do (($eqv? #\#)) (c* ($many nb-char)) ($return (list->string c*))))
;; [76] b-comment ::= b-non-content | /* End of file */
(define b-comment ($or b-non-content $eof))
;; [77] s-b-comment ::= ( s-separate-in-line c-nb-comment-text? )?
;;                      b-comment
(define s-b-comment ($seq ($optional s-separate-in-line
				     ($optional c-nb-comment-text))
			  b-comment))

;; [78] l-comment ::= s-separate-in-line c-nb-comment-text? b-comment
(define l-comment
  ($do s-separate-in-line
       (comment ($optional c-nb-comment-text '()))
       b-comment
       ($return `(comment ,comment))))

;; [79] s-l-comments ::= ( s-b-comment | /* Start of line */ )
;;                       l-comment*
(define (merge-comments c*)
  (list 'comment (string-join (map cadr c*) "\n")))
(define s-l-comments
  ($do (($or s-b-comment sol))
       (c* ($many l-comment))
       ($return (merge-comments c*))))


;; [80] s-separate(n,c) ::= c = block-out ⇒ s-separate-lines(n)
;;                          c = block-in  ⇒ s-separate-lines(n)
;;                          c = flow-out  ⇒ s-separate-lines(n)
;;                          c = flow-in   ⇒ s-separate-lines(n)
;;                          c = block-key ⇒ s-separate-in-line
;;                          c = flow-key  ⇒ s-separate-in-line
(define (s-separate n c)
  ($cond ((eq? c 'block-out) (s-separate-lines n))
	 ((eq? c 'block-in) (s-separate-lines n))
	 ((eq? c 'flow-out) (s-separate-lines n))
	 ((eq? c 'flow-in) (s-separate-lines n))
	 ((eq? c 'block-key) s-separate-in-line)
	 ((eq? c 'flow-key) s-separate-in-line)
	 (else (assertion-violation 's-separate "Uknown context" c))))
;; [81] s-separate-lines(n) ::=   ( s-l-comments s-flow-line-prefix(n) )
;;                            | s-separate-in-line
(define (s-separate-lines n)
  ($or ($seq s-l-comments (s-flow-line-prefix n))
       s-separate-in-line))

  ;; [84] ns-directive-name ::= ns-char+
(define ns-directive-name
  ($do (c* ($many ns-char 1)) ($return (list->string c*))))
;; [85] ns-directive-parameter ::= ns-char+
(define ns-directive-parameter
  ($do (c* ($many ns-char 1)) ($return (list->string c*))))
;; [83] ns-reserved-directive ::= ns-directive-name
;;                              ( s-separate-in-line ns-directive-parameter )*
;; TOOD should we show warning?
(define ns-reserved-directive
  ($do (n ns-directive-name)
       (p ($many ($seq s-separate-in-line ns-directive-parameter)))
       ($return `(,(string->symbol n) ,p))))

;; [87] ns-yaml-version ::= ns-dec-digit+ “.” ns-dec-digit+
(define ns-yaml-version
  ($do (major ($many ns-dec-digit 1))
       (($eqv? #\.))
       (minor ($many ns-dec-digit 1))
       ($return (list (string->number (list->string major))
		      (string->number (list->string minor))))))

;; [86] ns-yaml-directive ::= “Y” “A” “M” “L”
;;                            s-separate-in-line ns-yaml-version
(define ns-yaml-directive
  ($do (($token "YAML"))
       (v ($seq s-separate-in-line ns-yaml-version))
       ($return `(YAML ,v))))

;; [90] c-primary-tag-handle ::= “!”
(define c-primary-tag-handle ($do (($eqv? #\!)) ($return 'primay)))

;; [91] c-secondary-tag-handle ::= “!” “!”
(define c-secondary-tag-handle ($do (($token "!!")) ($return 'secondary)))

;; [92] c-named-tag-handle ::= “!” ns-word-char+ “!”
(define c-named-tag-handle
  ($do (($eqv?  #\!))
       (n ($many ns-word-char 1))
       (($eqv?  #\!))
       ($return (list->string n))))

;; [89] c-tag-handle ::= c-named-tag-handle
;;                     | c-secondary-tag-handle
;;                     | c-primary-tag-handle
(define c-tag-handle
  ($or c-named-tag-handle c-secondary-tag-handle c-primary-tag-handle))

;; [94] c-ns-local-tag-prefix ::= “!” ns-uri-char*
(define c-ns-local-tag-prefix
  ($do (($eqv? #\!)) (c* ($many ns-uri-char)) ($return (list->string c*))))
;; [95] ns-global-tag-prefix ::= ns-tag-char ns-uri-char*
(define ns-global-tag-prefix
  ($do (c ns-tag-char)
       (c* ($many ns-uri-char))
       ($return (list->string (cons c c*)))))
;; [93] ns-tag-prefix ::= c-ns-local-tag-prefix | ns-global-tag-prefix
(define ns-tag-prefix ($or c-ns-local-tag-prefix ns-global-tag-prefix))

;; [88] ns-tag-directive ::= “T” “A” “G”
;;                           s-separate-in-line c-tag-handle
;;                           s-separate-in-line ns-tag-prefix
(define ns-tag-directive
  ($do (($token "TAG"))
       (h ($seq s-separate-in-line c-tag-handle))
       (p ($seq s-separate-in-line ns-tag-prefix))
       ($return `(TAG ,h ,p))))

;; [82] l-directive ::= “%”
;;                    ( ns-yaml-directive
;;                    | ns-tag-directive
;;                    | ns-reserved-directive )
;;                    s-l-comments
(define l-directive
  ($do (($eqv? #\%))
       (d ($or ns-yaml-directive
	       ns-tag-directive
	       ns-reserved-directive))
       s-l-comments
       ($return d)))

;; TBD 6.9 and below sections...
;; [100] c-non-specific-tag ::= “!”
(define c-non-specific-tag ($eqv? #\!))
;; [99] c-ns-shorthand-tag ::= c-tag-handle ns-tag-char+
(define (resolve-tag tag-handle name) name)
(define c-ns-shorthand-tag
  ($do (th c-tag-handle)
       (c* ($many ns-tag-char 1))
       ($return `(tag ,(resolve-tag th (list->string c*))))))
       
;; [98] c-verbatim-tag ::= “!” “<” ns-uri-char+ “>”
(define c-verbatim-tag
  ($do (($token "!<"))
       (c* ($many ns-uri-char 1))
       (($eqv? #\>))
       ($return `(tag ,(list->string c*)))))
;; [97] c-ns-tag-property ::= c-verbatim-tag
;;                          | c-ns-shorthand-tag
;;                          | c-non-specific-tag
(define c-ns-tag-property
  ($or c-verbatim-tag
       c-ns-shorthand-tag
       c-non-specific-tag))

;; [102] ns-anchor-char ::= ns-char - c-flow-indicator
(define ns-anchor-char
  ($in-cset (char-set-difference +ns-char-set+ +flow-indicator-set+)))
;; [103] ns-anchor-name ::= ns-anchor-char+
(define ns-anchor-name
  ($do (c* ($many ns-anchor-char 1)) ($return (list->string c*))))
;; [101] c-ns-anchor-property ::= “&” ns-anchor-name
(define c-ns-anchor-property
  ($do (($eqv? #\&))
       (n ns-anchor-name)
       ($return `(anchor ,(list->string n)))))

;; [96] c-ns-properties(n,c) ::= ( c-ns-tag-property
;;                                 ( s-separate(n,c) c-ns-anchor-property )? )
;;                             | ( c-ns-anchor-property
;;                                 ( s-separate(n,c) c-ns-tag-property )? )
(define (c-ns-properties n c)
  ($or ($seq c-ns-tag-property
	     ($optional ($seq (s-separate n c) c-ns-anchor-property)))
       ($seq c-ns-anchor-property
	     ($optional ($seq (s-separate n c) c-ns-tag-property)))))

;; 7.1
;; [104] c-ns-alias-node ::= “*” ns-anchor-name
(define c-ns-alias-node
  ($do (($eqv? #\*)) (n ns-anchor-name) ($return `(alias ,n))))

;; [105] e-scalar ::= /* Empty */
(define e-scalar (lambda (l) (return-result 'null l)))
;; [106] e-node ::= e-scalar
(define e-node e-scalar)

;; [107] nb-double-char ::= c-ns-esc-char | ( nb-json - “\” - “"” )
(define nb-double-char
  ($or c-ns-esc-char
       ($in-cset (char-set-difference +json-char-set+ (char-set #\\ #\")))))
;; [108] ns-double-char ::= nb-double-char - s-white
(define ns-double-char
  ($seq ($peek ($not s-white)) nb-double-char))

;; [111] nb-double-one-line ::= nb-double-char*
(define nb-double-one-line
  ($do (c* ($many nb-double-char)) ($return (list->string c*))))

;; [112] s-double-escaped(n) ::= s-white* “\” b-non-content
;;                               l-empty(n,flow-in)* s-flow-line-prefix(n)
(define (s-double-escaped n)
  ($do (($many s-white))
       (($eqv? #\\))
       (br b-non-content)
       (($many (l-empty n 'flow-in)))
       ((s-flow-line-prefix n))
       ;; FIXME
       ($return "\n")))
       
;; [113] s-double-break(n) ::= s-double-escaped(n) | s-flow-folded(n)
(define (s-double-break n) ($or (s-double-escaped n) (s-flow-folded n)))
;; [114] nb-ns-double-in-line ::= ( s-white* ns-double-char )*
(define nb-ns-double-in-line ($many ($seq ($many s-white) ns-double-char)))
;; [115] s-double-next-line(n) ::= s-double-break(n)
;;                                 ( ns-double-char nb-ns-double-in-line
;;                                   ( s-double-next-line(n) | s-white* ) )?
(define (s-double-next-line n)
  ($seq (s-double-break n)
	($optional ($seq ns-double-char nb-ns-double-in-line
			 ($or (s-double-next-line n) ($many s-white))))))
;; [116] nb-double-multi-line(n) ::= nb-ns-double-in-line
;;                                   ( s-double-next-line(n) | s-white* )
(define (nb-double-multi-line n)
  ($seq nb-ns-double-in-line
	($or (s-double-next-line n) ($many s-white))))

;; [110] nb-double-text(n,c) ::= c = flow-out  ⇒ nb-double-multi-line(n)
;;                               c = flow-in   ⇒ nb-double-multi-line(n)
;;                               c = block-key ⇒ nb-double-one-line
;;                               c = flow-key  ⇒ nb-double-one-line  
(define (nb-double-one-line n c)
  ($cond ((eq? c 'flow-out) (nb-double-multi-line n))
	 ((eq? c 'flow-in) (nb-double-multi-line n))
	 ((eq? c 'flow-key) (nb-double-one-line n))
	 ((eq? c 'block-key) (nb-double-one-line n))
	 (else (assertion-violation 'nb-double-one-line "Unknown context" c))))
;; [109] c-double-quoted(n,c) ::= “"” nb-double-text(n,c) “"”
(define (c-double-quoted n c)
  ($do (($eqv? #\")) (text (nb-double-text n c)) (($eqv? #\")) ($return text)))


(define (parse-yaml input)
  (define lseq (generator->lseq (port->char-generator input)))
  (define context (make-parsing-context lseq))
  (parameterize ((*parsing-context* context))
    ;; parse toplevel
    ))
)
