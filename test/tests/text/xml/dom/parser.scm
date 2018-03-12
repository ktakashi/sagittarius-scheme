(import (rnrs)
	(text xml dom parser)
	(sagittarius generators)
	(peg)
	(srfi :127 lseqs)
	(srfi :64))

(test-begin "DOM parser")

(define (string->lseq s) (generator->lseq (string->generator s)))
(define-syntax test-parser
  (syntax-rules ()
    ((_ expected parser)
     (let-values (((r v n) parser))
       (test-assert '("parser result" parser) (parse-success? r))
       (test-equal '("parser next" parser) '() n)
       (test-equal '("parser value" parser) expected v)))))
(test-parser "name" ($xml:name (string->lseq "name")))
(test-parser '("name1" "name2") ($xml:names (string->lseq "name1 name2")))
(let ((s "-.1\xB7;\x0300;\x036F;\x203F;\x2040;"))
  (test-parser s ($xml:nmtoken (string->lseq s)))
  (test-parser `(,s ,s) ($xml:nmtokens (string->lseq (string-append s " " s)))))

(test-parser '(char-ref 10 1234) ($xml:char-ref (string->lseq "&#1234;")))
(test-parser '(char-ref 16 #x1234) ($xml:char-ref (string->lseq "&#x1234;")))

(test-parser '(entity-ref "amp") ($xml:entity-ref (string->lseq "&amp;")))
(test-parser '(entity-ref "quot") ($xml:entity-ref (string->lseq "&quot;")))

(test-parser '(pe-ref "style") ($xml:pe-reference (string->lseq "%style;")))

(test-parser '(entity-value "abc") ($xml:entity-value (string->lseq "\"abc\"")))
(test-parser '(entity-value) ($xml:entity-value (string->lseq "\"\"")))
(test-parser '(entity-value "abc" (entity-ref "amp") "def")
	     ($xml:entity-value (string->lseq "\"abc&amp;def\"")))
(test-parser '(entity-value "abc") ($xml:entity-value (string->lseq "'abc'")))
(test-parser '(entity-value) ($xml:entity-value (string->lseq "''")))
(test-parser '(entity-value "abc" (entity-ref "amp") "def")
	     ($xml:entity-value (string->lseq "'abc&amp;def'")))

(test-parser '(attr-value "abc") ($xml:attr-value (string->lseq "\"abc\"")))
(test-parser '(attr-value) ($xml:attr-value (string->lseq "\"\"")))
(test-parser '(attr-value "abc" (entity-ref "amp") "def")
	     ($xml:attr-value (string->lseq "\"abc&amp;def\"")))
(test-parser '(attr-value "abc") ($xml:attr-value (string->lseq "'abc'")))
(test-parser '(attr-value) ($xml:attr-value (string->lseq "''")))
(test-parser '(attr-value "abc" (entity-ref "amp") "def")
	     ($xml:attr-value (string->lseq "'abc&amp;def'")))


(test-parser "system literal" ($xml:system-literal (string->lseq "\"system literal\"")))
(test-parser "system\x1234;literal" ($xml:system-literal (string->lseq "\"system\x1234;literal\"")))
(test-parser "system literal" ($xml:system-literal (string->lseq "'system literal'")))
(test-parser "system\x1234;literal" ($xml:system-literal (string->lseq "'system\x1234;literal'")))

(test-parser "pub-id" ($xml:pubid-literal (string->lseq "\"pub-id\"")))
(test-parser "pub-id" ($xml:pubid-literal (string->lseq "'pub-id'")))

(test-parser '(comment " declarations for <head> & <body> ")
	     ($xml:comment
	      (string->lseq "<!-- declarations for <head> & <body> -->")))

(test-parser '(PI) ($xml:pi (string->lseq "<?xml?>")))
(test-parser '(PI "verion='1.0'")
	     ($xml:pi (string->lseq "<?xml verion='1.0'?>")))

(test-end)
