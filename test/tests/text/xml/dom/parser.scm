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

(test-parser '(cdata "<greeting>Hello, world!</greeting>")
	     ($xml:cd-sect (string->lseq "<![CDATA[<greeting>Hello, world!</greeting>]]>")))

(test-parser '(xml-decl (version "1.0"))
	     ($xml:xml-decl (string->lseq "<?xml version=\"1.0\"?>")))
(test-parser '(xml-decl (version "1.0"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0'?>")))
(test-parser '(xml-decl (version "1.0") (encoding "utf-8"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' encoding=\"utf-8\"?>")))
(test-parser '(xml-decl (version "1.0") (encoding "utf-8"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' encoding='utf-8'?>")))
(test-parser '(xml-decl (version "1.0") (standalone "yes"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' standalone=\"yes\"?>")))
(test-parser '(xml-decl (version "1.0") (standalone "no"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' standalone=\"no\"?>")))
(test-parser '(xml-decl (version "1.0") (standalone "yes"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' standalone='yes'?>")))
(test-parser '(xml-decl (version "1.0") (standalone "no"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' standalone='no'?>")))

(test-parser `(prolog (xml-decl (version "1.0")))
	     ($xml:prolog (string->lseq "<?xml version='1.0'?>")))
(test-parser `(prolog (xml-decl (version "1.0"))
		      (comment " comment "))
	     ($xml:prolog (string->lseq "<?xml version='1.0'?><!-- comment -->")))

(test-parser '(element "br" empty)
	     ($xml:element-decl (string->lseq "<!ELEMENT br EMPTY>")))
(test-parser '(element "p" (pcdata "emph"))
	     ($xml:element-decl (string->lseq "<!ELEMENT p (#PCDATA|emph)* >")))
(test-parser '(element (pe-ref "name.para") (pe-ref "content.para"))
	     ($xml:element-decl (string->lseq "<!ELEMENT %name.para; %content.para; >")))
(test-parser '(element "container" any)
	     ($xml:element-decl (string->lseq "<!ELEMENT container ANY>")))
;; PCDATA
(test-parser '(element "p" (pcdata "a" "ul" "b" "i" "em"))
	     ($xml:element-decl (string->lseq "<!ELEMENT p (#PCDATA|a|ul|b|i|em)*>")))
(test-parser '(element "p" (pcdata (pe-ref "font") (pe-ref "phrase")
				   (pe-ref "special") (pe-ref "form")))
	     ($xml:element-decl (string->lseq "<!ELEMENT p (#PCDATA | %font; | %phrase; | %special; | %form;)* >")))
(test-parser '(element "b" (pcdata))
	     ($xml:element-decl (string->lseq "<!ELEMENT b (#PCDATA)>")))

(test-parser '(element "spec" (seq "front" "body" (? "back")))
	     ($xml:element-decl (string->lseq "<!ELEMENT spec (front, body, back?)>")))
(test-parser '(element "div1" (seq "head" (* (choice "p" "list" "note")) (* "div2")))
	     ($xml:element-decl (string->lseq "<!ELEMENT div1 (head, (p | list | note)*, div2*)>")))
(test-parser '(element "dictionary-body" (* (choice (pe-ref "div.mix") (pe-ref "dict.mix"))))
	     ($xml:element-decl (string->lseq "<!ELEMENT dictionary-body (%div.mix; | %dict.mix;)*>")))

(test-parser '(doctype "greeting" (system "hello.dtd"))
	     ($xml:doctype-decl (string->lseq "<!DOCTYPE greeting SYSTEM \"hello.dtd\">")))

(test-parser '(doctype "greeting" (subset (element "greeting" (pcdata))))
	     ($xml:doctype-decl (string->lseq "<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>")))
(test-parser '(prolog (xml-decl (version "1.0"))
		      (doctype "greeting" (system "hello.dtd")))
	     ($xml:prolog (string->lseq "<?xml version=\"1.0\"?><!DOCTYPE greeting SYSTEM \"hello.dtd\">")))
(test-end)
