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

(test-parser '(qname #f "name") ($xml:qname (string->lseq "name")))
(test-parser '(qname "pre" "name") ($xml:qname (string->lseq "pre:name")))

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

(test-parser '(att-value "abc") ($xml:att-value (string->lseq "\"abc\"")))
(test-parser '(att-value) ($xml:att-value (string->lseq "\"\"")))
(test-parser '(att-value "abc" (entity-ref "amp") "def")
	     ($xml:att-value (string->lseq "\"abc&amp;def\"")))
(test-parser '(att-value "abc") ($xml:att-value (string->lseq "'abc'")))
(test-parser '(att-value) ($xml:att-value (string->lseq "''")))
(test-parser '(att-value "abc" (entity-ref "amp") "def")
	     ($xml:att-value (string->lseq "'abc&amp;def'")))


(test-parser "system literal" ($xml:system-literal (string->lseq "\"system literal\"")))
(test-parser "system\x1234;literal" ($xml:system-literal (string->lseq "\"system\x1234;literal\"")))
(test-parser "system literal" ($xml:system-literal (string->lseq "'system literal'")))
(test-parser "system\x1234;literal" ($xml:system-literal (string->lseq "'system\x1234;literal'")))

(test-parser "pub-id" ($xml:pubid-literal (string->lseq "\"pub-id\"")))
(test-parser "pub-id" ($xml:pubid-literal (string->lseq "'pub-id'")))

(test-parser '(comment " declarations for <head> & <body> ")
	     ($xml:comment
	      (string->lseq "<!-- declarations for <head> & <body> -->")))

(test-parser '(PI "xml-stylesheet" #f)
	     ($xml:pi (string->lseq "<?xml-stylesheet?>")))
(test-parser '(PI "xml-stylesheet" "verion='1.0'")
	     ($xml:pi (string->lseq "<?xml-stylesheet verion='1.0'?>")))

(test-parser '(cdata "<greeting>Hello, world!</greeting>")
	     ($xml:cd-sect (string->lseq "<![CDATA[<greeting>Hello, world!</greeting>]]>")))

(test-parser '(xml-decl (version "1.0") #f #f)
	     ($xml:xml-decl (string->lseq "<?xml version=\"1.0\"?>")))
(test-parser '(xml-decl (version "1.0") #f #f)
	     ($xml:xml-decl (string->lseq "<?xml version='1.0'?>")))
(test-parser '(xml-decl (version "1.0") (encoding "utf-8") #f)
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' encoding=\"utf-8\"?>")))
(test-parser '(xml-decl (version "1.0") (encoding "utf-8") #f)
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' encoding='utf-8'?>")))
(test-parser '(xml-decl (version "1.0") #f (standalone "yes"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' standalone=\"yes\"?>")))
(test-parser '(xml-decl (version "1.0") #f (standalone "no"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' standalone=\"no\"?>")))
(test-parser '(xml-decl (version "1.0") #f (standalone "yes"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' standalone='yes'?>")))
(test-parser '(xml-decl (version "1.0") #f (standalone "no"))
	     ($xml:xml-decl (string->lseq "<?xml version='1.0' standalone='no'?>")))

(test-parser `(prolog (xml-decl (version "1.0") #f #f) (misc) #f (misc))
	     ($xml:prolog (string->lseq "<?xml version='1.0'?>")))
(test-parser `(prolog (xml-decl (version "1.0") #f #f)
		      (misc (comment " comment ")) #f (misc))
	     ($xml:prolog (string->lseq "<?xml version='1.0'?><!-- comment -->")))

(test-parser '(!element (qname #f "br") empty)
	     ($xml:element-decl (string->lseq "<!ELEMENT br EMPTY>")))
(test-parser '(!element (qname #f "p") (pcdata (qname #f "emph")))
	     ($xml:element-decl (string->lseq "<!ELEMENT p (#PCDATA|emph)* >")))
(test-parser '(!element (pe-ref "name.para") (pe-ref "content.para"))
	     ($xml:element-decl (string->lseq "<!ELEMENT %name.para; %content.para; >")))
(test-parser '(!element (qname #f "container") any)
	     ($xml:element-decl (string->lseq "<!ELEMENT container ANY>")))
;; PCDATA
(test-parser '(!element (qname #f "p")
			(pcdata (qname #f "a")
				(qname #f "ul")
				(qname #f "b")
				(qname #f "i")
				(qname #f "em")))
	     ($xml:element-decl (string->lseq "<!ELEMENT p (#PCDATA|a|ul|b|i|em)*>")))
(test-parser '(!element (qname #f "p")
			(pcdata (pe-ref "font") (pe-ref "phrase")
				(pe-ref "special") (pe-ref "form")))
	     ($xml:element-decl (string->lseq "<!ELEMENT p (#PCDATA | %font; | %phrase; | %special; | %form;)* >")))
(test-parser '(!element (qname #f "b") (pcdata))
	     ($xml:element-decl (string->lseq "<!ELEMENT b (#PCDATA)>")))

(test-parser '(!element (qname #f "spec")
			(seq (qname #f "front")
			     (qname #f "body")
			     (? (qname #f "back"))))
	     ($xml:element-decl (string->lseq "<!ELEMENT spec (front, body, back?)>")))
(test-parser '(!element (qname #f "div1")
			(seq (qname #f "head")
			     (* (choice (qname #f "p")
					(qname #f "list")
					(qname #f "note")))
			     (* (qname #f "div2"))))
	     ($xml:element-decl (string->lseq "<!ELEMENT div1 (head, (p | list | note)*, div2*)>")))
(test-parser '(!element (qname #f "dictionary-body")
			(* (choice (pe-ref "div.mix") (pe-ref "dict.mix"))))
	     ($xml:element-decl (string->lseq "<!ELEMENT dictionary-body (%div.mix; | %dict.mix;)*>")))

;; <!ENTITY ... >
(test-parser '(!entity pe "ISOLat2"
		       (system "http://www.xml.com/iso/isolat2-xml.entities"))
	     ($xml:entity-decl (string->lseq "<!ENTITY % ISOLat2 SYSTEM \"http://www.xml.com/iso/isolat2-xml.entities\" >")))

(test-parser '(!entity ge "Pub-Status"
		      (entity-value "This is a pre-release of the specification."))
	     ($xml:entity-decl (string->lseq "<!ENTITY Pub-Status \"This is a pre-release of the specification.\">")))
(test-parser '(!entity ge "open-hatch"
		      (system "http://www.textuality.com/boilerplate/OpenHatch.xml"))
	     ($xml:entity-decl (string->lseq "<!ENTITY open-hatch
         SYSTEM \"http://www.textuality.com/boilerplate/OpenHatch.xml\">")))
(test-parser '(!entity ge "open-hatch"
		      (public "-//Textuality//TEXT Standard open-hatch boilerplate//EN" "http://www.textuality.com/boilerplate/OpenHatch.xml"))
	     ($xml:entity-decl (string->lseq "<!ENTITY open-hatch
         PUBLIC \"-//Textuality//TEXT Standard open-hatch boilerplate//EN\"
         \"http://www.textuality.com/boilerplate/OpenHatch.xml\">")))
(test-parser '(!entity ge "hatch-pic"
		       (system "../grafix/OpenHatch.gif") (ndata "gif"))
	     ($xml:entity-decl (string->lseq "<!ENTITY hatch-pic 
         SYSTEM \"../grafix/OpenHatch.gif\"
         NDATA gif >")))

;; <!ATTLIST ...>
(test-parser '(!attlist (qname #f "termdef")
			(att-def (qname #f "id") id required)
			(att-def (qname #f "name") cdata implied))
	     ($xml:attlist-decl (string->lseq "<!ATTLIST termdef
          id      ID      #REQUIRED
          name    CDATA   #IMPLIED>")))
(test-parser '(!attlist (qname #f "list")
			(att-def (qname #f "type")
				 ("bullets" "ordered" "glossary")
				 (fixed (att-value "ordered"))))
	     ($xml:attlist-decl (string->lseq "<!ATTLIST list type (bullets|ordered|glossary)  \"ordered\">")))
(test-parser '(!attlist (qname #f "form")
			(att-def (qname #f "method")
				 cdata (fixed (att-value "POST"))))
	     ($xml:attlist-decl (string->lseq "<!ATTLIST form method  CDATA   #FIXED \"POST\">")))

;; <!NOTATION ... >
(test-parser '(!notation "name" (system "URI"))
	     ($xml:notation-decl (string->lseq "<!NOTATION name SYSTEM \"URI\">")))
(test-parser '(!notation "name" (public "public_ID"))
	     ($xml:notation-decl (string->lseq "<!NOTATION name PUBLIC \"public_ID\">")))
(test-parser '(!notation "name" (public "public_ID" "URI"))
	     ($xml:notation-decl (string->lseq "<!NOTATION name PUBLIC \"public_ID\" \"URI\">")))

(test-parser '(!doctype (qname #f "greeting") (system "hello.dtd") (subset))
	     ($xml:doctype-decl (string->lseq "<!DOCTYPE greeting SYSTEM \"hello.dtd\">")))

(test-parser '(!doctype (qname #f "greeting")
			#f (subset (!element (qname #f "greeting") (pcdata))))
	     ($xml:doctype-decl (string->lseq "<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>")))

(test-parser '(prolog (xml-decl (version "1.0") #f #f) (misc)
		      (!doctype (qname #f "greeting") (system "hello.dtd") (subset)) (misc))
	     ($xml:prolog (string->lseq "<?xml version=\"1.0\"?><!DOCTYPE greeting SYSTEM \"hello.dtd\">")))

(test-parser '(element (qname #f "IMG")
		(attributes ((qname #f "align") "left")
			    ((qname #f "src") "http://www.w3.org/Icons/WWW/w3c_home")))
	     ($xml:element (string->lseq "<IMG align=\"left\" src=\"http://www.w3.org/Icons/WWW/w3c_home\" />")))

(test-parser '(element (qname #f "br") (attributes))
	     ($xml:element (string->lseq "<br/>")))

(test-parser '(element (qname #f "br") (attributes))
	     ($xml:element (string->lseq "<br></br>")))
(test-parser '(element (qname #f "p") (attributes) "text")
	     ($xml:element (string->lseq "<p>text</p>")))
(test-parser '(element (qname #f "p") (attributes) "text"
		       (element (qname #f "a")
			 (attributes ((qname #f "href") "link")) "foo"))
	     ($xml:element (string->lseq "<p>text<a href=\"link\">foo</a></p>")))

(test-parser '(element (qname "edi" "price")
		(attributes ((xmlns "edi") "http://ecommerce.example.org/schema")
			    ((qname #f "units") "Euro"))
		"32.18")
	     ($xml:element (string->lseq "<edi:price xmlns:edi='http://ecommerce.example.org/schema' units='Euro'>32.18</edi:price>")))

(test-parser '(element (qname #f "x")
		(attributes ((xmlns "edi") "http://ecommerce.example.org/schema"))
		"\n  "
		(comment " the 'taxClass' attribute's namespace is http://ecommerce.example.org/schema ")
		"\n  "
		(element (qname #f "lineItem")
		  (attributes ((qname "edi" "taxClass") "exempt"))
		  "Baby food")
		"\n")
	     ($xml:element (string->lseq "<x xmlns:edi='http://ecommerce.example.org/schema'>
  <!-- the 'taxClass' attribute's namespace is http://ecommerce.example.org/schema -->
  <lineItem edi:taxClass=\"exempt\">Baby food</lineItem>
</x>")))

(test-parser '(document (prolog (xml-decl (version "1.0") #f #f) (misc)
				(!doctype (qname #f "greeting")
					  (system "hello.dtd") (subset))
				(misc))
			(element (qname #f "greeting")
			  (attributes) "Hello, world!"))
	     ($xml:document (string->lseq "<?xml version=\"1.0\"?>
<!DOCTYPE greeting SYSTEM \"hello.dtd\">
<greeting>Hello, world!</greeting> ")))

(test-parser '(document (prolog (xml-decl (version "1.0") (encoding "UTF-8") #f)
				(misc)
				(!doctype (qname #f "greeting") #f
					  (subset (!element (qname #f "greeting") (pcdata))))
				(misc))
			(element (qname #f "greeting")
			  (attributes) "Hello, world!"))
	     ($xml:document (string->lseq "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>
<greeting>Hello, world!</greeting>")))

(test-end)
