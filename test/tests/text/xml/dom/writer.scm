(import (rnrs)
	(text xml dom writer)
	(text xml dom nodes)
	(text xml dom factory)
	(srfi :1)
	(srfi :64))

(test-begin "DOM writer")

(define (writer-test expected proc)
  (let-values (((out extract) (open-string-output-port)))
    ;; (write (begin (proc out) (extract))) (newline)
    (test-equal expected (begin (proc out) (extract)))))

;; historical reason...
(define legazy-options
  (xml-write-options-builder
   (omit-xml-declaration? #f)
   (standalone? #f)))
(define xml-writer (make-dom-writer legazy-options))

(writer-test
 "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\
  <foo:foo xmlns:foo=\"urn:foo\" foo:buz=\"buzzz\" foo:bla=\"blabla\">\
    <foo:bar/>\
  </foo:foo>"
 (lambda (out)
   (let* ((document (make-xml-document))
	  (e (document:create-element-ns document "urn:foo" "foo:foo")))
     (node:append-child! document e)
     (element:set-attribute-ns! e "urn:foo" "foo:bla" "blabla")
     (element:set-attribute! e "foo:buz" "buzzz")
     (node:append-child! e (document:create-element-ns document
						       "urn:foo" "foo:bar"))
     (xml-writer document out))))

;; it's rather weird to put here to test insert/replace node but
;; it's easier to do it here.
(let* ((document (make-xml-document))
       (e (document:create-element document "foo")))
  (node:append-child! document e)
  (node:insert-before! document e (document:create-comment document "boo"))
  (let ((e2 (document:create-element document "foo2"))
	(e3 (document:create-element document "foo3"))
	(e4 (document:create-element document "foo4"))
	(e5 (document:create-element document "foo5")))
    (node:append-child! e e2)
    (node:insert-before! e e2 e3)
    (node:insert-before! e e2 e4)
    (node:insert-before! e #f e5)
    (writer-test
     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\
      <!--boo-->\n\
      <foo><foo3/><foo4/><foo2/><foo5/></foo>"
     (lambda (out)(xml-writer document out)))
    (node:replace-child! e e5 (document:create-element document "foo6"))
    (writer-test
     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\
      <!--boo-->\n\
      <foo><foo3/><foo4/><foo2/><foo6/></foo>"
     (lambda (out)(xml-writer document out)))
    (node:remove-child! e e2)
    (writer-test
     "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\
      <!--boo-->\n\
      <foo><foo3/><foo4/><foo6/></foo>"
     (lambda (out)(xml-writer document out)))
    ))

;; canonicalisation
(define c14n-write (make-dom-writer *xml:c14n*))
(define c14n/comment-write (make-dom-writer *xml:c14n-w/comment*))
(define input-section-3.1
  "<?xml version=\"1.0\"?>

<?xml-stylesheet   href=\"doc.xsl\"
   type=\"text/xsl\"   ?>

<!DOCTYPE doc SYSTEM \"doc.dtd\">

<doc>Hello, world!<!-- Comment 1 --></doc>

<?pi-without-data     ?>

<!-- Comment 2 -->

<!-- Comment 3 -->")
(define input-section-3.2
  "<doc>
   <clean>   </clean>
   <dirty>   A   B   </dirty>
   <mixed>
      A
      <clean>   </clean>
      B
      <dirty>   A   B   </dirty>
      C
   </mixed>
</doc>")
(define input-section-3.3
  "<!DOCTYPE doc [<!ATTLIST e9 attr CDATA \"default\">]>
<doc>
   <e1   />
   <e2   ></e2>
   <e3   name = \"elem3\"   id=\"elem3\"   />
   <e4   name=\"elem4\"   id=\"elem4\"   ></e4>
   <e5 a:attr=\"out\" b:attr=\"sorted\" attr2=\"all\" attr=\"I'm\"
      xmlns:b=\"http://www.ietf.org\"
      xmlns:a=\"http://www.w3.org\"
      xmlns=\"http://example.org\"/>
   <e6 xmlns=\"\" xmlns:a=\"http://www.w3.org\">
      <e7 xmlns=\"http://www.ietf.org\">
         <e8 xmlns=\"\" xmlns:a=\"http://www.w3.org\">
            <e9 xmlns=\"\" xmlns:a=\"http://www.ietf.org\"/>
         </e8>
      </e7>
   </e6>
</doc>")
(define input-section-3.4
  "<!DOCTYPE doc [
<!ATTLIST normId id ID #IMPLIED>
<!ATTLIST normNames attr NMTOKENS #IMPLIED>
]>
<doc>
   <text>First line&#x0d;&#10;Second line</text>
   <value>&#x32;</value>
   <compute><![CDATA[value>\"0\" && value<\"10\" ?\"valid\":\"error\"]]></compute>
   <compute expr='value>\"0\" &amp;&amp; value&lt;\"10\" ?\"valid\":\"error\"'>valid</compute>
   <norm attr=' &apos;   &#x20;&#13;&#xa;&#9;   &apos; '/>
   <normNames attr='   A   &#x20;&#13;&#xa;&#9;   B   '/>
   <normId id=' &apos;   &#x20;&#13;&#xa;&#9;   &apos; '/>
</doc>")
;; we don't handle SYSTEM entity
#;(define input-section-3.5
  "<!DOCTYPE doc [
<!ATTLIST doc attrExtEnt ENTITY #IMPLIED>
<!ENTITY ent1 \"Hello\">
<!ENTITY ent2 SYSTEM \"world.txt\">
<!ENTITY entExt SYSTEM \"earth.gif\" NDATA gif>
<!NOTATION gif SYSTEM \"viewgif.exe\">
]>
<doc attrExtEnt=\"entExt\">
   &ent1;, &ent2;!
</doc>

<!-- Let world.txt contain \"world\" (excluding the quotes) -->")
(define input-section-3.6
  "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><doc>&#169;</doc>")

(define (string->dom s) (input-port->dom-tree (open-string-input-port s)))
(define (test-canonicalization writer input expect)
  (let-values (((out e) (open-string-output-port)))
    (writer (string->dom input) out)
    (let ((r (e)))
      (unless (string=? expect r)
	(display expect) (newline)
	(display r) (newline))
      (test-equal input expect r))))
(test-canonicalization c14n-write input-section-3.1
		       "<?xml-stylesheet href=\"doc.xsl\"
   type=\"text/xsl\"   ?>
<doc>Hello, world!</doc>
<?pi-without-data?>")

(test-canonicalization c14n/comment-write input-section-3.1
		       "<?xml-stylesheet href=\"doc.xsl\"
   type=\"text/xsl\"   ?>
<doc>Hello, world!<!-- Comment 1 --></doc>
<?pi-without-data?>
<!-- Comment 2 -->
<!-- Comment 3 -->")

(test-canonicalization c14n-write input-section-3.2
		       "<doc>
   <clean>   </clean>
   <dirty>   A   B   </dirty>
   <mixed>
      A
      <clean>   </clean>
      B
      <dirty>   A   B   </dirty>
      C
   </mixed>
</doc>")

(test-canonicalization c14n-write input-section-3.3
		       "<doc>
   <e1></e1>
   <e2></e2>
   <e3 id=\"elem3\" name=\"elem3\"></e3>
   <e4 id=\"elem4\" name=\"elem4\"></e4>
   <e5 xmlns=\"http://example.org\" xmlns:a=\"http://www.w3.org\" xmlns:b=\"http://www.ietf.org\" attr=\"I'm\" attr2=\"all\" b:attr=\"sorted\" a:attr=\"out\"></e5>
   <e6 xmlns:a=\"http://www.w3.org\">
      <e7 xmlns=\"http://www.ietf.org\">
         <e8 xmlns=\"\">
            <e9 xmlns:a=\"http://www.ietf.org\" attr=\"default\"></e9>
         </e8>
      </e7>
   </e6>
</doc>")

(test-canonicalization c14n-write input-section-3.4
		       "<doc>
   <text>First line&#xD;
Second line</text>
   <value>2</value>
   <compute>value&gt;\"0\" &amp;&amp; value&lt;\"10\" ?\"valid\":\"error\"</compute>
   <compute expr=\"value>&quot;0&quot; &amp;&amp; value&lt;&quot;10&quot; ?&quot;valid&quot;:&quot;error&quot;\">valid</compute>
   <norm attr=\" '    &#xD;&#xA;&#x9;   ' \"></norm>
   <normNames attr=\"A &#xD;&#xA;&#x9; B\"></normNames>
   <normId id=\"' &#xD;&#xA;&#x9; '\"></normId>
</doc>")
(test-end)

