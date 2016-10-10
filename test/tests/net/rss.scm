(import (rnrs)
	(net rss)
	(text sxml ssax)
	(srfi :19)
	(srfi :64))

(test-begin "RSS 2.0")

(title? (make-title '() "test"))
(test-assert (title? (make-title '() "test")))
(test-assert (link? (make-link '() "test")))
(test-assert (description? (make-description '() "test")))
(test-assert (managing-editor? (make-managing-editor '() "test")))
(test-assert (web-master? (make-web-master '() "test")))
(test-assert (pub-date? (make-pub-date '() (current-date))))
(test-assert (last-build-date? (make-last-build-date '() (current-date))))
(test-assert (generator? (make-generator '() "test")))
(test-assert (docs? (make-docs '() "test")))
(test-assert (ttl? (make-ttl '() 60)))
(test-assert (skip-hours? (make-skip-hours '() 24)))
(test-assert (skip-days? (make-skip-days '() "test")))
(test-assert (comments? (make-comments '() "test")))
(test-assert (author? (make-author '() "test")))
(test-assert (width? (make-width '() 140)))
(test-assert (height? (make-height '() 400)))
(test-assert (rating? (make-rating '() "test")))
(test-assert (name? (make-name '() "test")))

(test-error (pub-date? (make-pub-date '() "date")))
(test-error (last-build-date? (make-last-build-date '() "date")))
(test-error (ttl? (make-ttl '() "ttl")))
(test-error (skip-hours? (make-skip-hours '() "hours")))
(test-error (width? (make-width '() "width")))
(test-error (height? (make-height '() "width")))

(test-equal "test" (rss-simple-content (make-title '() "test")))
(test-equal 140 (rss-simple-content (make-width '() 140)))

(test-assert (image? (make-image '() 
				 (make-url '() "url")
				 (make-title '() "title")
				 (make-link '() "link")
				 (make-width '() 100)
				 (make-height '() 400)
				 (make-description '() "description"))))

(test-assert (text-input? 
	      (make-text-input '() 
			       (make-title '() "title")
			       (make-description '() "description")
			       (make-name '() "link")
			       (make-link '() "link"))))

;; attributes
(test-assert (guid? (make-guid "guid" #t)))
(test-error assertion-violation? (make-guid "guid" "true"))
(let ((guid (make-guid "guid" #t)))
  (test-equal "guid" (rss-simple-content guid))
  (test-assert (guid-permalink? guid)))

(test-assert (cloud? (make-cloud "cloud" "domain" "22" "/path" "proc" "http")))
(let ((cloud (make-cloud "cloud" "domain" "22" "/path" "proc" "http")))
  (test-equal "cloud"  (rss-simple-content cloud))
  (test-equal "domain" (cloud-domain cloud))
  (test-equal "22"     (cloud-port cloud))
  (test-equal "/path"  (cloud-path cloud))
  (test-equal "proc"   (cloud-register-procedure cloud))
  (test-equal "http"   (cloud-protocol cloud)))

(test-assert (category? (make-category "category" "domain")))
(let ((category (make-category "category" "domain")))
  (test-equal "category" (rss-simple-content category))
  (test-equal "domain" (category-domain category)))

(test-assert (source? (make-source "source" "url")))
(let ((source (make-source "source" "url")))
  (test-equal "source" (rss-simple-content source))
  (test-equal "url" (source-url source)))

(test-assert (enclosure? (make-enclosure "enclosure" "url" 1024 "type")))
(test-error assertion-violation?
	    (make-enclosure "enclosure" "url" "1024" "type"))
(let ((enclosure (make-enclosure "enclosure" "url" 1024 "type")))
  (test-equal "enclosure" (rss-simple-content enclosure))
  (test-equal "url" (enclosure-url enclosure))
  (test-equal 1024 (enclosure-length enclosure))
  (test-equal "type" (enclosure-type enclosure)))

(test-assert (item? (make-item '() (make-title '() "title")
			       #f #f #f #f #f #f #f #f #f)))
(test-error assertion-violation? (make-item '() #f #f #f #f #f #f #f #f #f #f))

(test-assert (rss?
	      (make-rss '()
	       (make-channel '()
			     (make-title '() "title")
			     (make-link '() "link")
			     (make-description '() "description")
			     #f #f #f #f #f #f #f #f #f #f
			     #f #f #f #f #f #f #f))))
(test-error assertion-violation? (make-rss '() #f))

(define rss-text "
 <?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<rss version=\"2.0\">

<channel>
  <title>W3Schools Home Page</title>
  <link>http://www.w3schools.com</link>
  <description>Free web building tutorials</description>
  <item>
    <title>RSS Tutorial</title>
    <link>http://www.w3schools.com/xml/xml_rss.asp</link>
    <description>New RSS tutorial on W3Schools</description>
  </item>
  <item>
    <title>XML Tutorial</title>
    <link>http://www.w3schools.com/xml</link>
    <description>New XML tutorial on W3Schools</description>
  </item>
</channel>

</rss>")

(rss? (rss->object 
		    (ssax:xml->sxml (open-string-input-port rss-text) '())))
(test-assert (rss? (rss->object 
		    (ssax:xml->sxml (open-string-input-port rss-text) '()))))

(test-end)
