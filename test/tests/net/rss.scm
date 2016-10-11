(import (rnrs)
	(net rss)
	(text sxml ssax)
	(srfi :19)
	(srfi :64))

(test-begin "RSS 2.0")

(test-assert (rss-title? (make-rss-title '() "test")))
(test-assert (rss-link? (make-rss-link '() "test")))
(test-assert (rss-description? (make-rss-description '() "test")))
(test-assert (rss-managing-editor? (make-rss-managing-editor '() "test")))
(test-assert (rss-web-master? (make-rss-web-master '() "test")))
(test-assert (rss-pub-date? (make-rss-pub-date '() (current-date))))
(test-assert (rss-last-build-date? (make-rss-last-build-date '() (current-date))))
(test-assert (rss-generator? (make-rss-generator '() "test")))
(test-assert (rss-docs? (make-rss-docs '() "test")))
(test-assert (rss-ttl? (make-rss-ttl '() 60)))
(test-assert (rss-skip-hours? (make-rss-skip-hours '() 24)))
(test-assert (rss-skip-days? (make-rss-skip-days '() "test")))
(test-assert (rss-comments? (make-rss-comments '() "test")))
(test-assert (rss-author? (make-rss-author '() "test")))
(test-assert (rss-width? (make-rss-width '() 140)))
(test-assert (rss-height? (make-rss-height '() 400)))
(test-assert (rss-rating? (make-rss-rating '() "test")))
(test-assert (rss-name? (make-rss-name '() "test")))

(test-error (rss-pub-date? (make-rss-pub-date '() "date")))
(test-error (rss-last-build-date? (make-rss-last-build-date '() "date")))
(test-error (rss-ttl? (make-rss-ttl '() "ttl")))
(test-error (rss-skip-hours? (make-rss-skip-hours '() "hours")))
(test-error (rss-width? (make-rss-width '() "width")))
(test-error (rss-height? (make-rss-height '() "width")))

(test-equal "test" (rss-simple-content (make-rss-title '() "test")))
(test-equal 140 (rss-simple-content (make-rss-width '() 140)))

(test-assert (rss-image? (make-rss-image '() 
			  (make-rss-url '() "url")
			  (make-rss-title '() "title")
			  (make-rss-link '() "link")
			  (make-rss-width '() 100)
			  (make-rss-height '() 400)
			  (make-rss-description '() "description"))))

(test-assert (rss-text-input? 
	      (make-rss-text-input '() 
			       (make-rss-title '() "title")
			       (make-rss-description '() "description")
			       (make-rss-name '() "link")
			       (make-rss-link '() "link"))))

;; attributes
(test-assert (rss-guid? (make-rss-guid "guid" #t)))
(test-error assertion-violation? (make-rss-guid "guid" "true"))
(let ((guid (make-rss-guid "guid" #t)))
  (test-equal "guid" (rss-simple-content guid))
  (test-assert (rss-guid-permalink? guid)))

(test-assert (rss-cloud?
	      (make-rss-cloud "cloud" "domain" "22" "/path" "proc" "http")))
(let ((cloud (make-rss-cloud "cloud" "domain" "22" "/path" "proc" "http")))
  (test-equal "cloud"  (rss-simple-content cloud))
  (test-equal "domain" (rss-cloud-domain cloud))
  (test-equal "22"     (rss-cloud-port cloud))
  (test-equal "/path"  (rss-cloud-path cloud))
  (test-equal "proc"   (rss-cloud-register-procedure cloud))
  (test-equal "http"   (rss-cloud-protocol cloud)))

(test-assert (rss-category? (make-rss-category "category" "domain")))
(let ((category (make-rss-category "category" "domain")))
  (test-equal "category" (rss-simple-content category))
  (test-equal "domain" (rss-category-domain category)))

(test-assert (rss-source? (make-rss-source "source" "url")))
(let ((source (make-rss-source "source" "url")))
  (test-equal "source" (rss-simple-content source))
  (test-equal "url" (rss-source-url source)))

(test-assert (rss-enclosure?
	      (make-rss-enclosure "enclosure" "url" 1024 "type")))
(test-error assertion-violation?
	    (make-rss-enclosure "enclosure" "url" "1024" "type"))
(let ((enclosure (make-rss-enclosure "enclosure" "url" 1024 "type")))
  (test-equal "enclosure" (rss-simple-content enclosure))
  (test-equal "url"  (rss-enclosure-url enclosure))
  (test-equal 1024   (rss-enclosure-length enclosure))
  (test-equal "type" (rss-enclosure-type enclosure)))

(test-assert (rss-item? (make-rss-item '() (make-rss-title '() "title")
				   #f #f #f #f #f #f #f #f #f)))
(test-error assertion-violation? 
	    (make-rss-item '() #f #f #f #f #f #f #f #f #f #f))

(test-assert (rss?
	      (make-rss '()
	       (make-rss-channel '()
				 (make-rss-title '() "title")
				 (make-rss-link '() "link")
				 (make-rss-description '() "description")
				 #f #f #f #f #f #f #f #f #f #f
				 #f #f #f #f #f #f #f))))
(test-error assertion-violation? (make-rss '() #f))

(test-assert (rss?
	      (rss:rss
	       (rss:channel
		(rss:title "title")
		(rss:link "link")
		(rss:description "description")
		(rss:item
		 (rss:title "item-title"))))))

(test-assert (rss-item?
	      (rss:item
	       (rss:link "link")
	       (rss:title "item-title"))))
(test-assert (rss-title?
	      (rss-item-title
	       (rss:item
		(rss:link "link")
		(rss:title "item-title")))))

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

(test-assert (rss? (sxml->rss-object
		    (ssax:xml->sxml (open-string-input-port rss-text) '()))))

(test-end)
