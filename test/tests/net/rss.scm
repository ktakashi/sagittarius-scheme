(import (rnrs)
	(net rss)
	(text sxml ssax)
	(srfi :19)
	(srfi :64))

(test-begin "RSS 2.0")

(test-assert (title? (make-title 'tag '() '("test"))))
(test-assert (link? (make-link 'tag '() '("test"))))
(test-assert (description? (make-description 'tag '() '("test"))))
(test-assert (managing-editor? (make-managing-editor 'tag '() '("test"))))
(test-assert (web-master? (make-web-master 'tag '() '("test"))))
(test-assert (pub-date? (make-pub-date 'tag '() (list (current-date)))))
(test-assert (last-build-date? (make-last-build-date 'tag '() (list (current-date)))))
(test-assert (generator? (make-generator 'tag '() '("test"))))
(test-assert (docs? (make-docs 'tag '() '("test"))))
(test-assert (ttl? (make-ttl 'tag '() '(60))))
(test-assert (skip-hours? (make-skip-hours 'tag '() '(24))))
(test-assert (skip-days? (make-skip-days 'tag '() '("test"))))
(test-assert (comments? (make-comments 'tag '() '("test"))))
(test-assert (author? (make-author 'tag '() '("test"))))
(test-assert (width? (make-width 'tag '() '(140))))
(test-assert (height? (make-height 'tag '() '(400))))
(test-assert (rating? (make-rating 'tag '() '("test"))))
(test-assert (name? (make-name 'tag '() '("test"))))

(test-error (pub-date? (make-pub-date 'tag '() '("date"))))
(test-error (last-build-date? (make-last-build-date 'tag '() '("date"))))
(test-error (ttl? (make-ttl 'tag '() '("ttl"))))
(test-error (skip-hours? (make-skip-hours 'tag '() '("hours"))))
(test-error (width? (make-width 'tag '() '("width"))))
(test-error (height? (make-height 'tag '() '("width"))))

(test-equal "test" (rss-simple-content (make-title 'tag '() '("test"))))
(test-equal 140 (rss-simple-content (make-width 'tag '() '(140))))

(test-assert (image? (make-image 'tag '() 
				 (list (make-url 'tag '() '("url"))
				       (make-title 'tag '() '("title"))
				       (make-link 'tag '() '("link"))
				       (make-width 'tag '() '(100))
				       (make-height 'tag '() '(400))
				       (make-description 'tag '() '("description"))))))

(test-assert (text-input? 
	      (make-text-input 'tag '() 
			       (list (make-title 'tag '() '("title"))
				     (make-description 'tag '() '("description"))
				     (make-name 'tag '() '("link"))
				     (make-link 'tag '() '("link"))))))

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

(test-assert (rss? (rss->object 
		    (ssax:xml->sxml (open-string-input-port rss-text) '()))))

(test-end)
