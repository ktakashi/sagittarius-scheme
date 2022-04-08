(import (rnrs)
	(sagittarius) 
	(text markdown)
	(text markdown converter html)

	(text json)
	(text json pointer)

	(text sxml ssax)
	(text sxml serializer)

	(util file)
	(srfi :64)
	(srfi :39))

(test-begin "Markdown - New")

(define (make-markdoen-tester parser closing-tag-mismatches)
  (lambda (file)
    (define test-cases (call-with-input-file file json-read))
    (define markdown-pointer (json-pointer "/markdown"))
    (define html-pointer (json-pointer "/html"))
    (define example-pointer (json-pointer "/example"))
    (define options (markdown-conversion-options-builder
		     (context-data (markdown-html-conversion-context-builder
				    (url-encoder commonmark-url-encoder)))))
    
    (define (test-markdown test)
      (define markdown (markdown-pointer test))
      (define html (html-pointer test))
      (define example (example-pointer test))  
      
      (let* ((e (parse-markdown parser
				(open-string-input-port markdown)))
	     (sxml (markdown-converter:convert default-markdown-converter
					       'html e options))
	     (result (srl:sxml->html-noindent sxml)))
	(if (member example closing-tag-mismatches)
	    ;; okay, let's parse the expected HTML and compare
	    (let ((shtml (ssax:xml->sxml (open-string-input-port html) '())))
	      (test-equal (format "Example ~d (Closing tag)" example) shtml
			  ;; We need to re-parse as converted SXML contains
			  ;; newlines
			  (ssax:xml->sxml (open-string-input-port result) '())))
	    (begin
	      (unless (equal? html result)
		(write html) (newline)
		(write result) (newline))
	      (test-equal (format "Example ~d" example) html result)))))
    (test-group file (for-each test-markdown test-cases))))

(define commonmark-data-dir (string-append (current-directory)
					   "/test/data/markdown/commonmark"))
(define closing-tag-mismatches
  ;; Basically the closing tag which we don't have much control
  ;; as we put markdown node into SXML instead of emitting string
  '(
    79			    ;; <h2></h2> or <h2 />
    280 281 282 283 284 315 ;; <li></li> or <li />
    483 486		    ;; <a></a> or <a />
    ))
(define test-commonmark (make-markdoen-tester commonmark-parser
					      closing-tag-mismatches))
(parameterize ((*srl:empty-elements* '()) ;; to XHTMLish
	       (*srl:escape-alist-char-data*
		(cons '(#\" . "&quot;") (*srl:escape-alist-char-data*))))
  (test-group "Commonmark"
   (for-each test-commonmark (find-files commonmark-data-dir))))

(define gfm-data-dir (string-append (current-directory)
				    "/test/data/markdown/gfm"))
(define test-gfm (make-markdoen-tester markdown-parser '(5 7)))

(parameterize ((*srl:boolean-attributes* '())) ;; GFM says disabled=""
  (test-group "GFM" (for-each test-gfm (find-files gfm-data-dir))))

(define other-data-dir (string-append (current-directory)
				      "/test/data/markdown/others"))
;; It's not GFM specific perser, so just like this is fine
(test-group "Others" (for-each test-gfm (find-files other-data-dir)))

(test-end)

(test-runner-reset (test-runner-current))

(test-begin "Markdown - Legacy")

(define-syntax test-parser
  (syntax-rules ()
    ((_ expected str)
     (test-equal str expected
		 (markdown-read (open-string-input-port str) :as 'sexp)))))

(test-parser '(:doc (:header :h6 "hoge")) "###### hoge\n")
(test-parser '(:doc (:header :h6 "hoge__a")) "###### hoge__a\n")
(test-parser '(:doc (:header :h6 "hoge'a")) "###### hoge'a\n")

(test-parser '(:doc (:header :h2 "Header") (:header :h1 "Header2"))
	     "Header\n------\n\nHeader2\n=======\n")

;; with inline
(test-parser '(:doc (:header :h6 (:emph "hoge"))) "###### *hoge*\n")
(test-parser '(:doc (:header :h6 (:strong "hoge"))) "###### **hoge**\n")
(test-parser '(:doc (:header :h6 (:emph "hoge"))) "###### _hoge_\n")
(test-parser '(:doc (:header :h6 (:strong "hoge"))) "###### __hoge__\n")

;; inline link
;; auto link
(test-parser '(:doc (:header :h1 (:link (:label "http://hogehoge")
				"http://hogehoge" "")))
	     "# <http://hogehoge>\n")
(test-parser '(:doc (:header :h1 (:link (:label "ktakashi@ymail.com")
				"mailto:ktakashi@ymail.com" "")))
	     "# <ktakashi@ymail.com>\n")
(test-parser '(:doc (:header :h1 (:link (:label "mailto:ktakashi@ymail.com")
				"mailto:ktakashi@ymail.com" "")))
	     "# <mailto:ktakashi@ymail.com>\n")

;; explicit link
(test-parser '(:doc (:header :h1 (:link (:label "link") "source" "title")))
	     "# [link](source 'title')\n")
(test-parser '(:doc (:header :h1 (:link (:label "link") "source" "title")))
	     "# [link](source \"title\")\n")

;; strike
(test-parser '(:doc (:plain (:strike "stike") " strike"))
	     "~~stike~~ strike")
(test-parser '(:doc (:plain (:strike (:code "code") " ok")))
	     "~~`code` ok~~")

;; image
(test-parser '(:doc (:header :h1 (:image (:link (:label "link") "source" "title"))))
	    "# ![link](source 'title')\n")

;; note
(test-parser '(:doc (:note (:ref "note") (:plain "note")))
	     "[^note]: note\n")
(test-parser '(:doc (:note (:ref "note") (:plain "note" :eol "note2")))
	     "[^note]: note\n    note2\n")
(test-parser '(:doc (:note (:ref "note") (:plain "note" :eol "note2") (:plain "note3")))
	     "[^note]: note\n    note2\n \n    note3\n")
;; note with code
(test-parser '(:doc (:note (:ref "note") (:plain "note " (:code "code"))))
	     "[^note]: note `code`\n")

;; inline note
(test-parser '(:doc (:plain (:note "1")) (:note (:ref "1") (:plain "note"))) "^[note]")

;; inline note
(test-parser '(:doc (:header :h1 (:note "1")) (:note (:ref "1") (:plain "note"))) "# ^[note]\n")

;; block quote
(test-parser '(:doc (:blockquote (:plain "hoge" :eol "fuga"))) ">hoge\n>fuga\n\n")
(test-parser '(:doc (:blockquote (:plain "hoge" :eol "fuga"))) ">hoge\nfuga\n\n")

;; paragraph
(test-parser '(:doc (:plain "hogehoge" :eol "fugafuga")) 
	     "hogehoge\nfugafuga\n\n")

;; plain
(test-parser '(:doc (:header :h1 "____****")) "# ____****")

;; verbatim
(test-parser '(:doc (:verbatim "hogehoge\nfugafuga\n"))
	     "    hogehoge\n    fugafuga\n")
;; ``` style
(test-parser '(:doc (:verbatim "hogehoge\nfugafuga\n"))
	     "```\nhogehoge\nfugafuga\n```\n")
;; call #136
(test-parser '(:doc (:verbatim "hogehoge\n\nfugafuga\n"))
	     "```\nhogehoge\n\nfugafuga\n```\n")

;; reference
;; reference won't work as we eliminate them
#|
(test-parser '(:doc (:reference (:label "ref") "source" "")) "[ref]: source\n")
(test-parser '(:doc (:reference (:label "ref") "source" ""))
	     "[ref]: source \"\"\n")
(test-parser '(:doc (:reference (:label "ref") "source" "title"))
	     "[ref]: source \"title\"\n")
(test-parser '(:doc (:reference (:label "ref") "source" "title"))
	     "[ref]: source 'title'\n")
(test-parser '(:doc (:reference (:label "ref") "source" "title"))
	     "[ref]: source (title)\n")
|#

;; list
(test-parser '(:doc (:ordered-list (:item (:plain "item1")) (:item (:plain "item2"))))
	     "1. item1\n2. item2\n")
(test-parser '(:doc (:bullet-list (:item (:plain "item1")) (:item (:plain "item2"))))
	     "* item1\n* item2\n")
;; list with inline
(test-parser '(:doc (:bullet-list (:item (:plain "item1 " (:code "code")))))
	     "* item1 `code`\n")

;; inline code
(test-parser '(:doc (:header :h1 (:code "code"))) "# `code`\n")
(test-parser '(:doc (:header :h1 (:code "co`de"))) "# ``co`de``\n")

;; line
(test-parser '(:doc (:line)) "----\n\n")

;; entity
(test-parser '(:doc (:plain "\x12345;")) "&#x12345;")
(test-parser '(:doc (:plain "\x12345;")) "&#X12345;")
(test-parser '(:doc (:plain " ")) "&#32;")
(test-parser '(:doc (:plain "¥")) "&yen;")

;; escaped char
(test-parser '(:doc (:plain  "¥&yen;")) "&yen;\\&yen;")

;; converter
(define-syntax test-converter
  (syntax-rules ()
    ((_ sxml s-mark opts ...)
     (test-equal s-mark sxml (markdown-sexp->sxml s-mark opts ...)))))

;; line
(test-converter '(div (@) (hr (@))) '(:doc (:line)))

;; header
(test-converter '(div (@) (h1 (@) "header")) '(:doc (:header :h1 "header")))
(test-converter '(div (@) (h2 (@) "header")) '(:doc (:header :h2 "header")))

;; paragraph
(test-converter '(div (@) (p (@) "sentence")) '(:doc (:paragraph "sentence")))
(test-converter '(div (@) (p (@) "sentence")) 
		'(:doc (:paragraph "sentence" :eol)))
(test-converter '(div (@) (p (@) "sentence1" "\n" "sentence2")) 
		'(:doc (:paragraph "sentence1" :eol "sentence2")))

;; blockquote
(test-converter '(div (@) (blockquote (@) "sentence")) 
		'(:doc (:blockquote "sentence")))
(test-converter '(div (@) (blockquote (@) "sentence")) 
		'(:doc (:blockquote "sentence" :eol)))
(test-converter '(div (@) (blockquote (@) "sentence1" "\n" "sentence2")) 
		'(:doc (:blockquote "sentence1" :eol "sentence2")))

;; list
(test-converter '(div (@) (ol (@) (li (@) "item1") (li (@) "item2")))
		'(:doc (:ordered-list (:item "item1") (:item "item2"))))
(test-converter '(div (@) (ul (@) (li (@) "item1") (li (@) "item2")))
		'(:doc (:bullet-list (:item "item1") (:item "item2"))))

;; plain
(test-converter '(div (@) (p (@) "sentence")) '(:doc (:plain "sentence")))

;; verbatim
(test-converter '(div (@) (pre (@) "sentence")) '(:doc (:verbatim "sentence")))
;; won't happen with parser
(test-converter '(div (@) (pre (@) "sentence" "sentence2"))
		'(:doc (:verbatim "sentence" "sentence2")))

;; reference
;; a bit tricky because all references are collected and appended
;; to the bottom of the document
(test-converter '(div (@) (div (@ (id "references"))
			       (div (@)
				    "[ref]: source 'title'")))
		'(:doc (:reference (:label "ref") "source" "title"))
		:no-reference #f)
(test-converter '(div (@) 
		      (p (@)
			 (a (@ (href "http://foo")
			       (title "title"))
			    "label"))
		      (div (@ (id "references"))
			   (div (@)
				"[ref]: http://foo 'title'")))
		'(:doc (:paragraph (:link (:label "label") "http://foo" "title"))
		       (:reference (:label "ref") "http://foo" "title"))
		:no-reference #f)

;; html-block
(test-converter '(div (@) (table)) '(:doc (:html-block (table))))

;; TODO more inlines
(test-converter '(div (@) (p (@) (img (@ (src "image.jpg") 
					 (alt "alt text")
					 (title "title")))))
		'(:doc (:plain (:image (:link (:label "alt text") 
					      "image.jpg" "title")))))
;; emph
(test-converter '(div (@) (p (@) (em (@) "blabla")))
		'(:doc (:plain (:emph "blabla"))))
;; strong
(test-converter '(div (@) (p (@) (strong (@) "blabla")))
		'(:doc (:plain (:strong "blabla"))))
;; strike
(test-converter '(div (@) (p (@) (del (@) "strike")))
		'(:doc (:plain (:strike "strike"))))

;; high level APIs

(define-syntax test-reader
  (syntax-rules ()
    ((_ expected str opts ...)
     (test-equal expected expected (string->markdown str opts ...)))))

(test-reader '(div (@) (h1 (@) "header")) "# header\n")
(test-reader "<div><h1>header</h1></div>" "# header\n" :as 'html :no-indent #t)
(test-reader '(:doc (:header :h1 "header")) "# header\n" :as 'sexp)

(test-end)
