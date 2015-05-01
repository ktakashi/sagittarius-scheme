(import (rnrs)
	(text markdown)
	(srfi :64))

(test-begin "Markdown")

(define-syntax test-parser
  (syntax-rules ()
    ((_ expected str)
     (test-equal str expected (parse-markdown (open-string-input-port str))))))

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
(test-parser '(:doc (:header :h1 (:link (:label "mailto:ktakashi@ymail.com")
				"ktakashi@ymail.com" "")))
	     "# <ktakashi@ymail.com>\n")
(test-parser '(:doc (:header :h1 (:link (:label "mailto:ktakashi@ymail.com")
				"ktakashi@ymail.com" "")))
	     "# <mailto:ktakashi@ymail.com>\n")

;; explicit link
(test-parser '(:doc (:header :h1 (:link (:label "link") "source" "title")))
	     "# [link](source 'title')\n")
(test-parser '(:doc (:header :h1 (:link (:label "link") "source" "title")))
	     "# [link](source \"title\")\n")

;; image
(test-parser '(:doc (:header :h1 (:image (:link (:label "link") "source" "title"))))
	    "# ![link](source 'title')\n")

;; note
(test-parser '(:doc (:note (:ref "note") "note"))
	     "[^note]: note\n")
(test-parser '(:doc (:note (:ref "note") "note" :eol "note2"))
	     "[^note]: note\n    note2\n")
(test-parser '(:doc (:note (:ref "note") "note" :eol "note2" :eol "note3"))
	     "[^note]: note\n    note2\n \n    note3\n")
;; note with code
(test-parser '(:doc (:note (:ref "note") "note" " " (:code "code")))
	     "[^note]: note `code`\n")

;; inline note
(test-parser '(:doc (:plain (:note "note"))) "^[note]")

;; inline note
(test-parser '(:doc (:header :h1 (:note "note"))) "# ^[note]\n")

;; block quote
(test-parser '(:doc (:blockquote "hoge" :eol "fuga" :eol)) ">hoge\n>fuga\n\n")
(test-parser '(:doc (:blockquote "hoge" :eol "fuga" :eol)) ">hoge\nfuga\n\n")

;; paragraph
(test-parser '(:doc (:paragraph "hogehoge" :eol "fugafuga" :eol)) 
	     "hogehoge\nfugafuga\n\n")

;; plain
(test-parser '(:doc (:plain "#" " " "____" "****")) "# ____****")

;; verbatim
(test-parser '(:doc (:verbatim "hogehoge\nfugafuga"))
	     "    hogehoge\n    fugafuga\n")
;; ``` style
(test-parser '(:doc (:verbatim "hogehoge\nfugafuga"))
	     "```\nhogehoge\nfugafuga\n```\n")

;; reference
(test-parser '(:doc (:reference (:label "ref") "source" "")) "[ref]: source\n")
(test-parser '(:doc (:reference (:label "ref") "source" ""))
	     "[ref]: source \"\"\n")
(test-parser '(:doc (:reference (:label "ref") "source" "title"))
	     "[ref]: source \"title\"\n")
(test-parser '(:doc (:reference (:label "ref") "source" "title"))
	     "[ref]: source 'title'\n")
(test-parser '(:doc (:reference (:label "ref") "source" "title"))
	     "[ref]: source (title)\n")

;; list
(test-parser '(:doc (:ordered-list (:item "item1") (:item "item2")))
	     "1. item1\n2. item2\n")
(test-parser '(:doc (:bullet-list (:item "item1") (:item "item2")))
	     "* item1\n* item2\n")
;; list with inline
(test-parser '(:doc (:bullet-list (:item "item1" " " (:code "code"))))
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
(test-parser '(:doc (:plain (& "yen"))) "&yen;")

;; escaped char
(test-parser '(:doc (:plain (& "yen") "&" "yen;")) "&yen;\\&yen;")

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
(test-converter '(div (@) (p (@) "sentence" "\n")) 
		'(:doc (:paragraph "sentence" :eol)))
(test-converter '(div (@) (p (@) "sentence1" "\n" "sentence2")) 
		'(:doc (:paragraph "sentence1" :eol "sentence2")))

;; blockquote
(test-converter '(div (@) (blockquote (@) "sentence")) 
		'(:doc (:blockquote "sentence")))
(test-converter '(div (@) (blockquote (@) "sentence" "\n")) 
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

;; high level APIs

(define-syntax test-reader
  (syntax-rules ()
    ((_ expected str opts ...)
     (test-equal str expected (string->markdown str opts ...)))))

(test-reader '(div (@) (h1 (@) "header")) "# header\n")
(test-reader "<div><h1>header</h1></div>" "# header\n" :as 'html :no-indent #t)
(test-reader '(:doc (:header :h1 "header")) "# header\n" :as 'sexp)

(test-end)
