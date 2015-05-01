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
(test-parser '(:doc (:header :h1 (:link (:label "link") "source " "title")))
	     "# [link](source 'title')\n")
(test-parser '(:doc (:header :h1 (:link (:label "link") "source " "title")))
	     "# [link](source \"title\")\n")

;; image
(test-parser '(:doc (:header :h1 (:image (:link (:label "link") "source " "title"))))
	    "# ![link](source 'title')\n")

;; note
(test-parser '(:doc (:note (:ref "note") "note"))
	     "[^note]: note\n")
(test-parser '(:doc (:note (:ref "note") "note" "note2"))
	     "[^note]: note\n    note2\n")
(test-parser '(:doc (:note (:ref "note") "note" "note2" "note3"))
	     "[^note]: note\n    note2\n \n    note3\n")

;; inline note
(test-parser '(:doc (:header :h1 (:note "note"))) "# ^[note]\n")

;; block quote
(test-parser '(:doc (:blockquote "hoge" "fuga" "\n")) ">hoge\n>fuga\n\n")
(test-parser '(:doc (:blockquote "hoge" "fuga" "\n")) ">hoge\nfuga\n\n")

;; paragraph
(test-parser '(:doc (:paragraph "hogehoge" :eol "fugafuga" :eol)) 
	     "hogehoge\nfugafuga\n\n")

;; plain
(test-parser '(:doc (:plain "#" " " "____" "****")) "# ____****")

;; verbatim
(test-parser '(:doc (:verbatim "hogehoge\nfugafuga"))
	     "    hogehoge\n    fugafuga\n")

;; reference
(test-parser '(:doc (:reference (:label "ref") "source" "")) "[ref]: source\n")
(test-parser '(:doc (:reference (:label "ref") "source " ""))
	     "[ref]: source \"\"\n")
(test-parser '(:doc (:reference (:label "ref") "source " "title"))
	     "[ref]: source \"title\"\n")
(test-parser '(:doc (:reference (:label "ref") "source " "title"))
	     "[ref]: source 'title'\n")
(test-parser '(:doc (:reference (:label "ref") "source " "title"))
	     "[ref]: source (title)\n")

;; list
(test-parser '(:doc (:ordered-list (:item "item1") (:item "item2")))
	     "1. item1\n2. item2\n")
(test-parser '(:doc (:bullet-list (:item "item1") (:item "item2")))
	     "* item1\n* item2\n")

;; inline code
(test-parser '(:doc (:header :h1 (:code "code"))) "# `code`\n")
(test-parser '(:doc (:header :h1 (:code "co`de"))) "# ``co`de``\n")

(test-end)
