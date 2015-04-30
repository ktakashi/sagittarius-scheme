(import (rnrs)
	(text markdown)
	(srfi :64))

(test-begin "Markdown")

(define-syntax test-parser
  (syntax-rules ()
    ((_ expected str)
     (test-equal str expected (parse-markdown (open-string-input-port str))))))

(test-parser '(:doc (:h6 "hoge")) "###### hoge\n")
(test-parser '(:doc (:h6 "hoge__a")) "###### hoge__a\n")
(test-parser '(:doc (:h6 "hoge'a")) "###### hoge'a\n")

;; with inline
(test-parser '(:doc (:h6 (:emph "hoge"))) "###### *hoge*\n")
(test-parser '(:doc (:h6 (:strong "hoge"))) "###### **hoge**\n")
(test-parser '(:doc (:h6 (:emph "hoge"))) "###### _hoge_\n")
(test-parser '(:doc (:h6 (:strong "hoge"))) "###### __hoge__\n")

;; inline link
(test-parser '(:doc (:h1 (:link (:url "http://hogehoge"))))
	     "# <http://hogehoge>\n")
(test-parser '(:doc (:h1 (:link (:email "ktakashi@ymail.com"))))
	     "# <ktakashi@ymail.com>\n")
(test-parser '(:doc (:h1 (:link (:email "ktakashi@ymail.com"))))
	     "# <mailto:ktakashi@ymail.com>\n")

(test-parser '(:doc (:h1 (:link (:label "link") "source " "title")))
	     "# [link](source 'title')\n")
(test-parser '(:doc (:h1 (:link (:label "link") "source " "title")))
	     "# [link](source \"title\")\n")

(test-end)
