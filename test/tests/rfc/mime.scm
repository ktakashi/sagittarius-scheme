;; -*- mode:scheme; coding: utf-8; -*-
(import (rnrs)
	(rfc mime)
	(rfc :5322)
	(srfi :26 cut)
	(srfi :64 testing))

(define utf-8-text "寿限無、寿限無五劫の擦り切れ海砂利水魚の水行末 雲来末 風来末食う寝る処に住む処やぶら小路の藪柑子パイポパイポ パイポのシューリンガンシューリンガンのグーリンダイグーリンダイのポンポコピーのポンポコナーの長久命の長助")

;; This text is encoded by Gauche.
(define encoded-utf-8-text
  "=?utf-8?B?5a+/6ZmQ54Sh44CB5a+/6ZmQ54Sh5LqU5Yqr44Gu5pOm44KK5YiH44KM5rW356CC?=\r
 =?utf-8?B?5Yip5rC06a2a44Gu5rC06KGM5pyrIOmbsuadpeacqyDpoqjmnaXmnKvpo58=?=\r
 =?utf-8?B?44GG5a+d44KL5Yem44Gr5L2P44KA5Yem44KE44G244KJ5bCP6Lev44Gu6Jeq?=\r
 =?utf-8?B?5p+R5a2Q44OR44Kk44Od44OR44Kk44OdIOODkeOCpOODneOBruOCt+ODpQ==?=\r
 =?utf-8?B?44O844Oq44Oz44Ks44Oz44K344Ol44O844Oq44Oz44Ks44Oz44Gu44Kw44O8?=\r
 =?utf-8?B?44Oq44Oz44OA44Kk44Kw44O844Oq44Oz44OA44Kk44Gu44Od44Oz44Od44Kz?=\r
 =?utf-8?B?44OU44O844Gu44Od44Oz44Od44Kz44OK44O844Gu6ZW35LmF5ZG944Gu6ZW3?=\r
 =?utf-8?B?5Yqp?=")

(define decoded-utf-8-text
  "寿限無、寿限無五劫の擦り切れ海砂\r
 利水魚の水行末 雲来末 風来末食\r
 う寝る処に住む処やぶら小路の藪\r
 柑子パイポパイポ パイポのシュ\r
 ーリンガンシューリンガンのグー\r
 リンダイグーリンダイのポンポコ\r
 ピーのポンポコナーの長久命の長\r
 助")
(define multi-part
  "X-sender: <sender@sendersdomain.com>\r
X-receiver: <somerecipient@recipientdomain.com>\r
From: \"Senders Name\" <sender@sendersdomain.com>\r
To: \"Recipient Name\" <somerecipient@recipientdomain.com>\r
Message-ID: <5bec11c119194c14999e592feb46e3cf@sendersdomain.com>\r
Date: Sat, 24 Sep 2005 15:06:49 -0400\r
Subject: Sample Multi-Part\r
MIME-Version: 1.0\r
Content-Type: multipart/alternative; boundary=\"----=_NextPart_DC7E1BB5_1105_4DB3_BAE3_2A6208EB099D\"\r
\r
------=_NextPart_DC7E1BB5_1105_4DB3_BAE3_2A6208EB099D\r
Content-type: text/plain; charset=iso-8859-1\r
Content-Transfer-Encoding: quoted-printable\r
\r
Sample Text Content
------=_NextPart_DC7E1BB5_1105_4DB3_BAE3_2A6208EB099D\r
Content-type: text/html; charset=iso-8859-1\r
Content-Transfer-Encoding: quoted-printable\r
\r
<html>
<head>
</head>
<body>
<div style=3D\"FONT-SIZE: 10pt; FONT-FAMILY: Arial\">Sample HTML =
Content</div>
</body>
</html>

------=_NextPart_DC7E1BB5_1105_4DB3_BAE3_2A6208EB099D--\r\n")

(test-begin "RFC Mime test")
;; from Gauche start
(test-equal "mime-parse-version"
	    '((1 0) (1 0) (1 0) (1 0) #f)
	    (map mime-parse-version
		 '(" 1.0"
		   " 1.0 (produced by MetaSend Vx.x) "
		   " (produced by MetaSend Vx.x) 1.0"
		   " 1.(produced by MetaSend Vx.x (beta))0"
		   " none ")
		 ))

(test-equal "mime-parse-content-type" '("text" "plain")
	    (mime-parse-content-type " text/plain (client: foo bar)"))
(test-equal "mime-parse-content-type" '("text" "plain" ("charset" . "us-ascii"))
	    (mime-parse-content-type " text/plain ;charset=\"us-ascii\""))
(test-equal "mime-parse-content-type" '("text" "plain" ("charset" . "us-ascii"))
	    (mime-parse-content-type 
	     " text/plain; charset=us-ascii (Plain Text)"))
(test-equal "mime-parse-content-type"
	    '("text" "plain" ("charset" . "iso-2022-jp"))
	    (mime-parse-content-type
	     " text/(Plain Text)plain ; (Japanese) charset=iso-2022-jp"))

(test-equal "mime-parse-content-type"
	    '("text" "plain" ("zzz" . "yyy") ("xxx" . "www"))
	    (mime-parse-content-type
	     " text/plain ;zzz=\"yyy\"; xxx = www (AAA)"))
(test-equal "mime-parse-content-type"
	    '("multipart" "alternative"
	      ("boundary" . "=_alternative 006EBAA488256DF0_="))
	    (mime-parse-content-type
	     "multipart/alternative; boundary=\"=_alternative 006EBAA488256DF0_=\""))

(test-equal "mime-compose-parameters (simple)"
	    "; ab=cd; ef=gh"
	    (mime-compose-parameters '((ab . cd) (ef . gh)) #f))
(test-equal "mime-compose-parameters (quote)"
	    "; ab=\"c d\"; ef=\"\\\"\\\\\""
	    (mime-compose-parameters '((ab . "c d") (ef . "\"\\")) #f))
(test-equal "mime-compose-parameters (long)"
	    "; ab=cd;\r\n foo=012345678901234567890123456789012345679012345678901234567890123456789"
	    (mime-compose-parameters
	     '((ab . cd)
	       (foo . "012345678901234567890123456789012345679012345678901234567890123456789"))
	     #f))

(test-equal "mime-encode-text (pass-through)" "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod\r\n tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim\r\n veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea\r\n commodo consequat. Duis aute irure dolor in reprehenderit in voluptate\r\n velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat\r\n cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id\r\n est laborum."
	    (mime-encode-text "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."))
(test-equal "mime-encode-text (pass-through, nonbreak)" "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
	    (mime-encode-text "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                         :line-width #f))

(test-equal "mime-encode-text" "=?utf-8?B?zrvjga7lroflrpnjgbjjgojjgYbjgZPjgZ0=?=\r\n =?utf-8?B?44CCV2VsY29tZSB0byDOuy1zcGFjZQ==?="
	    (mime-encode-text "\x03bb;\x306e;\x5b87;\x5b99;\x3078;\x3088;\x3046;\x3053;\x305d;\x3002;\x0057;\x0065;\x006c;\x0063;\x006f;\x006d;\x0065;\x0020;\x0074;\x006f;\x0020;\x03bb;\x002d;\x0073;\x0070;\x0061;\x0063;\x0065;" :line-width 50))
(test-equal "mime-encode-text (nobreak)" "=?utf-8?B?zrvjga7lroflrpnjgbjjgojjgYbjgZPjgZ3jgIJXZWxjb21lIHRvIM67LXNwYWNl?="
	    (mime-encode-text "\x03bb;\x306e;\x5b87;\x5b99;\x3078;\x3088;\x3046;\x3053;\x305d;\x3002;\x0057;\x0065;\x006c;\x0063;\x006f;\x006d;\x0065;\x0020;\x0074;\x006f;\x0020;\x03bb;\x002d;\x0073;\x0070;\x0061;\x0063;\x0065;" :line-width #f))

(test-equal "mime-decode-word" "this is some text"
	    (mime-decode-word "=?iso-8859-1?q?this=20is=20some=20text?="))
(test-equal "mime-decode-text" "this is some text"
	    (mime-decode-text
	     "=?iso-8859-1?q?this=20is?= =?iso-8859-1?q?some=20text?="))
(test-equal "mime-decode-text" "this is some text"
	    (mime-decode-text
	     "=?iso-8859-1?q?this=20is?= some=?iso-8859-1?q?=20text?="))

(test-equal "mime-encode-word" "=?iso-8859-1?Q?this=20is=20some=20text?="
	    (mime-encode-word "this is some text" :charset 'iso-8859-1
			      :transfer-encoding 'quoted-printable))
(test-equal "mime-encode-text" "=?iso-8859-1?B?VGhlIHF1aWNr?=\r\n =?iso-8859-1?B?IGJyb3duIGZv?=\r\n =?iso-8859-1?B?eCBqdW1wcyBv?=\r\n =?iso-8859-1?B?dmVyIHRoZSBs?=\r\n =?iso-8859-1?B?YXp5IGRvZw==?="
	    (mime-encode-text "The quick brown fox jumps over the lazy dog"
			      :charset 'iso-8859-1 :force #t
			      :line-width 30))
(test-equal "mime-encode-text" "\r\n =?iso-8859-1?B?VGhlIHF1aWNrIGJyb3du?=\r\n =?iso-8859-1?B?IGZveCBqdW1wcyBvdmVy?=\r\n =?iso-8859-1?B?IHRoZSBsYXp5IGRvZw==?="
	    (mime-encode-text "The quick brown fox jumps over the lazy dog"
			      :charset 'iso-8859-1 :force #t
			      :line-width 40
			      :start-column 20))

(test-equal "mime-decode-word" "Keith_Moore"
	    (mime-decode-word "=?US-ASCII?Q?Keith_Moore?="))
(test-equal "mime-decode-word" "Keith_Moore"
	    (mime-decode-word "=?US-ASCII?B?S2VpdGhfTW9vcmU=?="))
(test-equal "mime-decode-text" "Keith/Moore"
	    (mime-decode-text "=?US-ASCII?B?S2VpdGg=?=/=?US-ASCII?Q?Moore?="))

(test-equal "mime-encode-text" "=?us-ascii?B?VGhlIHF1aWNr?=\r\n =?us-ascii?B?IGJyb3duIGZv?=\r\n =?us-ascii?B?eCBqdW1wcyBv?=\r\n =?us-ascii?B?dmVyIHRoZSBs?=\r\n =?us-ascii?B?YXp5IGRvZw==?="
	    (mime-encode-text "The quick brown fox jumps over the lazy dog"
			      :charset 'us-ascii :force #t
			      :line-width 30))

;; we don't support iso-2022-jp for now.
;;(test-equal "mime-decode-word" "\x5ddd;\x5408; \x53f2;\x6717;"
;;	    (mime-decode-word "=?ISO-2022-JP?B?GyRCQG45ZxsoQiAbJEI7S08vGyhC?="))
;;(test-equal "mime-decode-text" "(\x5ddd;\x5408; \x53f2;\x6717;)"
;;	    (mime-decode-text "(=?iso-2022-jp?b?GyRCQG45ZxsoQg==?= =?iso-2022-jp?b?GyRCO0tPLxsoQg==?=)"))
;;(test-equal "mime-encode-word" "=?iso-2022-jp?B?GyRCQG45ZxsoQiAbJEI7S08vGyhC?="
;;	    (mime-encode-word "\x5ddd;\x5408; \x53f2;\x6717;"
;;			 :charset 'iso-2022-jp))

(define (mime-message-resolver mesg parent)
  (unless (eqv? (mime-part-parent mesg) parent) (error "parent link broken"))
  (cons* (string-append (mime-part-type mesg) "/" (mime-part-subtype mesg))
         (mime-part-index mesg)
         (if (string? (mime-part-content mesg))
           (list (mime-part-content mesg))
           (map (cut mime-message-resolver <> mesg) (mime-part-content mesg)))))

(define (mime-message-tester num headers)
  (let ((src (string-append (current-directory)
			    "/test/data/rfc-mime-"
			    (number->string num)
			    ".txt"))
        (res (call-with-input-file 
		 (string-append (current-directory)
				"/test/data/rfc-mime-"
				(number->string num)
				".res.txt")
               read)))
    (call-with-input-file src
      (lambda (inp)
        (let* ((title (get-line inp)) ;; test title
               (expl  (get-line inp)) ;; explanation (ignored)
               (headers (or headers (rfc5322-read-headers inp))))
          (test-equal (format "mime-parse-message (~a - ~a)" num title)
		      res
		      (and (equal? (mime-parse-version
				    (rfc5322-header-ref headers "mime-version"))
				   '(1 0))
			   (mime-message-resolver
			    (mime-parse-message inp headers
						(cut mime-body->string <> <>))
			    #f)
			   )))))
    ))


(import (sagittarius control)) ;; for dotimes
(dotimes (n 8)
  (mime-message-tester
   n
   (and (= n 6)
        '(("mime-version" " 1.0")
          ("content-type" "multipart/form-data; boundary=\"---------------------------6578815652962098482130719379\"")))))

(let1 b (mime-make-boundary)
  (test-equal "mime-compose-message (simple)"
	      (string-append "\r\n--"b"\r\n"
			     "Content-type: text/plain\r\n"
			     "Content-transfer-encoding: 7bit\r\n\r\n"
			     "This is a pen."
			     "\r\n--"b"\r\n"
			     "Content-type: text/html; charset=us-ascii\r\n\r\n"
			     "<html><head></head><body></body></html>"
			     "\r\n--"b"\r\n"
			     "Content-type: application/octet-stream\r\n"
			     "Content-transfer-encoding: base64\r\n\r\n"
			     "YWJjZGVmZw=="
			     "\r\n--"b"--\r\n")
	      (receive (s bb)
		  (mime-compose-message-string
		   '((("text" "plain")
		      (("content-transfer-encoding" "7bit"))
		      "This is a pen.")
		     (("text" "html" ("charset" . "us-ascii"))
		      ()
		      "<html><head></head><body></body></html>")
		     (("application" "octet-stream")
		      (("content-transfer-encoding" "base64"))
		      "abcdefg"))
		   :boundary b)
		(and (equal? b bb) s))))

(import (match))
(define (mime-roundtrip-tester num)
  (define (gen-parts mesg)
    (match mesg
      [(ctype _ (? string? body))
       `(,(mime-parse-content-type ctype) '() ,body)]
      [("message/rfc822" _ ("text/plain" _ body))
       `(("message" "rfc822") '()
         ,(string-append "content-type: text/plain\r\n\r\n" body))]
      [(ctype _ children ...)
       `(,(mime-parse-content-type ctype) '()
         (subparts ,@(map gen-parts children)))]))
  (let1 src (call-with-input-file
		(string-append (current-directory)
			       "/test/data/rfc-mime-"
			       (number->string num)
			       ".res.txt") read)
    (receive (composed boundary)
        (mime-compose-message-string (list (gen-parts src)))
      (test-equal (format "mime-roundtrip (~a)" num)
		  `("multipart/mixed" 0 ,src)
		  (mime-message-resolver
		   (let1 in (open-bytevector-input-port (string->utf8 composed))
		     (call-with-port
		      (transcoded-port in (native-transcoder))
		      (cut mime-parse-message <>
			   `(("mime-version" "1.0")
			     ("content-type" 
			      ,(string-append "multipart/mixed; boundary=\""
					      boundary
					      "\"")))
			   (cut mime-body->string <> <>))))
		   #f)))))
                     
(dotimes (n 8) (mime-roundtrip-tester n))
    

;; from Gauche end
(test-equal "encoding"
	    encoded-utf-8-text
	    (mime-encode-text utf-8-text))
(test-equal "decoding"
	    decoded-utf-8-text
	    (mime-decode-text encoded-utf-8-text))
(test-equal "parsing"
	    '(#t "multipart" "alternative"
		 (("x-sender" "<sender@sendersdomain.com>")
		  ("x-receiver" "<somerecipient@recipientdomain.com>")
		  ("from" "\"Senders Name\" <sender@sendersdomain.com>")
		  ("to" "\"Recipient Name\" <somerecipient@recipientdomain.com>")
		  ("message-id" "<5bec11c119194c14999e592feb46e3cf@sendersdomain.com>")
		  ("date" "Sat, 24 Sep 2005 15:06:49 -0400")
		  ("subject" "Sample Multi-Part")
		  ("mime-version" "1.0")
		  ("content-type" "multipart/alternative; boundary=\"----=_NextPart_DC7E1BB5_1105_4DB3_BAE3_2A6208EB099D\""))
		 #t)
	    (let* ((utf8-multi-part (string->utf8 multi-part))
		   (bin (open-bytevector-input-port utf8-multi-part))
		   (in (transcoded-port bin (make-transcoder (utf-8-codec))))
		   (headers (rfc5322-read-headers in))
		   (r '()))
	      (if (mime-parse-version (rfc5322-header-ref headers "mime-version"))
		  (let ((packet (mime-parse-message in headers (lambda (packet port)
								 (get-string-all port)))))
		    (set! r (cons (mime-part? packet) r))
		    (set! r (cons (mime-part-type packet) r))
		    (set! r (cons (mime-part-subtype packet) r))
		    (set! r (cons (mime-part-headers packet) r))
		    (set! r (cons (for-all mime-part? (mime-part-content packet)) r))
		    (reverse r))
		  #f)))

(test-end)