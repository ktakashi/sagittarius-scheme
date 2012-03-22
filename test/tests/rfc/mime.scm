;; -*- mode:scheme; coding: utf-8; -*-
(import (rnrs)
	(rfc mime)
	(rfc :5322)
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

(test-begin "(run-rfc-mime-test)")
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
	    (let* ((in (open-string-input-port multi-part))
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