;; -*- scheme -*-
(import (rnrs)
	(rfc base32)
	(srfi :1)
	(srfi :64 testing))

(test-begin "RFC Base32")

(define *long-string*
  ;; first verse of Wikipedia Base32
  "Base32 is an encoding method based on the base-32 numeral system. It uses an alphabet of 32 digits, each of which represents a different combination of 5 bits (25). Since base32 is not very widely adopted, the question of notation-which characters to use to represent the 32 digits-is not as settled as in the case of more well-known numeral systems (such as hexadecimal), though RFCs and unofficial and de-facto standards exist. One way to represent Base32 numbers in human-readable form is using digits 0-9 followed by the twenty-two upper-case letters A-V. However, many other variations are used in different contexts. Historically, Baudot code could be considered a modified (stateful) base32 code.")
(define *encoded-long-string*
  ;; encoded by online encoder
  "IJQXGZJTGIQGS4ZAMFXCAZLOMNXWI2LOM4QG2ZLUNBXWIIDCMFZWKZBAN5XCA5DIMUQGEYLTMUWTGMRANZ2W2ZLSMFWCA43ZON2GK3JOEBEXIIDVONSXGIDBNYQGC3DQNBQWEZLUEBXWMIBTGIQGI2LHNF2HGLBAMVQWG2BAN5TCA53INFRWQIDSMVYHEZLTMVXHI4ZAMEQGI2LGMZSXEZLOOQQGG33NMJUW4YLUNFXW4IDPMYQDKIDCNF2HGIBIGI2SSLRAKNUW4Y3FEBRGC43FGMZCA2LTEBXG65BAOZSXE6JAO5UWIZLMPEQGCZDPOB2GKZBMEB2GQZJAOF2WK43UNFXW4IDPMYQG433UMF2GS33OFV3WQ2LDNAQGG2DBOJQWG5DFOJZSA5DPEB2XGZJAORXSA4TFOBZGK43FNZ2CA5DIMUQDGMRAMRUWO2LUOMWWS4ZANZXXIIDBOMQHGZLUORWGKZBAMFZSA2LOEB2GQZJAMNQXGZJAN5TCA3LPOJSSA53FNRWC223ON53W4IDOOVWWK4TBNQQHG6LTORSW24ZAFBZXKY3IEBQXGIDIMV4GCZDFMNUW2YLMFEWCA5DIN52WO2BAKJDEG4ZAMFXGIIDVNZXWMZTJMNUWC3BAMFXGIIDEMUWWMYLDORXSA43UMFXGIYLSMRZSAZLYNFZXILRAJ5XGKIDXMF4SA5DPEBZGK4DSMVZWK3TUEBBGC43FGMZCA3TVNVRGK4TTEBUW4IDIOVWWC3RNOJSWCZDBMJWGKIDGN5ZG2IDJOMQHK43JNZTSAZDJM5UXI4ZAGAWTSIDGN5WGY33XMVSCAYTZEB2GQZJAOR3WK3TUPEWXI53PEB2XA4DFOIWWGYLTMUQGYZLUORSXE4ZAIEWVMLRAJBXXOZLWMVZCYIDNMFXHSIDPORUGK4RAOZQXE2LBORUW63TTEBQXEZJAOVZWKZBANFXCAZDJMZTGK4TFNZ2CAY3PNZ2GK6DUOMXCASDJON2G64TJMNQWY3DZFQQEEYLVMRXXIIDDN5SGKIDDN52WYZBAMJSSAY3PNZZWSZDFOJSWIIDBEBWW6ZDJMZUWKZBAFBZXIYLUMVTHK3BJEBRGC43FGMZCAY3PMRSS4===")

(define utf8-transcoder (make-transcoder (utf-8-codec) 'none))
(define (test-encode-port name open-in open-out expect v
			  :key (transcoder utf8-transcoder)
			  :allow-other-keys opt)
  (define (test-out)
    (define (all)
      (define out (open-output-bytevector))
      (define bout (apply open-out out opt))
      (put-bytevector bout (string->bytevector v transcoder))
      (close-port bout)
      (test-equal (format "~a (output port all)" name) expect 
		  (utf8->string (get-output-bytevector out))))
    (define (one)
      (define out (open-output-bytevector))
      (define bout (apply open-out out opt))
      (let ((bv (string->bytevector v transcoder)))
	(do ((len (bytevector-length bv)) (i 0 (+ i 1)))
	    ((= i len) (close-port bout))
	  (put-u8 bout (bytevector-u8-ref bv i))))
      (test-equal (format "~a (output port one)" name) expect 
		  (utf8->string (get-output-bytevector out))))
    (all)
    (one))
  (define (test-in)
    (define (all)
      (define in (open-bytevector-input-port (string->bytevector v transcoder)))
      (define bin (apply open-in in opt))
      (let ((bv (get-bytevector-all bin)))
	(close-port bin)
	(unless (eof-object? bv)
	  (test-equal (format "~a (input port all)" name) expect 
		      (utf8->string bv)))))
    (define (one)
      (define in (open-bytevector-input-port (string->bytevector v transcoder)))
      (define bin (apply open-in in opt))
      (let-values (((out extract) (open-bytevector-output-port)))
	(do ((b (get-u8 bin) (get-u8 bin)))
	    ((eof-object? b) (close-port bin))
	  (put-u8 out b))
	(test-equal (format "~a (input port one)" name) expect 
		    (utf8->string (extract)))))
    (all)
    (one))

  (test-out)
  (test-in))

(define-syntax test-encode
  (syntax-rules (base32-encode-string)
    ((_ name expect (base32-encode-string v opt ...))
     (begin
       (test-equal '(name v) expect (base32-encode-string v opt ...))
       (test-encode-port '(name v)
			 open-base32-encode-input-port
			 open-base32-encode-output-port
			 expect v opt ...)))))

(test-encode "encode" "" (base32-encode-string ""))
(test-encode "encode" "MY======" (base32-encode-string "f"))
(test-encode "encode" "MZXQ====" (base32-encode-string "fo"))
(test-encode "encode" "MZXW6===" (base32-encode-string "foo"))
(test-encode "encode" "MZXW6YQ=" (base32-encode-string "foob"))
(test-encode "encode" "MZXW6YTB" (base32-encode-string "fooba"))
(test-encode "encode" "MZXW6YTBOI======" (base32-encode-string "foobar"))

(test-encode "encode" "MY"         (base32-encode-string "f" :padding? #f))
(test-encode "encode" "MZXQ"       (base32-encode-string "fo" :padding? #f))
(test-encode "encode" "MZXW6"      (base32-encode-string "foo" :padding? #f))
(test-encode "encode" "MZXW6YQ"    (base32-encode-string "foob" :padding? #f))
(test-encode "encode" "MZXW6YTB"   (base32-encode-string "fooba" :padding? #f))
(test-encode "encode" "MZXW6YTBOI" (base32-encode-string "foobar" :padding? #f))

(test-encode "encode" *encoded-long-string*
	     (base32-encode-string *long-string* :line-width #f))

(define (test-decode-port name open-in open-out expect v
			  :key (transcoder utf8-transcoder))
  (define (test-input)
    (define (test-all)
      (define in (open-bytevector-input-port (string->utf8 v)))
      (define bin (open-in in))
      (let ((e (get-bytevector-all bin)))
	(unless (eof-object? e)
	  (test-equal (format "~a (input port all)" name) expect 
		      (bytevector->string e transcoder))))
      (close-port bin)) ;; just for GC friendliness...
    (define (test1)
      (define in (open-bytevector-input-port (string->utf8 v)))
      (define bin (open-in in))
      (let-values (((out extract) (open-string-output-port)))
	(let loop ()
	  (let ((b (get-u8 bin)))
	    (if (eof-object? b)
		(test-equal (format "~a (input port one)" name) expect 
			    (extract))
		(let ((c (integer->char b)))
		  (put-char out c)
		  (loop))))))
      (close-port bin)) ;; just for GC friendliness...
    (test-all)
    (test1))
  (define (test-output)
    (define (test-all)
      (define out (open-output-bytevector))
      (define bout (open-out out))
      (put-bytevector bout (string->utf8 v))
      (close-port bout)
      (test-equal (format "~a (output port all)" name) expect 
		  (bytevector->string (get-output-bytevector out) transcoder)))
    (define (test1)
      (define out (open-output-bytevector))
      (define bout (open-out out))
      (let ((bv (string->utf8 v)))
	(do ((len (bytevector-length bv))
	     (i 0 (+ i 1)))
	    ((= i len) (close-port bout))
	  (put-u8 bout (bytevector-u8-ref bv i))))
      (test-equal (format "~a (output port one)" name) expect
		  (bytevector->string (get-output-bytevector out) transcoder)))

    (test-all)
    (test1))
  (test-input)
  (test-output))

(define-syntax test-decode
  (syntax-rules (base32-decode-string)
    ((_ name expect (base32-decode-string v))
     (begin
       (test-equal '(name v) expect (base32-decode-string v))
       (test-decode-port '(name v)
			 open-base32-decode-input-port
			 open-base32-decode-output-port
			 expect v)))))

(test-decode "decode" ""       (base32-decode-string ""                ))
(test-decode "decode" "f"      (base32-decode-string "MY======"        ))
(test-decode "decode" "fo"     (base32-decode-string "MZXQ===="        ))
(test-decode "decode" "foo"    (base32-decode-string "MZXW6==="        ))
(test-decode "decode" "foob"   (base32-decode-string "MZXW6YQ="        ))
(test-decode "decode" "fooba"  (base32-decode-string "MZXW6YTB"        ))
(test-decode "decode" "foobar" (base32-decode-string "MZXW6YTBOI======"))

(test-decode "decode" "f"      (base32-decode-string "MY"        ))
(test-decode "decode" "fo"     (base32-decode-string "MZXQ"      ))
(test-decode "decode" "foo"    (base32-decode-string "MZXW6"     ))
(test-decode "decode" "foob"   (base32-decode-string "MZXW6YQ"   ))
(test-decode "decode" "fooba"  (base32-decode-string "MZXW6YTB"  ))
(test-decode "decode" "foobar" (base32-decode-string "MZXW6YTBOI"))
(test-decode "decode" *long-string* (base32-decode-string *encoded-long-string*))

(test-end)
