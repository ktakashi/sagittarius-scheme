(import (rnrs)
        (sagittarius crypto ciphers)
        (sagittarius crypto keys))

(define key (generate-symmetric-key *scheme:aes-256*))
;; GCM-SIV nonce size = 12
(define iv (make-bytevector 12))

;; AES-256-GCM-SIV 
(define aes-cipher
  (make-block-cipher *scheme:aes-256* *mode:gcm-siv* no-padding))

(define parameter
  (make-cipher-parameter
   (make-iv-parameter iv)
   ;; empyt AAD :)
   (make-aad-parameter #vu8())))

(define (encrypt-messages . messages)
  (for-each (lambda (message)
              ;; output space is not needed, and this procedure returns 0
              (block-cipher-encrypt! aes-cipher message 0 #vu8() 0)) messages)
  (let* ((out-size (block-cipher-last-block-size aes-cipher 0))
         (out (make-bytevector out-size)))
    ;; passing dummy block
    (block-cipher-encrypt-last-block! aes-cipher #vu8() 0 out 0)
    (values out (block-cipher-done/tag aes-cipher))))

(block-cipher-init! aes-cipher (cipher-direction encrypt) key parameter)
(let-values (((enc tag) (encrypt-messages (string->utf8 "Hello, GCM-SIV") (string->utf8 "\nI'm done"))))
  (block-cipher-init! aes-cipher (cipher-direction decrypt) key (make-cipher-parameter parameter (make-tag-parameter tag)))
  (let* ((size (block-cipher-last-block-size aes-cipher enc))
         (msg (make-bytevector size)))
    (block-cipher-decrypt-last-block! aes-cipher enc 0 msg 0)
    (block-cipher-done/tag! aes-cipher tag)
    (utf8->string msg)))
;; "Hello, GCM-SIV\nI'm done"
