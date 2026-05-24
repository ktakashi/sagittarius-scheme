(import (rnrs)
	(rfc ssh)
        (rfc sftp)
	(srfi :39)
	(clos core)
	(clos user)
	(sagittarius mop allocation)
	(time)
	(sagittarius crypto keys)
	(sagittarius crypto pem)
	(sagittarius crypto x509)
	(sagittarius crypto pkcs keys))

;; (define-class <foo> (<allocation-mixin>)
;;   ((foo)
;;    (bar :allocation :virtual :init-keyword :foo
;; 	:slot-ref (lambda (o) (slot-ref o 'foo))
;; 	:slot-set! (lambda (o v) (slot-set! o 'foo v)))))
;; (define-class <bar> (<foo>)
;;   ((buz :init-value '())))
;; (let ((foo (make <foo> :foo 'foo)))
;;   (change-class foo <bar>)
;;   (print foo)
;;   (print (slot-ref foo 'foo))
;;   (print (slot-ref foo 'bar))
;;   (print (slot-ref foo 'buz)))
  

#;(let ((kp (ssh-read-identity-file "/Users/yo32es/.ssh/id_rsa" "snrykn15")))
  (call-with-output-file "priv.pem"
    (lambda (out) (write-pem-object (->pem-object (key-pair-private kp)) out)))
  (call-with-output-file "pub.pem"
    (lambda (out) (write-pem-object (->pem-object (key-pair-public kp)) out))))
(define priv
  (pkcs-one-asymmetric-key-private-key
   (pem-object->object (call-with-input-file "priv.pem" read-pem-object))))
(define pub
  (subject-public-key-info->public-key
   (pem-object->object (call-with-input-file "pub.pem" read-pem-object))))

(define conn (make-client-sftp-connection "192.168.64.6" "22"))
;;(define authenticator (sftp-public-key-authentication "takashi"))
(define authenticator (sftp-public-key-authentication "takashi" priv pub))
(parameterize ((*ssh:ext-info-handler* print))
  (open-client-sftp-connection! conn :authenticate authenticator))

(print conn)
(let ((handle (sftp-open conn "/home/takashi/tmp.zip"
			 (bitwise-ior +ssh-fxf-read+))))
  (print "receiving file ===>")
  (print handle)
  (print (bytevector-length (time (sftp-read conn handle (sftp-binary-receiver))))))
#;(let ((handle (sftp-open conn "/home/takashi/tmp.zip"
			 (bitwise-ior +ssh-fxf-write+ +ssh-fxf-creat+))))
  (print handle)
  (call-with-input-file "/Users/yo32es/Downloads/sagittarius-17020740688.zip"
    (lambda (bin)
      (print "sending file ====>")
      (time (sftp-write! conn handle bin)))
    :transcoder #f))


(close-client-sftp-connection! conn)


