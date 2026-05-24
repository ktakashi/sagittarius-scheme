(import (rnrs)
	(sagittarius pam)
        (sagittarius stty)
	(sagittarius process)
	(srfi :39 parameters))

(define (prompts-handler prompts)
  (define (prompt-handler prompt)
    (display (cdr prompt)) (flush-output-port (current-output-port))
    (if (eq? (car prompt) 'echo-off)
	(with-stty '((not echo) echonl) 
         (lambda () (get-line (current-input-port))))
        (get-line (current-input-port))))
  (vector-map prompt-handler prompts))
(print '==========)
(print *pam:conversation-error-handler*)
(print (get-passwd "yo32es2"))
(parameterize ((*pam:conversation-error-handler* print))
  (cond ((pam-authenticate "login" "yo32es2" prompts-handler) =>
	 (lambda (token)
           ;; do whatever with the token
	   (print (create-process "ls" '("-l" "/etc")
				  :call? #f
				  :stdout (current-output-port)
				  :transcoder (native-transcoder)
				  :token token))
 	   (pam-invalidate-token! token)))
	(else 
         ;; handle authentication error
	 )))
