;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http1.scm - HTTP/1.1 parser and writer
;;;

#!nounbound
#!read-macro=sagittarius/regex
(library (net http-server http1)
    (export http-server:http1-request?
	    http-server:http1-connection?
	    make-http-server:http1-connection
	    *http-server:http1-driver*

	    <http1-config> http1-config? make-http1-config
	    http1-config-max-request-par-connection
	    http1-config-max-pipelined-requests)
    (import (rnrs)
	    (clos user)
	    (sagittarius) ;; for get-keyword
	    (net socket)
	    (net http-server exchange)
	    (net http-server types)
	    (net http-server request)
	    (net http-server response)
	    (net http-server protocol)
	    (net http-server upgrade)
	    (rfc :5322)
	    (sagittarius regex)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (util bytevector))

(define +default-max-request-par-connection+ 100)
(define +default-max-pipelined-requests+ 16)

(define-class <http1-config> (<http-config>)
  ((max-requests-per-connection :init-keyword :max-requests-per-connection
				:init-value +default-max-request-par-connection+
				:reader http1-config-max-request-par-connection)
   (max-pipelined-requests :init-keyword :max-pipelined-requests
			   :init-value +default-max-pipelined-requests+
			   :reader http1-config-max-pipelined-requests)))

(define (http1-config? o) (is-a? o <http1-config>))
(define (make-http1-config . rest) (apply make <http1-config> rest))

(define-record-type http-server:http1-connection
  (parent http-server:http-connection)
  (protocol (lambda (n)
	      (lambda (server socket process close state driver)
		((n server socket process close state driver))))))

(define-record-type http-server:http1-request
  (parent http-server:request)
  (protocol (lambda (n)
	      (lambda (method target path query version headers body remote-info
		       exchange)
		((n method target path query version headers body remote-info
		    '()
		    exchange))))))

(define +crlf+ #vu8(#x0d #x0a))
(define +crlf-crlf+ #vu8(#x0d #x0a #x0d #x0a))

(define bv-sub bytevector-copy)
(define bv-concat bytevector-concatenate)
(define find-bytes bytevector-contains)
(define trim string-trim-both)

(define (split-once s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((= i len) (values s #f))
            ((char=? (string-ref s i) ch)
             (values (substring s 0 i) (substring s (+ i 1) len)))
            (else
             (loop (+ i 1)))))))

(define (parse-request-line line)
  (cond ((#/^(.+)\s+(.+)\s+(.+)$/ line) =>
	 (lambda (m)
	   (values (string->symbol (m 1))
                   (m 2)
                   (m 3)
                   #f)))
	(else (values #f #f #f "Malformed request line"))))


(define (parse-target target)
  (let-values (((path query) (split-once target #\?)))
    (values (if (string=? path "") "/" path) query)))

(define (parse-content-length raw)
  (http-server:parse-nonnegative-integer raw))


;; state offsets are absolute and valid while the buffer only grows by append.
(define-record-type http1-consume-state
  (fields (mutable stage)
          (mutable scan)
          (mutable line-end)
          (mutable method)
          (mutable target)
          (mutable version)
          (mutable headers)
          (mutable framing)
          (mutable body-start)
          (mutable length)
          (mutable chunks)
          (mutable chunk-scan)
          (mutable chunk-size)
	  app-handler
	  connection-info)
  (protocol (lambda (p)
	      (lambda (app-handler connection-info)
		(p 'start 0 #f #f #f #f #f 'none 0 #f '() 0 0
		   app-handler connection-info)))))

(define (make-remainder buffer next)
  (bv-sub buffer next (bytevector-length buffer)))

(define (make-http1-request* method target version headers body connection-info)
  (let-values (((path query) (parse-target target)))
    (define exchange (make-http-server:exchange connection-info))
    (make-http-server:http1-request method target path query
				    version headers body
				    (http-server:connection-info-remote
				     connection-info)
				    exchange)))

(define (reset-http1-consume-state! st)
  (http1-consume-state-stage-set! st 'start)
  (http1-consume-state-scan-set! st 0)
  (http1-consume-state-line-end-set! st #f)
  (http1-consume-state-method-set! st #f)
  (http1-consume-state-target-set! st #f)
  (http1-consume-state-version-set! st #f)
  (http1-consume-state-headers-set! st #f)
  (http1-consume-state-framing-set! st 'none)
  (http1-consume-state-body-start-set! st 0)
  (http1-consume-state-length-set! st #f)
  (http1-consume-state-chunks-set! st '())
  (http1-consume-state-chunk-scan-set! st 0)
  (http1-consume-state-chunk-size-set! st 0))

;; the line must end with \r\n
(define (strict-read-line bin)
  (let-values (((out e) (open-bytevector-output-port)))
    (let loop ((cr? #f))
      (let ((u8 (get-u8 bin)))
	(cond ((eof-object? u8) (utf8->string (e)))
	      ;; \r\n
	      ((and (= u8 #x0a) cr?) (utf8->string (e)))
	      ((= u8 #x0d)
	       (when cr? (put-u8 out #x0d)) ;; emit previous \r
	       (loop #t))
	      (else (put-u8 out u8) (loop #f)))))))

(define (resume-scan len)
  ;; keep overlap for delimiters that can be split across reads.
  (if (> len 3) (- len 3) 0))

(define (parse-header-bytevector bv)
  (let ((bin (open-bytevector-input-port bv)))
    (let ((headers (rfc5322-read-headers bin #t strict-read-line)))
      (make-http-server:headers headers))))

(define (http1-ready st buffer body next)
  (let ((method (http1-consume-state-method st))
        (target (http1-consume-state-target st))
        (version (http1-consume-state-version st))
        (headers (http1-consume-state-headers st))
        (connection-info (http1-consume-state-connection-info st)))
    (reset-http1-consume-state! st)
    (values 'ready
            (make-http1-request* method target version headers body connection-info)
            #f
            (make-remainder buffer next)
            #f)))

(define (consume:start st buffer max-header-bytes next)
  (let* ((blen (bytevector-length buffer))
         (line-end (find-bytes buffer +crlf+ (http1-consume-state-scan st))))
    (cond ((not line-end)
           (if (> blen max-header-bytes)
               (values 'error 431 "Headers too large" buffer #f)
               (begin
                 (http1-consume-state-scan-set! st (resume-scan blen))
                 (values 'start #f #f buffer st))))
          ((> line-end max-header-bytes)
           (values 'error 431 "Headers too large" buffer #f))
          (else
           (let-values (((method target version line-error)
                         (parse-request-line
                          (utf8->string (bv-sub buffer 0 line-end)))))
             (cond (line-error
                    (values 'error 400 line-error buffer #f))
                   ((not (or (string=? version "HTTP/1.1")
                             (string=? version "HTTP/1.0")))
                    (values 'error 505 "Unsupported HTTP version" buffer #f))
                   (else
                    (http1-consume-state-method-set! st method)
                    (http1-consume-state-target-set! st target)
                    (http1-consume-state-version-set! st version)
                    (http1-consume-state-line-end-set! st line-end)
                    (http1-consume-state-scan-set! st line-end)
                    (http1-consume-state-stage-set! st 'line)
                    (next))))))))

(define (consume:line st buffer max-header-bytes max-body-bytes next)
  (define head-ref http-server:headers-ref)
  (define (chunked? headers)
    (http-server:headers-contains-token? headers
                                         "transfer-encoding"
                                         "chunked"))
  (let* ((blen (bytevector-length buffer))
         (head-end (find-bytes buffer +crlf-crlf+
                               (http1-consume-state-scan st))))
    (cond ((not head-end)
           (if (> blen max-header-bytes)
               (values 'error 431 "Headers too large" buffer #f)
               (begin
                 (http1-consume-state-scan-set! st (resume-scan blen))
                 (values 'line #f #f buffer st))))
          ((> head-end max-header-bytes)
           (values 'error 431 "Headers too large" buffer #f))
          (else
           (guard (e (else
                      (values 'error 400 (condition-message e) buffer #f)))
             (let* ((line-end (http1-consume-state-line-end st))
                    (headers (parse-header-bytevector
                              (bv-sub buffer (+ line-end 2) head-end)))
                    (te (head-ref headers "transfer-encoding" #f))
                    (cl (head-ref headers "content-length" #f))
                    (body-start (+ head-end 4)))
               (cond ((and te cl)
                      (values 'error 400
                              "Both transfer-encoding and content-length are present"
                              buffer
                              #f))
                     ((and te (chunked? headers))
                      (http1-consume-state-headers-set! st headers)
                      (http1-consume-state-body-start-set! st body-start)
                      (http1-consume-state-framing-set! st 'chunked)
                      (http1-consume-state-chunks-set! st '())
                      (http1-consume-state-chunk-size-set! st 0)
                      (http1-consume-state-chunk-scan-set! st body-start)
                      (http1-consume-state-stage-set! st 'header)
                      (next))
                     (cl
                      (let ((len (parse-content-length cl)))
                        (cond ((not len)
                               (values 'error 400
                                       "Malformed content-length"
                                       buffer
                                       #f))
                              ((> len max-body-bytes)
                               (values 'error 413
                                       "Request body too large"
                                       buffer
                                       #f))
                              (else
                               (http1-consume-state-headers-set! st headers)
                               (http1-consume-state-body-start-set! st body-start)
                               (http1-consume-state-framing-set! st 'length)
                               (http1-consume-state-length-set! st len)
                               (http1-consume-state-stage-set! st 'header)
                               (next)))))
                     (else
                      (http1-consume-state-headers-set! st headers)
                      (http1-consume-state-body-start-set! st body-start)
                      (http1-consume-state-framing-set! st 'none)
                      (http1-consume-state-stage-set! st 'header)
                      (next)))))))))

(define (consume:header st buffer max-body-bytes)
  (define (consume-chunk st buffer max-body-bytes)
    (define u8 bytevector-u8-ref)
    (define blen (bytevector-length buffer))
    (define i (http1-consume-state-chunk-scan st))
    (define line-end (find-bytes buffer +crlf+ i))

    (if (not line-end)
        (values 'header #f #f buffer st)
        (let ((size (string->number 
		     (utf8->string (bv-sub buffer i line-end)) 16)))
          (if (or (not size) (< size 0))
              (values 'error 400 "Malformed chunk size" buffer #f)
              (let* ((chunk-start (+ line-end 2))
                     (chunk-end (+ chunk-start size)))
                (cond ((> (+ chunk-end 2) blen)
                       (values 'header #f #f buffer st))
                      ((= size 0)
                       (cond ((and (= (u8 buffer chunk-end) #x0d)
                                   (= (u8 buffer (+ chunk-end 1)) #x0a))
                              (http1-ready st buffer
                                           (bv-concat
                                            (reverse!
					     (http1-consume-state-chunks st)))
                                           (+ chunk-end 2)))
                             ((find-bytes buffer +crlf-crlf+ chunk-end) =>
                              (lambda (end)
                                (http1-ready st buffer
                                             (bv-concat
                                              (reverse!
                                               (http1-consume-state-chunks st)))
                                             (+ end 4))))
                             (else
                              (values 'header #f #f buffer st))))
                      ((or (not (= (u8 buffer chunk-end) #x0d))
                           (not (= (u8 buffer (+ chunk-end 1)) #x0a)))
                       (values 'error 400 "Malformed chunk payload" buffer #f))
                      (else
                       (let ((n (+ (http1-consume-state-chunk-size st) size)))
                         (if (> n max-body-bytes)
                             (values 'error 413 "Request body too large"
				     buffer #f)
                             (begin
                               (http1-consume-state-chunks-set!
                                st
                                (cons (bv-sub buffer chunk-start chunk-end)
                                      (http1-consume-state-chunks st)))
                               (http1-consume-state-chunk-size-set! st n)
                               (http1-consume-state-chunk-scan-set! st
                                (+ chunk-end 2))
                               (consume-chunk st buffer max-body-bytes)))))))))))
  (case (http1-consume-state-framing st)
    ((none)
     (http1-ready st buffer #vu8() (http1-consume-state-body-start st)))
    ((length)
     (let* ((body-start (http1-consume-state-body-start st))
            (len (http1-consume-state-length st))
            (blen (bytevector-length buffer))
            (next (+ body-start len)))
       (if (> next blen)
           (values 'header #f #f buffer st)
           (http1-ready st buffer (bv-sub buffer body-start next) next))))
    ((chunked) (consume-chunk st buffer max-body-bytes))
    (else (values 'error 500 "Unknown body framing state" buffer #f))))

(define (http-server:http1-consume conn)
  (define config (or (http-server:http-connection-config conn)
		     (make-http1-config)))
  (define max-header-bytes (http-config-max-header-bytes config))
  (define max-body-bytes (http-config-max-body-bytes config))
  (define buffer (http-server:http-connection-buffer conn))
  (define st (http-server:http-connection-parse-state conn))
  (define (consume st buffer)
    (let loop ()
      (case (http1-consume-state-stage st)
	((start) (consume:start st buffer max-header-bytes loop))
	((line) (consume:line st buffer max-header-bytes max-body-bytes loop))
	((header) (consume:header st buffer max-body-bytes))
	(else (values 'error 500 "Unknown consume stage" buffer #f)))))
  (let-values (((kind code message remaining next-state) (consume st buffer)))
    (http-server:http-connection-buffer-set! conn remaining)
    (values kind code message)))
		

(define (make-error-response code message)
  (let ((res (make-http-server:response code)))
    (http-server:response-text! res message)
    (http-server:response-header-set! res "content-type"
				      "text/plain; charset=utf-8")
    res))

(define (http-server:http1-feed conn)
  (define config (or (http-server:http-connection-config conn)
		     (make-http1-config)))
  (define max-requests-per-connection
    (http1-config-max-request-par-connection config))
  (define max-pipelined-requests (http1-config-max-pipelined-requests config))
  (define driver (http-server:http-connection-driver conn))
  (define st (http-server:http-connection-parse-state conn))
  (define app-handler (http1-consume-state-app-handler st))
  (define (make-application-error-response exchange)
    (let ((er (make-http-server:response 500 exchange)))
      (http-server:response-text! er "Unhandled application error")
      er))
  (define (invoke-handler app-handler req res)
    (let ((exchange (http-server:request-exchange req)))
      (%http-server:exchange-mark-handling! exchange)
      (guard (e (else (make-application-error-response exchange)))
  (let ((response (%http-server:normalize-handler-response
      (app-handler req res) res exchange)))
    (let-values (((ok? code message)
      (http-server:validate-final-response
       (http-server:request-method req)
       (http-server:response-status response)
       'http1)))
      (if ok?
    response
    (let ((err (make-http-server:response code exchange)))
      (http-server:response-text! err message)
      err)))))))
  (define (serve! driver conn exchange req result)
    (guard (e (else
	       (%http-server:exchange-mark-aborted! exchange)
	       #t))
      (%http-server:exchange-mark-response-started! exchange)
      (let ((r (http-server:protocol-driver-serve! driver conn req result)))
	(%http-server:exchange-mark-completed! exchange)
	r)))

  (let loop ((served 0))
    (let-values (((kind req b) (http-server:http1-consume conn)))
      (cond ((or (eq? kind 'start) (eq? kind 'line) (eq? kind 'header)) #f)
            ((eq? kind 'error)
             (let* ((code req)
                    (message b)
                    (res (make-error-response code message)))
               (http-server:protocol-driver-serve! driver conn #f res)
               #t))
            (else
             (let-values (((upgrade-status upgraded-conn keep-open?)
                           (http-server:http-connection-attempt-upgrade!
                            conn req
                            (http-server:http-connection-buffer conn)
                            app-handler)))
               (cond
		((eq? upgrade-status 'handled)
                 (if keep-open? upgraded-conn #t))
		((eq? upgrade-status 'error) #t)
		(else
                 (let* ((exchange (http-server:request-exchange req))
			(res (make-http-server:response 200 exchange))
			(result (invoke-handler app-handler req res))
			(close? (serve! driver conn exchange req result)))
                   (http-server:http-connection-request-count-set!
                    conn
                    (+ 1 (http-server:http-connection-request-count conn)))
                   (if (or close?
                           (>= (http-server:http-connection-request-count conn)
                               max-requests-per-connection))
                       (http-server:close-connection! conn)
                       (if (and (< served max-pipelined-requests)
				(> (bytevector-length
                                    (http-server:http-connection-buffer conn)) 0))
                           (loop (+ served 1))
                           #f)))))))))))
    

(define +http1-stream-chunk-size+ 8192)
(define +chunked-stream-end+ #vu8(#x30 #x0d #x0a #x0d #x0a))

(define (response-body->payload body)
  (cond ((bytevector? body) (values 'bytes body))
        ((string? body) (values 'bytes (string->utf8 body)))
        ((and (input-port? body) (binary-port? body)) (values 'port body))
        (else (values #f #f))))

(define (close-input-port-quietly port)
  (guard (e (else #f))
    (close-port port)))

(define (stream-body-to-socket! socket port chunk-handler)
  (dynamic-wind
    (lambda () #f)
    (lambda ()
    (let loop ()
      (let ((bv (get-bytevector-n port +http1-stream-chunk-size+)))
	(unless (eof-object? bv)
          (chunk-handler bv)
          (loop)))))
    (lambda () (close-input-port-quietly port))))

(define (stream-raw-body! socket port)
  (stream-body-to-socket! socket port
   (lambda (chunk)
     (socket-send socket chunk))))

(define (stream-chunked-body! socket port)
  (stream-body-to-socket! socket port
   (lambda (chunk)
     (let ((size (bytevector-length chunk)))
       (socket-send socket (string->utf8 (string-append (number->string size 16)
                                                        "\r\n")))
       (socket-send socket chunk)
       (socket-send socket +crlf+))))
  (socket-send socket +chunked-stream-end+))

(define utf8-transcoder (make-transcoder (utf-8-codec) (eol-style lf)))
(define (write-head! socket code reason headers)
  (let-values (((out extract) (open-bytevector-output-port utf8-transcoder)))
    (display "HTTP/1.1 " out)
    (display code out)
    (display " " out)
    (display reason out)
    (display "\r\n" out)
    (for-each (lambda (kv)
                (for-each (lambda (v)
                            (display (car kv) out)
                            (display ": " out)
                            (display v out)
                            (display "\r\n" out))
                          (cdr kv)))
              headers)
    (display "\r\n" out)
    (socket-send socket (extract))))

(define (request-close? req)
  (let ((hdrs (http-server:request-headers req)))
    (or (http-server:headers-contains-token? hdrs "connection" "close")
	(and (string=? (http-server:request-http-version req) "HTTP/1.0")
             (not (http-server:headers-contains-token? hdrs "connection" "keep-alive"))))))

(define (filter-response-headers headers)
  (let loop ((rest headers) (out '()))
    (cond ((null? rest) (reverse out))
          ((http-server:hop-by-hop-header? (caar rest))
           (if (or (string=? (caar rest) "connection")
                   (string=? (caar rest) "transfer-encoding"))
               (loop (cdr rest) (cons (car rest) out))
               (loop (cdr rest) out)))
          (else
           (loop (cdr rest) (cons (car rest) out))))))

(define (http-server:http1-serve socket req res)
  (define (ensure-header res name value)
    (unless (http-server:response-header-ref res name #f)
      (http-server:response-header-set! res name value)))
  (define (request-method req)
    (and req (http-server:request-method req)))
  (define (request-http11? req)
    (and req (string=? (http-server:request-http-version req) "HTTP/1.1")))
  (define (response-headers)
    (http-server:response-headers res))
  (define (header-ref name)
    (http-server:response-header-ref res name #f))
  (define (delete-header! name)
    (http-server:headers-delete! (response-headers) name))
  (define (set-header! name value)
    (http-server:response-header-set! res name value))
  (define (contains-token? name token)
    (http-server:headers-contains-token? (response-headers) name token))
  (let* ((code (http-server:response-status res))
         (reason (or (http-server:response-reason res)
                     (http-server:reason-phrase code))))
    (let-values (((body-kind body)
                  (response-body->payload (http-server:response-body res))))
      (if (not body-kind)
        (let ((err (make-http-server:response 500)))
          (http-server:response-text! err "Unsupported response body type")
          (http-server:http1-serve socket req err))
        (let* ((mode (http-server:response-content-mode
                      (request-method req)
                      code))
               (content-allowed? (http-server:response-content-allowed? mode))
               (content-length-policy
                (http-server:response-content-length-policy mode))
               (transfer-coding-allowed?
                (http-server:response-transfer-coding-allowed? mode))
               (streaming? (eq? body-kind 'port))
               (buffered? (eq? body-kind 'bytes))
               (body-size (and buffered? (bytevector-length body)))
               (chunked-stream?
                (and content-allowed? streaming? (request-http11? req)))
               (close? (or (not req)
                           (request-close? req)
                           (contains-token? "connection" "close")
                           (and content-allowed?
                                streaming?
                                (not chunked-stream?))))
	       (value (if close? "close" "keep-alive")))
	  (ensure-header res "date" (http-server:current-http-date))

          (when (not content-allowed?)
            (when streaming?
              (close-input-port-quietly body)))

          (if (and content-allowed? buffered?)
              (begin
                (set-header! "content-length" (number->string body-size))
                (delete-header! "transfer-encoding"))
              (case content-length-policy
                ((forbidden)
                 (delete-header! "content-length"))
                ((zero-or-omit)
                 (set-header! "content-length" "0"))
                ((representation)
                 (let ((provided (header-ref "content-length")))
                   (cond ((and (eq? (request-method req) 'HEAD)
                       (not (eqv? code 304))
                       body-size)
                      ;; HEAD can report corresponding representation length.
                      (set-header! "content-length"
                           (number->string body-size)))
                     ((not provided) #f)
                         ((not (http-server:parse-nonnegative-integer provided))
                          (delete-header! "content-length"))
                         ((and streaming? provided)
                          ;; Input-port response length is not verifiable.
                      (delete-header! "content-length")))))))

          (cond ((not transfer-coding-allowed?)
                 (delete-header! "transfer-encoding"))
                (chunked-stream?
                 (delete-header! "content-length")
                 (set-header! "transfer-encoding" "chunked"))
                (else
                 (delete-header! "transfer-encoding")))

          ;; Keep framing coherent.
          (when (and (header-ref "content-length")
                     (header-ref "transfer-encoding"))
            (delete-header! "transfer-encoding"))

	  (set-header! "connection" value)
          (write-head!
           socket
           code
           reason
           (filter-response-headers
            (http-server:headers->alist (response-headers))))
          (when content-allowed?
            (if streaming?
                (if chunked-stream?
                    (stream-chunked-body! socket body)
                    (stream-raw-body! socket body))
                (socket-send socket body)))
          close?)))))

(define (http-server:http1-connect server socket app-handler . rest)
  (let* ((conn (make-http-server:http1-connection
		server socket
		http-server:http1-consume
		(lambda (conn) #t) ;; nothing to do
		#f
		*http-server:http1-driver*))
	 (remote-info (http-server:connection-remote-info conn))
	 (connection-info (if (tls-socket? socket)
			      (make-http-server:connection-info
			       remote-info
			       (tls-socket-peer-certificate socket)
			       (tls-socket-selected-alpn socket)
			       "http/1.1")
			      (make-http-server:connection-info
			       remote-info #f #f "http/1.1")))
	 (state (make-http1-consume-state app-handler
		 connection-info)))
    (http-server:http-connection-parse-state-set! conn state)
    conn))

(define *http-server:http1-driver*
  (make-http-server:protocol-driver "http/1.1"
				    http-server:http1-feed
				    http-server:http1-serve
				    http-server:http1-connect))

)
