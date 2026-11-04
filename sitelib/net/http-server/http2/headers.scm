;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http2/state.scm - HTTP/2 server driver state
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
(library (net http-server http2 headers)
    (export parse-target
	    parse-http2-request-headers
	    response->hpack-headers
	    collect-push-headers
	    method->http-token)
    (import (rnrs)
	    (net http-server request)
	    (net http-server response)
	    (net http-server types))

(define (parse-target target)
  (let-values (((path query) (split-once target #\?)))
    (values (if (string=? path "") "/" path) query)))

(define (parse-http2-request-headers headers)
  (define header-map (make-http-server:headers))
  (define (fail message) (values #f #f #f #f #f message))
  (define (method-connect? method)
    (and method (string=? (string-upcase method) "CONNECT")))
  (define (add-authority-as-host! authority)
    (when (and authority
               (not (http-server:headers-ref header-map "host" #f)))
      (http-server:headers-add! header-map "host" authority)))
  (let loop ((rest headers)
	     (pseudo-open? #t)
	     (method #f)
	     (scheme #f)
	     (target #f)
	     (authority #f))
    (if (null? rest)
        (let ((method-token (and method (string-upcase method))))
          (cond ((not method-token)
                 (fail "Missing required pseudo headers"))
                ((method-connect? method-token)
                 (cond ((or scheme target)
                        (fail "CONNECT must use only :method and :authority pseudo headers"))
                       ((not authority)
                        (fail "Missing required pseudo headers"))
                       (else
                        (add-authority-as-host! authority)
                        (values (string->symbol method-token)
                                authority
                                authority
                                #f
                                header-map
                                #f))))
                ((and scheme target)
                 (let-values (((path query) (parse-target target)))
                   (add-authority-as-host! authority)
                   (values (string->symbol method-token)
                           target
                           path
                           query
                           header-map
                           #f)))
                (else
                 (fail "Missing required pseudo headers"))))
        (let-values (((name value err pseudo?) (decode-header-entry (car rest))))
          (if err
              (fail err)
              (cond
               ((contains-uppercase-ascii? name)
                (fail "Uppercase header name is not allowed"))
               ((and pseudo? (not pseudo-open?))
                (fail "Pseudo header must appear before regular headers"))
               (pseudo?
                (cond
                 ((string=? name ":method")
                  (if method
                      (fail "Duplicate :method")
		      (loop (cdr rest)
			    pseudo-open? value scheme target authority)))
                 ((string=? name ":scheme")
                  (if scheme
                      (fail "Duplicate :scheme")
		      (loop (cdr rest)
			    pseudo-open? method value target authority)))
                 ((string=? name ":path")
                  (if target
                      (fail "Duplicate :path")
		      (loop (cdr rest)
			    pseudo-open? method scheme value authority)))
                 ((string=? name ":authority")
                  (if authority
                      (fail "Duplicate :authority")
		      (loop (cdr rest)
			    pseudo-open? method scheme target value)))
                 (else (fail "Unknown pseudo header"))))
               (else
		(cond
                 ((connection-specific-header? name)
                  (fail "Connection-specific header is not allowed in HTTP/2"))
                 ((and (string=? name "te")
                       (not (string-ci=? value "trailers")))
                  (fail "Only TE: trailers is allowed in HTTP/2"))
                 (else
                  (http-server:headers-add! header-map name value)
                  (loop (cdr rest) #f method scheme target authority))))))))))

(define (response->hpack-headers req res)
  (define (header-entry name value)
    (list (string->utf8 name) (string->utf8 value)))
  (define (delete-header! name)
    (http-server:headers-delete! (http-server:response-headers res) name))
  (define (set-header! name value)
    (http-server:response-header-set! res name value))
  (define (header-ref name)
    (http-server:response-header-ref res name #f))
  (define (collect headers nominated)
    (let loop ((rest (http-server:headers->alist headers)) (out '()))
      (if (null? rest)
          (reverse out)
          (let* ((name (http-server:normalize-header-name (caar rest)))
                 (values (cdar rest)))
            (if (or (response-prohibited-header? name nominated)
                    (and (> (string-length name) 0)
                         (char=? (string-ref name 0) #\:)))
                (loop (cdr rest) out)
                (loop (cdr rest)
                      (append (map (lambda (v) (header-entry name v)) values)
                              out)))))))

  (let* ((status (http-server:response-status res))
         (method (and req (http-server:request-method req)))
         (mode (http-server:response-content-mode method status))
         (content-allowed? (http-server:response-content-allowed? mode))
         (content-length-policy
          (http-server:response-content-length-policy mode))
         (nominated
          (connection-option-tokens (http-server:response-headers res)))
         (body (http-server:response-body res)))
    (let-values (((body-kind body-bytes body-port)
                  (response-body->payload body)))
      (if (not body-kind)
          (let ((err (make-http-server:response 500)))
            (http-server:response-text! err "Unsupported response body type")
            (response->hpack-headers req err))
          (let* ((buffered? (eq? body-kind 'bytes))
                 (streaming? (eq? body-kind 'port))
                 (body-size (and buffered? (bytevector-length body-bytes)))
                 (skip-body? (not content-allowed?))
                 (bytes (if (and content-allowed? buffered?) body-bytes #vu8()))
                 (port (if (and content-allowed? streaming?) body-port #f)))
            (unless (header-ref "date")
              (set-header! "date" (http-server:current-http-date)))

            (when (and skip-body? streaming?)
              (guard (e (else #f))
                (close-port body-port)))

            (if (and content-allowed? buffered?)
                (set-header! "content-length" (number->string body-size))
                (case content-length-policy
                  ((forbidden)
                   (delete-header! "content-length"))
                  ((zero-or-omit)
                   (set-header! "content-length" "0"))
                  ((representation)
                   (let ((provided (header-ref "content-length")))
                     (cond ((and (eq? method 'HEAD)
                         (not (eqv? status 304))
                         body-size)
                        ;; HEAD may advertise the corresponding representation size.
                        (set-header! "content-length"
                             (number->string body-size)))
                       ((not provided) #f)
                           ((not (http-server:parse-nonnegative-integer provided))
                            (delete-header! "content-length"))
                           ((and streaming? provided)
                            ;; Input-port response length is not verifiable.
                        (delete-header! "content-length")))))))

            ;; HTTP/2 responses must never carry HTTP/1.x framing headers.
            (delete-header! "connection")
            (delete-header! "proxy-connection")
            (delete-header! "keep-alive")
            (delete-header! "transfer-encoding")
            (delete-header! "upgrade")
            (delete-header! "te")

            ;; Streaming responses don't have a verified content-length.
            (when (and content-allowed? streaming?)
              (delete-header! "content-length"))

            (values (cons (header-entry ":status" (number->string status))
                          (collect (http-server:response-headers res)
                                   nominated))
                    bytes
                    port
                  skip-body?))))))

(define (collect-push-headers headers)
  (define (header-entry name value)
    (list (string->utf8 name) (string->utf8 value)))
  (let ((nominated (connection-option-tokens headers)))
    (let loop ((rest (http-server:headers->alist headers)) (out '()))
      (if (null? rest)
          (reverse out)
          (let* ((name (http-server:normalize-header-name (caar rest)))
                 (values (cdar rest)))
            (if (or (response-prohibited-header? name nominated)
                    (and (> (string-length name) 0)
                         (char=? (string-ref name 0) #\:)))
                (loop (cdr rest) out)
                (loop (cdr rest)
                      (append (map (lambda (v) (header-entry name v)) values)
                              out))))))))

(define (method->http-token method)
  (cond ((symbol? method) (string-upcase (symbol->string method)))
        ((string? method) (string-upcase method))
        (else
         (assertion-violation 'method->http-token
                              "Unsupported HTTP method"
                              method))))
;; helper
(define (split-once s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((= i len) (values s #f))
            ((char=? (string-ref s i) ch)
             (values (substring s 0 i) (substring s (+ i 1) len)))
            (else
             (loop (+ i 1)))))))

(define (split-comma s)
  (define len (string-length s))
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i len)
           (reverse (cons (substring s start i) out)))
          ((char=? (string-ref s i) #\,)
           (loop (+ i 1) (+ i 1)
                 (cons (substring s start i) out)))
          (else (loop (+ i 1) start out)))))

(define (string-member? target lis)
  (let loop ((rest lis))
    (and (pair? rest)
         (or (string=? target (car rest))
             (loop (cdr rest))))))

(define (connection-option-tokens headers)
  (let loop-value ((values (http-server:headers-ref* headers "connection" '()))
                   (out '()))
    (if (null? values)
        out
        (let loop-token ((tokens (split-comma (car values)))
                         (current out))
          (if (null? tokens)
              (loop-value (cdr values) current)
              (let ((name (http-server:normalize-header-name (car tokens))))
                (loop-token (cdr tokens)
                            (if (or (string=? name "")
                                    (string-member? name current))
                                current
                                (cons name current)))))))))

(define (response-prohibited-header? name nominated)
  (or (http-server:hop-by-hop-header? name)
      (string-member? name nominated)
      (string=? name "te")
      (string=? name "proxy-connection")))

(define (connection-specific-header? name)
  (or (string=? name "connection")
      (string=? name "keep-alive")
      (string=? name "proxy-connection")
      (string=? name "transfer-encoding")
      (string=? name "upgrade")))

(define (entry->name&value e)
  (let ((name (and (pair? e) (car e)))
        (value (and (pair? e) (pair? (cdr e)) (cadr e))))
    (if (and (bytevector? name) (bytevector? value))
        (values name value)
        (values #f #f))))

(define (decode-header-entry e)
  (define (pseudo-header? name)
    (and (> (string-length name) 0)
	 (char=? (string-ref name 0) #\:)))
  (let-values (((name value) (entry->name&value e)))
    (if (not name)
        (values #f #f "Malformed HPACK entry" #f)
        (guard (ex (else (values #f #f "Malformed UTF-8 in header" #f)))
	  (let ((h (utf8->string name)))
          (values h (utf8->string value) #f (pseudo-header? h)))))))


(define (response-body->payload body)
  (cond ((bytevector? body) (values 'bytes body #f))
        ((string? body) (values 'bytes (string->utf8 body) #f))
        ((and (input-port? body) (binary-port? body))
         (values 'port #vu8() body))
        (else (values #f #f #f))))

(define (contains-uppercase-ascii? s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (and (< i n)
           (let ((c (string-ref s i)))
             (or (and (char>=? c #\A) (char<=? c #\Z))
                 (loop (+ i 1))))))))
)
