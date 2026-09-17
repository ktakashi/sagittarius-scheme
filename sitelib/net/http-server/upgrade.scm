;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/upgrade.scm - protocol-agnostic HTTP upgrade support
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
(library (net http-server upgrade)
  (export http-server:connection?
          make-http-server:connection
          http-server:connection-server
          http-server:connection-socket

          http-server:custom-connection?
          make-http-server:custom-connection
          http-server:custom-connection-process
          http-server:custom-connection-close

          http-server:http-connection
          http-server:http-connection?
          make-http-server:http-connection
          http-server:http-connection-buffer
          http-server:http-connection-buffer-set!
          http-server:http-connection-request-count
          http-server:http-connection-request-count-set!
          http-server:http-connection-parse-state
          http-server:http-connection-parse-state-set!
          http-server:http-connection-driver
          http-server:http-connection-driver-set!
          http-server:http-connection-protocol-connection
          http-server:http-connection-protocol-connection-set!
          http-server:http-connection-protocol-registry
          http-server:http-connection-upgrade-registry

          http-server:upgrade-registry?
          make-http-server:upgrade-registry
          http-server:register-upgrade-handler!
          http-server:upgrade-handler-registered?

          http-server:http-connection-attempt-upgrade!
          http-server:connection-close!)
  (import (rnrs)
          (clos user)
          (net http-server types)
          (net http-server request)
          (rename (only (net http-server protocol)
                        http-server:connection
                        http-server:connection?
                        make-http-server:connection
                        http-server:connection-server
                        http-server:connection-socket
                        http-server:connection-process
                        http-server:connection-close
                        http-server:connection-close!)
                  (http-server:connection-close!
                   protocol-connection-close!)))

  (define-record-type http-server:custom-connection
    (parent http-server:connection))

  (define http-server:custom-connection-process
    http-server:connection-process)

  (define http-server:custom-connection-close
    http-server:connection-close)

  (define-record-type http-server:http-connection
    (parent http-server:connection)
    (fields (mutable buffer
                     http-server:http-connection-buffer
                     http-server:http-connection-buffer-set!)
            (mutable request-count
                     http-server:http-connection-request-count
                     http-server:http-connection-request-count-set!)
            (mutable parse-state
                     http-server:http-connection-parse-state
                     http-server:http-connection-parse-state-set!)
            (mutable driver
                     http-server:http-connection-driver
                     http-server:http-connection-driver-set!)
            (mutable protocol-connection
                     http-server:http-connection-protocol-connection
                     http-server:http-connection-protocol-connection-set!)
            protocol-registry
            upgrade-registry))

  (define-record-type (http-server:upgrade-registry
                       %make-http-server:upgrade-registry
                       http-server:upgrade-registry?)
    (fields (mutable handlers))
    (protocol (lambda (p)
                (lambda ()
                  (p '())))))

  (define (normalize-token token)
    (http-server:normalize-header-name token))

  (define (remove-upgrade-handler handlers token)
    (let loop ((rest handlers) (out '()))
      (cond ((null? rest) (reverse out))
            ((string=? (caar rest) token)
             (loop (cdr rest) out))
            (else
             (loop (cdr rest) (cons (car rest) out))))))

  (define (http-server:register-upgrade-handler! registry token handler)
    (let* ((k (normalize-token token))
           (rest (remove-upgrade-handler
                  (http-server:upgrade-registry-handlers registry)
                  k)))
      (http-server:upgrade-registry-handlers-set!
       registry
       (cons (cons k handler) rest))))

  (define (http-server:upgrade-handler-registered? registry token)
    (let ((k (normalize-token token)))
      (let loop ((handlers (http-server:upgrade-registry-handlers registry)))
        (and (pair? handlers)
             (or (string=? (caar handlers) k)
                 (loop (cdr handlers)))))))

  ;; Returns 3 values: status, new-connection, keep-open?
  ;; status is one of 'none, 'handled, 'error.
  (define (http-server:http-connection-attempt-upgrade! conn req remainder app-handler)
    (let* ((headers (http-server:request-headers req))
           (upgrade-registry (http-server:http-connection-upgrade-registry conn)))
      (if (and (http-server:http-connection? conn)
               (http-server:headers-contains-token? headers "connection" "upgrade"))
          (let loop ((handlers (http-server:upgrade-registry-handlers upgrade-registry)))
            (if (null? handlers)
                (values 'none conn #t)
                (let* ((token (caar handlers))
                       (handler (cdar handlers)))
                  (if (http-server:headers-contains-token? headers "upgrade" token)
                      (let-values (((status new-conn keep-open?)
                                    (handler conn req remainder app-handler)))
                        (if (eq? status 'declined)
                            (loop (cdr handlers))
                            (values status new-conn keep-open?)))
                      (loop (cdr handlers))))))
          (values 'none conn #t))))

  (define (make-http-server:upgrade-registry)
    (%make-http-server:upgrade-registry))

  (define (http-server:connection-close! conn)
    (cond ((http-server:http-connection? conn)
           (let ((protocol-conn (http-server:http-connection-protocol-connection conn)))
             (when protocol-conn
               (guard (e (else #f))
                 (protocol-connection-close! protocol-conn)))))
          ((http-server:custom-connection? conn)
           (guard (e (else #f))
             ((http-server:custom-connection-close conn))))
          (else #f))
    #t)
)
