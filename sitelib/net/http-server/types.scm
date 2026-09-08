;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/types.scm - HTTP server shared types and helpers
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

#!nounbound
(library (net http-server types)
  (export http-server:headers?
          make-http-server:headers
          http-server:headers-empty?
          http-server:headers-ref
          http-server:headers-ref*
          http-server:headers-set!
          http-server:headers-add!
          http-server:headers-delete!
          http-server:headers->alist
          http-server:headers-contains-token?

          http-server:normalize-header-name
          http-server:hop-by-hop-header?

          http-server:reason-phrase
          http-server:status-class

          http-server:current-http-date)
  (import (rnrs)
          (srfi :19))

  (define (ascii-downcase-char c)
    (if (and (char>=? c #\A) (char<=? c #\Z))
        (integer->char (+ (char->integer c) 32))
        c))

  (define (ascii-downcase-string s)
    (list->string (map ascii-downcase-char (string->list s))))

  (define (string-trim-ascii s)
    (define len (string-length s))
    (define (space? c)
      (or (char=? c #\space) (char=? c #\tab)
          (char=? c #\return) (char=? c #\newline)))
    (define (find-start i)
      (if (or (= i len) (not (space? (string-ref s i)))) i (find-start (+ i 1))))
    (define (find-end i)
      (if (or (< i 0) (not (space? (string-ref s i)))) i (find-end (- i 1))))
    (let* ((start (find-start 0))
           (end (find-end (- len 1))))
      (if (> start end) "" (substring s start (+ end 1)))))

  (define (http-server:normalize-header-name name)
    (ascii-downcase-string (string-trim-ascii name)))

  (define-record-type (http-server:headers
                       %make-http-server:headers
                       http-server:headers?)
    (fields (mutable entries)))

  (define (make-http-server:headers :optional (alist '()))
    (let ((h (%make-http-server:headers '())))
      (for-each (lambda (kv)
                  (let ((name (car kv))
                        (values (cdr kv)))
                    (for-each (lambda (v)
                                (http-server:headers-add! h name v))
                              values)))
                alist)
      h))

  (define (http-server:headers-empty? headers)
    (null? (http-server:headers-entries headers)))

  (define (find-entry headers name)
    (let ((key (http-server:normalize-header-name name)))
      (let loop ((rest (http-server:headers-entries headers)))
        (and (pair? rest)
             (let ((kv (car rest)))
               (if (string=? (car kv) key)
                   kv
                   (loop (cdr rest))))))))

  (define (http-server:headers-ref* headers name :optional (default '()))
    (cond ((find-entry headers name) => cdr)
          (else default)))

  (define (http-server:headers-ref headers name :optional (default #f))
    (let ((v* (http-server:headers-ref* headers name #f)))
      (if (and v* (pair? v*)) (car v*) default)))

  (define (replace-entry entries key values)
    (let loop ((rest entries) (out '()))
      (cond ((null? rest) (reverse (cons (cons key values) out)))
            ((string=? (caar rest) key)
             (append (reverse out) (cons (cons key values) (cdr rest))))
            (else (loop (cdr rest) (cons (car rest) out))))))

  (define (->string v)
    (if (string? v)
        v
        (let-values (((out extract) (open-string-output-port)))
          (display v out)
          (extract))))

  (define (http-server:headers-set! headers name value)
    (let* ((key (http-server:normalize-header-name name))
           (v (->string value)))
      (http-server:headers-entries-set!
       headers
       (replace-entry (http-server:headers-entries headers) key (list v)))))

  (define (http-server:headers-add! headers name value)
    (let* ((key (http-server:normalize-header-name name))
           (v (->string value))
           (entry (find-entry headers key)))
      (if entry
          (http-server:headers-entries-set!
           headers
           (replace-entry (http-server:headers-entries headers)
                          key
                          (append (cdr entry) (list v))))
          (http-server:headers-entries-set!
           headers
           (append (http-server:headers-entries headers)
                   (list (cons key (list v))))))))

  (define (http-server:headers-delete! headers name)
    (let ((key (http-server:normalize-header-name name)))
      (http-server:headers-entries-set!
       headers
       (let loop ((rest (http-server:headers-entries headers)) (out '()))
         (cond ((null? rest) (reverse out))
               ((string=? (caar rest) key) (loop (cdr rest) out))
               (else (loop (cdr rest) (cons (car rest) out))))))))

  (define (http-server:headers->alist headers)
    (map (lambda (kv) (cons (car kv) (cdr kv)))
         (http-server:headers-entries headers)))

  (define (split-comma s)
    (define len (string-length s))
    (let loop ((i 0) (start 0) (out '()))
      (cond ((= i len)
             (reverse (cons (string-trim-ascii (substring s start i)) out)))
            ((char=? (string-ref s i) #\,)
             (loop (+ i 1) (+ i 1)
                   (cons (string-trim-ascii (substring s start i)) out)))
            (else (loop (+ i 1) start out)))))

  (define (http-server:headers-contains-token? headers name token)
    (let* ((needle (ascii-downcase-string token))
           (vals (http-server:headers-ref* headers name '())))
      (let loop-v ((rest vals))
        (and (pair? rest)
             (or (let loop-t ((tok* (split-comma (car rest))))
                   (and (pair? tok*)
                        (or (string=? (ascii-downcase-string (car tok*)) needle)
                            (loop-t (cdr tok*)))))
                 (loop-v (cdr rest)))))))

  (define +hop-by-hop-header-names+
    '("connection" "keep-alive" "proxy-authenticate" "proxy-authorization"
      "te" "trailer" "transfer-encoding" "upgrade"))

  (define (http-server:hop-by-hop-header? name)
    (let ((key (http-server:normalize-header-name name)))
      (let loop ((rest +hop-by-hop-header-names+))
        (and (pair? rest)
             (or (string=? key (car rest))
                 (loop (cdr rest)))))))

  (define +status-reasons+
    '((100 . "Continue")
      (101 . "Switching Protocols")
      (200 . "OK")
      (201 . "Created")
      (202 . "Accepted")
      (204 . "No Content")
      (206 . "Partial Content")
      (301 . "Moved Permanently")
      (302 . "Found")
      (304 . "Not Modified")
      (307 . "Temporary Redirect")
      (308 . "Permanent Redirect")
      (400 . "Bad Request")
      (401 . "Unauthorized")
      (403 . "Forbidden")
      (404 . "Not Found")
      (405 . "Method Not Allowed")
      (408 . "Request Timeout")
      (411 . "Length Required")
      (413 . "Payload Too Large")
      (414 . "URI Too Long")
      (415 . "Unsupported Media Type")
      (417 . "Expectation Failed")
      (431 . "Request Header Fields Too Large")
      (500 . "Internal Server Error")
      (501 . "Not Implemented")
      (502 . "Bad Gateway")
      (503 . "Service Unavailable")
      (504 . "Gateway Timeout")
      (505 . "HTTP Version Not Supported")))

  (define (http-server:reason-phrase code)
    (cond ((assq code +status-reasons+) => cdr)
          (else "Unknown")))

  (define (http-server:status-class code)
    (cond ((and (integer? code) (<= 100 code 199)) 'informational)
          ((and (integer? code) (<= 200 code 299)) 'success)
          ((and (integer? code) (<= 300 code 399)) 'redirect)
          ((and (integer? code) (<= 400 code 499)) 'client-error)
          ((and (integer? code) (<= 500 code 599)) 'server-error)
          (else 'unknown)))

  (define (http-server:current-http-date)
    (date->string (current-date 0) "~a, ~d ~b ~Y ~H:~M:~S GMT"))
)
