;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; punycode.scm - Punycode encoder/decoder
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

;; references:
;; - RFC 3492: https://www.rfc-editor.org/info/rfc3492
;; - RFC 5891: https://www.rfc-editor.org/info/rfc5891
#!nounbound
(library (rfc punycode)
    (export punycode-encode
            punycode-decode
            punycode-label?)
    (import (rnrs)
            (srfi :13 strings))

  (define +punycode-base+ 36)
  (define +punycode-t-min+ 1)
  (define +punycode-t-max+ 26)
  (define +punycode-skew+ 38)
  (define +punycode-damp+ 700)
  (define +punycode-initial-bias+ 72)
  (define +punycode-initial-n+ 128)

  (define (ascii-char? c) (< (char->integer c) #x80))

  (define (punycode-digit c)
    (cond ((char<=? #\a c #\z) (- (char->integer c) (char->integer #\a)))
          ((char<=? #\A c #\Z) (- (char->integer c) (char->integer #\A)))
          ((char<=? #\0 c #\9) (+ 26 (- (char->integer c) (char->integer #\0))))
          (else #f)))

  (define (punycode-char n)
    (cond ((<= 0 n 25) (integer->char (+ n (char->integer #\a))))
          ((<= 26 n 35) (integer->char (+ (- n 26) (char->integer #\0))))
          (else (assertion-violation 'punycode-encode
                                     "Invalid punycode digit value" n))))

  (define (punycode-adapt delta num-points first?)
    (let* ((delta (if first?
                      (div delta +punycode-damp+)
                      (div delta 2)))
           (delta (+ delta (div delta num-points))))
      (let loop ((delta delta) (k 0))
        (if (> delta (div (* (- +punycode-base+ +punycode-t-min+)
                             +punycode-t-max+)
                          2))
            (loop (div delta (- +punycode-base+ +punycode-t-min+))
                  (+ k +punycode-base+))
            (+ k (div (* (+ (- +punycode-base+ +punycode-t-min+) 1) delta)
                      (+ delta +punycode-skew+)))))))

  (define (insert-at lst i v)
    (if (zero? i)
        (cons v lst)
        (cons (car lst) (insert-at (cdr lst) (- i 1) v))))

  (define (decode-punycode/internal payload)
    (define n +punycode-initial-n+)
    (define i 0)
    (define bias +punycode-initial-bias+)
    (define delim (string-index-right payload #\-))
    (define basic (if delim (substring payload 0 delim) ""))
    (if (not (string-every ascii-char? basic))
        #f
        (let ((output (string->list basic))
              (index (if delim (+ delim 1) 0)))
          (let loop ((index index) (n n) (i i) (bias bias)
                     (output output) (first? #t))
            (if (>= index (string-length payload))
                (list->string output)
                (let ((old-i i))
                  (let digit-loop ((index index) (i i) (w 1)
                                   (k +punycode-base+))
                    (if (>= index (string-length payload))
                        #f
                        (let ((digit (punycode-digit (string-ref payload index))))
                          (if (not digit)
                              #f
                              (let* ((i (+ i (* digit w)))
                                     (t (cond ((<= k bias) +punycode-t-min+)
                                              ((>= k (+ bias +punycode-t-max+))
                                               +punycode-t-max+)
                                              (else (- k bias)))))
                                (if (< digit t)
                                    (let* ((out-len (+ (length output) 1))
                                           (bias (punycode-adapt (- i old-i)
                                                                 out-len
                                                                 first?))
                                           (n (+ n (div i out-len)))
                                           (i (mod i out-len))
                                           (output (insert-at output i
                                                              (integer->char n))))
                                      (loop (+ index 1) n (+ i 1) bias output #f))
                                    (digit-loop (+ index 1)
                                                i
                                                (* w (- +punycode-base+ t))
                                                (+ k +punycode-base+)))))))))))))
          )

  (define (punycode-encode (s string?))
    (let* ((input (map char->integer (string->list s)))
           (input-length (length input)))
      (let-values (((basic non-basic)
                    (partition (lambda (cp) (< cp #x80)) input)))
        (let ((basic-chars (map integer->char basic)))
          (let loop ((n +punycode-initial-n+)
                     (delta 0)
                     (bias +punycode-initial-bias+)
                     (h (length basic))
                     (b (length basic))
                     (result (if (pair? basic-chars)
                                 (append basic-chars (list #\-))
                                 basic-chars)))
            (if (= h input-length)
                (list->string result)
                (let* ((m (apply min (filter (lambda (cp) (>= cp n)) input)))
                       (delta (+ delta (* (- m n) (+ h 1))))
                       (n m))
                  (let emit-loop ((rest input) (delta delta) (bias bias)
                                  (h h) (result result))
                    (cond
                     ((null? rest)
                      (loop (+ n 1) (+ delta 1) bias h b result))
                     ((< (car rest) n)
                      (emit-loop (cdr rest) (+ delta 1) bias h result))
                     ((= (car rest) n)
                      (let encode-loop ((q delta) (k +punycode-base+) (result result))
                        (let ((t (cond ((<= k bias) +punycode-t-min+)
                                       ((>= k (+ bias +punycode-t-max+))
                                        +punycode-t-max+)
                                       (else (- k bias)))))
                          (if (< q t)
                              (let* ((result (append result (list (punycode-char q))))
                                     (bias (punycode-adapt delta (+ h 1) (= h b))))
                                (emit-loop (cdr rest) 0 bias (+ h 1) result))
                              (let* ((code (+ t (mod (- q t) (- +punycode-base+ t))))
                                     (result (append result (list (punycode-char code))))
                                     (q (div (- q t) (- +punycode-base+ t))))
                                (encode-loop q (+ k +punycode-base+) result))))))
                     (else
                      (emit-loop (cdr rest) delta bias h result)))))))))))

  (define (punycode-decode (payload string?))
    (let ((decoded (decode-punycode/internal payload)))
      (if decoded
          decoded
          (assertion-violation 'punycode-decode
                               "Invalid punycode payload" payload))))

  (define +idna-disallowed-exceptions+
    '(#x0640 #x07FA #x302E #x302F))

  (define +virama-codepoints+
    '(#x094D #x09CD #x0A4D #x0ACD #x0B4D #x0BCD #x0C4D #x0CCD #x0D4D #x0DCA
      #x0E3A #x0F84 #x1039 #x103A #x1714 #x1734 #x17D2 #x1A60 #x1B44 #x1BAA
      #x1BAB #x1BF2 #x1BF3 #x2D7F #xA806 #xA8C4 #xA953 #xA9C0 #xAAF6 #xABED
      #x10A3F #x11046 #x110B9 #x11133 #x111C0 #x11235 #x112EA #x1134D #x11442
      #x114C2 #x115BF #x1163F #x116B6 #x1172B #x11839 #x1193D #x119E0 #x11A34
      #x11A47 #x11A99 #x11C3F #x11D44 #x11D45 #x11D97))

  (define (greek-char? c)
    (let ((cp (char->integer c)))
      (or (<= #x0370 cp #x03FF)
          (<= #x1F00 cp #x1FFF))))
  (define (hebrew-char? c)
    (<= #x0590 (char->integer c) #x05FF))
  (define (hira-kata-han-char? c)
    (let ((cp (char->integer c)))
      (or (<= #x3040 cp #x309F)
          (<= #x30A0 cp #x30FF)
          (<= #x4E00 cp #x9FFF))))

  (define (label-context-valid? label)
    (define chars (string->list label))
    (define n (length chars))
    (define (char-at i) (list-ref chars i))
    (and (or (= n 0)
             (let ((gc (char-general-category (car chars))))
               (not (memq gc '(Mn Mc Me)))))
         (let loop ((i 0))
           (if (= i n)
               #t
               (let* ((c (char-at i))
                      (cp (char->integer c)))
                 (cond
                  ((memv cp +idna-disallowed-exceptions+) #f)
                  ((= cp #x00B7)
                   (and (> i 0) (< (+ i 1) n)
                        (char=? (char-at (- i 1)) #\l)
                        (char=? (char-at (+ i 1)) #\l)
                        (loop (+ i 1))))
                  ((= cp #x0375)
                   (and (< (+ i 1) n)
                        (greek-char? (char-at (+ i 1)))
                        (loop (+ i 1))))
                  ((or (= cp #x05F3) (= cp #x05F4))
                   (and (> i 0)
                        (hebrew-char? (char-at (- i 1)))
                        (loop (+ i 1))))
                  ((= cp #x30FB)
                   (and (exists (lambda (x)
                                  (and (not (= (char->integer x) #x30FB))
                                       (hira-kata-han-char? x)))
                                chars)
                        (loop (+ i 1))))
                  ((= cp #x200D)
                   (and (> i 0)
                        (memv (char->integer (char-at (- i 1)))
                              +virama-codepoints+)
                        (loop (+ i 1))))
                  (else (loop (+ i 1)))))))
         (let ((has-arabic-indic
                (exists (lambda (c) (<= #x0660 (char->integer c) #x0669)) chars))
               (has-ext-arabic-indic
                (exists (lambda (c) (<= #x06F0 (char->integer c) #x06F9)) chars)))
           (not (and has-arabic-indic has-ext-arabic-indic)))
         (or (< n 4)
             (not (and (char=? (char-at 2) #\-)
                       (char=? (char-at 3) #\-))))))

  (define (punycode-label? (label string?))
    (and (<= (string-length label) 63)
         (string-prefix? "xn--" label)
         (> (string-length label) 4)
         (let ((payload (substring label 4 (string-length label))))
           (and (> (string-length payload) 1)
                (let ((decoded (decode-punycode/internal payload)))
                  (and decoded
                       (label-context-valid? decoded)))))))
)
