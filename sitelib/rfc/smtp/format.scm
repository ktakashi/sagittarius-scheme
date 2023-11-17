;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/smtp/format.scm - SMTP format check
;;;  
;;;   Copyright (c) 2010-2017  Takashi Kato  <ktakashi@ymail.com>
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

;; references
;;   - https://tools.ietf.org/html/rfc5321
;;   - https://tools.ietf.org/html/rfc5322
(library (rfc smtp format)
    (export smtp-valid-address?
	    ipv4-address?
	    ipv6-address?)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs)
	    (rfc :5322))

;; https://tools.ietf.org/html/rfc5321#section-2.3.11
;; Check against Mailbox rule
(define (smtp-valid-address? s) (parse-ok? s $mailbox))
(define (ipv4-address? s)(parse-ok? s $ipv4-address-literal))
(define (ipv6-address? s) (parse-ok? s $ipv6-addr))

(define (parse-ok? s parser)
  (define in (if (input-port? s)
		 (generator->lseq (port->char-generator s))
		 (string->list s)))
  (let-values (((s v nl) (parser in)))
    (and (parse-success? s)
	 (null? nl))))
;;; parsers
;;; https://www.rfc-editor.org/rfc/rfc5321#section-4.1.2
;;; https://www.rfc-editor.org/rfc/rfc6531#section-3.3 (allowing UTF-8)
(define ascii/printing (char-set-intersection char-set:ascii char-set:printing))
(define ascii/graphic (char-set-intersection char-set:ascii char-set:graphic))
(define unicode-non-ascii (char-set-difference char-set:full char-set:ascii))

(define qtext-set (char-set-difference ascii/printing (char-set #\\ #\")
				       unicode-non-ascii))
(define alpha-set (char-set-intersection char-set:ascii char-set:letter))
(define digit-set (char-set-intersection char-set:ascii char-set:digit))
(define hex-digit-set (char-set-intersection char-set:ascii char-set:hex-digit))
(define dcontent-set (char-set-difference ascii/graphic (char-set #\[ #\\ #\])))
(define atext-set (char-set-union
		   alpha-set
		   digit-set
		   (string->char-set "!#$%&'*+-/=?^_`{|}~")
		   unicode-non-ascii))

(define $alpha ($char-set-contains? alpha-set))
(define $digit ($char-set-contains? digit-set))
(define $hexdig ($char-set-contains? hex-digit-set))

(define $qtext-smtp ($char-set-contains? qtext-set))
(define $quoted-pair-smtp
  ($seq ($eqv? #\/) ($char-set-contains? ascii/printing)))
(define $q-content-smtp ($or $qtext-smtp $quoted-pair-smtp))
(define $quoted-string ($seq ($eqv? #\") ($many $q-content-smtp) ($eqv? #\")))

(define $atext ($char-set-contains? atext-set))
(define $atom ($many $atext 1))
(define $dot-string ($seq $atom ($many ($seq ($eqv? #\.) $atom))))

(define $local-part ($or $dot-string $quoted-string))

(define $let-dig ($or $alpha $digit
		      ;; it's not entirely true (say this contains white space)
		      ;; but for now
		      ($char-set-contains? unicode-non-ascii)))
(define $ldh-char ($or $let-dig ($eqv? #\-)))
(define $ldh-str
  ($seq ($many ($seq $ldh-char ($not ($peek ($or ($eqv? #\.) $eof))))) $let-dig))
(define $sub-domain ($seq $let-dig ($optional $ldh-str)))
(define $domain ($seq $sub-domain ($many ($seq ($eqv? #\.) $sub-domain))))

(define $snum
  ($let ((d* ($many $digit 1 3)))
    (if (and (or (null? (cdr d*)) (not (eqv? #\0 (car d*))))
	     (<= 0 (string->number (list->string d*)) 255))
	($return d*)
	($fail "range must be 0-255"))))
(define $ipv4-address-literal ($seq $snum ($repeat ($seq ($eqv? #\.) $snum) 3)))

(define $ipv6-hex ($many $hexdig 1 4))
(define $ipv6-seg ($seq ($eqv? #\:) $ipv6-hex))
(define $ipv6-comp
  ;; TODO implement comment part
  ($seq ($optional ($seq $ipv6-hex ($many $ipv6-seg 0 5)))
	($token "::")
	($optional ($seq $ipv6-hex ($many $ipv6-seg 0 5)))))
(define $ipv6v4-full
  ($seq $ipv6-hex ($repeat $ipv6-seg 5) ($eqv? #\:) $ipv4-address-literal))
(define $ipv6v4-seg ($seq ($eqv? #\:) $snum ($eqv? #\.)))
(define ipv6v4-comp
  ($seq ($optional ($seq $ipv6-hex ($many $ipv6-seg 0 3)))
	($token "::")
	($optional ($seq $ipv6-hex
			 ;; peek `:n{1,3}.` not to consume IPv4 segment
			 ($many ($seq ($peek ($not $ipv6v4-seg)) $ipv6-seg) 0 3)
			 ($eqv? #\:)))
	$ipv4-address-literal))
(define $ipv6-full ($seq $ipv6-hex ($repeat $ipv6-seg 7)))
(define $ipv6-addr ($or $ipv6-full $ipv6v4-full ipv6v4-comp $ipv6-comp))
(define $ipv6-address-literal ($seq ($token "IPv6:") $ipv6-addr))

(define $dcontent ($char-set-contains? dcontent-set))
(define $standardized-tag $ldh-str)
(define $general-address-literal
  ($seq $standardized-tag ($eqv? #\:) ($many $dcontent 1)))

(define $address-literal ($seq ($eqv? #\[)
			       ($or $ipv4-address-literal
				    $ipv6-address-literal
				    $general-address-literal)
			       ($eqv? #\])))

(define $mailbox ($seq $local-part ($eqv? #\@) ($or $domain $address-literal)))

)
