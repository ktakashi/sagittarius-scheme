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
;;   - https://errata.rfc-editor.org/eid4315/ (errata for IPv6 format)
(library (rfc smtp format)
    (export smtp-valid-address?
	    ipv4-address?
	    ipv6-address?)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (srfi :1 lists)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs)
	    (rfc :5322))

;; https://tools.ietf.org/html/rfc5321#section-2.3.11
;; Check against Mailbox rule
(define (smtp-valid-address? s) (parse-ok? s $mailbox))
(define (ipv4-address? s)(parse-ok? s $ipv4-address-literal))
(define (ipv6-address? s) (parse-ok? s ($or $ipv6-address-literal $ipv6-addr)))

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

(define qtext-set
  (char-set-union
   (char-set-difference ascii/printing (char-set #\\ #\"))
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
  ($seq ($eqv? #\\) ($char-set-contains? ascii/printing)))
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
(define $:: ($token "::"))
(define $: ($eqv? #\:))
(define $? $optional)
(define ($* p n) ($many p 0 n))
;; IPv6-comp      =                          "::" [IPv6-hex *6(":" IPv6-hex)]
;;                | IPv6-hex                 "::" [IPv6-hex *5(":" IPv6-hex)]
;;                | IPv6-hex 1(":" IPv6-hex) "::" [IPv6-hex *4(":" IPv6-hex)]
;;                | IPv6-hex 2(":" IPv6-hex) "::" [IPv6-hex *3(":" IPv6-hex)]
;;                | IPv6-hex 3(":" IPv6-hex) "::" [IPv6-hex *2(":" IPv6-hex)]
;;                | IPv6-hex 4(":" IPv6-hex) "::" [IPv6-hex *1(":" IPv6-hex)]
;;                | IPv6-hex 5(":" IPv6-hex) "::" [IPv6-hex]
;;                | IPv6-hex 6(":" IPv6-hex) "::"
;;                ; The "::" represents at least one 16-bit groups of
;;                ; zeros.  No more than 7 groups in addition to the
;;                ; "::" may be present.
(define ($ipv6-trail n) ($seq $ipv6-hex ($* $ipv6-seg n)))
(define $ipv6-comp
  ($or ($seq                                 $:: ($? ($ipv6-trail 6)))
       ($seq $ipv6-hex                       $:: ($? ($ipv6-trail 5)))
       ($seq $ipv6-hex ($repeat $ipv6-seg 1) $:: ($? ($ipv6-trail 4)))
       ($seq $ipv6-hex ($repeat $ipv6-seg 2) $:: ($? ($ipv6-trail 3)))
       ($seq $ipv6-hex ($repeat $ipv6-seg 3) $:: ($? ($ipv6-trail 2)))
       ($seq $ipv6-hex ($repeat $ipv6-seg 4) $:: ($? ($ipv6-trail 1)))
       ($seq $ipv6-hex ($repeat $ipv6-seg 5) $:: ($? $ipv6-hex))
       ($seq $ipv6-hex ($repeat $ipv6-seg 6) $::)))
;; IPv6v4-full    = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
(define $ipv6v4-full
  ($seq $ipv6-hex ($repeat $ipv6-seg 5) ($eqv? #\:) $ipv4-address-literal))

;; IPv6-full      = IPv6-hex 7(":" IPv6-hex)
;;
;;
;;
;; IPv6v4-comp    = (                        "::" [IPv6-hex *4(":" IPv6-hex) ":"]
;;                | IPv6-hex                 "::" [IPv6-hex *3(":" IPv6-hex) ":"]
;;                | IPv6-hex 1(":" IPv6-hex) "::" [IPv6-hex *2(":" IPv6-hex) ":"]
;;                | IPv6-hex 2(":" IPv6-hex) "::" [IPv6-hex *1(":" IPv6-hex) ":"]
;;                | IPv6-hex 3(":" IPv6-hex) "::" [IPv6-hex ":"]
;;                | IPv6-hex 4(":" IPv6-hex) "::")
;;                IPv4-address-literal
;;                ; The "::" represents at least one 16-bit groups of
;;                ; zeros.  No more than 5 groups in addition to the
;;                ; "::" and IPv4-address-literal may be present.
;; manually composing exact match, using $many ends up wrong matching
(define ($ipv6-seg* n)
  (define (make-seg n)
    (apply $seq `(,$ipv6-hex ,@(map (lambda (#:_) $ipv6-seg) (iota n)) ,$:)))
  (apply $or (map (lambda (n) (make-seg n)) (iota (+ n 1) n -1))))
(define ipv6v4-comp
  ($seq ($or ($seq                                 $:: ($? ($ipv6-seg* 4)))
	     ($seq $ipv6-hex                       $:: ($? ($ipv6-seg* 3)))
	     ($seq $ipv6-hex ($repeat $ipv6-seg 1) $:: ($? ($ipv6-seg* 2)))
	     ($seq $ipv6-hex ($repeat $ipv6-seg 2) $:: ($? ($ipv6-seg* 1)))
	     ($seq $ipv6-hex ($repeat $ipv6-seg 3) $:: ($? ($ipv6-seg* 0)))
	     ($seq $ipv6-hex ($repeat $ipv6-seg 4) $::))
	$ipv4-address-literal))
(define $ipv6-full ($seq $ipv6-hex ($repeat $ipv6-seg 7)))
(define $ipv6-addr ($or $ipv6-full $ipv6v4-full ipv6v4-comp $ipv6-comp))
(define $ipv6-address-literal
  ($seq ($or ($token "IPv6:") ($token "ipv6:")) $ipv6-addr))

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
