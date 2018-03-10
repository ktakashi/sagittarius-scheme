(import (rnrs)
	(text xml dom parser)
	(sagittarius generators)
	(peg)
	(srfi :127 lseqs)
	(srfi :64))

(test-begin "DOM parser")

(define (string->lseq s) (generator->lseq (string->generator s)))
(define-syntax test-parser
  (syntax-rules ()
    ((_ expected parser)
     (let-values (((r v n) parser))
       (test-assert '("parser result" parser) (parse-success? r))
       (test-equal '("parser next" parser) '() n)
       (test-equal '("parser value" parser) expected v)))))
(test-parser "name" ($xml:name (string->lseq "name")))
(test-parser '("name1" "name2") ($xml:names (string->lseq "name1 name2")))
(let ((s "-.1\xB7;\x0300;\x036F;\x203F;\x2040;"))
  (test-parser s ($xml:nmtoken (string->lseq s)))
  (test-parser `(,s ,s) ($xml:nmtokens (string->lseq (string-append s " " s)))))

(test-end)
