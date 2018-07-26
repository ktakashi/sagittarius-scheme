(import (rnrs)
	(text yaml scanner)
	(text yaml tokens)
	(srfi :127)
	(srfi :64))

(define (find-index name vec)
  (do ((i 0 (+ i 1)))
      ((eq? name (vector-ref vec i)) i)))
(define-syntax test-record
  (syntax-rules ()
    ((_ "collect" msg rt o ((n v acc) ...) ())
     (let* ((rtd (record-type-descriptor rt))
	    (fields (record-type-field-names rtd)))
       (define acc (record-accessor rtd (find-index 'n fields)))
       ...
       (test-equal '(msg n v) v (acc o)) ...))
    ((_ "collect" msg rt o ((n v acc) ...) ((slot val) rest ...))
     (test-record "collect" msg rt o ((n v acc) ... (slot val tmp)) (rest ...)))
    ((_ record-type ((slot val) ...) value)
     (test-record record-type record-type ((slot val) ...) value))
    ((_ message record-type ((slot val) ...) value)
     (test-record "collect" message record-type value () ((slot val) ...)))))

(test-begin "YAML scanner")

(define-syntax test-scanner
  (syntax-rules ()
    ((_ "emit" scanner) (begin))
    ((_ "emit" scanner (rt (slot val) ...) e* ...)
     (begin
       (test-record rt ((slot val) ...) (lseq-car scanner))
       (test-scanner "emit" (lseq-cdr scanner) e* ...)))
    ((_ input (rt (s v) ...) rest ...)
     (let ((scanner (port->yaml-scanner-lseq (open-string-input-port input))))
       (test-scanner "emit" scanner (rt (s v) ...) rest ...)))))

(test-scanner "plain" (<scalar-token> (value "plain") (plain? #t)))
(test-scanner "plain #comment\nbar"
	      (<scalar-token> (value "plain") (plain? #t))
	      (<scalar-token> (value "bar") (plain? #t)))

(test-end)
