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
       (define pred (record-predicate rtd))
       (define acc (record-accessor rtd (find-index 'n fields)))
       ...
       ;; (begin (write (list 'n v (acc o))) (newline)) ...
       (test-assert '(msg predicate) (pred o))
       (test-equal '(msg n v) v (acc o)) ...))
    ((_ "collect" msg rt o ((n v acc) ...) ((slot val) rest ...))
     (test-record "collect" msg rt o ((n v acc) ... (slot val tmp)) (rest ...)))
    ((_ record-type ((slot val) ...) value)
     (test-record record-type record-type ((slot val) ...) value))
    ((_ message record-type ((slot val) ...) value)
     (test-record "collect" message record-type value () ((slot val) ...)))))

(test-begin "YAML scanner")

(define (string->scanner s)
  (port->yaml-scanner-lseq (open-string-input-port s)))
(define-syntax test-scanner
  (syntax-rules ()
    ((_ "emit" scanner) (begin))
    ((_ "emit" scanner (rt (slot val) ...) e* ...)
     (begin
       (test-record rt ((slot val) ...) (lseq-car scanner))
       (test-scanner "emit" (lseq-cdr scanner) e* ...)))
    ((_ input (rt (s v) ...) rest ...)
     (let ((scanner (string->scanner input)))
       (test-scanner "emit" scanner (rt (s v) ...) rest ...)))))

(test-scanner "plain" (<scalar-token> (value "plain") (plain? #t)))
(test-scanner "plain #comment\nbar"
	      (<scalar-token> (value "plain") (plain? #t))
	      (<scalar-token> (value "bar") (plain? #t)))

(test-scanner "%YAML 1.0"
	      (<directive-token> (value '(1 . 0)) (name "YAML")))
(test-error yaml-scanner-error? (string->scanner "% invalid"))
(test-error yaml-scanner-error? (string->scanner "%YAML 1.x"))
(test-error yaml-scanner-error? (string->scanner "%YAML 1-1"))
(test-error yaml-scanner-error? (string->scanner "%YAML 1.1x"))
;; after a directive, there must be a line break (or EOF)
(test-error yaml-scanner-error? (string->scanner "%YAML 1.1 boo"))

(test-scanner "%TAG !yaml! tag:yaml.org,2002:"
	      (<directive-token> (value '("!yaml!" . "tag:yaml.org,2002:"))
				 (name "TAG")))
(test-scanner "%TAG ! tag:yaml.org,2002:/"
	      (<directive-token> (value '("!" . "tag:yaml.org,2002:/"))
				 (name "TAG")))
(test-scanner "%TAG !! tag:yaml.org,2002:/"
	      (<directive-token> (value '("!!" . "tag:yaml.org,2002:/"))
				 (name "TAG")))
(test-scanner "%TAG !! tag:%20:with:%20:space"
	      (<directive-token> (value '("!!" . "tag: :with: :space"))
				 (name "TAG")))
(test-error yaml-scanner-error? (string->scanner "%TAG !"))
(test-error yaml-scanner-error? (string->scanner "%TAG ! !>"))
(test-error yaml-scanner-error? (string->scanner "%TAG ! ! !"))
(test-error yaml-scanner-error? (string->scanner "%TAG foo bar"))
(test-error yaml-scanner-error? (string->scanner "%TAG !! tag:%2"))

(test-scanner "---" (<document-start-token>))
(test-scanner "--- " (<document-start-token>))
(test-scanner "---\n" (<document-start-token>))

(test-scanner "-"  (<block-sequence-start-token>) (<block-entry-token>))
(test-scanner "- --"(<block-sequence-start-token>)  (<block-entry-token>))
(test-scanner "-\n--" (<block-sequence-start-token>) (<block-entry-token>))

(test-scanner "..." (<document-end-token>))
(test-scanner ". .." (<scalar-token>) (<scalar-token>))

(test-scanner "[" (<flow-sequence-start-token>))
(test-scanner "]" (<flow-sequence-end-token>))
(test-scanner "{" (<flow-mapping-start-token>))
(test-scanner "}" (<flow-mapping-end-token>))
(test-scanner "," (<flow-entry-token>))

(test-scanner "?" (<block-mapping-start-token>) (<key-token>))
(test-scanner "[ ? ]"
	      (<flow-sequence-start-token>)
	      (<key-token>)
	      (<flow-sequence-end-token>))

(test-scanner ":" (<block-mapping-start-token>) (<value-token>))

(test-scanner "[ :"
	      (<flow-sequence-start-token>)
	      (<value-token>))

(test-scanner "&value" (<anchor-token> (value "value")))
(test-error yaml-scanner-error? (string->scanner "& value"))
(test-error yaml-scanner-error? (string->scanner "&value$"))

(test-scanner "*value" (<alias-token> (value "value")))
(test-error yaml-scanner-error? (string->scanner "* value"))
(test-error yaml-scanner-error? (string->scanner "*value$"))

(test-scanner "!<tag:yaml.org,2002:str>"
	      (<tag-token> (value '(#f . "tag:yaml.org,2002:str"))))
(test-scanner "!" (<tag-token> (value '(#f . "!"))))
(test-scanner "!!str" (<tag-token> (value '("!!" . "str"))))
(test-scanner "!str" (<tag-token> (value '("!" . "str"))))
(test-scanner "!foo!str" (<tag-token> (value '("!foo!" . "str"))))
(test-error yaml-scanner-error? (string->scanner "!<tag:yaml.org,2002:str"))
(test-error yaml-scanner-error? (string->scanner "!<tag:yaml.org,2002:str>foo"))

(test-scanner "|\n block scalar\n next line"
	      (<scalar-token> (value "block scalar\nnext line")
			      (style #\|)
			      (plain? #f)))
(test-scanner "|\n \n  \n  literal\n   \n  \n  text\n\n #comment"
	      (<scalar-token> (value "\n\nliteral\n \n\ntext\n")
			      (style #\|)
			      (plain? #f)))
(test-scanner "| # Empty header\n literal"
	      (<scalar-token> (value "literal")))
(test-scanner "|+ # Chompoing indicator\n keep\n\n"
	      (<scalar-token> (value "keep\n\n")))
(test-scanner "|1- # Both indicator\n  strip\n\n"
	      (<scalar-token> (value " strip")))
(test-error yaml-scanner-error? (string->scanner "| - # wrong indicator"))
(test-error yaml-scanner-error? (string->scanner "|0- # wrong indicator"))
(test-error yaml-scanner-error? (string->scanner "|10- # wrong indicator"))

(test-scanner ">1 # Indentation indicator\n  folded\n"
	      (<scalar-token> (value " folded\n")
			      (style #\>)
			      (plain? #f)))
(test-scanner ">\n folded\n text\n\n"
	      (<scalar-token> (value "folded text\n")
			      (style #\>)
			      (plain? #f)))
(test-scanner ">\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"
	      (<scalar-token> (value "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n")
			      (style #\>)
			      (plain? #f)))

(test-end)
