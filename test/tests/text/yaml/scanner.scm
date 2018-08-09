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
       ;; (begin (display 'rt) (write o) (newline))
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
       (test-scanner "emit" scanner
		     (<stream-start-token>) (rt (s v) ...) rest ...)))))
(define-syntax test-scanner-error
  (syntax-rules ()
    ((_ input)
     (let ((scanner (string->scanner input)))
       ;; skip stream-start
       (test-error input yaml-scanner-error? (lseq-realize  scanner))))))

(test-scanner "plain" (<scalar-token> (value "plain") (plain? #t)))
(test-scanner "plain #comment\nbar"
	      (<scalar-token> (value "plain") (plain? #t))
	      (<scalar-token> (value "bar") (plain? #t)))

(test-scanner "%YAML 1.0"
	      (<directive-token> (value '(1 . 0)) (name "YAML")))
(test-scanner-error "% invalid")
(test-scanner-error "%YAML 1.x")
(test-scanner-error "%YAML 1-1")
(test-scanner-error "%YAML 1.1x")
;; after a directive, there must be a line break (or EOF)
(test-scanner-error "%YAML 1.1 boo")

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
(test-scanner-error "%TAG !")
(test-scanner-error "%TAG ! !>")
(test-scanner-error "%TAG ! ! !")
(test-scanner-error "%TAG foo bar")
(test-scanner-error "%TAG !! tag:%2")

(test-scanner "---" (<document-start-token>))
(test-scanner "--- " (<document-start-token>))
(test-scanner "---\n" (<document-start-token>))

(test-scanner "-"  (<block-sequence-start-token>) (<block-entry-token>))
(test-scanner "- --"(<block-sequence-start-token>)  (<block-entry-token>))
(test-scanner "-\n--" (<block-sequence-start-token>) (<block-entry-token>))

(test-scanner "..." (<document-end-token>))
(test-scanner ". .." (<scalar-token>))

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
(test-scanner-error "& value")
(test-scanner-error "&value$")

(test-scanner "*value" (<alias-token> (value "value")))
(test-scanner-error "* value")
(test-scanner-error "*value$")

(test-scanner "!<tag:yaml.org,2002:str>"
	      (<tag-token> (value '(#f . "tag:yaml.org,2002:str"))))
(test-scanner "!" (<tag-token> (value '(#f . "!"))))
(test-scanner "!!str" (<tag-token> (value '("!!" . "str"))))
(test-scanner "!str" (<tag-token> (value '("!" . "str"))))
(test-scanner "!foo!str" (<tag-token> (value '("!foo!" . "str"))))
(test-scanner-error "!<tag:yaml.org,2002:str")
(test-scanner-error "!<tag:yaml.org,2002:str>foo")

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
(test-scanner-error "| - # wrong indicator")
(test-scanner-error "|0- # wrong indicator")
(test-scanner-error "|10- # wrong indicator")

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

(test-scanner "\"implicit block key\""
	      (<scalar-token> (value "implicit block key")
			      (style #\")
			      (plain? #f)))
(test-scanner "\"folded \nto a space,\t\n \n to a line feed, or \t\\\n \\ \tnon-content\""
	      (<scalar-token> (value "folded to a space,\nto a line feed, or \t \tnon-content")
			      (style #\")))
(test-scanner "\" 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty \""
	      (<scalar-token> (value " 1st non-empty\n2nd non-empty 3rd non-empty ")))

(test-scanner "\"\\0\\a\\b\\t\\	\\n\\v\\f\\r\\e\\ \\\"\\\\\\N\\_\\L\\P\""
	      (<scalar-token> (value "\x0;\x07;\x08;\x09;\x09;\x0a;\x0b;\x0c;\x0d;\x1b; \"\\\x85;\xa0;\x2028;\x2029;")))

(test-scanner "\"\\x20\\u0020\\U00000020\"" (<scalar-token> (value "   ")))

(test-scanner-error "\"foo")
(test-scanner-error "\"\\x0\"")
(test-scanner-error "\"\\u0\"")
(test-scanner-error "\"\\U0\"")
(test-scanner-error "\"\\xVV\"")
(test-scanner-error "\"\\T\"")

(test-scanner "'here''s to \"quotes\"'"
	      (<scalar-token> (value "here's to \"quotes\"")
			      (style #\')
			      (plain? #f)))
(test-scanner "'implicit block key'"
	      (<scalar-token> (value "implicit block key")
			      (style #\')
			      (plain? #f)))
(test-scanner "' 1st non-empty\n\n 2nd non-empty \n\t3rd non-empty '"
	      (<scalar-token> (value " 1st non-empty\n2nd non-empty 3rd non-empty ")
			      (style #\')))

(test-scanner "scalar\n..."
	      (<scalar-token> (value "scalar"))
	      (<document-end-token>))

(test-scanner "foo:\n- bar"
	      (<block-mapping-start-token>)
	      (<key-token>)
	      (<scalar-token> (value "foo"))
	      (<value-token>)
	      (<block-entry-token>)
	      (<scalar-token> (value "bar"))
	      (<block-end-token>))

(test-scanner "- { foo:bar }"
	      (<block-sequence-start-token>)
	      (<block-entry-token>)
	      (<flow-mapping-start-token>)
	      (<scalar-token>)
	      (<flow-mapping-end-token>)
	      (<block-end-token>))

(test-scanner "foo:\n  - bar\n  - 1234\n  - foo"
	      (<block-mapping-start-token>)
	      (<key-token>)
	      (<scalar-token>)
	      (<value-token>)
	      (<block-sequence-start-token>)
	      (<block-entry-token>)
	      (<scalar-token>)
	      (<block-entry-token>)
	      (<scalar-token>)
	      (<block-entry-token>)
	      (<scalar-token>)
	      (<block-end-token>)
	      (<block-end-token>))

(test-scanner "
- &alias1 1
- &alias2 2
- << : [ *alias1, *alias2 ]
  r: 10"
	      (<block-sequence-start-token>)
	      (<block-entry-token>)
	      (<anchor-token> (value "alias1"))
	      (<scalar-token> (value "1"))
	      (<block-entry-token>)
	      (<anchor-token> (value "alias2"))
	      (<scalar-token> (value "2"))
	      (<block-entry-token>)
	      (<block-mapping-start-token>)
	      (<key-token>)
	      (<scalar-token> (value "<<"))
	      (<value-token>)
	      (<flow-sequence-start-token>)
	      (<alias-token> (value "alias1"))
	      (<flow-entry-token>)
	      (<alias-token> (value "alias2"))
	      (<flow-sequence-end-token>)
	      (<key-token>)
	      (<scalar-token> (value "r"))
	      (<value-token>)
	      (<scalar-token> (value "10"))
	      (<block-end-token>))
	      
	      
(test-end)
