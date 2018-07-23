(import (rnrs)
	(text yaml parser)
	(sagittarius generators)
	(peg)
	(srfi :127)
	(srfi :39)
	(srfi :64))

(test-begin "YAML")

(define (string->lseq s) (generator->lseq (string->generator s)))

(define (yaml-test expected parser text)
  (define lseq (string->lseq text))
  (parameterize ((*parsing-context* (make-parsing-context lseq)))
    (let-values (((s v nl) (parser lseq)))
      (test-assert (parse-success? s))
      (test-equal expected v))))

(yaml-test '(comment " This stream contains no")
	   l-comment "# This stream contains no\n# documents, only comments")
(yaml-test '(comment " This stream contains no")
	   l-comment "# This stream contains no")

(yaml-test '(comment " This stream contains no\n documents, only comments")
	   s-l-comments "# This stream contains no\n# documents, only comments")

(yaml-test '(YAML (1 2)) l-directive "%YAML 1.2")
(yaml-test '(TAG primay "tag:yaml.org,2002:")
	   l-directive "%TAG ! tag:yaml.org,2002:")
(yaml-test '(TAG secondary "tag:yaml.org,2002:")
	   l-directive "%TAG !! tag:yaml.org,2002:")
(yaml-test '(TAG "yaml" "tag:yaml.org,2002:")
	   l-directive "%TAG !yaml! tag:yaml.org,2002:")

(test-end)
