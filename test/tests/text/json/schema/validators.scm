(import (rnrs)
	(text json validator)
	(text json schema validators)
	(srfi :133)
	(srfi :64))

(test-begin "JSON Schema validators")

(test-error (json-schema->json-validator ""))
(test-error (json-schema->json-validator #(("if" . ""))))


(test-end)
