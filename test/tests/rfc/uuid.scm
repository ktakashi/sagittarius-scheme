(import (rnrs)
	(sagittarius)
	(rfc uuid)
	(srfi :64 testing))

(test-begin "RFC UUID tests")

;; uuid?
(test-assert "uuid?" (uuid? (make-null-uuid)))
(test-assert "uuid?" (uuid? (make-v1-uuid)))
(test-assert "uuid?" (uuid? (make-v3-uuid +namespace-dns+ "name")))
(test-assert "uuid?" (uuid? (make-v4-uuid)))
(test-assert "uuid?" (uuid? (make-v5-uuid +namespace-dns+ "name")))

;; uuid=?
;; v1 and v4 does not return the same uuid. well more like should not
(test-assert "uuid=?" (uuid=? (make-null-uuid) (make-null-uuid)))
(test-assert "uuid=?" (uuid=? (make-v3-uuid +namespace-dns+ "name")
			      (make-v3-uuid +namespace-dns+ "name")))
(test-assert "uuid=?" (uuid=? (make-v5-uuid +namespace-dns+ "name")
			      (make-v5-uuid +namespace-dns+ "name")))
(test-assert "uuid=?(v1)"
	     (not (uuid=? (make-v1-uuid) (make-v1-uuid))))
(test-assert "uuid=?(v4)"
	     (not (uuid=? (make-v4-uuid) (make-v4-uuid))))

;; v3 and v5 always return the same UUID if the given arguments are the same
(let* ((v3-uuid-string  "C6437EF1-5B86-3A4E-A071-C2D4AD414E65")
       (v3-uuid (string->uuid v3-uuid-string))
       (v3-uuid2 (make-v3-uuid +namespace-dns+ "name")))
  (test-assert "uuid? (string->uuid)" (uuid? v3-uuid))
  (test-assert "uuid=?" (uuid=? v3-uuid2 v3-uuid))
  (test-assert "compare (v3)" (string=? v3-uuid-string (uuid->string v3-uuid2)))
  )

(let* ((v5-uuid-string "9B8EDCA0-90F2-5031-8E5D-3F708834696C")
       (v5-uuid (string->uuid v5-uuid-string))
       (v5-uuid2 (make-v5-uuid +namespace-dns+ "name")))
  (test-assert "uuid? (string->uuid)" (uuid? v5-uuid))
  (test-assert "uuid=?" (uuid=? v5-uuid2 v5-uuid))
  (test-assert "compare (v5)" (string=? v5-uuid-string (uuid->string v5-uuid2)))
  )

;; bytevector conversion
(let* ((v3-uuid-bv
	#vu8(198 67 126 241 91 134 58 78 160 113 194 212 173 65 78 101))
       (v3-uuid (bytevector->uuid v3-uuid-bv))
       (v3-uuid2 (make-v3-uuid +namespace-dns+ "name")))
  (test-assert "uuid? (bytevector->uuid)" (uuid? v3-uuid))
  (test-assert "uuid=?" (uuid=? v3-uuid2 v3-uuid))
  (test-assert "compare (v3)"
	       (bytevector=? v3-uuid-bv (uuid->bytevector v3-uuid2))))

(let* ((v5-uuid-bv
	#vu8(155 142 220 160 144 242 80 49 142 93 63 112 136 52 105 108))
       (v5-uuid (bytevector->uuid v5-uuid-bv))
       (v5-uuid2 (make-v5-uuid +namespace-dns+ "name")))
  (test-assert "uuid? (bytevector->uuid)" (uuid? v5-uuid))
  (test-assert "uuid=?" (uuid=? v5-uuid2 v5-uuid))
  (test-assert "compare (v5)"
	       (bytevector=? v5-uuid-bv (uuid->bytevector v5-uuid2))))

(test-end)