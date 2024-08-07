(import (rnrs)
	(sagittarius)
	(rfc uuid)
	(srfi :39 parameters)
	(srfi :64 testing))

(test-begin "RFC UUID tests")

;; uuid?
(test-assert "uuid? (null)" (uuid? (make-null-uuid)))
(test-assert "uuid? (v1)" (uuid? (make-v1-uuid)))
(test-assert "uuid? (v3)" (uuid? (make-v3-uuid +namespace-dns+ "name")))
(test-assert "uuid? (v4)" (uuid? (make-v4-uuid)))
(test-assert "uuid? (v5)" (uuid? (make-v5-uuid +namespace-dns+ "name")))

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

(test-assert (null-uuid? (make-null-uuid)))
(test-assert (v1-uuid? (make-v1-uuid)))
(test-assert (v4-uuid? (make-v4-uuid)))

(test-assert (v6-uuid? (make-v6-uuid)))
(test-assert (v7-uuid? (make-v7-uuid)))

(test-error (string->uuid "2eb8aa08-aa98-11ea-b4ga-73b441d16380"))

(let ((uuid (make-null-uuid)))
  (test-equal 0 (uuid-time-low uuid))
  (test-equal 0 (uuid-time-mid uuid))
  (test-equal 0 (uuid-time-high uuid))
  (test-equal 0 (uuid-clock-seq-var uuid))
  (test-equal 0 (uuid-clock-seq-low uuid))
  (test-equal 0 (uuid-node uuid)))

;; v3 and v5 always return the same UUID if the given arguments are the same
(let* ((v3-uuid-string  "C6437EF1-5B86-3A4E-A071-C2D4AD414E65")
       (v3-uuid (string->uuid v3-uuid-string))
       (v3-uuid2 (make-v3-uuid +namespace-dns+ "name")))
  (test-assert "uuid? (string->uuid)" (uuid? v3-uuid))
  (test-assert "uuid=?" (uuid=? v3-uuid2 v3-uuid))
  (test-assert "compare (v3)" (string=? v3-uuid-string (uuid->string v3-uuid2)))
  (test-assert (v3-uuid? v3-uuid)))

(let* ((v5-uuid-string "9B8EDCA0-90F2-5031-8E5D-3F708834696C")
       (v5-uuid (string->uuid v5-uuid-string))
       (v5-uuid2 (make-v5-uuid +namespace-dns+ "name")))
  (test-assert "uuid? (string->uuid)" (uuid? v5-uuid))
  (test-assert "uuid=?" (uuid=? v5-uuid2 v5-uuid))
  (test-assert "compare (v5)" (string=? v5-uuid-string (uuid->string v5-uuid2)))
  (test-assert (v5-uuid? v5-uuid)))

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

(let ((m1 (make-max-uuid))
      (m2 (string->uuid "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF")))
  (test-assert "max uuid (1)" (max-uuid? m1))
  (test-assert "max uuid (2)" (max-uuid? m2))
  (test-assert (uuid=? m1 m2)))

;; v1 and v6 test vector
(parameterize ((*uuid-node* #vu8(#x9F #x6B #xDE #xCE #xD8 #x46))
	       (*uuid-clock-seq* #x33C8))
  (let ((timestamp #x1EC9414C232AB00))
    (test-assert (uuid=? (string->uuid "C232AB00-9414-11EC-B3C8-9F6BDECED846")
			 (make-v1-uuid timestamp)))
    (test-assert (uuid=? (string->uuid "1EC9414C-232A-6B00-B3C8-9F6BDECED846")
			 (make-v6-uuid timestamp)))))

(let ((uuid (make-v7-uuid #x017F22E279B0)))
  (test-equal #x017F22E279B0 (v7-uuid-unix-ts-ms uuid))
  (test-equal 7 (uuid-version uuid)))


(test-assert -1 (uuid-compare (make-v4-uuid) (make-v4-uuid)))

(test-end)
