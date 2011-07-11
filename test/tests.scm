;; -*- scheme -*-
(import (tests srfi :13)
	(tests srfi :14)
	(tests rfc base64)
	(tests rfc quoted-printable)
	(tests rfc mime))

(run-srfi-13-tests)
(run-srfi-14-tests)
(run-rfc-base64-tests)
(run-rfc-quoted-printable-tests)
(run-rfc-mime-test)