;; -*- scheme -*-
(import (tests rfc base64)
	(tests srfi :13)
	(tests srfi :14))
(run-rfc-base64-tests)
(run-srfi-13-tests)
(run-srfi-14-tests)