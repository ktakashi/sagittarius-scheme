(import (rnrs)
	(srfi :64)
	(srfi :1)
	(archive)
	;; for find-files
	(util file)
	(pp))

;; only high level APIs for now

(test-begin "archive")

(define data-dir (build-path* (current-directory) "test" "tests"))

(define (test-archive type dst-file)
  (let ((files (find-files data-dir)))
    (call-with-port (open-file-output-port dst-file (file-options no-fail))
      (lambda (out)
	(call-with-archive-output type out
	  (lambda (archive-out)
	    (test-assert "archive-input?" (archive-output? archive-out))
	    (for-each (lambda (f)
			(let ((e (create-entry archive-out f)))
			  (test-assert "archive-entry?" (archive-entry? e))
			  (test-equal (format "entry type: ~a" type)
				      (if (file-directory? f)
					  'directory
					  'file)
				      (archive-entry-type e))
			  (test-equal "entry name" f
				      (archive-entry-name e))
			  (append-entry! archive-out e)))
		      files)))))
    (call-with-input-file dst-file
      (lambda (in)
	(call-with-archive-input type in
	  (lambda (archive-in)
	    (do-entry (e archive-in)
	      (let ((name (archive-entry-name e))
		    (type (archive-entry-type e)))
		(test-assert "filename" (string? name))
		(test-assert "type" (memv type '(file directory)))
		(test-assert "archive-entry?" (archive-entry? e)))))))
      :transcoder #f)
    (delete-file dst-file)))

(test-archive 'zip "tests.zip")
(test-archive 'tar "tests.tar")

(test-end)

