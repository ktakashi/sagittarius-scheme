;; -*- mode: scheme; coding: utf-8; -*-
(import (rnrs)
	(sagittarius regex))

(define module-re (regex "@module@"))
(define module-l-re (regex "@module-l@"))
(define cmake-tmpl "CMakeLists.txt.in")
(define header-tmpl "header.tmpl")
(define body-tmpl "body.tmpl")

(define (write-template tmpl real module)
  (let ((real (format "~a/~a" module real)))
    (if (file-exists? real)
	(delete-file real))
    (let ((in (open-file-input-port tmpl (file-options no-fail) 'block (native-transcoder))))
      (let* ((content (get-string-all in))
	     (replaced (regex-replace-all module-re content module)))
	(set! replaced (regex-replace-all module-l-re replaced (string-upcase module)))
	(let ((out (open-file-output-port real
					  (file-options no-fail)
					  'block
					  (native-transcoder))))
	  (put-string out replaced)
	  (close-output-port out)))
      (close-input-port in))))

(let ((args (command-line))
      (module #f))
  (if (= (length args) 1)
      (begin
	(display "module name > ")(flush-output-port (current-output-port))
	(let ((name (read (current-input-port))))
	  (set! module (if (symbol? name) (symbol->string name) name))))
      (set! module (cadr args)))
  (if (not (file-exists? module))
      (create-directory module))
  (write-template cmake-tmpl "CMakeLists.txt" module)
  (write-template header-tmpl (format "~a.h" module) module)
  (write-template body-tmpl (format "~a.c" module) module))
