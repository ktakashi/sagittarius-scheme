;; -*- Scheme -*-
(define ensure-library-name
  (lambda (tag)
    (case tag
      ((:null) '(core))
      ((:sagittarius) '(sagittarius))
      ((:base) '(core base))
      ;; only 'program' to run R6RS script
      ((:r6rs-script) '(r6rs-script))
      (else
       (error 'ensure-library-name "invalid library tag:" tag)))))

(define-syntax define-enum
  (er-macro-transformer
   (lambda (form rename compare)
     (define make-tag-list
       (lambda (name tags)
	 `(define-constant ,name ',tags)))
     (define make-enum
       (lambda (name vals)
	 (let ((len (length vals)))
	   (let loop ((i 0)
		      (vals vals)
		      (r '())
		      (tags '()))
	     (if (= i len)
		 (cons (make-tag-list name (reverse tags)) (reverse r))
		 (begin
		   (loop (+ i 1)
			 (cdr vals)
			 (cons `(define-constant ,(car vals) ,i) r)
			 (cons (cons (car vals) i) tags))))))))
     (let ((name (cadr form))
	   (vals (cddr form)))
       `(begin ,@(make-enum name vals))))))

;;;;
;; IForm
(define-enum .intermediate-tags.
  $UNDEF
  $DEFINE
  $LREF
  $LSET
  $GREF
  $GSET
  $CONST
  $IF
  $LET
  $LAMBDA
  $RECEIVE
  $LABEL
  $SEQ
  $CALL
  $ASM
  $IT
  $LIST
  $LIBRARY)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
