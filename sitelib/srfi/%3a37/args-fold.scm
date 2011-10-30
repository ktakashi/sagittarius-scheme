#!compatible
(library (srfi :37 args-fold)
  (export args-fold
	  ;; issue, option is already defined, so import can not
	  ;; figure out which one should take.
	  ;;(rename (make-option option))
	  option
	  option?
	  option-names
	  option-required-arg?
	  option-optional-arg?
	  option-processor)
  (import (rnrs))
  
  (define-record-type (option: option option?)
    (fields 
      (immutable names option-names)
      (immutable required-arg? option-required-arg?)
      (immutable optional-arg? option-optional-arg?)
      (immutable processor option-processor))
    (protocol 
      (lambda (c) 
        (lambda (n ra oa p)
          (if (and 
                (and (list? n)
                     (positive? (length n))
                     (for-all (lambda (x) 
                                (or (and (string? x) (positive? (string-length x))) 
                                    (char? x))) 
                              n))
                (boolean? ra)
                (boolean? oa)
                (not (and ra oa))
                (procedure? p))
            (c n ra oa p)
            (assertion-violation 'option "invalid arguments" n ra oa p))))))

  (define args-fold
    (lambda (args
	     options
	     unrecognized-option-proc
	     operand-proc
	     . seeds)
      (letrec
	  ((find
	    (lambda (l ?)
	      (cond ((null? l) #f)
		    ((? (car l)) (car l))
		    (else (find (cdr l) ?)))))
	   (find-option
	    ;; ISSUE: This is a brute force search. Could use a table.
	    (lambda (name)
	      (find
	       options
	       (lambda (option)
		 (find
		  (option-names option)
		  (lambda (test-name)
		    (equal? name test-name)))))))
	   (scan-short-options
	    (lambda (index shorts args seeds)
	      (if (= index (string-length shorts))
		  (scan-args args seeds)
		  (let* ((name (string-ref shorts index))
			 (option (or (find-option name)
				     (option (list name)
					     #f
					     #f
					     unrecognized-option-proc))))
		    (cond ((and (< (+ index 1) (string-length shorts))
				(or (option-required-arg? option)
				    (option-optional-arg? option)))
			   (let-values
			       ((seeds (apply (option-processor option)
					      option
					      name
					      (substring
					       shorts
					       (+ index 1)
					       (string-length shorts))
					      seeds)))
			     (scan-args args seeds)))
			  ((and (option-required-arg? option)
				(pair? args))
			   (let-values
			       ((seeds (apply (option-processor option)
					      option
					      name
					      (car args)
					      seeds)))
			     (scan-args (cdr args) seeds)))
			  (else
			   (let-values
			       ((seeds (apply (option-processor option)
					      option
					      name
					      #f
					      seeds)))
			     (scan-short-options
			      (+ index 1)
			      shorts
			      args
			      seeds))))))))
	   (scan-operands
	    (lambda (operands seeds)
	      (if (null? operands)
		  (apply values seeds)
		  (let-values ((seeds (apply operand-proc
					     (car operands)
					     seeds)))
		    (scan-operands (cdr operands) seeds)))))
	   (scan-args
	    (lambda (args seeds)
	      (if (null? args)
		  (apply values seeds)
		  (let ((arg (car args))
			(args (cdr args)))
		    ;; NOTE: This string matching code would be simpler
		    ;; using a regular expression matcher.
		    (cond
		     (;; (rx bos "--" eos)
		      (string=? "--" arg)
		      ;; End option scanning:
		      (scan-operands args seeds))
		     (;;(rx bos
		      ;;    "--"
		      ;;    (submatch (+ (~ "=")))
		      ;;    "="
		      ;;    (submatch (* any)))
		      (and (> (string-length arg) 4)
			   (char=? #\- (string-ref arg 0))
			   (char=? #\- (string-ref arg 1))
			   (not (char=? #\= (string-ref arg 2)))
			   (let loop ((index 3))
			     (cond ((= index (string-length arg))
				    #f)
				   ((char=? #\= (string-ref arg index))
				    index)
				   (else
				    (loop (+ 1 index))))))
		      ;; Found long option with arg:
		      => (lambda (=-index)
			   (let*-values
			       (((name)
				 (substring arg 2 =-index))
				((option-arg)
				 (substring arg
					    (+ =-index 1)
					    (string-length arg)))
				((option)
				 (or (find-option name)
				     (option (list name)
					     #t
					     #f
					     unrecognized-option-proc)))
				(seeds
				 (apply (option-processor option)
					option
					name
					option-arg
					seeds)))
			     (scan-args args seeds))))
		     (;;(rx bos "--" (submatch (+ any)))
		      (and (> (string-length arg) 3)
			   (char=? #\- (string-ref arg 0))
			   (char=? #\- (string-ref arg 1)))
		      ;; Found long option:
		      (let* ((name (substring arg 2 (string-length arg)))
			     (option (or (find-option name)
					 (option
					  (list name)
					  #f
					  #f
					  unrecognized-option-proc))))
			(if (and (option-required-arg? option)
				 (pair? args))
			    (let-values
				((seeds (apply (option-processor option)
					       option
					       name
					       (car args)
					       seeds)))
			      (scan-args (cdr args) seeds))
			    (let-values
				((seeds (apply (option-processor option)
					       option
					       name
					       #f
					       seeds)))
			      (scan-args args seeds)))))
		     (;; (rx bos "-" (submatch (+ any)))
		      (and (> (string-length arg) 1)
			   (char=? #\- (string-ref arg 0)))
		      ;; Found short options
		      (let ((shorts (substring arg 1 (string-length arg))))
			(scan-short-options 0 shorts args seeds)))
		     (else
		      (let-values ((seeds (apply operand-proc arg seeds)))
			(scan-args args seeds)))))))))
	(scan-args args seeds))))
)
