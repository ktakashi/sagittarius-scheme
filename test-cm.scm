(import (rnrs)
	(sagittarius continuations)
	(srfi :64))

(test-begin "continuation mark")

(test-assert (continuation-mark-set? (current-continuation-marks)))

(test-equal 'mark
	    (with-continuation-mark 'key 'mark
	     (call-with-immediate-continuation-mark 'key values)))

(test-equal 'default
	    (let ([tag (make-continuation-prompt-tag)])
	      (with-continuation-mark 'key 'mark
	       (call-with-continuation-prompt
		(lambda ()
		  (call-with-immediate-continuation-mark 'key values 'default))
		tag))))

(let ()
  (define tag (make-continuation-prompt-tag))
  (define key (make-continuation-mark-key))
  (define key1 (make-continuation-mark-key))
  (define key2 (make-continuation-mark-key))

  ;; With default prompt tag, marks outside the prompt boundary are
  ;; not visible.  mark2 replaces mark1 (same key, tail position), but
  ;; both are outside the prompt.
  (test-equal '(mark3)
	      (with-continuation-mark key 'mark1
	       (with-continuation-mark key 'mark2
	        (call-with-continuation-prompt
		 (lambda ()
		   (with-continuation-mark key 'mark3
		    (continuation-mark-set->list #f key)))))))

  ;; With custom prompt tag for installation but default tag for
  ;; querying, the prompt boundary is not detected, so all marks are
  ;; visible.
  (test-equal '(#(mark3 default) #(mark1 mark2))
	      (with-continuation-mark key1 'mark1
	       (with-continuation-mark key2 'mark2
	        (call-with-continuation-prompt
		 (lambda ()
		   (with-continuation-mark key1 'mark3
		    (continuation-mark-set->list* #f (list key1 key2) 'default)))
		 tag)))))

(test-equal 'mark2
	    (let ([tag (make-continuation-prompt-tag)]
		  [key (make-continuation-mark-key)])
	      (with-continuation-mark key 'mark1
	       (call-with-continuation-prompt
		(lambda ()
		  (with-continuation-mark key 'mark2
		   (continuation-mark-set-first #f key)))
		tag))))

(test-equal 'mark
	    (let ([tag (make-continuation-prompt-tag 'mytag)]
		  [key (make-continuation-mark-key)])
	      (define k
		(with-continuation-mark key 'mark
		 (call-with-continuation-prompt
		  (lambda ()
		    (call/cc values))
		  tag)))
	      (continuation-mark-set-first (continuation-marks k) key)))

(test-equal 'mark1
	    (with-continuation-mark 'key 'mark1
                (call-with-immediate-continuation-mark 'key values)))

(test-equal 'mark2
	    (with-continuation-mark 'key 'mark1
                (with-continuation-mark 'key 'mark2
                  (call-with-immediate-continuation-mark 'key values))))

(test-equal '(#f) 
	    (with-continuation-mark 'key 'mark1
              (list
                (call-with-immediate-continuation-mark 'key values))))

(test-equal '((mark1) (mark2))
       (with-continuation-mark 'key1 'mark1
         (with-continuation-mark 'key2 'mark2
           (list
            (continuation-mark-set->list #f 'key1)
            (continuation-mark-set->list #f 'key2)))))

(test-equal '((mark1) (mark2))
      (with-continuation-marks (['key1 'mark1]
                                ['key2 'mark2])
        (list
          (continuation-mark-set->list #f 'key1)
          (continuation-mark-set->list #f 'key2))))

(test-equal '(1)
      (let f ([n 10])
        (if (fxzero? n)
            (continuation-mark-set->list #f 'key)
            (with-continuation-mark 'key n
              (f (fx- n 1))))))

(test-equal '(mark2)
      (with-continuation-mark 'key 'mark1
        (call-with-continuation-prompt
         (lambda ()
           (with-continuation-mark 'key 'mark2
             (continuation-mark-set->list #f 'key))))))

(test-equal '(mark2)
      (with-continuation-mark 'key 'mark1
        (list
         (with-continuation-mark 'key 'mark2
           (continuation-mark-set-first #f 'key)))))

(test-equal '(((#(#f mark2) #(mark1 mark2))))
      (with-continuation-mark 'key1 'mark1
        (with-continuation-mark 'key2 'mark2
          (list
           (with-continuation-mark 'key3 'mark3
             (list
              (with-continuation-mark 'key2 'mark2
                (continuation-mark-set->list* #f '(key1 key2)))))))))


(test-equal 'mark
      (with-continuation-mark 'key 'mark
	(call-with-immediate-continuation-mark 'key values)))
(test-equal 'default
      (let ([tag (make-continuation-prompt-tag)])
	(with-continuation-mark 'key 'mark
	  (call-with-continuation-prompt
	   (lambda ()
	     (call-with-immediate-continuation-mark 'key values 'default))
	   tag))))

(test-equal #t (continuation-mark-set? (current-continuation-marks)))

(test-equal '(mark3 mark2)
      (let ([tag (make-continuation-prompt-tag)]
	    [key (make-continuation-mark-key)])
	(with-continuation-mark key 'mark1
	  (with-continuation-mark key 'mark2
	    (call-with-continuation-prompt
	     (lambda ()
	       (with-continuation-mark key 'mark3
		 (continuation-mark-set->list #f key)))
	     tag)))))
(test-equal '(#(mark3 default) #(mark1 mark2))
      (let ([tag (make-continuation-prompt-tag)]
	    [key1 (make-continuation-mark-key)]
	    [key2 (make-continuation-mark-key)])
	(with-continuation-mark key1 'mark1
	  (with-continuation-mark key2 'mark2
	    (call-with-continuation-prompt
	     (lambda ()
	       (with-continuation-mark key1 'mark3
		 (continuation-mark-set->list* #f (list key1 key2) 'default)))
	     tag)))))

(test-equal 'mark2
      (let ([tag (make-continuation-prompt-tag)]
	    [key (make-continuation-mark-key)])
	(with-continuation-mark key 'mark1
	  (call-with-continuation-prompt
	   (lambda ()
	     (with-continuation-mark key 'mark2
	       (continuation-mark-set-first #f key)))
	   tag))))

(test-equal 'mark
      (let ([tag (make-continuation-prompt-tag 'mytag)]
	    [key (make-continuation-mark-key)])
        (define k
	  (with-continuation-mark key 'mark
	    (call-with-continuation-prompt
	      (lambda ()
                (call/cc values))
	      tag)))
        (continuation-mark-set-first (continuation-marks k) key)))

(test-equal #t (continuation-mark-key? (make-continuation-mark-key)))
(test-equal #f (equal? (make-continuation-mark-key) (make-continuation-mark-key)))

(test-end)
