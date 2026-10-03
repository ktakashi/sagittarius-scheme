(import (rnrs)
	(srfi :226 control call-in-initial-continuation)
	(srfi :226 control parameters)
	(srfi :226 control continuation-marks)
	(srfi :64))

(test-begin "call-in-initial-continuation")

(test-equal #f (with-continuation-mark 'key 'mark
           (call-in-initial-continuation
            (lambda ()
              (continuation-mark-set-first #f 'key)))))
(test-equal '(#f 1)
      (let ([tag (make-continuation-prompt-tag)]
	    [p (make-parameter 0)])
	(parameterize ([p 1])
	  (call-in-initial-continuation
	   (lambda ()
	     (list (continuation-prompt-available? (call/cc values))
		   (p)))))))

(test-end)
