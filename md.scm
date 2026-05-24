(import (rnrs)
	(text markdown)
	(sagittarius document)
	(pp))

(define s "* @[-[text](../link)]")

(pp (write-document 'markdown
     (port->document 'markdown (open-string-input-port s) )))
