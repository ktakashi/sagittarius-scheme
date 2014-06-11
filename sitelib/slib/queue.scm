;; -*- scheme -*-
;; now it's only for forwarding
(library (slib queue)
    (export make-queue
	    queue?
	    queue-empty?
	    queue-push!
	    queue-pop!
	    enqueue!
	    dequeue!
	    dequeue-all!
	    queue-front
	    queue-rear)
    (import (util queue)))