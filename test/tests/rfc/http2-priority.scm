(import (rnrs)
        (rfc http2 priority)
        (rfc http2 conditions)
        (srfi :64))

(test-begin "rfc/http2-priority")

(define (parent-of tree id)
  (let ((node (http2-priority-tree-ref tree id)))
    (and node (http2-priority-node-parent node))))

(define (children-of tree id)
  (let ((node (http2-priority-tree-ref tree id)))
    (and node (http2-priority-node-children node))))

(let ()
  (define tree (make-http2-priority-tree))
  (define node (http2-priority-tree-add! tree 1 0 15 #f))
  (test-assert "default insertion depends on root" (= 0 (http2-priority-node-parent node)))
  (test-equal "wire weight converts to effective weight" 16 (http2-priority-node-weight node)))

(let ()
  (define tree (make-http2-priority-tree))
  (test-error "self dependency is rejected"
              http2-error?
              (http2-priority-tree-add! tree 1 1 15 #f)))

(let ()
  (define tree (make-http2-priority-tree))
  (http2-priority-tree-add! tree 1 0 15 #f)
  (http2-priority-tree-add! tree 3 0 15 #f)
  (http2-priority-tree-add! tree 5 0 15 #t)
  (test-equal "exclusive dependency makes sole child"
              '(5)
              (children-of tree 0))
  (test-equal "exclusive dependency adopts old children"
              '(1 3)
              (children-of tree 5)))

(let ()
  (define tree (make-http2-priority-tree))
  (http2-priority-tree-add! tree 1 0 15 #f)
  (http2-priority-tree-add! tree 3 1 15 #f)
  (http2-priority-tree-add! tree 1 3 15 #f)
  (test-equal "descendant reprioritization moves descendant up"
              0
              (parent-of tree 3))
  (test-equal "descendant reprioritization then applies new dependency"
              3
              (parent-of tree 1)))

(let ()
  (define tree (make-http2-priority-tree))
  (http2-priority-tree-add! tree 1 0 15 #f)
  (http2-priority-tree-add! tree 3 1 15 #f)
  (http2-priority-tree-add! tree 5 1 15 #f)
  (http2-priority-tree-remove! tree 1)
  (test-equal "remove reparents first child"
              0
              (parent-of tree 3))
  (test-equal "remove reparents second child"
              0
              (parent-of tree 5)))

(let ()
  (define tree (make-http2-priority-tree 2))
  (http2-priority-tree-add! tree 1 0 15 #f #t)
  (http2-priority-tree-add! tree 3 0 15 #f #t)
  (http2-priority-tree-add! tree 5 0 15 #f #t)
  (test-equal "placeholder cap evicts oldest"
              #f
              (http2-priority-tree-ref tree 1))
  (test-assert "newer placeholder is kept"
               (http2-priority-tree-ref tree 3))
  (test-assert "newest placeholder is kept"
               (http2-priority-tree-ref tree 5)))

(let ()
  (define tree (make-http2-priority-tree))
  (define sent1 0)
  (define sent3 0)
  (http2-priority-tree-add! tree 1 0 0 #f)
  (http2-priority-tree-add! tree 3 0 2 #f)
  (do ((i 0 (+ i 1)))
      ((= i 400))
    (let ((sid (http2-priority-tree-schedule tree
                                             (lambda (id)
                                               (or (= id 1) (= id 3))))))
      (cond ((= sid 1) (set! sent1 (+ sent1 1)))
            ((= sid 3) (set! sent3 (+ sent3 1)))
            (else #f))
      (http2-priority-tree-account! tree sid 1)))
  (test-assert "scheduler gives more slots to heavier stream"
               (and (> sent3 sent1)
                    (> (/ sent3 sent1) 2)
                    (< (/ sent3 sent1) 4))))

(let ()
  (define tree (make-http2-priority-tree))
  (http2-priority-tree-add! tree 1 0 15 #f)
  (http2-priority-tree-add! tree 3 0 15 #f)
  (test-equal "inactive stream is skipped"
              1
              (http2-priority-tree-schedule tree
                                            (lambda (id)
                                              (= id 1)))))

(test-end)
