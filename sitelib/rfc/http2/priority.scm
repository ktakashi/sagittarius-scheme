;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http2/priority.scm - HTTP/2 priority tree and scheduler
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
(library (rfc http2 priority)
  (export make-http2-priority-tree
          http2-priority-tree?
          http2-priority-tree-add!
          http2-priority-tree-remove!
          http2-priority-tree-ref
          http2-priority-node?
          http2-priority-node-id
          http2-priority-node-weight
          http2-priority-node-parent
          http2-priority-node-children
          http2-priority-tree-schedule
          http2-priority-tree-account!
          +http2-default-weight+)
  (import (rnrs)
          (rfc http2 conditions))

(define +http2-default-weight+ 16)
(define +http2-default-max-nodes+ 205)

(define-record-type http2-priority-node
  (fields id
          (mutable parent)
          (mutable children)
          (mutable weight)
          (mutable deficit)
          (mutable placeholder?)))

(define-record-type http2-priority-tree
  (fields nodes
          max-nodes
          (mutable placeholders))
  (protocol
   (lambda (p)
     (lambda (:optional (max-nodes +http2-default-max-nodes+))
       (let* ((nodes (make-eqv-hashtable))
	      (tree (p nodes (max 1 max-nodes) '())))
	 (hashtable-set! nodes 0
	   (make-http2-priority-node 0 0 '() +http2-default-weight+ 0 #f))
	 tree)))))

(define (http2-priority-tree-ref tree id)
  (hashtable-ref (http2-priority-tree-nodes tree) id #f))

(define (remove-id ids id)
  (let loop ((rest ids) (out '()))
    (if (null? rest)
        (reverse out)
        (if (= (car rest) id)
            (loop (cdr rest) out)
            (loop (cdr rest) (cons (car rest) out))))))

(define (insert-sorted ids id)
  (let loop ((rest ids) (out '()))
    (cond ((null? rest)
           (reverse (cons id out)))
          ((= (car rest) id)
           (reverse (append out rest)))
          ((< id (car rest))
           (reverse (append out (cons id rest))))
          (else
           (loop (cdr rest) (cons (car rest) out))))))

(define (mark-placeholder! tree id)
  (unless (memv id (http2-priority-tree-placeholders tree))
    (http2-priority-tree-placeholders-set!
     tree
     (append (http2-priority-tree-placeholders tree) (list id)))))

(define (unmark-placeholder! tree id)
  (http2-priority-tree-placeholders-set!
   tree
   (remove-id (http2-priority-tree-placeholders tree) id)))

(define (node-count tree)
  (- (hashtable-size (http2-priority-tree-nodes tree)) 1))

(define (normalize-weight wire-or-effective)
  (cond ((not (integer? wire-or-effective)) +http2-default-weight+)
        ((and (<= 0 wire-or-effective) (<= wire-or-effective 255))
         (+ wire-or-effective 1))
        ((< wire-or-effective 1) 1)
        ((> wire-or-effective 256) 256)
        (else wire-or-effective)))

(define (detach-child! tree parent-id child-id)
  (let ((parent (http2-priority-tree-ref tree parent-id)))
    (when parent
      (http2-priority-node-children-set!
       parent
       (remove-id (http2-priority-node-children parent) child-id)))))

(define (attach-child! tree parent-id child-id)
  (let ((parent (http2-priority-tree-ref tree parent-id))
        (child (http2-priority-tree-ref tree child-id)))
    (when (and parent child)
      (http2-priority-node-parent-set! child parent-id)
      (http2-priority-node-children-set!
       parent
       (insert-sorted (http2-priority-node-children parent) child-id)))))

(define (descendant-of? tree ancestor-id id)
  (let loop ((current id))
    (cond ((zero? current) #f)
          ((= current ancestor-id) #t)
          (else
           (let ((node (http2-priority-tree-ref tree current)))
             (and node (loop (http2-priority-node-parent node))))))))

(define (evict-oldest-placeholder! tree)
  (let loop ((rest (http2-priority-tree-placeholders tree)))
    (and (pair? rest)
         (let* ((id (car rest))
                (node (http2-priority-tree-ref tree id)))
           (if (and node (http2-priority-node-placeholder? node))
               (begin
                 (http2-priority-tree-remove! tree id)
                 #t)
               (loop (cdr rest)))))))

;; Bound placeholder/idle nodes to avoid memory growth from PRIORITY spam.
(define (enforce-node-cap! tree)
  (let loop ()
    (when (> (node-count tree) (http2-priority-tree-max-nodes tree))
      (when (evict-oldest-placeholder! tree)
        (loop)))))

(define (ensure-node! tree id placeholder?)
  (let ((node (http2-priority-tree-ref tree id)))
    (if node
        (begin
          (unless placeholder?
            (http2-priority-node-placeholder?-set! node #f)
            (unmark-placeholder! tree id))
          node)
        (let ((new-node
               (make-http2-priority-node id 0 '() +http2-default-weight+ 0 placeholder?)))
          (hashtable-set! (http2-priority-tree-nodes tree) id new-node)
          (attach-child! tree 0 id)
          (when placeholder?
            (mark-placeholder! tree id))
          (enforce-node-cap! tree)
          new-node))))

(define (http2-priority-tree-add! tree id dependency weight exclusive? . maybe-placeholder)
  (let ((placeholder? (and (pair? maybe-placeholder) (car maybe-placeholder))))
    (when (or (not (integer? id)) (<= id 0))
      (http2-protocol-error 'http2-priority-tree-add!
                            "Invalid stream identifier"
                            id))
    (when (= id dependency)
      (http2-protocol-error 'http2-priority-tree-add!
                            "A stream cannot depend on itself"
                            id))
    (let* ((node (ensure-node! tree id (and placeholder? #t)))
           (parent-id (if (and dependency (integer? dependency) (>= dependency 0))
                          dependency
                          0))
           (parent (ensure-node! tree parent-id #t)))
      (http2-priority-node-weight-set! node (normalize-weight weight))

      ;; RFC 7540 5.3.3 reprioritization: move descendant first to avoid cycles.
      (when (descendant-of? tree id parent-id)
        (let* ((node-parent-id (http2-priority-node-parent node))
               (parent-parent-id (http2-priority-node-parent parent)))
          (detach-child! tree parent-parent-id parent-id)
          (detach-child! tree node-parent-id id)
          (attach-child! tree node-parent-id parent-id)))

      (let ((old-parent-id (http2-priority-node-parent node)))
        (detach-child! tree old-parent-id id))

      (if exclusive?
          (let ((old-children (remove-id (http2-priority-node-children parent) id)))
            (http2-priority-node-children-set! parent '())
            (attach-child! tree parent-id id)
            (for-each
             (lambda (child-id)
               (attach-child! tree id child-id))
             old-children))
          (attach-child! tree parent-id id))

      (enforce-node-cap! tree)
      node)))

(define (http2-priority-tree-remove! tree id)
  (let ((node (http2-priority-tree-ref tree id)))
    (if (or (not node) (zero? id))
        #f
        (let ((parent-id (http2-priority-node-parent node))
              (children (http2-priority-node-children node)))
          (detach-child! tree parent-id id)
          (for-each
           (lambda (child-id)
             (attach-child! tree parent-id child-id))
           children)
          (http2-priority-node-children-set! node '())
          (when (http2-priority-node-placeholder? node)
            (unmark-placeholder! tree id))
          (hashtable-delete! (http2-priority-tree-nodes tree) id)
          #t))))

(define (node-active? tree id stream-active?)
  (let ((node (http2-priority-tree-ref tree id)))
    (and node
         (or (and (not (zero? id)) (stream-active? id))
             (let loop ((children (http2-priority-node-children node)))
               (and (pair? children)
                    (or (node-active? tree (car children) stream-active?)
                        (loop (cdr children)))))))))

(define (pick-child tree child-ids stream-active?)
  (let loop ((rest child-ids) (best #f))
    (if (null? rest)
        best
        (let* ((id (car rest))
               (node (http2-priority-tree-ref tree id)))
          (if (and node (node-active? tree id stream-active?))
              (let ((best-node (and best (http2-priority-tree-ref tree best))))
                (if (or (not best-node)
                        (< (http2-priority-node-deficit node)
                           (http2-priority-node-deficit best-node))
                        (and (= (http2-priority-node-deficit node)
                                (http2-priority-node-deficit best-node))
                             (< id best)))
                    (loop (cdr rest) id)
                    (loop (cdr rest) best)))
              (loop (cdr rest) best))))))

(define (http2-priority-tree-schedule tree stream-active?)
  (let loop ((id 0))
    (let* ((node (http2-priority-tree-ref tree id))
           (next (and node
                      (pick-child tree
                                  (http2-priority-node-children node)
                                  stream-active?))))
      (if (not next)
          #f
          (if (stream-active? next)
              next
              (loop next))))))

(define (ceil-div n d)
  (if (zero? d)
      n
      (let ((q (div n d))
            (r (mod n d)))
        (if (zero? r) q (+ q 1)))))

(define (http2-priority-tree-account! tree stream-id bytes)
  (when (and (integer? bytes) (> bytes 0))
    (let loop ((id stream-id))
      (let ((node (http2-priority-tree-ref tree id)))
        (when node
          (http2-priority-node-deficit-set!
           node
           (+ (http2-priority-node-deficit node)
              (ceil-div (* bytes 256) (http2-priority-node-weight node))))
          (unless (zero? id)
            (loop (http2-priority-node-parent node))))))))
)
