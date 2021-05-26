(uiop:define-package #:dag-search/abc/astar
    (:use #:cl
          #:priority-queue
          #:dag-search/score/all
          #:dag-search/search-node/all)
  (:export #:astar-search)
  (:documentation "A* search."))

(in-package #:dag-search/abc/astar)


(defgeneric astar-search (initial-node dag-score supplied-liks k-groups
                          &optional allowable-fn)
  (:documentation "Runs A* search.

supplied-liks is an a list of variable -> hash of parents -> lik found so far.")
  (:method ((initial-node search-node) dag-score supplied-liks k-groups
            &optional (allowable-fn nil))
    (flet ((compute-func (variable parents)
             (let ((lik (- (gethash parents (cdr (assoc variable supplied-liks))))))
               (list (+ (penalty dag-score variable parents) lik)
                     lik))))
      (let ((q (make-pqueue #'<)) 
            to-expand
            successors
            (n-vars (n-vars dag-score))
            next-best
            apply-bounds-p
            (terminal-bound-p nil)
            (expanded-list (make-hash-table :test #'equalp))
            (liks nil)
            (scores nil))
        
        (pqueue-push q initial-node 0)
        
        ;; Begin by computing all scores
        (loop for variable below n-vars do
          (setf scores (acons variable (make-hash-table :test #'equalp) scores)
                liks (acons variable (make-hash-table :test #'equalp) liks))
          (loop for parents being the hash-keys of (cdr (assoc variable supplied-liks))
                do (destructuring-bind (score lik)
                       (compute-func variable parents)
                     (setf (gethash parents (cdr (assoc variable scores)))
                           score)
                     (setf (gethash parents (cdr (assoc variable liks)))
                           lik))))
        
        (setf to-expand (pqueue-pop q))
        (loop while t do
          ;; For now, terminate when we expand a terminal node.
                                        ;(when (equal (list-length (assigned to-expand)) n-vars)
                                        ;  (return-from abc-search (list to-expand q scores expanded-list)))
          
          ;; Requeue a terminal node if it is not complete
          (setf terminal-bound-p nil)
          
          (when (equal (list-length (assigned to-expand)) n-vars)
            (setf terminal-bound-p nil)
            (loop for i below n-vars do
              (unless (aref (exact-p to-expand) i)
                (setf terminal-bound-p t)
                (print "calling bounds -- error?")
                (destructuring-bind (score lik)
                    (compute-func i (cdr (assoc i (edges to-expand))))
                  (setf (gethash (cdr (assoc i (edges to-expand)))
                                 (cdr (assoc i scores)))
                        score)
                  (setf (gethash (cdr (assoc i (edges to-expand)))
                                 (cdr (assoc i liks)))
                        lik))
                (pqueue-push to-expand (+ (g to-expand) (h to-expand)) q)
                (return)))
            (unless terminal-bound-p
              (return-from astar-search (list to-expand q scores expanded-list))))
          
          
          (unless terminal-bound-p
            
            ;; Check if already in the expanded list
            (unless (gethash (assigned to-expand) expanded-list)
              
              ;; Add to expanded list when there is a chain of exact assigned variables.
              (loop for i below (list-length (assigned-ordering to-expand)) do
                (if (aref (exact-p to-expand) i)
                    (setf (gethash (sort (copy-tree (subseq (assigned-ordering to-expand)
                                                            0 (1+ i)))
                                         #'<)
                                   expanded-list)
                          t)
                    (return)))
              
                                        ;(format t "~%== Expanding ~a ==~%" (assigned to-expand))
              
              ;; Make successor nodes, sort, and indicate to apply bounds
              ;; TODO: We can test if bounds are unnecessary
              (setf successors (make-successor-nodes to-expand
                                                     scores
                                                     liks
                                                     dag-score
                                                     #'compute-func
                                                     k-groups
                                                     allowable-fn))
              (loop for successor in successors do
                                        ;(format t "child ~a~%" (assigned successor))
                (pqueue-push successor (+ (g successor) (h successor)) q))))
          
          
          (setf apply-bounds-p t)
          
          ;; As long as applying bounds causes the front element to change, keep
          ;; applying
          (loop while apply-bounds-p do
            (setf to-expand (pqueue-pop q))
            (apply-bounds to-expand scores dag-score k-groups allowable-fn)
            (setf next-best (pqueue-front q)
                  apply-bounds-p (< (+ (g next-best) (h next-best))
                                    (+ (g to-expand) (h to-expand))))
            (when apply-bounds-p
              (pqueue-push to-expand (+ (g to-expand) (h to-expand)) q))))))))
