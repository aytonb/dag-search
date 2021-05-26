(uiop:define-package #:dag-search/abc/abc
    (:use #:cl
          #:priority-queue
          #:dag-search/score/all
          #:dag-search/search-node/all)
  (:export #:abc-search
           #:abc-check-stuck)
  (:documentation "A*BC search."))

(in-package #:dag-search/abc/abc)


(defgeneric abc-search (initial-node dag-score lik-func k-groups
                        &optional candidate-dag allowable-fn max-set-fn)
  (:documentation "Runs A*BC search.

lik-func is a function on variable and parents that computes the score and likelihood.")
  (:method ((initial-node search-node) dag-score lik-func k-groups
            &optional (candidate-dag nil) (allowable-fn nil) (max-set-fn nil))
    (flet ((compute-func (variable parents)
             (let ((lik (- (funcall lik-func variable parents))))
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

        (pqueue-push initial-node 0 q)

        ;; Fill in the scores list with the original liks, and flip lik signs
        ;; (loop for (var . var-liks) in liks do
        ;;   (setf scores (acons var (make-hash-table :test #'equalp)
        ;;                       scores))
        ;;   (loop for parents being the hash-keys of var-liks
        ;;           using (hash-value par-lik)
        ;;         do (setf (gethash parents (cdr (assoc var scores)))
        ;;                  (- (penalty dag-score var parents) par-lik)
        ;;                  (gethash parents var-liks)
        ;;                  (- par-lik))))

        ;; Begin by computing the score of the largest parent set of every variable
        (loop for variable below n-vars
              for parents = (if max-set-fn
                                (funcall max-set-fn variable)
                                (loop for i below n-vars
                                      unless (equal i variable)
                                        collect i))
              do ;;(unless (cdr (assoc variable liks))
                 (setf scores (acons variable (make-hash-table :test #'equalp)
                                     scores)
                       liks (acons variable (make-hash-table :test #'equalp)
                                   liks))
                 ;;)
                 (unless (gethash parents (cdr (assoc variable liks)))
                  (destructuring-bind (score lik)
                     (compute-func variable parents)
                    (setf (gethash parents (cdr (assoc variable scores)))
                          score)
                    (setf (gethash parents (cdr (assoc variable liks)))
                          lik))))

        ;; Solve candidate dag
        (when candidate-dag
          (loop for (variable . parents) in candidate-dag do
            (unless (or (gethash parents (cdr (assoc variable liks)))
                        ;; Don't evaluate an unallowed candidate DAG (for now)
                        (and allowable-fn
                             (not (funcall allowable-fn
                                           variable parents))))
              (destructuring-bind (score lik)
                  (compute-func variable parents)
                (setf (gethash parents (cdr (assoc variable scores)))
                      score
                      (gethash parents (cdr (assoc variable liks)))
                      lik)))))

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
                ;(print "calling bounds")
                (destructuring-bind (score lik)
                    (compute-func i (cdr (assoc i (edges to-expand))))
                  (setf (gethash (cdr (assoc i (edges to-expand)))
                                 (cdr (assoc i scores)))
                        score)
                  (setf (gethash (cdr (assoc i (edges to-expand)))
                                 (cdr (assoc i liks)))
                        lik))
                ;; (best-parents-active-bound to-expand
                ;;                            i
                ;;                            (bound i
                ;;                                   (aref (possible-parent-sets
                ;;                                          to-expand)
                ;;                                         i)
                ;;                                   scores
                ;;                                   dag-score
                ;;                                   allowable-fn)
                ;;                            scores
                ;;                            liks
                ;;                            dag-score
                ;;                            compute-func
                ;;                            allowable-fn)
                (pqueue-push to-expand (+ (g to-expand) (h to-expand)) q)
                (return)))
            (unless terminal-bound-p
              (return-from abc-search (list to-expand q scores expanded-list))))
          
          
          (unless terminal-bound-p
            
            ;; Use if we want to extract bounds from a node we will expand
            
            ;; Get a bound for the expanded node, using the best parents so far for the node
            ;; (when (assigned to-expand)
            ;;   (let ((last-assigned (first (last (assigned-ordering to-expand)))))
            ;;   (unless (aref (exact-p to-expand) last-assigned)
            ;;     (setf (gethash (cdr (assoc last-assigned (edges to-expand)))
            ;;                    (cdr (assoc last-assigned scores)))
            ;;           (funcall compute-func last-assigned (cdr (assoc last-assigned (edges to-expand))))))))
            
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
            (setf next-best (pqueue-front q))
            (if next-best
                (setf apply-bounds-p (< (+ (g next-best) (h next-best))
                                        (+ (g to-expand) (h to-expand))))
                ;; If no next-best exists, then don't requeue.
                (setf apply-bounds-p nil))
            (when apply-bounds-p
              (pqueue-push to-expand (+ (g to-expand) (h to-expand)) q))))))))


(defgeneric abc-check-stuck (initial-node scores data compute-func lik-compute-func k-groups)
  (:documentation "Runs A*BC search.

scores is an a list of variable -> hash of parents -> score found so far.

data is the raw data set.

compute-func is a function on variable and parents that computes the score.")
  (:method ((initial-node search-node) scores data compute-func lik-compute-func k-groups)
    (let ((q (make-pqueue #'<)) 
          to-expand
          successors
          (n-vars (n-vars data))
          next-best
          apply-bounds-p
          (terminal-bound-p nil)
          (expanded-list (make-hash-table :test #'equalp))
          (liks nil))

      (pqueue-push initial-node 0 q)
      

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
              (return-from abc-check-stuck (list nil to-expand q scores expanded-list))))
          (unless terminal-bound-p
            (return-from abc-check-stuck (list t to-expand q scores expanded-list))))

        
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

            (setf successors (make-successor-nodes to-expand scores liks data compute-func lik-compute-func k-groups))
            (loop for successor in successors do
              (pqueue-push successor (+ (g successor) (h successor)) q))))
        

        (setf apply-bounds-p t)

        ;; As long as applying bounds causes the front element to change, keep
        ;; applying
        (loop while apply-bounds-p do
          (setf to-expand (pqueue-pop q))
          (apply-bounds to-expand scores data k-groups)
          (setf next-best (pqueue-front q)
                apply-bounds-p (< (+ (g next-best) (h next-best))
                                  (+ (g to-expand) (h to-expand))))
          (when apply-bounds-p
            (pqueue-push to-expand (+ (g to-expand) (h to-expand)) q)))))))
