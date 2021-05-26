(uiop:define-package #:dag-search/search-node/successor-nodes
    (:use #:cl
          #:dag-search/score/all
          #:dag-search/search-node/search-node
          #:dag-search/search-node/bound-strategy
          #:dag-search/search-node/heuristics)
  (:export #:make-successor-nodes)
  (:documentation "Functions to make new successor nodes."))

(in-package #:dag-search/search-node/successor-nodes)


(defgeneric make-successor-nodes (search-node scores liks dag-score compute-func k-groups &optional allowable-fn)
  (:documentation "Makes the successor nodes of a search node.")
  (:method ((node search-node) scores liks dag-score compute-func k-groups
            &optional (allowable-fn nil))
    (let ((n-variables (array-dimension (exact-p node) 0))
          new-node
          new-score
          make-new-node)
      (loop for variable below n-variables
            do (setf make-new-node
                     (if (member variable (assigned node))
                         nil
                         (if allowable-fn
                           (block allowable-new-node
                             (loop for i from 0 upto (list-length (assigned node))
                                   do (loop for parents in (subsets-of-size (assigned node) i) do
                                     (when (funcall allowable-fn variable parents)
                                       (return-from allowable-new-node t)))
                                   finally (return nil)))
                           t)))
            ;unless (member variable (assigned node))
              
            when make-new-node do
                 (setf new-node (make-instance 'search-node
                                               :assigned (sort (list* variable
                                                                      (copy-tree (assigned node)))
                                                               #'<)
                                               :assigned-ordering (append (assigned-ordering node) (list variable))
                                               :possible-parent-sets (copy-seq (possible-parent-sets node))
                                               :exact-p (copy-seq (exact-p node))))

                 (setf new-score (bound variable (assigned node) scores dag-score
                                        allowable-fn))

                 ;; Use if we want to extract bounds from the successor nodes
                 ;; Commenting this out creates the A*BC default strategy
                 
                 ;; If the score is not exact, compute it for the parents.
                 ;; (setf new-score (best-parents-bound node
                 ;;                                     variable
                 ;;                                     new-score
                 ;;                                     scores
                 ;;                                     data-set
                 ;;                                     compute-func))
                 (setf new-score (best-parents-active-bound node 
                                                            variable
                                                            new-score
                                                            scores
                                                            liks
                                                            dag-score
                                                            compute-func
                                                            allowable-fn))

                 ;; (setf new-score (best-parents-likelihood-bound node
                 ;;                                                variable
                 ;;                                                new-score
                 ;;                                                scores
                 ;;                                                liks
                 ;;                                                data-set
                 ;;                                                compute-func
                 ;;                                                lik-compute-func))
                 
                 ;; (setf new-score (leaps-bound node
                 ;;                              variable
                 ;;                              new-score
                 ;;                              scores
                 ;;                              liks
                 ;;                              data-set
                 ;;                              compute-func
                 ;;                              lik-compute-func))
                 
                 (setf (g new-node) (+ (g node) (first new-score))
                       (h new-node) (heuristic new-node scores dag-score k-groups
                                               allowable-fn)
                       (aref (possible-parent-sets new-node) variable) (assigned node)
                       (edges new-node) (acons variable (second new-score) (edges node))
                       (aref (exact-p new-node) variable) (third new-score))
            and collect new-node))))
