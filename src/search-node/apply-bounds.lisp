(uiop:define-package #:dag-search/search-node/apply-bounds
    (:use #:cl
          #:dag-search/score/all
          #:dag-search/search-node/search-node
          #:dag-search/search-node/heuristics)
  (:export #:apply-bounds)
  (:documentation "Apply bounds to a node."))

(in-package #:dag-search/search-node/apply-bounds)


(defgeneric apply-bounds (search-node scores dag-score k-groups
                          &optional allowable-fn)
  (:documentation "Applies any new bounds to search node.")
  (:method ((node search-node) scores dag-score k-groups
            &optional (allowable-fn nil))
    ;; TODO: Could keep better track here
    (let ((n-variables (array-dimension (exact-p node) 0))
          new-score)
      (setf (g node)
            (loop for variable below n-variables
                  when (member variable (assigned node))
                    do (setf new-score
                             (bound variable
                                    (aref (possible-parent-sets node)
                                          variable)
                                    scores
                                    dag-score
                                    allowable-fn)
                             (cdr (assoc variable (edges node))) (second new-score)
                             (aref (exact-p node) variable) (third new-score))
                    and sum (first new-score)))
      (setf (h node) (heuristic node scores dag-score k-groups allowable-fn)))))
