(uiop:define-package #:dag-search/search-node/heuristics
    (:use #:cl
          #:dag-search/score/all
          #:dag-search/search-node/search-node)
  (:export #:heuristic)
  (:documentation "Functions to handle heuristics."))

(in-package #:dag-search/search-node/heuristics)


;; TODO: Generalize
(defgeneric heuristic (search-node scores dag-score k-groups &optional allowable-fn)
  (:documentation "Calculates a heuristic value as the best value for all remaining nodes.")
  (:method ((node search-node) scores dag-score k-groups
            &optional (allowable-fn nil))
    ;; (simple-heuristic node scores data-set allowable-fn)
    (static-k-cycle-heuristic node scores dag-score k-groups allowable-fn)))


(defgeneric simple-heuristic (search-node scores dag-score &optional allowable-fn)
  (:documentation "A simple heuristic, each node is independent.")
  (:method ((node search-node) scores dag-score &optional (allowable-fn nil))
    (let ((heuristic 0)
          (n-variables (array-dimension (exact-p node) 0))
          possible-parents)
      (loop for variable below n-variables do
        (unless (member variable (assigned node))
          (setf possible-parents (loop for i below n-variables
                                       unless (equal i variable) collect i))
          (incf heuristic (first (bound variable possible-parents scores dag-score
                                        allowable-fn)))))
      heuristic)))


(defgeneric static-k-cycle-heuristic (search-node scores dag-score k-groups
                                      &optional allowable-fn)
  (:documentation "A heuristic with the cycles eliminated from a group of cycles.")
  (:method ((node search-node) scores dag-score k-groups
            &optional (allowable-fn nil))
    (let ((heuristic 0))
      (loop for i below (list-length k-groups)
            for k-group = (nth i k-groups)
            for other-k-groups = (remove k-group k-groups :test #'equalp)
            do (incf heuristic
                     (k-group-heuristic-value node scores dag-score k-group
                                              other-k-groups allowable-fn)))
      heuristic)))


(defgeneric k-group-heuristic-value (search-node scores dag-score k-group
                                     other-k-groups &optional allowable-fn)
  (:documentation "Heuristic value for a k group.")
  (:method ((node search-node) scores dag-score k-group other-k-groups
            &optional (allowable-fn nil))
    (let* ((to-assign (set-difference k-group (assigned node)))
           (possible-parents (apply #'concatenate 'list (assigned node) other-k-groups))
           (orderings (all-orderings to-assign))
           (best-value most-positive-double-float))

      ;; Different orderings of unassigned variables in the k-group
      (loop for ordering in orderings do
        (let ((value 0))
          (loop for i below (list-length ordering)
                for assign = (nth i to-assign)
                ;; Possible parents are variables outside the k group and previously
                ;; ordered variables in the same k group
                for parents = (concatenate 'list
                                           (subseq ordering (1+ i))
                                           possible-parents)
                do (incf value (first (bound assign parents scores dag-score
                                             allowable-fn))))
          (when (< value best-value)
            (setf best-value value))))

      best-value)))


(defun all-orderings (elements &key (orderings (list nil)))
  "Returns a list of all possible orderings of the elements."
  (let ((new-orderings nil))
    (when (equal (list-length (first orderings))
                 (list-length elements))
      (return-from all-orderings orderings))

    (loop for ordering in orderings do
      (loop for element in elements do
        (unless (member element ordering)
          (setf new-orderings (list* (list* element ordering) new-orderings)))))

    (all-orderings elements :orderings new-orderings)))
