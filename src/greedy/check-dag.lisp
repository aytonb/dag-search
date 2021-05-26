(uiop:define-package #:dag-search/greedy/check-dag
    (:use #:cl)
  (:export #:acyclic-p))

(in-package #:dag-search/greedy/check-dag)


(defun acyclic-p (dag)
  (let ((n-vars (list-length dag))
        (child-form nil))
    (dotimes (var n-vars)
      (setf child-form (acons var nil child-form))
      (dotimes (child n-vars)
        (unless (equal child var)
          (when (member var (cdr (assoc child dag)))
            (setf (cdr (assoc var child-form))
                  (nconc (cdr (assoc var child-form))
                         (list child)))))))

    ;; Attempt to prune variables with no children
    (dotimes (i n-vars)
      (block prune-1
        (loop for (var . children) in child-form do
          ;; When no children
          (unless children
            ;; Remove the variable
            (setf child-form (remove var child-form :key #'car))
            ;; Remove var as a child of other variables
            (loop for (other-var . other-children) in child-form do
              (setf (cdr (assoc other-var child-form))
                    (remove var other-children)))
            (return-from prune-1)))

        ;; If we get here, we are cyclic
        (return-from acyclic-p nil)))

    ;; Everything pruned, we are acyclic
    t))
