(uiop:define-package #:dag-search/dag-gp-test
    (:use #:cl
          #:dag-search/discrete-score
          #:dag-search/calculate-discrete-scores
          #:dag-search/score/deep-gp
          #:dag-search/score/consistent-score
          #:dag-search/score/likelihood-from-score
          #:dag-search/search-node
          #:dag-search/abc)
  (:export ))

(in-package #:dag-search/dag-gp-test)


(defun all-eq-ni-zn-cd-test ()
  (let ((scores-alist `((0 ((1 2) . -39.24281526127421d0)
                         ((1) . -36.6574993872334d0)
                         ((2) . -5.343657351493114d0)
                         (nil . 22.79682644994014d0))
                        (1 ((0 2) . -37.28551410689345d0)
                         ((0) . -14.02180500114626d0)
                         ((2) . -9.66538214940068d0)
                         (nil . 42.76883661956843d0))
                        (2 ((0 1) . ,(- 6d0 0.8882533799869923d0))
                         ((0) . 28.971695554263576d0)
                         ((1) . 8.648187718238468d0)
                         (nil . 65.44379196074723d0))))
        (lik-alist '((0 ((1 2) . 45.24281526127421d0)
                      ((1) . 40.6574993872334d0)
                      ((2) . 9.343657351493114d0)
                      (nil . -20.79682644994014d0))
                     (1 ((0 2) . 43.28551410689345d0)
                      ((0) . 18.02180500114626d0)
                      ((2) . 13.66538214940068d0)
                      (nil . -40.76883661956843d0))
                     (2 ((0 1) . 0.8882533799869923d0)
                      ((0) . -24.971695554263576d0)
                      ((1) . -4.648187718238468d0)
                      (nil . -63.44379196074723d0))))
        hash
        initial-node
        compute-func
        lik-compute-func
        out
        (all-scores nil)
        (all-lik nil)
        (k-groups (list (list 0 1) (list 2)))
        (data (make-instance 'gp-data :n-vars 3)))

    (loop for (variable . parent-scores) in scores-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-scores (acons variable hash all-scores)))
    
    (loop for (variable . parent-scores) in lik-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-lik (acons variable hash all-lik)))

    (setf initial-node (make-initial-search-node 3)
          compute-func (lambda (v p) (format t "computing ~a <- ~a~%" v p) (gethash p (cdr (assoc v all-scores))))
          lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
          out (abc-search initial-node data compute-func lik-compute-func k-groups))

    (append out (list data all-lik))))
