(uiop:define-package #:dag-search/score/consistent-score
    (:use #:cl
          #:dag-search/score/calculate
          #:dag-search/score/penalty
          )
  (:export #:make-scores-consistent)
  (:documentation "Utilities to ensure consistency of complete score sets."))

(in-package #:dag-search/score/consistent-score)


(defun make-scores-consistent (scores-alist dag-score)
  (let ((new-scores nil)
        new-variable-scores
        all-vars
        possible-parents)

    (setf all-vars (sort (loop for (var . var-scores) in scores-alist
                               collect var)
                         #'<))

    ;; Loop across variables
    (loop for (var . var-scores) in scores-alist do
      (setf new-variable-scores (acons nil (cdr (assoc nil var-scores)) nil)
            possible-parents (remove-if (lambda (x) (equal x var)) all-vars))
      (format t "possible-parents = ~a~%" possible-parents)
      ;; Iterate across each parent set
      (loop for k from 1 upto (list-length possible-parents) do
        (format t "k = ~a~%" k)
        (format t "possible-parents = ~a~%" possible-parents)
        (format t "subsets-of-size k = ~a~%" (subsets-of-size possible-parents k))
        (loop for parents in (subsets-of-size possible-parents k)
              ;; True measured negative likelihoood
              for lik = (- (cdr (assoc parents
                                       var-scores
                                       :test #'equalp))
                           (penalty dag-score var parents))
              ;; Iterate across possible subsets
              do (format t "parents are ~a~%" parents)
                 (loop for sub-k below (list-length parents) do
                   (loop for sub-parents in (subsets-of-size parents sub-k)
                         ;; Fixed subset negative likelihood
                         for sub-parents-lik = (- (cdr (assoc sub-parents
                                                              new-variable-scores
                                                              :test #'equalp))
                                                  (penalty dag-score var sub-parents))

                      ;; Negative likelihood must be less than the subsets,
                      ;; if not, adjust to the subsets
                      do (when (> lik sub-parents-lik)
                           (setf lik sub-parents-lik))))

                 ;; Write the new score for this parent set
                 (setf new-variable-scores
                       (acons parents
                              (+ (penalty dag-score var parents) lik)
                              new-variable-scores))))

      ;; Collect the new variable scores in new-scores
      (setf new-scores (acons var new-variable-scores new-scores)))

    new-scores))
