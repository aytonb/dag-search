(uiop:define-package #:dag-search/score/likelihood-from-score
    (:use #:cl
          #:dag-search/score/penalty)
  (:export #:likelihood-from-score)
  (:documentation "Get likelihoods from all scores."))

(in-package #:dag-search/score/likelihood-from-score)


(defun likelihood-from-score (scores-alist dag-score)
  (let ((lik-alist nil)
        var-liks)

    (loop for (var . var-scores) in scores-alist do
      (setf var-liks nil)
      (loop for (parents . score) in var-scores do
        (setf var-liks (acons parents
                              (- (penalty dag-score var parents)
                                 score)
                              var-liks)))
      (setf lik-alist (acons var var-liks lik-alist)))

    lik-alist))
