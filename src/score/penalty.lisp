(uiop:define-package #:dag-search/score/penalty
    (:use #:cl
          #:dag-search/score/score)
  (:export #:penalty)
  (:documentation "Compute the complexity penalty for parent sets."))

(in-package #:dag-search/score/penalty)


(defgeneric penalty (score variable parents)
  (:documentation "Gives the penalty to be subtracted from likelihood.")

  (:method ((score custom-score) variable parents)
    (funcall (penalty-fn score) variable parents))

  (:method ((score aic-score) variable parents)
    (n-params score variable parents))

  (:method ((score bic-score) variable parents)
    (/ (* (n-params score variable parents)
          (log (n-data-points score)))
       2)))


(defgeneric n-params (score variable parents)
  (:documentation "The number of parameters to get likelihood from parents.")

  (:method ((score gp-score) variable parents)
    ;; 2 parameters per parent
    ;; TODO: Generalize for different kernels
    ;;(* 2 (list-length parents))
    (list-length parents)
    )

  (:method ((score discrete-score) variable parents)
    (let ((n-params (1- (aref (domains score) variable))))
      (loop for parent in parents do
        (setf n-params (* n-params (aref (domains score) parent))))
      n-params)))
