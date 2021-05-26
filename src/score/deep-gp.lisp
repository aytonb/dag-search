(uiop:define-package #:dag-search/score/deep-gp
    (:use #:cl
          #:gaussian-process/heterogeneous
          #:dag-search/discrete-score)
  (:export )
  (:documentation "Score function computation for deep GP models."))

(in-package #:dag-search/score/deep-gp)


(defun make-dimension-distance-fn (d)
  ;; Makes a function to measure squared distance along the dimension d 
  (lambda (loc-1 loc-2)
    (expt (- (nth d loc-1)
             (nth d loc-2))
          2)))


(defmethod max-log-likelihood ((data-set gp-data) variable parents)
  ;; Get likelihood where data is a Gaussian Process of its parents
  (if (equal parents nil)
      ;; If no parents, the data is assumed independent Gaussian
      (let ((var-estimate 0)
            (log-likelihood 0)
            time-1
            time-2)
        (setf time-1 (get-internal-real-time))
        (loop for dat in (data data-set)
              do (incf var-estimate (expt (aref dat variable) 2)))
        (setf var-estimate (/ var-estimate (n-data-points data-set)))
        (loop for dat in (data data-set)
              do (incf log-likelihood (- (* 0.5 (+ (/ (expt (aref dat variable) 2)
                                                      var-estimate)
                                                   (log var-estimate))))))
        (setf time-2 (get-internal-real-time))
        (values log-likelihood (/ (coerce (- time-2 time-1) 'float)
                                  internal-time-units-per-second)))

      ;; If there are parents, construct and train a GP
      (let* ((adjacency (make-array '(1 1)
                                    :element-type 'integer
                                    :initial-contents '((0))))
             (n-parents (list-length parents))
             (kerns (loop for i below n-parents
                          collect (make-instance 'rbf-kernel
                                                 :dist-index i)))
             (dist-fns (loop for i below n-parents
                             collect (make-dimension-distance-fn i)))
             (gp (make-instance 'heterogeneous-gp
                                :input-dim n-parents
                                :output-dim 1
                                :n-latent (list ;(n-data-points data-set)
                                           ;;5
                                           10
                                           )
                                :outputs (list (make-instance 'gaussian-output
                                                              :noise-var (noise-var data-set)))
                                :kernels (list (make-instance 'sum-kernel
                                                              :constituents kerns))
                                :dist-fns dist-fns
                                :adjacency adjacency))
             log-likelihood
             time-1
             time-2)

        (setf time-1 (get-internal-real-time))

        (loop for dat in (data data-set)
              do (add-measurement gp
                                  (loop for par in parents
                                        collect (aref dat par))
                                  0
                                  (aref dat variable)))
        
        (make-observed-distance-matrices gp :use-abs t)
        (initialize-latent-locations gp)
        (make-unobserved-distance-matrices gp :use-abs t)
        (initialize-q-cholesky gp)

        (setf log-likelihood (- (train gp)))
        (setf time-2 (get-internal-real-time))

        (values log-likelihood (/ (coerce (- time-2 time-1) 'float)
                                  internal-time-units-per-second)))))
