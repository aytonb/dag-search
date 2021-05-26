(uiop:define-package #:dag-search/score/bounds
    (:use #:cl
          ;;#:dag-search/discrete-score
          #:dag-search/score/calculate
          #:dag-search/score/penalty)
  (:export #:bound
           ;#:discrete-bound
           ;#:discrete-bound-with-estimate
           ;#:discrete-upper-and-lower-bounds
           )
  (:documentation "Computation of lower bounds."))

(in-package #:dag-search/score/bounds)


;; (defun gp-bound (variable possible-parents scores &key (params-per-var 2))
;;   "Computes a lower bound on the best score for a variable and set of parents.

;; scores is an a-list of variable -> hash of parents -> score."
;;   (let ((lower-bound +inf+)
;;         (lower-bound-set nil)
;;         lower-bound-exact
;;         (inexact-sets nil)
;;         penalty
;;         bound)

;;     (unless possible-parents
;;       (return-from gp-bound (list 0 nil t)))

;;     ;; Identify which sets in possible-parents have exact values and which do not.
;;     (loop for i from 1 upto (list-length possible-parents) do
;;       (loop for parents in (subsets-of-size possible-parents i)
;;             for score = (gethash parents (cdr (assoc variable scores)))
;;             ;; We produce a bound if there is a score, and add to inexact-sets
;;             ;;; if there is not.
;;             do (if score
;;                    (when (< score lower-bound)
;;                      (setf lower-bound score
;;                            lower-bound-set parents
;;                            lower-bound-exact t))
;;                    (setf inexact-sets (list* parents inexact-sets)))))
    
;;     ;; Compute bounds for the inexact sets I
;;     ;; For each I, find the largest bound from among exact supersets/
;;     (loop for inexact-set in inexact-sets
;;           for inexact-set-size = (list-length inexact-set)
;;           for greatest-bound = +-inf+ do
;;             ;; Look for supersets of inexact-set
;;             (loop for parents being the hash-key
;;                     using (hash-value score) of (cdr (assoc variable scores))
;;                   for parents-size = (list-length parents) do
;;                     (when (subsetp inexact-set parents)
;;                       (setf penalty (* params-per-var
;;                                        (- inexact-set-size parents-size))
;;                             bound (+ penalty score))
;;                       (when (> bound greatest-bound)
;;                         (setf greatest-bound bound))))
;;             ;; If greatest-bound is lower than lower-bound it is the new best
;;             (when (< greatest-bound lower-bound)
;;               (setf lower-bound greatest-bound
;;                     lower-bound-set inexact-set
;;                     lower-bound-exact nil)))

;;     (list lower-bound lower-bound-set lower-bound-exact)))


;; (defun test-gp-bound ()
;;   (let ((hash (make-hash-table :test #'equalp))
;;         scores
;;         (possible-parents (list 2)))
;;     (setf (gethash (list 1 2) hash) 10
;;           (gethash (list 1 3) hash) 9
;;           (gethash (list 2 3 4) hash) 16
;;           scores (acons 0 hash nil))

;;     ;; Best result should be (12 (2))
;;     (format t "~a~%~%" (gp-bound 0 possible-parents scores))

;;     ;; Best result should be (13 (2))
;;     (setf (gethash (list 2 3) hash) 15)
;;     (format t "~a~%~%" (gp-bound 0 possible-parents scores))

;;     ;; Best result should be (8 (1))
;;     (format t "~a~%~%" (gp-bound 0 (list 1 2) scores))

;;     ;; Best result should be (+-inf+ (1 2 3))
;;     (format t "~a~%~%" (gp-bound 0 (list 1 2 3) scores))

;;     ;; Best result should be (8.5 (1) t)
;;     (setf (gethash (list 1) hash) 8.5)
;;     (format t "~a~%~%" (gp-bound 0 (list 1) scores))))


(defun bound (variable possible-parents scores dag-score
              &optional (allowable-fn nil))
  "Computes a lower bound on the best score for a variable and set of parents.

scores is an a-list of variable -> hash of parents -> score."
  (let ((lower-bound most-positive-double-float)
        (lower-bound-set nil)
        lower-bound-exact
        lower-bound-lik-parents
        greatest-bound-lik-parents
        (inexact-sets nil)
        penalty
        bound)

    ;; Identify which sets in possible-parents have exact values and which do not.
    (loop for i from 0 upto (list-length possible-parents) do
      (loop for parents in (subsets-of-size possible-parents i)
            for score = (gethash parents (cdr (assoc variable scores)))
            ;; We produce a bound if there is a score, and add to inexact-sets
            ;;; if there is not.
            do (if score
                   (when (< score lower-bound)
                     (setf lower-bound score
                           lower-bound-set parents
                           lower-bound-lik-parents parents
                           lower-bound-exact t))
                   (when (or (not allowable-fn)
                             (funcall allowable-fn variable parents))
                     (setf inexact-sets (list* parents inexact-sets))))))
    
    ;; Compute bounds for the inexact sets I
    ;; For each I, find the largest bound from among exact supersets/
    (loop for inexact-set in inexact-sets
          for greatest-bound = most-negative-double-float do
            ;; Look for supersets of inexact-set
            (loop for parents being the hash-key
                    using (hash-value score) of (cdr (assoc variable scores)) do
                      (when (subsetp inexact-set parents)
                        (setf penalty (- (penalty dag-score variable inexact-set)
                                         (penalty dag-score variable parents))
                              bound (+ penalty score))
                        (when (> bound greatest-bound)
                          (setf greatest-bound bound
                                greatest-bound-lik-parents parents))))
            ;; If greatest-bound is lower than lower-bound it is the new best
            (when (< greatest-bound lower-bound)
              (setf lower-bound greatest-bound
                    lower-bound-set inexact-set
                    lower-bound-lik-parents greatest-bound-lik-parents
                    lower-bound-exact nil)))
    
    (list lower-bound lower-bound-set lower-bound-exact lower-bound-lik-parents)))


;; (defun test-discrete-bound ()
;;   (let ((data (make-instance 'discrete-data :path (asdf:system-relative-pathname :dag-search "./src/four-variable-linear-example.txt")))
;;         (hash (make-hash-table :test #'equalp))
;;         scores
;;         (possible-parents (list 2)))
;;     (setf (gethash (list 1 2) hash) 26
;;           (gethash (list 1 3) hash) 25
;;           ;(gethash (list 1 2 3) hash) 66
;;           scores (acons 0 hash nil))

;;     (format t "~a~%" (aic-penalty data 0 nil))
;;     (format t "~a~%" (aic-penalty data 0 (list 2)))
;;     (format t "~a~%" (aic-penalty data 0 (list 1 2)))
;;     (format t "~a~%" (aic-penalty data 0 (list 1 3)))
;;     (format t "~a~%~%" (aic-penalty data 0 (list 2 3)))

;;     ;; Best result should be (10 nil nil (1 2))
;;     (format t "~a~%~%" (discrete-bound 0 possible-parents scores data))

;;     ;; Best result should be (16 nil nil (2 3))
;;     (setf (gethash (list 2 3) hash) 32)
;;     (format t "~a~%~%" (discrete-bound 0 possible-parents scores data))

;;     ;; Best result should be (14 (1) nil (1 2))
;;     (format t "~a~%~%" (discrete-bound 0 (list 1 2) scores data))

;;     ;; Best result should be (+-inf+ (1 2 3) nil nil)
;;     (format t "~a~%~%" (discrete-bound 0 (list 1 2 3) scores data))

;;     ;; Best result should be (16 (1) t (1))
;;     (setf (gethash (list 1) hash) 16)
;;     (format t "~a~%~%" (discrete-bound 0 (list 1) scores data))

;;     (setf (gethash nil hash) 22)
;;     ;; Best result should be (20 (2) nil (2 3))
;;     (format t "~a~%" (discrete-bound 0 (list 2) scores data))
;;     ;; Best result should be (26 (2) nil nil)
;;     (format t "~a~%~%" (discrete-upper-bound 0 (list 2) scores data))

;;     ;; Best result should be (16 (1) t (1))
;;     (format t "~a~%" (discrete-bound 0 (list 1 2) scores data))
;;     ;; Best result should be (16 (1) t (1))
;;     (format t "~a~%~%" (discrete-upper-bound 0 (list 1) scores data))))


;; (defun discrete-bound-with-estimate (variable possible-parents scores data-set estimated-parents estimated-likelihood)
;;   "Computes a lower bound on the best score for a variable and set of parents.

;; scores is an a-list of variable -> hash of parents -> score."
;;   (let ((lower-bound +inf+)
;;         (lower-bound-set nil)
;;         lower-bound-exact
;;         lower-bound-lik-parents
;;         greatest-bound-lik-parents
;;         (inexact-sets nil)
;;         penalty
;;         bound
;;         (estimated-score (+ estimated-likelihood (aic-penalty data-set variable estimated-parents))))

;;     ;; Identify which sets in possible-parents have exact values and which do not.
;;     (loop for i from 0 upto (list-length possible-parents) do
;;       (loop for parents in (subsets-of-size possible-parents i)
;;             for score = (gethash parents (cdr (assoc variable scores)))
;;             ;; We produce a bound if there is a score, and add to inexact-sets
;;             ;;; if there is not.
;;             do (if score
;;                    (when (< score lower-bound)
;;                      (setf lower-bound score
;;                            lower-bound-set parents
;;                            lower-bound-lik-parents parents
;;                            lower-bound-exact t))
;;                    (setf inexact-sets (list* parents inexact-sets)))
               
;;                ;; Use the estimate if the score does not already exist
;;                (when (and (equalp parents estimated-parents)
;;                           (not score)
;;                           (< estimated-score lower-bound))
;;                  (setf lower-bound estimated-score
;;                        lower-bound-set parents
;;                        lower-bound-lik-parents parents
;;                        lower-bound-exact t))))
    
;;     ;; Compute bounds for the inexact sets I
;;     ;; For each I, find the largest bound from among exact supersets/
;;     (loop for inexact-set in inexact-sets
;;           for greatest-bound = +-inf+ do
;;             ;; When estimated-parents are a superset but its true value doesn't exist
;;             (when (and (subsetp inexact-set estimated-parents)
;;                        (not (gethash inexact-set (cdr (assoc variable scores)))))
;;               (setf bound (+ estimated-likelihood (aic-penalty data-set variable inexact-set)))
;;               (when (> bound greatest-bound)
;;                 (setf greatest-bound bound
;;                       greatest-bound-lik-parents estimated-parents)))
            
;;             ;; Look for supersets of inexact-set
;;             (loop for parents being the hash-key
;;                     using (hash-value score) of (cdr (assoc variable scores)) do
;;                       (when (subsetp inexact-set parents)
;;                         (setf penalty (- (aic-penalty data-set variable inexact-set)
;;                                          (aic-penalty data-set variable parents))
;;                               bound (+ penalty score))
;;                         (when (> bound greatest-bound)
;;                           (setf greatest-bound bound
;;                                 greatest-bound-lik-parents parents))))
;;             ;; If greatest-bound is lower than lower-bound it is the new best
;;             (when (< greatest-bound lower-bound)
;;               (setf lower-bound greatest-bound
;;                     lower-bound-set inexact-set
;;                     lower-bound-lik-parents greatest-bound-lik-parents
;;                     lower-bound-exact nil)))
    
;;     (list lower-bound lower-bound-set lower-bound-exact lower-bound-lik-parents)))


;; (defun discrete-upper-bound (variable candidate-parents scores data-set)
;;   "Computes an upper bound on the best score for a variable and set of parents.
;; The bound comes from subsets of the parents.

;; scores is an a-list of variable -> hash of parents -> score."
;;   (let ((upper-bound +inf+)
;;         upper-bound-exact
;;         upper-bound-lik-parents
;;         exact-score
;;         penalty
;;         bound)

;;     ;; Return the score if candidate-parents is exact.
;;     (setf exact-score (gethash candidate-parents (cdr (assoc variable scores))))
;;     (when exact-score
;;       (return-from discrete-upper-bound (list exact-score candidate-parents t candidate-parents)))

;;     ;; For all subsets of candidate-parents, use the maximum value
;;     (loop for i below (list-length candidate-parents) do
;;       (loop for parents in (subsets-of-size candidate-parents i)
;;             for score = (gethash parents (cdr (assoc variable scores)))
;;             do (when score
;;                  (setf penalty (- (aic-penalty data-set variable candidate-parents)
;;                                   (aic-penalty data-set variable parents))
;;                        bound (+ penalty score))
                 
;;                  (when (< bound upper-bound)
;;                    (setf upper-bound bound
;;                          upper-bound-lik-parents parents
;;                          upper-bound-exact nil)))))

;;     (list upper-bound candidate-parents upper-bound-exact upper-bound-lik-parents)))


;; (defun discrete-upper-and-lower-bounds (variable possible-parents scores data-set)
;;   "Computes lower and upper bounds for the scores of all sets in possible-parents.

;; For compatability reasons, outputs a sorted list of (lb ub parents)."
;;   (let ((bounds nil)
;;         lb
;;         ub)
;;     (loop for i from 0 upto (list-length possible-parents) do
;;       (loop for parents in (subsets-of-size possible-parents i) do
;;         (setf lb (first (discrete-bound variable parents scores data-set))
;;               ub (first (discrete-upper-bound variable parents scores data-set))
;;               bounds (list* (list lb ub parents) bounds))))
;;     (sort bounds #'< :key #'car)))
