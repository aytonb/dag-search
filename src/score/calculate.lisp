(uiop:define-package #:dag-search/score/calculate
    (:use #:cl
          #:dag-search/score/score
          #:dag-search/score/penalty)
  (:export #:calculate-all-scores
           #:calculate-all-likelihoods
           #:subsets-of-size)
  (:documentation "Computation of all necessary discrete scores."))

(in-package #:dag-search/score/calculate)


(defgeneric calculate-all-scores (dag-score &key no-prune)
  (:documentation "Computes all scores for a given data set. Returns an a-list of variable -> hash of parents -> score.")
  (:method ((dag-score dag-score) &key (no-prune nil))
    (declare (ignore no-prune))
    (let ((all-scores nil)
          (all-times nil))

      (loop for variable below (n-vars dag-score) do
        (multiple-value-bind (scores times)
            (calculate-all-variable-scores dag-score variable)
          (setf all-scores (acons variable scores all-scores)
                all-times (acons variable times all-times))))

      ;; (ecase score-function
      ;;   (:aic
      ;;    (if no-prune
      ;;        (loop for variable below (n-vars data-set) do
      ;;          (setf all-scores (acons variable (calculate-all-variable-aic-scores data-set variable) all-scores)))
      ;;        (loop for variable below (n-vars data-set) do
      ;;          (setf all-scores (acons variable (calculate-variable-aic-scores data-set variable) all-scores)))))
      ;;   (:gp-aic
      ;;    (if no-prune
      ;;        (loop for variable below (n-vars data-set) do
      ;;          (multiple-value-bind (scores times)
      ;;              (calculate-all-variable-gp-aic-scores data-set variable)
      ;;            (setf all-scores (acons variable scores all-scores)
      ;;                  all-times (acons variable times all-times))))
      ;;        (loop for variable below (n-vars data-set) do
      ;;          (setf all-scores (acons variable (calculate-variable-gp-aic-scores data-set variable) all-scores)))))
      ;;   (:log-likelihood
      ;;    (loop for variable below (n-vars data-set) do
      ;;      (setf all-scores (acons variable (calculate-all-variable-likelihood-scores data-set variable) all-scores)))))
      
      (values all-scores all-times))))


(defgeneric calculate-all-likelihoods (dag-score &key no-prune)
  (:documentation "Computes all scores for a given data set. Returns an a-list of variable -> hash of parents -> score.")
  (:method ((dag-score dag-score) &key (no-prune nil))
    (declare (ignore no-prune))
    (let ((all-likelihoods nil)
          likelihoods)

      (loop for variable below (n-vars dag-score) do
        (setf likelihoods
              (calculate-all-variable-likelihoods dag-score variable))
        (setf all-likelihoods (acons variable likelihoods all-likelihoods)))
      
      all-likelihoods)))


;; (defgeneric calculate-variable-aic-scores (data-set variable)
;;   (:documentation "Computes all aic scores for a given variable. Returns an a-list of parents -> score.")
;;   (:method ((data-set dag-data) variable)
;;     (let ((scores nil)
;;           (N (n-data-points data-set))
;;           (max-sets nil) ;; Any supersets of these is not optimal 
;;           possible-parents
;;           (ri (aref (domains data-set) variable))
;;           rpi
;;           set-size-threshold
;;           prune)

;;       ;; Generate a list of possible parents.
;;       (setf possible-parents (loop for var below (n-vars data-set)
;;                                    unless (equal var variable)
;;                                      collect var))

;;       ;; Set size threshold
;;       (setf set-size-threshold (* N (/ (log ri) (1- ri))))

;;       ;; There can be at most ceil(log2(N)) parents.
;;       (loop for k upto (min (ceiling (/ (log N) (log 2)))
;;                             (1- (n-vars data-set)))
;;             do (loop for parents in (subsets-of-size possible-parents k)
;;                      for penalty = (aic-penalty data-set variable parents) do
;;                        (setf prune nil)
                       
;;                        ;; Check if a superset of any max-sets, if so, exclude
;;                        (loop for max-set in max-sets do
;;                          (when (subsetp max-set parents)
;;                            (setf prune t)
;;                            (return)))
                       
;;                        (unless prune
;;                          ;; Compute rpi.
;;                          (setf rpi 1)
;;                          (loop for parent in parents do
;;                            (setf rpi (* rpi (aref (domains data-set) parent))))
;;                          ;; If rpi > set-size-threshold, no superset can be optimal.
;;                          (when (> rpi set-size-threshold)
;;                            (setf max-sets (list* parents max-sets)))
                         
;;                          ;; Check score relative to subsets
;;                          (unless (equal k 0)
;;                            ;; Not optimal if penalty - score(parents\Y) > 0.
;;                            (loop for exclude-parent in parents
;;                                  for one-excluded = (remove exclude-parent parents)
;;                                  for excluded-score = (cdr (assoc one-excluded scores :test #'equalp))
;;                                  do (when (or (not excluded-score)
;;                                               (> (- penalty excluded-score) 0))
;;                                       (setf prune t)
;;                                       (return)))))

                       
;;                        (unless prune
;;                          (setf scores
;;                                (acons parents
;;                                       (aic data-set variable parents) scores)))))

;;       scores)))


;; (defgeneric calculate-all-variable-aic-scores (data-set variable)
;;   (:documentation "Computes all aic scores for a given variable, including ones that can be excluded. Returns an a-list of parents -> score.")
;;   (:method ((data-set discrete-data) variable)
;;     (let ((scores nil) 
;;           possible-parents)

;;       ;; Generate a list of possible parents.
;;       (setf possible-parents (loop for var below (n-vars data-set)
;;                                    unless (equal var variable)
;;                                      collect var))

;;       (loop for k upto (1- (n-vars data-set))
;;             do (loop for parents in (subsets-of-size possible-parents k)
;;                      for penalty = (aic-penalty data-set variable parents) do
;;                        (setf scores
;;                              (acons parents
;;                                     (aic data-set variable parents) scores))))

;;       scores)))


(defgeneric calculate-all-variable-scores (dag-score variable)
  (:documentation "Computes all aic scores for a given variable, including ones that can be excluded. Returns an a-list of parents -> score.")
  (:method ((dag-score dag-score) variable)
    (let ((scores nil)
          (times nil)
          score
          possible-parents
          start-time
          end-time)

      ;; Generate a list of possible parents.
      (setf possible-parents (loop for var below (n-vars dag-score)
                                   unless (equal var variable)
                                     collect var))

      (loop for k upto (1- (n-vars dag-score)) do
        (loop for parents in (subsets-of-size possible-parents k) do
          (setf start-time (get-internal-real-time)
                score (- (penalty dag-score variable parents)
                         (max-log-likelihood dag-score
                                             variable
                                             parents))
                end-time (get-internal-real-time))
              (setf scores (acons parents
                                  score
                                  scores)
                    times (acons parents
                                 (/ (- end-time start-time)
                                    internal-time-units-per-second)
                                 times))))
      
      (values scores times))))


(defgeneric calculate-all-variable-likelihoods (dag-score variable)
  (:documentation "Computes all likelihoods for a given variable, including ones that can be excluded. Returns an a-list of parents -> score.")
  (:method ((dag-score dag-score) variable)
    (let ((scores nil)
          possible-parents)

      ;; Generate a list of possible parents.
      (setf possible-parents (loop for var below (n-vars dag-score)
                                   unless (equal var variable)
                                     collect var))

      (loop for k upto (1- (n-vars dag-score))
            do (loop for parents in (subsets-of-size possible-parents k) do
              (setf scores
                    (acons parents
                           (max-log-likelihood dag-score variable parents)
                           scores))))

      scores)))


(defun subsets-of-size (parents size &key (size-reached 0) (subsets (list nil)))
  "Returns all subsets of parents of the specified size."
  (when (equal size size-reached)
    (return-from subsets-of-size subsets))
  (let ((new-subsets nil)
        (new-size-reached (1+ size-reached))
        (n-parents (list-length parents)))
    (loop for subset in subsets
          for last-ele = (if subset
                             (car (last subset))
                             nil)
          for last-position = (if last-ele
                                  (position last-ele parents)
                                  -1)
          do (loop for i from (1+ last-position) below n-parents
                   for parent = (nth i parents) do
                     (when (and (or (not last-ele)
                                    (> parent last-ele))
                                (> (- n-parents i) (- size new-size-reached)))
                       (setf new-subsets (list* (append subset (list parent))
                                                new-subsets)))))
    (subsets-of-size parents
                     size
                     :size-reached new-size-reached
                     :subsets new-subsets)))


;; (defgeneric calculate-variable-gp-aic-scores (data-set variable)
;;   (:documentation "Computes all gp-aic scores for a given variable. Returns an a-list of parents -> score.")
;;   (:method ((data-set gp-data) variable)
;;     (let ((scores nil)
;;           possible-parents
;;           prune)

;;       ;; Generate a list of possible parents.
;;       (setf possible-parents (loop for var below (n-vars data-set)
;;                                    unless (equal var variable)
;;                                      collect var))

;;       ;; There is no limit on parents
;;       (loop for k upto (1- (n-vars data-set))
;;             do (loop for parents in (subsets-of-size possible-parents k)
;;                      for penalty = (* 2 k) do
;;                        (setf prune nil)
     
;;                        ;; Check score relative to subsets
;;                        (unless (equal k 0)
;;                          ;; Not optimal if penalty - score(parents\Y) > 0.
;;                          (loop for exclude-parent in parents
;;                                for one-excluded = (remove exclude-parent parents)
;;                                for excluded-score = (cdr (assoc one-excluded scores :test #'equalp))
;;                                do (when (> (- penalty excluded-score) 0)
;;                                     (setf prune t)
;;                                     (return))))
                     
;;                        (unless prune
;;                          (setf scores
;;                                (acons parents
;;                                       (gp-aic data-set variable parents) scores)))))
      
;;       scores)))


;; (defgeneric calculate-all-variable-gp-aic-scores (data-set variable)
;;   (:documentation "Computes all gp-aic scores for a given variable. Returns an a-list of parents -> score.")
;;   (:method ((data-set gp-data) variable)
;;     (let ((scores nil)
;;           (times nil)
;;           possible-parents)

;;       ;; Generate a list of possible parents.
;;       (setf possible-parents (loop for var below (n-vars data-set)
;;                                    unless (equal var variable)
;;                                      collect var))

;;       ;; There is no limit on parents
;;       (loop for k upto (1- (n-vars data-set))
;;             do (loop for parents in (subsets-of-size possible-parents k)
;;                      do (multiple-value-bind (score time)
;;                             (gp-aic data-set variable parents)
;;                           (setf scores (acons parents score scores)
;;                                 times (acons parents time times)))))
      
;;       (values scores times))))
