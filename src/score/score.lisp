(uiop:define-package #:dag-search/score/score
  (:use #:cl)
  (:export #:dag-score
           #:custom-score
           #:aic-score
           #:bic-score
           #:gp-score
           #:gp-aic-score
           #:gp-bic-score
           #:discrete-score
           #:discrete-aic-score
           #:discrete-bic-score

           #:n-vars
           #:n-data-points
           #:penalty-fn
           #:domains

           #:max-log-likelihood)
  (:documentation "Score object definitions."))

(in-package #:dag-search/score/score)


(defclass dag-score ()
  ((n-vars
    :accessor n-vars
    :initarg :n-vars
    :documentation "The number of variables in the problem.")
   (n-data-points
    :accessor n-data-points
    :initarg :n-data-points
    :documentation "The number of data points in the data set.")))


(defclass custom-score (dag-score)
  ((penalty-fn
    :accessor penalty-fn
    :initarg :penalty-fn
    :documentation "A function to be used to evaluate the penalty.")))


(defclass aic-score (dag-score)
  ())


(defclass bic-score (dag-score)
  ())


;; GP scores

(defclass gp-score (dag-score)
  ())


(defclass gp-aic-score (gp-score aic-score)
  ())


(defclass gp-bic-score (gp-score bic-score)
  ())


(defmethod initialize-instance :after ((gd gp-score) &key (path nil) (n nil))
  (when path
    (with-open-file (s path :direction :input)
      (let ((line (map 'vector
                       (lambda (str)
                         (with-input-from-string (var str)
                           (read var)))
                       (uiop:split-string (read-line s)))))
        (setf (n-vars gd) (array-dimension line 0)
              (n-data-points gd) 0)
        ;(setf (data gd) (list* line (data gd)))
        (loop for raw-line = (read-line s nil nil)
              for i from 2
              while raw-line do
                (setf line (map 'vector
                                (lambda (str)
                                  (with-input-from-string (var str)
                                    (read var)))
                                (uiop:split-string raw-line)))
                ;(setf (data gd) (list* line (data gd)))
                (incf (n-data-points gd))
                (when (and n
                           (equal i n))
                  (return)))))))


;; Discrete scores

(defclass discrete-score (dag-score)
  ((domains
    :initarg :domains
    :initform nil
    :accessor domains
    :documentation "A vector of the number of elements in domains of each variable.")
   (data
    :initarg :data
    :initform nil
    :accessor data
    :documentation "Data in the file as a list of arrays.")))


(defclass discrete-aic-score (discrete-score aic-score)
  ())


(defclass discrete-bic-score (discrete-score bic-score)
  ())


(defmethod initialize-instance :after ((dd discrete-score) &key (path nil))
  (when path
    (with-open-file (s path :direction :input)
      (let ((line (map 'vector #'parse-integer (uiop:split-string (read-line s)))))
        (setf (n-vars dd) (array-dimension line 0)
              (domains dd) (map 'vector #'1+ line))
        (setf (data dd) (list* line (data dd)))
        (loop for raw-line = (read-line s nil nil)
              while raw-line do
                (setf line (map 'vector #'parse-integer (uiop:split-string raw-line)))
                (loop for var across line
                      for i from 0
                      do (when (>= var (aref (domains dd) i))
                           (setf (aref (domains dd) i) (1+ var))))
                (setf (data dd) (list* line (data dd))))
        (setf (n-data-points dd) (list-length (data dd)))))))


;; (defclass gp-data (dag-data)
;;   ((noise-variance
;;     :initarg :noise-var
;;     :accessor noise-var
;;     :documentation "Variance of noise in the gp data.")))


(defgeneric get-all-parent-combinations (dag-score parents)
  (:documentation "Returns a list of all value combinations of parents in the order they are given. If parents is nil, returns (nil) for compatability reasons.")
  (:method ((dag-score discrete-score) parents)
    (let ((existing-sets (list nil)))
      (loop for parent in parents do
        (setf existing-sets (add-parent-combinations dag-score parent existing-sets)))
      existing-sets)))


(defgeneric add-parent-combinations (dag-score parent existing-sets)
  (:documentation "Adds all value combinations of parent to the sets given in existing-sets.")
  (:method ((dag-score discrete-score) parent existing-sets)
    (let ((domain-size (aref (domains dag-score) parent))
          (new-sets nil))
      (loop for existing-set in existing-sets do
        (loop for i below domain-size do
          (setf new-sets (list* (append existing-set (list i))
                                new-sets))))
      new-sets)))
  

(defgeneric max-log-likelihood (dag-score variable parents)
  (:documentation "Maximum log likelihood value of the variable given the parents")

  (:method ((dag-score discrete-score) variable parents)
    (let ((counts (make-hash-table :test #'equalp))
          (parent-combinations (get-all-parent-combinations dag-score parents))
          (domain-size (aref (domains dag-score) variable))
          (log-lik 0))
      ;; Make a hash table from parent sets to counts of each value
      (loop for parent-combination in parent-combinations do
        (setf (gethash parent-combination counts)
              (make-array domain-size
                          :initial-element 0
                          :adjustable nil
                          :fill-pointer nil
                          :displaced-to nil)))
      ;; Fill in the counts
      (loop for data in (data dag-score)
            for var-val = (aref data variable)
            ;; Get the list of parent values
            for par-vals = (loop for parent in parents
                                 collect (aref data parent))
            do (incf (aref (gethash par-vals counts) var-val)))
      ;; Now sum log-likelihood for each count vector
      (loop for par-vals being the hash-key
              using (hash-value var-counts) of counts
            for par-count = (reduce #'+ var-counts) do
              (loop for var-count across var-counts do
                (unless (equal var-count 0)
                  (incf log-lik (* var-count (log var-count)))))
              (unless (equal par-count 0)
                (incf log-lik (- (* par-count (log par-count))))))
      log-lik)))


;; (defgeneric aic-penalty (data-set variable parents)
;;   (:documentation "Computes the complexity penalty term for the Akaike information criterion.")
;;   (:method ((data-set discrete-data) variable parents)
;;     (let ((penalty (1- (aref (domains data-set) variable))))
;;       (loop for parent in parents do
;;         (setf penalty (* penalty (aref (domains data-set) parent))))
;;       penalty))
;;   ;; TODO: Check logic is all consistent
;;   (:method ((data-set gp-data) variable parents)
;;     (* 2 (list-length parents))))


;; (defgeneric aic (data-set variable parents)
;;   (:documentation "Computes the Akaike information criterion.")
;;   (:method ((data-set discrete-data) variable parents)
;;     ;(format t "here~%")
;;     (- (aic-penalty data-set variable parents)
;;        (max-log-likelihood data-set variable parents))))


;; (defgeneric gp-aic (data-set variable parents)
;;   (:documentation "Computes AIC with a gp-based penalty.")
;;   (:method ((data-set gp-data) variable parents)
;;     (multiple-value-bind (log-likelihood time)
;;         (max-log-likelihood data-set variable parents)
;;       (format t "var = ~a, parents = ~a, lik = ~a, score = ~a, time = ~a~%"
;;               variable parents log-likelihood (- (* 2 (list-length parents))
;;                                                  log-likelihood)
;;               time)
;;       (values (- (* 2 (list-length parents))
;;                  log-likelihood)
;;               time))))

