(uiop:define-package #:dag-search/sparse-discrete-score
    (:use #:cl
          #:dag-search/discrete-score
          #:dag-search/calculate-discrete-scores)
  (:export #:best-score
           #:make-sparse-discrete-scores)
  (:documentation "Computation of sparse discrete score structures."))

(in-package #:dag-search/sparse-discrete-score)


;; TODO: All these vector operations seem much less efficient than a subsetp
;; operation. Is this really the best way to do it?
(defclass variable-sparse-discrete-scores ()
  ((parents-vec
    :initform nil
    :accessor parents-vec
    :documentation "Vector of parent sets for a variable.")
   (scores-vec
    :initform nil
    :accessor scores-vec
    :documentation "Vector of scores for a variable.")
   (parent-binary-vec
    :initform nil
    :accessor parent-binary-vec
    :documentation "Vector of length variables. Each element is a vector that is 1 if the variable is in the corresponding parent set."))
  (:documentation "Collection of discrete score structures for fast querying."))


(defmethod initialize-instance :after ((vsds variable-sparse-discrete-scores) &key scores variable n-vars)
  "Initialized with scores as an a-list from parents to scores."
  (let ((sorted-scores (sort scores #'score-cardinality-sort)))
    ;; Initialize empty vectors.
    (setf (parents-vec vsds) (make-array 0 :adjustable t :fill-pointer 0)
          (scores-vec vsds) (make-array 0 :adjustable t :fill-pointer 0)
          (parent-binary-vec vsds) (make-array (list n-vars 0) :adjustable t))

    ;; Loop through sorted scores.
    (loop for (parents . score) in sorted-scores
          for best-sets-dim = (fill-pointer (scores-vec vsds)) do
            (block add-parents
              (loop for subset-index below best-sets-dim do
                ;; If parents contains subset-index with lower scores, skip it
                (when (and (parents-subset vsds variable n-vars parents subset-index)
                           (< (aref (scores-vec vsds) subset-index) score))
                  (return-from add-parents)))
              
              ;; Otherwise, add the new set in.
              (vector-push-extend parents (parents-vec vsds))
              (vector-push-extend score (scores-vec vsds))
              (adjust-array (parent-binary-vec vsds) (list n-vars (1+ best-sets-dim)))
              (loop for var below n-vars do
                (unless (equal var variable)
                  (if (member var parents)
                      (setf (aref (parent-binary-vec vsds) var best-sets-dim) t)
                      (setf (aref (parent-binary-vec vsds) var best-sets-dim) nil))))))))


(defun score-cardinality-sort (element-1 element-2)
  "Returns t if element-1 comes before element-2. Each element is (parents . score). Element-1 comes first if it has lower score. If scores are the same, parents has lower cardinality."
  (destructuring-bind (parents-1 . score-1) element-1
    (destructuring-bind (parents-2 . score-2) element-2
      (cond
        ((equal score-1 score-2)
         (if (<= (list-length parents-1)
                 (list-length parents-2))
             t
             nil))
        ((< score-1 score-2)
         t)
        ((> score-1 score-2)
         nil)))))


(defgeneric parents-subset (vsds variable n-vars parents subset-index)
  (:documentation "Returns t iff the set parents at subset-index are a subset of parents.")
  (:method ((vsds variable-sparse-discrete-scores) variable n-vars parents subset-index)
    (loop for var below n-vars do
      ;; All vars not variable and not in parents
      (unless (or (equal var variable)
                  (member var parents))
        ;; If parents is t, return nil
        (when (aref (parent-binary-vec vsds) var subset-index)
          (return-from parents-subset nil))))
    t))


(defclass sparse-discrete-scores ()
  ((n-vars
    :initform nil
    :accessor n-sds-vars
    :documentation "The number of variables in the problem.")
   (variable-scores
    :initform nil
    :accessor variable-scores
    :documentation "A list from variable to variable-sparse-discrete-scores instance."))
  (:documentation "Collection of discrete score structures for all variables."))


(defmethod initialize-instance :after ((sds sparse-discrete-scores) &key all-scores)
  "Initialized with all scores as an a-list from variables to a-lists of parents to scores."
  (let ((n-vars (list-length all-scores)))
    (setf (n-sds-vars sds) n-vars)
    (loop for (variable . scores) in all-scores do
      (setf (variable-scores sds)
            (acons variable
                   (make-instance 'variable-sparse-discrete-scores
                                  :n-vars n-vars
                                  :variable variable
                                  :scores scores)
                   (variable-scores sds))))))


(defgeneric best-score (score-structure variable parents)
  (:documentation "Returns (best-score parent-set) from among subsets of parents.")
  (:method ((sds sparse-discrete-scores) variable parents)
    (let ((vsds (cdr (assoc variable (variable-scores sds)))))
      ;; There should be guaranteed to be a subset among
      ;; variable-sparse-discrete-scores
      (loop for subset-index from 0 do
        (when (parents-subset vsds variable (n-sds-vars sds) parents subset-index)
          (return-from best-score (list (aref (scores-vec vsds) subset-index)
                                        (aref (parents-vec vsds) subset-index))))))))


(defun make-sparse-discrete-scores (path &key (score-function :aic))
  "Returns a sparse discrete score sitrcture made from the data at path."
  (let* ((data (make-instance 'discrete-data :path path))
         (all-scores (calculate-all-scores data :score-function score-function)))
    (make-instance 'sparse-discrete-scores :all-scores all-scores)))
