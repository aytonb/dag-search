(uiop:define-package #:dag-search/search-node/search-node
    (:use #:cl)
  (:export #:search-node
           #:make-initial-search-node
           #:g
           #:h
           #:assigned
           #:assigned-ordering
           #:possible-parent-sets
           #:exact-p
           #:edges)
  (:documentation "Search node definition."))

(in-package #:dag-search/search-node/search-node)


(defclass search-node ()
  ((edges
    :initarg :edges
    :initform nil
    :accessor edges
    :documentation "An a-list from a child to a list of parents.")
   (assigned
    :initarg :assigned
    :initform nil
    :accessor assigned
    :documentation "A list of variables which have their parents assigned.")
   (assigned-ordering
    :initarg :assigned-ordering
    :initform nil
    :accessor assigned-ordering
    :documentation "A list of variables in the order they are added to the DAG.")
   (possible-parent-sets
    :initarg :possible-parent-sets
    :initform nil
    :accessor possible-parent-sets
    :documentation "A vector of length number of variables, giving the possible-parent sets of each variable.")
   (exact-p
    :initarg :exact-p
    :initform nil
    :accessor exact-p
    :documentation "A vector of length number of variables, elements are true if the assigned variable is exact.")
   (g
    :initarg :g
    :initform 0
    :accessor g
    :documentation "Cost so far of a node.")
   (h
    :initarg :h
    :initform 0
    :accessor h
    :documentation "Heuristic cost to go for a node.")))


(defun make-initial-search-node (n-variables)
  (make-instance 'search-node
                 :possible-parent-sets (make-array n-variables :initial-element nil)
                 :exact-p (make-array n-variables :initial-element nil)))

