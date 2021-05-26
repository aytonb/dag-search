(uiop:define-package #:dag-search/score/prune-score
    (:use #:cl)
  (:export )
  (:documentation "Utilities to prune complete score a-lists, for testing purposes."))

(in-package #:dag-search/score/prune-score)


;; Actually for gp scores, we cannot prune by getting larger.

;; (defun prune-score-alist (scores-alist &key (score-function :gp-aic) (times-alist nil))
;;   (ecase score-function
;;     (:gp-aic
;;      (prune-score-alist-gp-aic scores-alist times-alist))))


;; (defun prune-score-alist-gp-aic (scores-alist times-alist)
;;   ;; Prunes superset when penalty > score(var|parents)
;;   )
