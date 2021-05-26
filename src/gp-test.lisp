(uiop:define-package #:dag-search/gp-test
    (:use #:cl
          #:dag-search/search-node
          #:dag-search/discrete-score
          #:dag-search/calculate-discrete-scores
          #:dag-search/abc)
  (:export )
  (:documentation "Test of functionality for gps."))

(in-package #:dag-search/gp-test)


(defun andro-test-1 ()
  (let ((data (make-instance 'gp-data :n-vars 4))
        (scores-alist nil)
        (lik-alist nil)
        hash
        initial-node
        compute-func
        lik-compute-func
        out
        (k-groups (list (list 0) (list 1) (list 2) (list 3))))

    (setf hash (make-hash-table :test #'equalp)
          (gethash nil hash) -49.9841705649412
          (gethash (list 1) hash) -52.8570071977267
          (gethash (list 2) hash) -53.75706257269097
          (gethash (list 3) hash) -53.45778922569613
          (gethash (list 1 2) hash) -52.757919457327866
          (gethash (list 1 3) hash) -54.33534259223859
          (gethash (list 2 3) hash) -55.654052202049144
          (gethash (list 1 2 3) hash) -54.987235770573
          scores-alist (acons 0 hash scores-alist)
          lik-alist (acons 0 hash lik-alist))

    (setf hash (make-hash-table :test #'equalp)
          (gethash nil hash) -10.217655588035871
          (gethash (list 0) hash) -9.671431990932849
          (gethash (list 2) hash) -69.1755069027284
          (gethash (list 3) hash) -11.457350242369824
          (gethash (list 0 2) hash) -68.71761959489426
          (gethash (list 0 3) hash) -10.484195870044637
          (gethash (list 2 3) hash) -68.2301788792212
          (gethash (list 0 2 3) hash) -67.74526353591165
          scores-alist (acons 1 hash scores-alist)
          lik-alist (acons 1 hash lik-alist))

    (setf hash (make-hash-table :test #'equalp)
          (gethash nil hash) -7.407212956881665
          (gethash (list 0) hash) -12.04023964411251
          (gethash (list 1) hash) -68.50160612495438
          (gethash (list 3) hash) -11.679917443373569
          (gethash (list 0 1) hash) -68.40186019224949
          (gethash (list 0 3) hash) -12.586060609757595
          (gethash (list 1 3) hash) -67.5601600743581
          (gethash (list 0 1 3) hash) -67.41174012848091
          scores-alist (acons 2 hash scores-alist)
          lik-alist (acons 2 hash lik-alist))

    (setf hash (make-hash-table :test #'equalp)
          (gethash nil hash) -10.386495428597208
          (gethash (list 0) hash) -18.89724261274266
          (gethash (list 1) hash) -13.165294882155631
          (gethash (list 2) hash) -13.380760548448482
          (gethash (list 0 1) hash) -19.008667088119342
          (gethash (list 0 2) hash) -19.346280407298572
          (gethash (list 1 2) hash) -12.380903586178292
          (gethash (list 0 1 2) hash) -18.98110174103317
          scores-alist (acons 3 hash scores-alist)
          lik-alist (acons 3 hash lik-alist))

    (setf initial-node (make-initial-search-node 4)
          compute-func (lambda (v p) (gethash p (cdr (assoc v scores-alist))))
          lik-compute-func (lambda (v p) (gethash p (cdr (assoc v lik-alist))))
          out (abc-search initial-node data compute-func lik-compute-func k-groups))

    out))
