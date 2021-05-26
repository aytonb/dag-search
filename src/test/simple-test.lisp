(uiop:define-package #:dag-search/test/simple-test
    (:use #:cl
          ;#:dag-search/search-node
          ;#:dag-search/discrete-score
          ;#:dag-search/calculate-discrete-scores
          #:dag-search/score/all
          #:dag-search/search-node/all
          #:dag-search/abc/all)
  (:export )
  (:documentation "Test of functionality so far."))

(in-package #:dag-search/test/simple-test)


;; (defun get-four-variable-scores ()
;;   (let* ((data (make-instance 'discrete-data :path (asdf:system-relative-pathname :dag-search "./src/four-variable-linear-example.txt")))
;;          (scores-alist (calculate-all-scores data :score-function :aic))
;;          (scores nil)
;;          hash)
;;     (loop for (variable . parent-scores) in scores-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf scores (acons variable hash scores)))
;;     (list scores data)))


(defun breast-cancer-4-var-test ()
  (let* ((dag-score (make-instance 'discrete-aic-score
                                   :path (asdf:system-relative-pathname
                                          :dag-search
                                          "./data/breast-cancer-4-var.txt")))
         (scores-alist (calculate-all-scores dag-score :no-prune t))
         ;(pruned-scores-alist (calculate-all-scores data :score-function :aic :no-prune nil))
         (lik-alist (calculate-all-likelihoods dag-score :no-prune t))
         (all-scores nil)
         ;(pruned-scores nil)
         (all-lik nil)
         hash
         initial-node
         compute-func
         ;lik-compute-func
         out
         (k-groups (list (list 0) (list 1) (list 2) (list 3))))
    
    (loop for (variable . parent-scores) in scores-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-scores (acons variable hash all-scores)))

    ;; (loop for (variable . parent-scores) in pruned-scores-alist do
    ;;   (setf hash (make-hash-table :test #'equalp))
    ;;   (loop for (parent . score) in parent-scores do
    ;;     (setf (gethash parent hash) score))
    ;;   (setf pruned-scores (acons variable hash pruned-scores)))

    (loop for (variable . parent-scores) in lik-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-lik (acons variable hash all-lik)))

    (setf initial-node (make-initial-search-node 4)
          compute-func (lambda (v p) (format t "computing ~a <- ~a~%" v p) (gethash p (cdr (assoc v all-lik))))
          ;lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
          out (abc-search initial-node dag-score compute-func k-groups))
    (append out (list all-scores dag-score all-lik))))


(defun breast-cancer-4-var-astar-test ()
  (let* ((dag-score (make-instance 'discrete-aic-score
                                   :path (asdf:system-relative-pathname
                                          :dag-search
                                          "./data/breast-cancer-4-var.txt")))
         (scores-alist (calculate-all-scores dag-score :no-prune t))
         ;(pruned-scores-alist (calculate-all-scores data :score-function :aic :no-prune nil))
         (lik-alist (calculate-all-likelihoods dag-score :no-prune t))
         (all-scores nil)
         ;(pruned-scores nil)
         (all-lik nil)
         hash
         initial-node
         compute-func
         ;lik-compute-func
         out
         (k-groups (list (list 0) (list 1) (list 2) (list 3))))
    
    (loop for (variable . parent-scores) in scores-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-scores (acons variable hash all-scores)))

    ;; (loop for (variable . parent-scores) in pruned-scores-alist do
    ;;   (setf hash (make-hash-table :test #'equalp))
    ;;   (loop for (parent . score) in parent-scores do
    ;;     (setf (gethash parent hash) score))
    ;;   (setf pruned-scores (acons variable hash pruned-scores)))

    (loop for (variable . parent-scores) in lik-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-lik (acons variable hash all-lik)))

    (setf initial-node (make-initial-search-node 4)
          compute-func (lambda (v p) (format t "computing ~a <- ~a~%" v p) (gethash p (cdr (assoc v all-lik))))
          ;lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
          out (astar-search initial-node dag-score all-lik k-groups))
    (append out (list all-scores dag-score all-lik))))


(defun breast-cancer-simple-test ()
  (let* ((dag-score (make-instance 'discrete-aic-score
                                   :path (asdf:system-relative-pathname
                                          :dag-search
                                          "./data/breast-cancer-simple.txt")))
         (scores-alist (calculate-all-scores dag-score :no-prune t))
         ;(pruned-scores-alist (calculate-all-scores data :score-function :aic :no-prune nil))
         (lik-alist (calculate-all-likelihoods dag-score :no-prune t))
         (all-scores nil)
         ;(pruned-scores nil)
         (all-lik nil)
         hash
         initial-node
         compute-func
         ;lik-compute-func
         out
         (k-groups (list (list 0 1 4) (list 2 3) (list 5))))
    
    (loop for (variable . parent-scores) in scores-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-scores (acons variable hash all-scores)))

    ;; (loop for (variable . parent-scores) in pruned-scores-alist do
    ;;   (setf hash (make-hash-table :test #'equalp))
    ;;   (loop for (parent . score) in parent-scores do
    ;;     (setf (gethash parent hash) score))
    ;;   (setf pruned-scores (acons variable hash pruned-scores)))

    (loop for (variable . parent-scores) in lik-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-lik (acons variable hash all-lik)))

    (setf initial-node (make-initial-search-node 6)
          compute-func (lambda (v p) (format t "computing ~a <- ~a~%" v p) (gethash p (cdr (assoc v all-lik))))
          ;lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
          out (abc-search initial-node dag-score compute-func k-groups))

    (append out (list all-scores dag-score all-lik))))


;; (defun breast-cancer-intermediate-test ()
;;   (let* ((time (get-internal-real-time))
;;          (data (make-instance 'discrete-data :path (asdf:system-relative-pathname :dag-search "./src/breast-cancer-intermediate.txt")))
;;          (scores-alist (calculate-all-scores data :score-function :aic :no-prune t))
;;          (pruned-scores-alist (calculate-all-scores data :score-function :aic :no-prune nil))
;;          (lik-alist (calculate-all-scores data :score-function :log-likelihood :no-prune t))
;;          (all-scores nil)
;;          (pruned-scores nil)
;;          (all-lik nil)
;;          hash
;;          initial-node
;;          compute-func
;;          lik-compute-func
;;          out
;;          (k-groups (list (list 0 1 2 3) (list 4 5 6 7))))
;;     (loop for (variable . parent-scores) in scores-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf all-scores (acons variable hash all-scores)))

;;     (loop for (variable . parent-scores) in pruned-scores-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf pruned-scores (acons variable hash pruned-scores)))

;;     (loop for (variable . parent-scores) in lik-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf all-lik (acons variable hash all-lik)))
    
;;     (setf initial-node (make-initial-search-node 8)
;;           compute-func (lambda (v p) (format t "~a -> ~a~%" v p) (gethash p (cdr (assoc v all-scores))))
;;           lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
;;           out (abc-search initial-node data compute-func lik-compute-func k-groups))
;;     (append out (list pruned-scores data (float (/ (- (get-internal-real-time) time) internal-time-units-per-second))))))


;; (defun breast-cancer-test ()
;;   (let* ((data (make-instance 'discrete-data :path (asdf:system-relative-pathname :dag-search "./src/breast-cancer.txt")))
;;          (scores-alist (calculate-all-scores data :score-function :aic :no-prune t))
;;          (pruned-scores-alist (calculate-all-scores data :score-function :aic :no-prune nil))
;;          (all-scores nil)
;;          (pruned-scores nil)
;;          hash
;;          initial-node
;;          compute-func
;;          out)
;;     (loop for (variable . parent-scores) in scores-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf all-scores (acons variable hash all-scores)))

;;     (loop for (variable . parent-scores) in pruned-scores-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf pruned-scores (acons variable hash pruned-scores)))
    
;;     (setf initial-node (make-initial-search-node 10)
;;           compute-func (lambda (v p) (gethash p (cdr (assoc v all-scores))))
;;           out (abc-search initial-node data compute-func))
;;     (append out (list pruned-scores data))))


;; (defun get-first-nodes ()
;;   (destructuring-bind (full-scores data) (get-four-variable-scores)
;;     (let ((scores nil)
;;           initial-node
;;           q
;;           (compute-func (lambda (v p) (gethash p (cdr (assoc v full-scores)))))
;;           to-expand)
;;       (setf scores (acons 0 (make-hash-table :test #'equalp) scores)
;;             scores (acons 1 (make-hash-table :test #'equalp) scores)
;;             scores (acons 2 (make-hash-table :test #'equalp) scores)
;;             scores (acons 3 (make-hash-table :test #'equalp) scores)
;;             (gethash (list 1 2 3) (cdr (assoc 0 scores)))
;;             (gethash (list 1 2 3) (cdr (assoc 0 full-scores)))
;;             (gethash (list 0 2 3) (cdr (assoc 1 scores)))
;;             (gethash (list 0 2 3) (cdr (assoc 1 full-scores)))
;;             (gethash (list 0 1 3) (cdr (assoc 2 scores)))
;;             (gethash (list 0 1 3) (cdr (assoc 2 full-scores)))
;;             (gethash (list 0 1 2) (cdr (assoc 3 scores)))
;;             (gethash (list 0 1 2) (cdr (assoc 3 full-scores)))
;;             initial-node (make-initial-search-node (list-length scores)))
;;       (setf q (make-successor-nodes initial-node scores data compute-func))
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
      
;;       (setf to-expand (first q)
;;             q (rest q))
;;       ;; Q = [ (3: {}) (2: {}) (0: {}) (1: {}) ]
;;       ;; Expand (3: {})
;;       (setf q (append q (make-successor-nodes to-expand scores data compute-func)))
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Q = [ (2: {}) (2: {3}, 3: {}) (0: {}) (1: {}) (0: {3}, 3: {}) (1: {3}, 3: {}) ]
;;       ;; Apply new bounds to first element - Nothing changes
;;       (apply-bounds (first q) scores data)

;;       ;; Expand (2: {})
;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Q = [ (2: {3}, 3: {}) (2: {}, 3: {2}) (0: {}) (1: {}) (0: {3}, 3: {}) (0: {2}, 2: {}) (1: {3}, 3: {}) (1: {2}, 2: {}) ]

;;       ;; Apply new bounds to the first element - Nothing changes
;;       (apply-bounds (first q) scores data)

;;       ;; expand (2: {3}, 3: {})
;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Apply new bounds to first element - Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
     
;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Something changes!
;;       (apply-bounds (first q) scores data)
;;       ;; Order changes!
;;       (setf q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))
;;       ;; Nothing changes
;;       (apply-bounds (first q) scores data)

;;       (setf to-expand (first q)
;;             q (rest q)
;;             q (append q (make-successor-nodes to-expand scores data compute-func))
;;             q (sort q (lambda (x y) (< (+ (g x) (h x)) (+ (g y) (h y))))))

;;       ;; Everything is computed :(
      

      
;;       (list q scores full-scores))))


;; (defun breast-cancer-simple-limit-test ()
;;   (let* ((data (make-instance 'discrete-data :path (asdf:system-relative-pathname :dag-search "./src/breast-cancer-simple.txt")))
;;          (scores-alist (calculate-all-scores data :score-function :aic :no-prune t))
;;          (pruned-scores-alist (calculate-all-scores data :score-function :aic :no-prune nil))
;;          (lik-alist (calculate-all-scores data :score-function :log-likelihood :no-prune t))
;;          (all-scores nil)
;;          (pruned-scores nil)
;;          (all-lik nil)
;;          (test-scores nil)
;;          hash
;;          initial-node
;;          compute-func
;;          lik-compute-func
;;          out
;;          (k-groups (list (list 0 1 4) (list 2 3) (list 5))))
    
;;     (loop for (variable . parent-scores) in scores-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf all-scores (acons variable hash all-scores)))

;;     (loop for (variable . parent-scores) in pruned-scores-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf pruned-scores (acons variable hash pruned-scores)))

;;     (loop for (variable . parent-scores) in lik-alist do
;;       (setf hash (make-hash-table :test #'equalp))
;;       (loop for (parent . score) in parent-scores do
;;         (setf (gethash parent hash) score))
;;       (setf all-lik (acons variable hash all-lik)))

;;     ;; 0, 2, 1 <- 2, 3 <- 0, 5 <- 0,2, 4 <- 5, score 1708.528

;;     ;; 19 elements of 0
;;     (setf hash (make-hash-table :test #'equalp)
;;           (gethash nil hash) (gethash nil (cdr (assoc 0 all-scores)))
;;           ;(gethash (list 1) hash) (gethash (list 1) (cdr (assoc 0 all-scores))) ;; cuttable
;;           (gethash (list 2) hash) (gethash (list 2) (cdr (assoc 0 all-scores)))
;;           (gethash (list 3) hash) (gethash (list 3) (cdr (assoc 0 all-scores)))
;;           (gethash (list 4) hash) (gethash (list 4) (cdr (assoc 0 all-scores)))
;;           (gethash (list 5) hash) (gethash (list 5) (cdr (assoc 0 all-scores)))
;;           (gethash (list 1 2) hash) (gethash (list 1 2) (cdr (assoc 0 all-scores)))
;;           (gethash (list 1 3) hash) (gethash (list 1 3) (cdr (assoc 0 all-scores)))
;;           (gethash (list 1 4) hash) (gethash (list 1 4) (cdr (assoc 0 all-scores)))
;;           (gethash (list 2 3) hash) (gethash (list 2 3) (cdr (assoc 0 all-scores)))
;;           ;(gethash (list 2 4) hash) (gethash (list 2 4) (cdr (assoc 0 all-scores))) ;; cuttable
;;           (gethash (list 2 5) hash) (gethash (list 2 5) (cdr (assoc 0 all-scores)))
;;           (gethash (list 3 5) hash) (gethash (list 3 5) (cdr (assoc 0 all-scores)))
;;           ;(gethash (list 4 5) hash) (gethash (list 4 5) (cdr (assoc 0 all-scores))) ;; cuttable
;;           (gethash (list 1 2 5) hash) (gethash (list 1 2 5) (cdr (assoc 0 all-scores)))
;;           ;(gethash (list 2 3 5) hash) (gethash (list 2 3 5) (cdr (assoc 0 all-scores))) ;; cuttable
;;           (gethash (list 2 4 5) hash) (gethash (list 2 4 5) (cdr (assoc 0 all-scores)))
;;           ;(gethash (list 3 4 5) hash) (gethash (list 3 4 5) (cdr (assoc 0 all-scores))) ;; cuttable
;;           (gethash (list 1 2 3 4 5) hash) (gethash (list 1 2 3 4 5) (cdr (assoc 0 all-scores)))
;;           test-scores (acons 0 hash test-scores))

;;     ;; 14 elements of 1
;;     (setf hash (make-hash-table :test #'equalp)
;;           (gethash nil hash) (gethash nil (cdr (assoc 1 all-scores)))
;;           (gethash (list 0) hash) (gethash (list 0) (cdr (assoc 1 all-scores)))
;;           (gethash (list 2) hash) (gethash (list 2) (cdr (assoc 1 all-scores)))
;;           ;(gethash (list 3) hash) (gethash (list 3) (cdr (assoc 1 all-scores))) ;; cuttable
;;           ;(gethash (list 4) hash) (gethash (list 4) (cdr (assoc 1 all-scores))) ;; cuttable
;;           ;(gethash (list 5) hash) (gethash (list 5) (cdr (assoc 1 all-scores))) ;; cuttable
;;           ;(gethash (list 0 2) hash) (gethash (list 0 2) (cdr (assoc 1 all-scores))) ;; cuttable
;;           (gethash (list 0 3) hash) (gethash (list 0 3) (cdr (assoc 1 all-scores)))
;;           (gethash (list 0 5) hash) (gethash (list 0 5) (cdr (assoc 1 all-scores)))
;;           (gethash (list 0 2 5) hash) (gethash (list 0 2 5) (cdr (assoc 1 all-scores)))
;;           (gethash (list 0 4 5) hash) (gethash (list 0 4 5) (cdr (assoc 1 all-scores)))
;;           ;(gethash (list 2 4 5) hash) (gethash (list 2 4 5) (cdr (assoc 1 all-scores))) ;; cuttable
;;           (gethash (list 3 4 5) hash) (gethash (list 3 4 5) (cdr (assoc 1 all-scores)))
;;           (gethash (list 0 2 3 4 5) hash) (gethash (list 0 2 3 4 5) (cdr (assoc 1 all-scores)))
;;           test-scores (acons 1 hash test-scores))

;;     ;; 14 elements of 2
;;     (setf hash (make-hash-table :test #'equalp)
;;           (gethash nil hash) (gethash nil (cdr (assoc 2 all-scores)))
;;           (gethash (list 0) hash) (gethash (list 0) (cdr (assoc 2 all-scores)))
;;           (gethash (list 1) hash) (gethash (list 1) (cdr (assoc 2 all-scores)))
;;           (gethash (list 3) hash) (gethash (list 3) (cdr (assoc 2 all-scores))) ;; cuttable but not in combination
;;           (gethash (list 4) hash) (gethash (list 4) (cdr (assoc 2 all-scores)))
;;           (gethash (list 5) hash) (gethash (list 5) (cdr (assoc 2 all-scores)))
;;           (gethash (list 0 1) hash) (gethash (list 0 1) (cdr (assoc 2 all-scores)))
;;           ;(gethash (list 0 3) hash) (gethash (list 0 3) (cdr (assoc 2 all-scores))) ;; cuttable
;;           (gethash (list 0 5) hash) (gethash (list 0 5) (cdr (assoc 2 all-scores)))
;;           (gethash (list 0 3 5) hash) (gethash (list 0 3 5) (cdr (assoc 2 all-scores)))
;;           (gethash (list 0 4 5) hash) (gethash (list 0 4 5) (cdr (assoc 2 all-scores)))
;;           (gethash (list 1 4 5) hash) (gethash (list 1 4 5) (cdr (assoc 2 all-scores)))
;;           ;(gethash (list 3 4 5) hash) (gethash (list 3 4 5) (cdr (assoc 2 all-scores))) ;; cuttable
;;           (gethash (list 0 1 3 4 5) hash) (gethash (list 0 1 3 4 5) (cdr (assoc 2 all-scores)))
;;           test-scores (acons 2 hash test-scores))

;;     ;; 14 elements of 3
;;     (setf hash (make-hash-table :test #'equalp)
;;           (gethash nil hash) (gethash nil (cdr (assoc 3 all-scores)))
;;           (gethash (list 0) hash) (gethash (list 0) (cdr (assoc 3 all-scores)))
;;           ;(gethash (list 1) hash) (gethash (list 1) (cdr (assoc 3 all-scores))) ;; cuttable
;;           (gethash (list 2) hash) (gethash (list 2) (cdr (assoc 3 all-scores)))
;;           ;(gethash (list 4) hash) (gethash (list 4) (cdr (assoc 3 all-scores))) ;; cuttable
;;           (gethash (list 5) hash) (gethash (list 5) (cdr (assoc 3 all-scores)))
;;           (gethash (list 0 1) hash) (gethash (list 0 1) (cdr (assoc 3 all-scores)))
;;           (gethash (list 0 2) hash) (gethash (list 0 2) (cdr (assoc 3 all-scores)))
;;           (gethash (list 0 4) hash) (gethash (list 0 4) (cdr (assoc 3 all-scores)))
;;           (gethash (list 0 5) hash) (gethash (list 0 5) (cdr (assoc 3 all-scores)))
;;           (gethash (list 0 2 5) hash) (gethash (list 0 2 5) (cdr (assoc 3 all-scores)))
;;           (gethash (list 1 2 5) hash) (gethash (list 1 2 5) (cdr (assoc 3 all-scores)))
;;           ;(gethash (list 2 4 5) hash) (gethash (list 2 4 5) (cdr (assoc 3 all-scores))) ;; cuttable
;;           (gethash (list 0 1 2 4 5) hash) (gethash (list 0 1 2 4 5) (cdr (assoc 3 all-scores)))
;;           test-scores (acons 3 hash test-scores))

;;     ;; 15 elements of 4
;;     (setf hash (make-hash-table :test #'equalp)
;;           (gethash nil hash) (gethash nil (cdr (assoc 4 all-scores)))
;;           (gethash (list 0) hash) (gethash (list 0) (cdr (assoc 4 all-scores)))
;;           ;(gethash (list 1) hash) (gethash (list 1) (cdr (assoc 4 all-scores))) ;; cuttable
;;           (gethash (list 2) hash) (gethash (list 2) (cdr (assoc 4 all-scores)))
;;           ;(gethash (list 3) hash) (gethash (list 3) (cdr (assoc 4 all-scores))) ;; cuttable
;;           (gethash (list 5) hash) (gethash (list 5) (cdr (assoc 4 all-scores)))
;;           ;(gethash (list 0 2) hash) (gethash (list 0 2) (cdr (assoc 4 all-scores))) ;; cuttable
;;           (gethash (list 0 3) hash) (gethash (list 0 3) (cdr (assoc 4 all-scores)))
;;           (gethash (list 0 5) hash) (gethash (list 0 5) (cdr (assoc 4 all-scores)))
;;           ;(gethash (list 2 5) hash) (gethash (list 2 5) (cdr (assoc 4 all-scores))) ;; cuttable
;;           (gethash (list 0 1 2) hash) (gethash (list 0 1 2) (cdr (assoc 4 all-scores)))
;;           (gethash (list 0 2 5) hash) (gethash (list 0 2 5) (cdr (assoc 4 all-scores)))
;;           ;(gethash (list 1 2 3) hash) (gethash (list 1 2 3) (cdr (assoc 4 all-scores))) ;; cuttable
;;           ;(gethash (list 1 3 5) hash) (gethash (list 1 3 5) (cdr (assoc 4 all-scores))) ;; cuttable
;;           (gethash (list 0 1 2 3 5) hash) (gethash (list 0 1 2 3 5) (cdr (assoc 4 all-scores)))
;;           test-scores (acons 4 hash test-scores))

;;     ;; 16 elements of 5
;;     (setf hash (make-hash-table :test #'equalp)
;;           (gethash nil hash) (gethash nil (cdr (assoc 5 all-scores)))
;;           (gethash (list 0) hash) (gethash (list 0) (cdr (assoc 5 all-scores)))
;;           (gethash (list 1) hash) (gethash (list 1) (cdr (assoc 5 all-scores))) ;; cuttable but not in combination
;;           (gethash (list 2) hash) (gethash (list 2) (cdr (assoc 5 all-scores)))
;;           (gethash (list 3) hash) (gethash (list 3) (cdr (assoc 5 all-scores)))
;;           (gethash (list 4) hash) (gethash (list 4) (cdr (assoc 5 all-scores)))
;;           (gethash (list 0 2) hash) (gethash (list 0 2) (cdr (assoc 5 all-scores)))
;;           (gethash (list 0 3) hash) (gethash (list 0 3) (cdr (assoc 5 all-scores)))
;;           (gethash (list 0 4) hash) (gethash (list 0 4) (cdr (assoc 5 all-scores)))
;;           ;(gethash (list 1 2) hash) (gethash (list 1 2) (cdr (assoc 5 all-scores))) ;; cuttable
;;           (gethash (list 2 4) hash) (gethash (list 2 4) (cdr (assoc 5 all-scores)))
;;           (gethash (list 0 1 2) hash) (gethash (list 0 1 2) (cdr (assoc 5 all-scores)))
;;           (gethash (list 0 2 3) hash) (gethash (list 0 2 3) (cdr (assoc 5 all-scores)))
;;           ;(gethash (list 1 2 3) hash) (gethash (list 1 2 3) (cdr (assoc 5 all-scores))) ;; cuttable
;;           ;(gethash (list 2 3 4) hash) (gethash (list 2 3 4) (cdr (assoc 5 all-scores))) ;; cuttable
;;           (gethash (list 0 1 2 3 4) hash) (gethash (list 0 1 2 3 4) (cdr (assoc 5 all-scores)))
;;           test-scores (acons 5 hash test-scores))

;;     (setf initial-node (make-initial-search-node 6)
;;           compute-func (lambda (v p) (format t "computing ~a <- ~a~%" v p) (gethash p (cdr (assoc v all-scores))))
;;           lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
;;           out (abc-check-stuck initial-node test-scores data compute-func lik-compute-func k-groups))

;;     (append out (list pruned-scores all-scores data all-lik))))
