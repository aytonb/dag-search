(uiop:define-package #:dag-search/search-node/bound-strategy
    (:use #:cl
          #:dag-search/score/all
          #:dag-search/search-node/search-node
          ;#:dag-search/gp-bounds
          ;#:dag-search/most-likely
          )
  (:export #:best-parents-bound
           #:best-parents-final-bound
           #:best-parents-complement-bound
           #:until-exact-bound
           #:best-parents-extra-bound
           #:best-parents-active-bound
           #:best-parents-likelihood-bound
           #:leaps-bound)
  (:documentation "Strategies for computing bounds."))

(in-package #:dag-search/search-node/bound-strategy)


;; best-parents-bound performs better than best-parents-final-bound
;; until-exact-bound performs the same as the best-parents bound, but slower
;; best-parents-extra bound needs more evaluations, but is faster and expands fewer nodes


(defun best-parents-bound (expanded-node variable best-score scores score-fn compute-func)
  "Computes a bound based on the best parent set for variable."
  (if (third best-score)
      best-score
      (progn
        (setf (gethash (second best-score) (cdr (assoc variable scores)))
              (funcall compute-func variable (second best-score)))
        (bound variable (assigned expanded-node) scores score-fn))))


(defun best-parents-final-bound (expanded-node variable best-score scores score-fn compute-func)
  "Computes a bound based on the best parent sets for all variables when all but
one variable has been expanded."
  (let* ((n-variables (array-dimension (exact-p expanded-node) 0)))

    (unless (equal (list-length (assigned expanded-node))
                   (1- n-variables))
      (return-from best-parents-final-bound best-score))

    (loop for var below n-variables do
      (let ((var-edges (cdr (assoc var (edges expanded-node) :test #'equalp))))
        (if var-edges
            (unless (aref (exact-p expanded-node) var)
              (setf (gethash var-edges (cdr (assoc var scores)))
                    (funcall compute-func var var-edges)))
            (unless (third best-score)
              (setf (gethash (second best-score) (cdr (assoc var scores)))
                    (funcall compute-func var (second best-score)))))))

    (bound variable (assigned expanded-node) scores score-fn)))


(defun until-exact-bound (expanded-node variable best-score scores score-fn compute-func)
  "Computes bounds until the bound is exact."
  (loop while (not (third best-score)) do
    (setf (gethash (second best-score) (cdr (assoc variable scores)))
          (funcall compute-func variable (second best-score))
          best-score (bound variable (assigned expanded-node) scores score-fn)))
  best-score)


(defun best-parents-extra-bound (expanded-node variable best-score scores score-fn compute-func)
  "Computes a bound based on the best parent set and an extra intermediate bound."
  (let ((parent-size (list-length (second best-score)))
        (lik-parent-set (fourth best-score))
        (lik-parent-size (list-length (fourth best-score)))
        extra-bound-size
        extra-bound-set
        rand-var)
    (if (third best-score)
        best-score
        (progn
          (setf (gethash (second best-score) (cdr (assoc variable scores)))
                (funcall compute-func variable (second best-score)))
          (unless (equal parent-size (1- lik-parent-size))
            (setf extra-bound-size (+ parent-size
                                           (floor (/ (- lik-parent-size parent-size) 2)))
                  extra-bound-set (copy-tree (second best-score)))
            (dotimes (i (- extra-bound-size parent-size))
              (setf rand-var (nth (random lik-parent-size) lik-parent-set))
              (loop while (member rand-var extra-bound-set) do
                (setf rand-var (nth (random lik-parent-size) lik-parent-set)))
              (setf extra-bound-set (list* rand-var extra-bound-set)))
            (setf extra-bound-set (sort extra-bound-set #'<)
                  (gethash extra-bound-set (cdr (assoc variable scores)))
                  (funcall compute-func variable extra-bound-set)))
          (bound variable (assigned expanded-node) scores score-fn)))))


;; If the complement is taken with respect to all variables, this doesn't work
;; very well at all
;; Current form works less well than best parents
(defun best-parents-complement-bound (expanded-node variable best-score scores score-fn compute-func)
  (let ((complement nil))
    (if (third best-score)
        best-score
        (progn
          (setf (gethash (second best-score) (cdr (assoc variable scores)))
                (funcall compute-func variable (second best-score)))
          (loop for v in (assigned expanded-node) do ;below n-variables do
            (unless (or (equal v variable)
                        (member v (second best-score)))
              (setf complement (append complement (list v)))))
          (setf (gethash complement (cdr (assoc variable scores)))
                (funcall compute-func variable complement))
          (bound variable (assigned expanded-node) scores score-fn)))))


(defun best-parents-active-bound (expanded-node variable best-score scores liks score-fn compute-func &optional (allowable-fn nil))
  "Actively searches for a bound if it expects one might be available."
  (if (third best-score)
      best-score
      (let ((max-one-element nil)
            (max-one-element-lik most-negative-double-float)
            (min-one-element nil)
            (min-one-element-lik most-positive-double-float)
            (one-element-liks nil)
            ;(n-variables (array-dimension (exact-p expanded-node) 0))
            (proposed-set nil)
            ;proposed-set-size
            (proposed-set-2 nil)
            ;(max-set nil)
            ;max-set-size
            ;max-set-lik
            ;estimated-lik
            ;estimated-post-bound
            output)
        
        ;; If we are trying to expand a one element, and there are two other one
        ;; elements left
        (when (<= (list-length (second best-score)) 5) ;; 1
          (loop for parents being the hash-keys in (cdr (assoc variable liks))
                  using (hash-value lik)
                do (when (and (equal (list-length parents) 1)
                              (not (subsetp parents (second best-score)))
                              (> lik max-one-element-lik))
                     (setf max-one-element parents
                           max-one-element-lik lik))
                   (when (and (equal (list-length parents) 1)
                              (not (subsetp parents (second best-score)))
                              (< lik min-one-element-lik))
                     (setf min-one-element parents
                           min-one-element-lik lik))
                   (when (equal (list-length parents) 1)
                     (setf one-element-liks (append one-element-liks (list (list (first parents) lik))))))

          (setf one-element-liks (sort one-element-liks #'> :key #'second))

          ;; When both min and max exist, and they are not the same 
          (when (and max-one-element
                     t
                     ;(not (equalp max-one-element min-one-element))
                     )

            ;; ;; Make the proposed set to include the max one element
            ;; (setf proposed-set (sort (copy-tree (append (second best-score)
            ;;                                             max-one-element))
            ;;                          #'<))
            ;; ;; Find the max element group without min-one-element 
            ;; (loop for var below n-variables do
            ;;   ;; (unless (or (equal var variable)
            ;;   ;;             (equal var (first min-one-element)))
            ;;   ;;   (setf proposed-set (append proposed-set (list var))))
            ;;   (unless (equal var variable)
            ;;     (setf max-set (append max-set (list var)))))

            ;; ;; Estimate the value of the proposed set linearly.
            ;; (setf proposed-set-size (list-length proposed-set)
            ;;       max-set-size (list-length max-set)
            ;;       max-set-lik (gethash max-set (cdr (assoc variable liks)))
            ;;       estimated-lik (- max-one-element-lik
            ;;                        (* (1- proposed-set-size)
            ;;                           (/ (- max-one-element-lik max-set-lik)
            ;;                              (1- max-set-size)))))

            ;; Make the 2 element proposed set
            (block construct-proposed-set-2
              (loop for (el lik) in one-element-liks do
                (unless (member el (second best-score))
                  (setf proposed-set-2 (append proposed-set-2 (list el))))
                (when (or (equal (list-length proposed-set-2)
                                 (1- (list-length one-element-liks)))
                          (equal (list-length proposed-set-2) ;(1- (/ n-variables 2))
                                 (cond
                                   ((<= (list-length (second best-score)) 2)
                                    1)
                                   (t 2))))
                  (return-from construct-proposed-set-2))))
            (setf proposed-set-2 (sort (copy-tree (append proposed-set-2
                                                          (second best-score)))
                                       #'<))

            ;; Compare the estimated-lik and min-one-element-lik
            ;; (format t "~%Variable is ~a~%" variable)
            ;; (format t "Would expand ~a~%" (second best-score))
            ;; (format t "Found max-one-element ~a with lik ~a~%" max-one-element max-one-element-lik)
            ;; (format t "Found min-one-element ~a with lik ~a~%" min-one-element min-one-element-lik)
            ;; (format t "One element liks are ~a~%" one-element-liks)
            ;; (format t "Proposed-set-2 is ~a~%" proposed-set-2)

            ;; We do better if we don't limit ourselves by the post bound
            ;; (setf estimated-post-bound (discrete-bound-with-estimate variable (assigned expanded-node) scores data-set proposed-set estimated-lik))
            ;; (format t "Under the proposal, the best bound would be ~a~%" (second estimated-post-bound))

            ;; If we expect the bound to do nothing, ignore it.
            (if nil ;; (equalp (second best-score)
                ;;         (second estimated-post-bound))
                (progn
                  (format t "Deciding to reject the proposal~%")
                  (setf output best-score))
                (if (gethash proposed-set-2 (cdr (assoc variable scores)))
                    (setf output best-score)
                    (progn
                      (destructuring-bind (score lik)
                          (funcall compute-func variable proposed-set-2)
                        (setf (gethash proposed-set-2 (cdr (assoc variable scores)))
                              score
                              (gethash proposed-set-2 (cdr (assoc variable liks)))
                              lik))
                      ;; (setf (gethash proposed-set (cdr (assoc variable scores)))
                      ;;       (funcall compute-func variable proposed-set)
                      ;;       (gethash proposed-set (cdr (assoc variable liks)))
                      ;;       (funcall lik-compute-func variable proposed-set))
                      (setf output (bound variable (assigned expanded-node) scores
                                          score-fn allowable-fn))
                      ;; (format t "True lik of proposed set is ~a~%" (gethash proposed-set (cdr (assoc variable liks))))
                      ;; (format t "Now computing best bound to be ~a~%" (second output))
                      )))


            (unless (equalp (second output) (second best-score))
              (return-from best-parents-active-bound output))
            
            ;; (format t "Original bound required~%")
            ))

        (destructuring-bind (score lik)
            (funcall compute-func variable (second best-score))
          (setf (gethash (second best-score) (cdr (assoc variable scores)))
                score
                (gethash (second best-score) (cdr (assoc variable liks)))
                lik))
        (bound variable (assigned expanded-node) scores score-fn allowable-fn))))


;;; Bound based on likelihood of being optimal
;; (defun best-parents-likelihood-bound (expanded-node variable best-score scores liks data-set compute-func lik-compute-func)
;;   (if (third best-score)
;;       best-score
;;       (let ((possible-parents nil)
;;             bounds
;;             reduced-bounds
;;             min-ub
;;             (looping t)
;;             (max-p 0)
;;             max-p-pars)
;;         (format t "Best score is ~a~%" best-score)
;;         (setf (gethash (second best-score) (cdr (assoc variable scores)))
;;               (funcall compute-func variable (second best-score)))
;;         (loop for v in (assigned expanded-node) do
;;           (unless (equal v variable)
;;             (setf possible-parents (append possible-parents (list v)))))
;;         (setf bounds (discrete-upper-and-lower-bounds variable possible-parents scores data-set)
;;               bounds (remove-if (lambda (x) (equalp (first x) (second x))) bounds))

;;         (loop while looping do
;;           (setf looping nil)
;;           (unless bounds
;;             (return))
;;           (setf min-ub (reduce #'min bounds :key #'second)
;;                 reduced-bounds (remove-if (lambda (x) (> (first x) min-ub)) bounds))
;;           (unless (equalp bounds reduced-bounds)
;;             (setf looping t
;;                   bounds reduced-bounds)))

;;         (format t "Bounds are ~a~%" bounds)

;;         (when bounds
;;           (setf min-ub (reduce #'min bounds :key #'second))
;;           (loop for i below (list-length bounds)
;;                 for p = (prob-of-being-min bounds min-ub i)
;;                 do (format t "i = ~a p = ~a~%" i p)
;;                    (when (> p max-p)
;;                      (setf max-p p
;;                            max-p-pars (third (nth i bounds)))))
;;           (format t "Bounds are ~a with min ~a~%" bounds min-ub)
;;           (format t "Max p pars is ~a~%" max-p-pars))

;;         (discrete-bound variable (assigned expanded-node) scores data-set))))


;;; Bound based on leaps algorithm

(defun leaps-bound (expanded-node variable best-score scores liks score-fn compute-func lik-compute-func)
  "Computes a bound by solving for the exact parent set using leaps."
  (let (output)
    (if (third best-score)
        best-score
        (progn
          (leaps variable (assigned expanded-node) scores liks compute-func lik-compute-func)
          (setf output (bound variable (assigned expanded-node) scores score-fn))
          (format t "finished leaps, output is ~a~%" output)
          output))))

(defun regression-tree (possible-parents)
  ;; The regression tree follows DFS.
  ;; The pivot index is the position of the variable that was just added
  ;; Children keep the same stage, increment stage if the list does not increase
  (let ((q (list nil))
        (expansions nil)
        element
        new-q
        last-index
        stage
        (n-parents (list-length possible-parents)))

    (loop while q do
      ;; Pop the first element
      (setf element (first q)
            q (rest q))

      (if element
          (progn
            (setf last-index (position (first (last element)) possible-parents))
            (if (<= (list-length element)
                    (list-length (first (first (last expansions)))))
                (setf stage (+ 1 (third (first (last expansions)))))
                (setf stage (third (first (last expansions))))))
          (setf last-index -1
                stage 0))

      ;; Place the expansion in the expansions list.
      (setf expansions (append expansions (list (list element last-index stage))))

      ;; For everything after the last expanded variable, add to the new-q 
      (setf new-q nil)
      
      (loop for i from (1+ last-index) below n-parents do
        (setf new-q (append new-q (list (append element (list (nth i possible-parents)))))))

      ;; Add the new-q to the front of the q.
      (setf q (append new-q q)))

    expansions))


(defun bound-tree (possible-parents)
  ;; The bound tree does DFS, but all adds children to output upon expansion
  ;; The source, which is an appropriate bound, is the parent
  (let ((q (list (list possible-parents -1))) ;; Each element is (included, removed)
        (output (list (list possible-parents nil)))
        element
        new-include
        (n-removable (1- (list-length possible-parents))))

    (loop while q do
      ;; Pop the first element
      (setf element (first q)
            q (rest q))

      ;; Generate the children, removing everything above the removed 
      (loop for i from (1+ (second element)) below n-removable do
        (setf new-include (remove (nth i possible-parents) (first element))
              output (append output (list (list new-include (first element))))
              q (list* (list new-include i) q))))

    output))


(defun min-score-of-size (variable scores size)
  "Returns the lowest found score with a set number of parents."
  (let ((min-score most-positive-double-float)
        (best-parents nil))
    (loop for parents being the hash-keys in (cdr (assoc variable scores))
            using (hash-value score)
          for parents-size = (list-length parents)
          do (when (and (equal parents-size size)
                        (< score min-score))
               (setf min-score score
                     best-parents parents)))
    (list min-score best-parents)))


(defun leaps (variable possible-parents scores liks compute-func lik-compute-func)
  (format t "starting leaps, var = ~a, parents = ~a~%" variable possible-parents)
  (unless possible-parents
    (setf (gethash nil (cdr (assoc variable scores)))
          (funcall compute-func variable nil))
    (setf (gethash nil (cdr (assoc variable liks)))
          (funcall lik-compute-func variable nil))
    (return-from leaps (acons 0 (gethash nil (cdr (assoc variable liks))) nil)))

  (let* ((parents-size (list-length possible-parents))
         (min-liks nil)
         (regressions (regression-tree (subseq possible-parents 0 (1- parents-size))))
         (bounds (bound-tree possible-parents))
         (n-regressions (list-length regressions))
         (index 1)
         (prev-index 0)
         new-stage)

    (format t "regressions = ~a~%" regressions)
    (format t "bounds = ~a~%" bounds)
    (format t "n-regressions = ~a~%" n-regressions)

    ;; Initialize min-liks
    (loop for i upto parents-size do
      (setf min-liks (acons i most-positive-double-float min-liks)))

    ;; Find actual min-liks
    (loop for parents being the hash-keys in (cdr (assoc variable liks))
            using (hash-value lik)
          for n-parents = (list-length parents) do
            (when (and (<= n-parents parents-size)
                       (< lik (cdr (assoc n-parents min-liks))))
              (setf (cdr (assoc n-parents min-liks)) lik)))

    ;; Do initial stage
    (setf (gethash nil (cdr (assoc variable scores)))
          (funcall compute-func variable nil))
    (setf (gethash nil (cdr (assoc variable liks)))
          (funcall lik-compute-func variable nil))
    (setf (gethash possible-parents (cdr (assoc variable scores)))
          (funcall compute-func variable possible-parents))
    (setf (gethash possible-parents (cdr (assoc variable liks)))
          (funcall lik-compute-func variable possible-parents))

    (loop while t do
      (format t "index = ~a~%" index)
      (when (equal index prev-index)
        (error "Something weird happened"))
      (setf prev-index index)
      (when (>= index n-regressions)
        (return-from leaps min-liks))

      (destructuring-bind (regression pivot stage) (nth index regressions)
        (destructuring-bind (bound source) (nth index bounds)
          (format t "considering ~a ~a~%" regression bound)
          (if (<= (cdr (assoc (list-length regression) min-liks))
                  (gethash source (cdr (assoc variable liks))))
              
              (block update-stage
                (setf new-stage (+ stage (expt 2 (- parents-size pivot 2))))
                (format t "Updating stage to ~a~%" new-stage)
                (loop for i from (1+ index) below n-regressions do
                  (when (equal (third (nth i regressions)) new-stage)
                    (setf index i)
                    (return-from update-stage)))
                (setf index n-regressions))
              
              (progn
                (setf (gethash regression (cdr (assoc variable scores)))
                      (funcall compute-func variable regression))
                (setf (gethash regression (cdr (assoc variable liks)))
                      (funcall lik-compute-func variable regression))
                (setf (gethash bound (cdr (assoc variable scores)))
                      (funcall compute-func variable bound))
                (setf (gethash bound (cdr (assoc variable liks)))
                      (funcall lik-compute-func variable bound))
                (incf index)

                (when (< (gethash regression (cdr (assoc variable liks)))
                         (cdr (assoc (list-length regression) min-liks)))
                  (setf (cdr (assoc (list-length regression) min-liks))
                        (gethash regression (cdr (assoc variable liks)))))

                (when (< (gethash bound (cdr (assoc variable liks)))
                         (cdr (assoc (list-length bound) min-liks)))
                  (setf (cdr (assoc (list-length bound) min-liks))
                        (gethash bound (cdr (assoc variable liks))))))))))))


(defun test-leaps ()
  (let ((true-liks (make-hash-table :test #'equalp))
        (scores (acons 0 (make-hash-table :test #'equalp) nil))
        (liks (acons 0 (make-hash-table :test #'equalp) nil))
        compute-func
        lik-compute-func)
    (setf (gethash (list 1 2 3 4 5) true-liks) most-negative-double-float
          (gethash (list 1 2 3 4) true-liks) 592
          (gethash (list 1 2 3 5) true-liks) 596
          (gethash (list 1 2 4 5) true-liks) 596
          (gethash (list 1 3 4 5) true-liks) 605
          (gethash (list 2 3 4 5) true-liks) 660
          (gethash (list 1 2 3) true-liks) 612
          (gethash (list 1 2 4) true-liks) 615
          (gethash (list 1 2 5) true-liks) 597
          (gethash (list 1 3 4) true-liks) 612
          (gethash (list 1 3 5) true-liks) 618
          (gethash (list 1 4 5) true-liks) 618
          (gethash (list 2 3 4) true-liks) 664
          (gethash (list 2 3 5) true-liks) 666
          (gethash (list 2 4 5) true-liks) 667
          (gethash (list 3 4 5) true-liks) 720
          (gethash (list 1 2) true-liks) 615
          (gethash (list 1 3) true-liks) 641
          (gethash (list 1 4) true-liks) 648
          (gethash (list 1 5) true-liks) 618
          (gethash (list 2 3) true-liks) 673
          (gethash (list 2 4) true-liks) 685
          (gethash (list 2 5) true-liks) 675
          (gethash (list 3 4) true-liks) 727
          (gethash (list 3 5) true-liks) 732
          (gethash (list 4 5) true-liks) 736
          (gethash (list 1) true-liks) 668
          (gethash (list 2) true-liks) 702
          (gethash (list 3) true-liks) 746
          (gethash (list 4) true-liks) 792
          (gethash (list 5) true-liks) 799
          (gethash nil true-liks) most-negative-double-float)


    (setf compute-func (lambda (x y)
                         (declare (ignore x y))
                         nil))
    (setf lik-compute-func (lambda (x y)
                             (declare (ignore x))
                             (format t "Evaluating ~a~%" y)
                             (gethash y true-liks)))

    (leaps 0 (list 1 2 3 4 5) scores liks compute-func lik-compute-func)))
