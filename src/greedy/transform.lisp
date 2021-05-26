(uiop:define-package #:dag-search/greedy/transform
    (:use #:cl
          #:dag-search/greedy/check-dag)
  (:export #:score-dag
           #:local-transforms
           #:random-dag))

(in-package #:dag-search/greedy/transform)


(defun add-parent (dag variable parent)
  (let ((new-dag (copy-tree dag)))
    (setf (cdr (assoc variable new-dag))
          (sort (append (cdr (assoc variable new-dag))
                        (list parent))
                #'<))
    new-dag))


(defun remove-parent (dag variable parent)
  (let ((new-dag (copy-tree dag)))
    (setf (cdr (assoc variable new-dag))
          (remove parent (cdr (assoc variable new-dag))))
    new-dag))


(defun reverse-parent (dag variable parent)
  (let ((new-dag (copy-tree dag)))
    (setf (cdr (assoc variable new-dag))
          (remove parent (cdr (assoc variable new-dag)))
          (cdr (assoc parent new-dag))
          (sort (append (cdr (assoc parent new-dag))
                        (list variable))
                #'<))
    new-dag))


(defun score-dag (dag scores compute-fn)
  (let ((score 0))
    (loop for (var . parents) in dag do
      (unless (gethash parents (cdr (assoc var scores)))
        (setf (gethash parents (cdr (assoc var scores)))
              (funcall compute-fn var parents)))
      (incf score (gethash parents (cdr (assoc var scores)))))
    score))


(defun local-transforms (dag n-vars scores tabu compute-fn &key (allowable-fn nil))
  (let (new-dag
        new-score
        (local-best-dag nil)
        local-best-score)
    
    (loop for (var . parents) in dag do
      (loop for par in parents do
        ;; Remove parent
        (when (or (not allowable-fn)
                  (funcall allowable-fn var (remove par parents)))
          (setf new-dag (remove-parent dag var par))
          (when (acyclic-p new-dag)
            (setf new-score (score-dag new-dag scores compute-fn))
            (when (and (not (member new-dag tabu :test #'equalp))
                       (or (not local-best-dag)
                           (< new-score local-best-score)))
              (setf local-best-score new-score
                    local-best-dag new-dag))))

        ;; Reverse parent
        (when (or (not allowable-fn)
                  (and (funcall allowable-fn var (remove par parents))
                       (funcall allowable-fn par
                                (sort (append (cdr (assoc par dag))
                                              (list var))
                                      #'<))))
          (setf new-dag (reverse-parent dag var par))
          (when (acyclic-p new-dag)
            (setf new-score (score-dag new-dag scores compute-fn))
            (when (and (not (member new-dag tabu :test #'equalp))
                       (or (not local-best-dag)
                           (< new-score local-best-score)))
              (setf local-best-score new-score
                    local-best-dag new-dag)))))

      ;; Add parent
      (loop for par below n-vars do
        (unless (or (equal par var)
                    (member par parents))
          (when (or (not allowable-fn)
                    (funcall allowable-fn var
                             (sort (append (cdr (assoc var dag))
                                           (list par))
                                   #'<)))
            (setf new-dag (add-parent dag var par))
            (when (acyclic-p new-dag)
              (setf new-score (score-dag new-dag scores compute-fn))
              (when (and (not (member new-dag tabu :test #'equalp))
                         (or (not local-best-dag)
                             (< new-score local-best-score)))
                (setf local-best-score new-score
                      local-best-dag new-dag)))))))

    (list local-best-dag local-best-score)))


(defun random-dag (n-vars &key (allowable-fn nil))
  (let ((vars (loop for i below n-vars collect i))
        (assigned nil)
        (dag nil)
        r
        new-child)
    (dotimes (i n-vars)
      (setf r (random (- n-vars i))
            new-child (nth r vars)
            vars (remove new-child vars)
            dag (acons new-child nil dag))
      (dotimes (j i)
        (setf r (random 1.0))
        (when (> r 0.5)
          (when (or (not allowable-fn)
                    (funcall allowable-fn new-child
                             (sort (append (cdr (assoc new-child dag))
                                           (list (nth j assigned)))
                                   #'<)))
            (setf (cdr (assoc new-child dag))
                  (append (cdr (assoc new-child dag)) (list (nth j assigned)))))))
      (setf assigned (append assigned (list new-child))
            (cdr (assoc new-child dag))
            (sort (cdr (assoc new-child dag)) #'<)))
    (setf dag (sort dag #'< :key #'car))
    dag))
