(uiop:define-package #:dag-search/greedy/tabu
    (:use #:cl
          #:dag-search/score/all
          #:dag-search/greedy/transform)
  (:export #:tabu-search))

(in-package #:dag-search/greedy/tabu)


(defun tabu-search (dag-score lik-func &key (candidate-dag nil)
                                         (max-restarts 5) (reach-dag nil)
                                         (allowable-fn nil))
  (flet ((compute-func (variable parents)
           (- (penalty dag-score variable parents)
              (funcall lik-func variable parents))))
    (let ((max-tabu-size 10)
          (max-iters 10) ;; 50
          (n-vars (n-vars dag-score))
          (scores nil)
          (best-dag nil)
          best-score
          local-best-dag
          (tabu nil)
          local-result
          random-attempts)

      (when reach-dag
        (setf max-restarts nil))

      ;; Make the no parent dag
      (loop for variable below n-vars do
        (setf scores (acons variable (make-hash-table :test #'equalp) scores)
              best-dag (acons variable nil best-dag)))

      ;; Set the no parent dag as the best dag
      ;; (setf best-dag (sort best-dag #'< :key #'car)
      ;;       best-score (score-dag best-dag scores #'compute-func)
      ;;       local-best-dag (copy-tree best-dag))
      ;; (push best-dag tabu)

      ;; Also evaluate the candidate-dag
      (when candidate-dag
        (setf local-result (score-dag candidate-dag scores #'compute-func))
        ;(when (< local-result best-score)
          (setf best-dag (copy-tree candidate-dag)
                best-score local-result
                local-best-dag (copy-tree candidate-dag))) ;)
      
      ;; Do a number of iterations
      (block search
        (loop for r from 0 do

          (unless (and (equal r 0)
                       candidate-dag)
            (setf random-attempts 0)
            (loop do
              (setf local-best-dag (random-dag n-vars :allowable-fn allowable-fn)
                    local-result (score-dag local-best-dag scores #'compute-func))
              (incf random-attempts)
              (when (or (> random-attempts 4)
                        (not (member local-best-dag tabu :test #'equalp)))
                (return)))
          
            ;; Update best result if local is better
            (when (< local-result best-score)
              (setf best-dag (copy-tree local-best-dag)
                    best-score local-result)))

          ;; Add local best to tabu list
          (push local-best-dag tabu)
          ;; If the tabu list is too big, cut it short
          (when (> (list-length tabu) max-tabu-size)
            (setf tabu (subseq tabu 0 max-tabu-size)))
          
          (dotimes (i max-iters)
            (setf local-result (local-transforms local-best-dag n-vars scores
                                                 tabu #'compute-func
                                                 :allowable-fn allowable-fn)
                  local-best-dag (first local-result))
            ;; Update best result if local is better
            (when (< (second local-result) best-score)
              (setf best-dag (copy-tree local-best-dag)
                    best-score (second local-result)))

            (when (and reach-dag
                       (member best-dag reach-dag :test #'equalp))
              (return-from search))
            
            ;; Add local best to tabu list
            (push local-best-dag tabu)
            ;; If the tabu list is too big, cut it short
            (when (> (list-length tabu) max-tabu-size)
              (setf tabu (subseq tabu 0 max-tabu-size)))
            
            ;;(format t "Iter=~a~%Best DAG = ~a~%Score = ~a~%Current DAG = ~a~%" i best-dag best-score local-best-dag)
            )

          ;; Randomization was here

          (when (and reach-dag
                       (member best-dag reach-dag :test #'equalp))
              (return-from search))
          

          (when (and max-restarts
                     (equal r max-restarts))
            (return-from search))))

      (list best-dag best-score scores))))


