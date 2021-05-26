(uiop:define-package #:dag-search/most-likely
    (:use #:cl)
  (:export #:prob-of-being-min)
  (:documentation "Computation of probabilities that uniform variables are the lowest."))

(in-package #:dag-search/most-likely)


(defun multiply-poly (pa pb)
  "Multiplies the polynomials pa and pb, represented in terms of (a b c) for
ax^2 + bx + c. Assumes pb is linear."
  (let* ((na (list-length pa))
         (prod (make-list (1+ na) :initial-element 0)))
    (loop for i below na do
      (incf (nth i prod) (* (nth i pa) (nth 0 pb)))
      (incf (nth (1+ i) prod) (* (nth i pa) (nth 1 pb))))
    prod))


(defun integrate-poly (p lb ub)
  "Integrates the polynomial from lb to ub."
  (let ((np (list-length p))
        (int 0))
    (loop for coef in p
          for deg from np downto 1 do
            ;; (incf int (* (/ coef deg) (- (expt ub deg) (expt lb deg))))
            (unless (or (equalp coef 0)
                        (equalp lb ub))
              (incf int (* (/ coef deg)
                           (exp (+ (* deg (log ub))
                                   (log (- 1 (expt (/ lb ub) deg))))))))
          )
    int))


(defun prob-of-being-min (sorted-bounds min-ub i)
  "Computes the probability that variable i is the lowest for uniform random
variables with pdfs from lb to ub.

sorted bounds is assumed to be sorted in order of increasing lb."
  (let* ((bounds-i (nth i sorted-bounds))
         (poly (list 0 (/ 1 (- (second bounds-i) (first bounds-i)))))
         (int 0))

    ;; Multiply by factors for every variable with lbs less than i.
    (loop for j from 1 upto i
          for bounds = (nth j sorted-bounds)
          for prev-bounds = (nth (1- j) sorted-bounds)
          for prev-lb = (first prev-bounds)
          for prev-ub = (second prev-bounds)
          for lb = (first bounds)
          for ub = (second bounds) do
            (setf poly (multiply-poly poly (list (- (/ 1 (- prev-ub prev-lb)))
                                                 (/ prev-ub (- prev-ub prev-lb))))))

    ;; Perform an integration for every variable after i.
    (loop for j from (1+ i) below (list-length sorted-bounds)
          for prev-bounds = (nth (1- j) sorted-bounds)
          for prev-lb = (first prev-bounds)
          for bounds = (nth j sorted-bounds)
          for lb = (first bounds)
          for ub = (second bounds) do
            (incf int (integrate-poly poly prev-lb lb))

            ;; Update the poly with the current variable
            (setf poly (multiply-poly poly (list (- (/ 1 (- ub lb)))
                                                 (/ ub (- ub lb))))))

    ;; If this is the last one, integrate up to min-ub
    (incf int (integrate-poly poly (first (first (last sorted-bounds))) min-ub))

    int))


(defun test-prob-of-being-min ()
  ;; Should be 9/20
  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 3 6))
                                      6 0))
  ;; Should be 11/20
  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 3 6))
                                      6 1))

  ;; Should be 83/200
  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 2 12)
                                            (list 3 6))
                                      6 0))
  ;; Should be 33/200
  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 2 12)
                                            (list 3 6))
                                      6 1))
  ;; Should be 21/50
  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 2 12)
                                            (list 3 6))
                                      6 2))

  ;; Should be 2213/5400
  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 2 12)
                                            (list 3 6)
                                            (list 4 10))
                                      6 0))

  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 2 12)
                                            (list 3 6)
                                            (list 4 10))
                                      6 1))
  
  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 2 12)
                                            (list 3 6)
                                            (list 4 10))
                                      6 2))

  (format t "~a~%" (prob-of-being-min (list (list 0 10)
                                            (list 2 12)
                                            (list 3 6)
                                            (list 4 10))
                                      6 3)))

;; Bounds are ((608.21606 718.1598 (0 2 5)) (608.21606 629.76324 (0 2))
;;             (608.21606 626.4855 (0 5)) (609.5498 657.46405 (2 5))
;;             (609.5498 621.85614 (2)) (609.5498 616.8407 (5)))
;; i = 0 p = 62.261215
;; i = 1 p = 66.55916
;; i = 2 p = 61.070198
;; i = 3 p = 61.5
;; i = 4 p = 65.5
;; i = 5 p = 66.0
