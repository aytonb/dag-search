(uiop:define-package #:dag-search/deep-gp-test
    (:use #:cl
          #:gaussian-process
          ;#:dag-search/discrete-score
          ;#:dag-search/calculate-discrete-scores
          #:dag-search/score/all
                                        ;#:dag-search/score/deep-gp
          ;#:dag-search/score/consistent-score
          ;#:dag-search/score/likelihood-from-score
          #:dag-search/search-node/all
          #:dag-search/abc/all)
  (:export ))

(in-package #:dag-search/deep-gp-test)


;; Simulate a 5 variable example, with
;;   A   B
;;   \  /|
;;    C  |
;;   / \ |
;;  D   E
(defun make-deep-gp-5-variable-data (n-data file-name)
  (let* ((noise-var 0.1)
         (A-var-1 1.5)
         (A (loop for i below n-data
                  collect (* (sqrt A-var-1)
                             (random-gaussian))))
         (B-var-1 0.9)
         (B (loop for i below n-data
                  collect (* (sqrt B-var-1)
                             (random-gaussian))))
         (C nil)
         (C-var-1 1)
         (C-len-1 4)
         (C-var-2 0.5)
         (C-len-2 2.5)
         (C-gp (make-instance 'gp
                              :covariance (lambda (x y)
                                            (+ (* C-var-1
                                                  (exp (- (/ (expt (- (first x)
                                                                      (first y))
                                                                   2)
                                                             (* 2 (expt C-len-1
                                                                        2))))))
                                               (* C-var-2
                                                  (exp (- (/ (expt (- (second x)
                                                                      (second y))
                                                                   2)
                                                             (* 2 (expt C-len-2
                                                                        2))))))))))
         (D nil)
         (D-var-1 1.3)
         (D-len-1 6)
         (D-gp (make-instance 'gp
                              :covariance (lambda (x y)
                                            (* D-var-1
                                               (exp (- (/ (expt (- (first x)
                                                                   (first y))
                                                                2)
                                                          (* 2 (expt D-len-1
                                                                     2)))))))))
         (E nil)
         (E-var-1 0.4)
         (E-len-1 3)
         (E-var-2 0.21)
         (E-len-2 0.4)
         (E-gp (make-instance 'gp
                              :covariance (lambda (x y)
                                            (+ (* E-var-1
                                                  (exp (- (/ (expt (- (first x)
                                                                      (first y))
                                                                   2)
                                                             (* 2 (expt E-len-1
                                                                        2))))))
                                               (* E-var-2
                                                  (exp (- (/ (expt (- (second x)
                                                                      (second y))
                                                                   2)
                                                             (* 2 (expt E-len-2
                                                                        2)))))))))))
    
    (loop for A-el in A
          for B-el in B
          do (destructuring-bind (pred-mean pred-var)
                 (single-predict C-gp (list A-el B-el))
               (let* ((obs-var (+ pred-var noise-var))
                      (pred (+ pred-mean
                               (* (sqrt obs-var) (random-gaussian)))))
                 (setf C (nconc C (list pred)))
                 (add-measurement C-gp (list A-el B-el) pred (sqrt noise-var)))))
    (loop for C-el in C
          do (destructuring-bind (pred-mean pred-var)
                 (single-predict D-gp (list C-el))
               (let* ((obs-var (+ pred-var noise-var))
                      (pred (+ pred-mean
                               (* (sqrt obs-var) (random-gaussian)))))
                 ;(format t "At C = ~a, pred (~a,~a) at ~a~%" C-el pred-mean pred-var pred)
                 (setf D (nconc D (list pred)))
                 (add-measurement D-gp (list C-el) pred (sqrt noise-var)))))
    (loop for B-el in B
          for C-el in C
          do (destructuring-bind (pred-mean pred-var)
                 (single-predict E-gp (list B-el C-el))
               (let* ((obs-var (+ pred-var noise-var))
                      (pred (+ pred-mean
                               (* (sqrt obs-var) (random-gaussian)))))
                 (setf E (nconc E (list pred)))
                 (add-measurement E-gp (list B-el C-el) pred (sqrt noise-var)))))

    (with-open-file (s (asdf:system-relative-pathname
                        :dag-search
                        (concatenate 'string "./data/" file-name))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (loop for A-el in A
            for B-el in B
            for C-el in C
            for D-el in D
            for E-el in E
            do (format s "~a ~a ~a ~a ~a~%" A-el B-el C-el D-el E-el)))))


(defun random-gaussian ()
  (let ((u1 (random 1.0))
        (u2 (random 2.0)))
    (* (sqrt (- (* 2 (log u1))))
       (cos (* 2 PI u2)))))


(defun deep-gp-test-structure-n-100-scores (file-name)
  (let* ((data (make-instance 'gp-data :path (asdf:system-relative-pathname :dag-search (concatenate 'string "./data/" file-name)) :n 100 :noise-var 0.1)))
    (multiple-value-bind (scores-alist times-alist)
        (calculate-all-scores data :no-prune t :score-function :gp-aic)
      (list scores-alist times-alist))))


;; This was optimized with 5 latent
(defun deep-gp-test-structure-1-n-100-test-1 ()
  (let* (;(data (make-instance 'gp-data :path (asdf:system-relative-pathname :dag-search "./data/deep-gp-1.txt") :n 100 :noise-var 0.1))
         (dag-score (make-instance 'gp-aic-score :n-vars 5))
         scores-alist
         times-alist
         consistent-scores-alist
         lik-alist
         (all-scores nil)
         (all-scores-time 0)
         (abc-scores-time 0)
         solution-time
         (all-lik nil)
         hash
         initial-node
         compute-func
         lik-compute-func
         out
         (k-groups ;; (list (list 0 1 2) (list 3 4))
           '((0) (1) (2) (3) (4))))

    (setf scores-alist
          '((4 ((0 1 2 3) . -53.89923802842335d0)
             ((0 2 3) . -55.89811692427328d0)
             ((0 1 2) . -54.344567314393494d0)
             ((0 1 3) . -50.09947858748864d0)
             ((1 2 3) . -55.903437583312105d0)
             ((2 3) . -57.89834914580095d0)
             ((1 2) . -56.53740079421068d0)
             ((1 3) . -51.45039079603245d0)
             ((0 1) . -51.405793154663d0)
             ((0 2) . -57.895223514605135d0)
             ((0 3) . -51.315053515865415d0)
             ((0) . -52.95718542532364d0)
             ((1) . -25.283416194012652d0)
             ((2) . -57.429423761175215d0)
             ((3) . -51.637387443173d0)
             (NIL . 35.60639294860169d0))
            (3 ((0 1 2 4) . -56.97124317521266d0)
             ((0 2 4) . -59.1316446140096d0)
             ((0 1 2) . -56.56888157811644d0)
             ((0 1 4) . -56.531562295075375d0)
             ((1 2 4) . -59.12998557426228d0)
             ((2 4) . -61.132305675411985d0)
             ((1 2) . -60.7880773795989d0)
             ((1 4) . -58.554522547079706d0)
             ((0 1) . -57.99292101226871d0)
             ((0 2) . -58.738137362159634d0)
             ((0 4) . -58.52944378121964d0)
             ((0) . -58.29437340660055d0)
             ((1) . -42.49951843673598d0)
             ((2) . -58.13628036156236d0)
             ((4) . -57.05869060482161d0)
             (NIL . -19.79975119594272d0))
            (2 ((0 1 3 4) . -62.67729704588912d0)
             ((0 3 4) . -40.2565303728222d0)
             ((0 1 3) . -64.57889461753763d0)
             ((0 1 4) . -63.90948444320716d0)
             ((1 3 4) . -41.51042584315836d0)
             ((3 4) . -42.94829655706329d0)
             ((1 3) . -41.64938070706501d0)
             ((1 4) . -41.56613346980523d0)
             ((0 1) . -65.91882925015824d0)
             ((0 3) . -66.68106761783062d0)
             ((0 4) . -65.76647748355826d0)
             ((0) . -67.46987286364475d0)
             ((1) . -36.55475481748205d0)
             ((3) . -39.962719541013236d0)
             ((4) . 95.86943184871716d0)
             (NIL . -40.59747819572606d0))
            (1 ((0 2 3 4) . 46.70393366618203d0)
             ((0 3 4) . 46.4739906655605d0)
             ((0 2 3) . 44.70391827877731d0)
             ((0 2 4) . 47.57921212546172d0)
             ((2 3 4) . 45.52719164362448d0)
             ((3 4) . 45.900225126730376d0)
             ((2 3) . 45.03346129052359d0)
             ((2 4) . 44.89441296781091d0)
             ((0 2) . 42.740272284261884d0)
             ((0 3) . 44.473966696087885d0)
             ((0 4) . 44.76275516411231d0)
             ((0) . 42.76275515455282d0)
             ((2) . 44.119973431925985d0)
             ((3) . 43.249407815375d0)
             ((4) . 45.77488564919518d0)
             (NIL . 43.80386723973327d0))
            (0 ((1 2 3 4) . 47.76413022030205d0)
             ((1 3 4) . 74.97728780824897d0)
             ((1 2 3) . 46.54909734240306d0)
             ((1 2 4) . 46.19293297172187d0)
             ((2 3 4) . 46.01947891130965d0)
             ((3 4) . 72.1035418655572d0)
             ((2 3) . 44.082785012033405d0)
             ((2 4) . 40.748918753964745d0)
             ((1 2) . 44.1926473280951d0)
             ((1 3) . 74.10572593996464d0)
             ((1 4) . 72.2243088844157d0)
             ((1) . 272.02159908724366d0)
             ((2) . 48.24778162240371d0)
             ((3) . 92.46412172424536d0)
             ((4) . 69.13083502980015d0)
             (NIL . 72.98254473864513d0))))

    (setf times-alist
          '((4 ((0 1 2 3) . 171.092)
             ((0 2 3) . 97.164)
             ((0 1 2) . 124.73)
             ((0 1 3) . 113.318)
             ((1 2 3) . 33.086)
             ((2 3) . 89.063)
             ((1 2) . 95.334)
             ((1 3) . 24.101)
             ((0 1) . 60.394)
             ((0 2) . 69.563)
             ((0 3) . 8.458)
             ((0) . 27.085)
             ((1) . 2.01)
             ((2) . 11.01)
             ((3) . 25.066)
             (NIL . 0.0))
            (3 ((0 1 2 4) . 29.257)
             ((0 2 4) . 83.819)
             ((0 1 2) . 32.522)
             ((0 1 4) . 59.365)
             ((1 2 4) . 177.396)
             ((2 4) . 25.231)
             ((1 2) . 48.231)
             ((1 4) . 54.233)
             ((0 1) . 41.466)
             ((0 2) . 13.003)
             ((0 4) . 43.988)
             ((0) . 35.437)
             ((1) . 1.985)
             ((2) . 10.174)
             ((4) . 10.796)
             (NIL . 0.0))
            (2 ((0 1 3 4) . 210.444)
             ((0 3 4) . 17.676)
             ((0 1 3) . 19.196)
             ((0 1 4) . 130.706)
             ((1 3 4) . 21.928)
             ((3 4) . 98.85)
             ((1 3) . 15.747)
             ((1 4) . 42.052)
             ((0 1) . 34.729)
             ((0 3) . 49.945)
             ((0 4) . 58.521)
             ((0) . 32.018)
             ((1) . 1.908)
             ((3) . 8.462)
             ((4) . 1.507)
             (NIL . 0.0))
            (1 ((0 2 3 4) . 3.491)
             ((0 3 4) . 0.857)
             ((0 2 3) . 0.922)
             ((0 2 4) . 0.212)
             ((2 3 4) . 1.095)
             ((3 4) . 3.505)
             ((2 3) . 29.438)
             ((2 4) . 7.786)
             ((0 2) . 0.717)
             ((0 3) . 0.34)
             ((0 4) . 0.388)
             ((0) . 0.074)
             ((2) . 0.154)
             ((3) . 0.094)
             ((4) . 0.144)
             (NIL . 0.0))
            (0 ((1 2 3 4) . 48.269)
             ((1 3 4) . 1.984)
             ((1 2 3) . 2.997)
             ((1 2 4) . 53.078)
             ((2 3 4) . 40.528)
             ((3 4) . 1.965)
             ((2 3) . 12.961)
             ((2 4) . 3.202)
             ((1 2) . 4.651)
             ((1 3) . 0.799)
             ((1 4) . 3.263)
             ((1) . 0.407)
             ((2) . 4.201)
             ((3) . 0.069)
             ((4) . 3.425)
             (NIL . 0.0))))
            
    (setf consistent-scores-alist (make-scores-consistent scores-alist
                                                          dag-score)
          lik-alist (likelihood-from-score consistent-scores-alist
                                           dag-score))
    

    (loop for (variable . parent-scores) in consistent-scores-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-scores (acons variable hash all-scores)))
    
    (loop for (variable . parent-scores) in lik-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-lik (acons variable hash all-lik)))

    (setf initial-node (make-initial-search-node 5)
          compute-func (lambda (v p) (format t "computing ~a <- ~a~%" v p) (gethash p (cdr (assoc v all-scores))))
          lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
          out (abc-search initial-node dag-score lik-compute-func k-groups))

    (loop for (variable . var-times) in times-alist do
      (loop for (parents . time) in var-times do
        (incf all-scores-time time)))
    (loop for (variable . var-times) in times-alist
          for abc-parents = (cdr (assoc variable (third out) :test #'equalp))
          do (loop for abc-par being the hash-key of abc-parents do
            (incf abc-scores-time (cdr (assoc abc-par var-times :test #'equalp)))))

    (append out (list all-lik abc-scores-time all-scores-time))))


;; This was optimized with 5 latent
(defun deep-gp-test-structure-1-n-100-test-2 ()
  (let* (;(data (make-instance 'gp-data :path (asdf:system-relative-pathname :dag-search "./data/deep-gp-2.txt") :n 100 :noise-var 0.1))
         (dag-score (make-instance 'gp-aic-score :n-vars 5))
         scores-alist
         times-alist
         consistent-scores-alist
         lik-alist
         (all-scores nil)
         (all-scores-time 0)
         (abc-scores-time 0)
         solution-time
         (all-lik nil)
         hash
         initial-node
         compute-func
         lik-compute-func
         out
         (k-groups ;(list (list 0 1 2) (list 3 4))
                   '((0) (1) (2) (3) (4))))

    (setf scores-alist
          '((4 ((0 1 2 3) . -55.64384475578251d0) ((0 2 3) . -57.64085875509029d0)
             ((0 1 2) . -57.596503195509456d0) ((0 1 3) . -53.984190776344214d0)
             ((1 2 3) . -57.64889995284267d0) ((2 3) . -59.6204468370087d0)
             ((1 2) . -59.6477710879665d0) ((1 3) . -53.34085195928272d0)
             ((0 1) . -56.00459494415721d0) ((0 2) . -59.63485682868463d0)
             ((0 3) . -53.92596582839747d0) ((0) . -54.507508340561415d0)
             ((1) . -53.31551140485331d0) ((2) . -59.4057559423839d0)
             ((3) . -53.736013501489786d0) (NIL . -46.62017914961643d0))
            (3 ((0 1 2 4) . -54.19071199803598d0) ((0 2 4) . -55.77546882055749d0)
             ((0 1 2) . -55.98649521996407d0) ((0 1 4) . -55.569257763735784d0)
             ((1 2 4) . -55.66062064650664d0) ((2 4) . -57.6058308455937d0)
             ((1 2) . -58.2399552152417d0) ((1 4) . -56.93302497454313d0)
             ((0 1) . -57.01376786080585d0) ((0 2) . -57.986545083938616d0)
             ((0 4) . -57.00907586280474d0) ((0) . -50.404101395647594d0)
             ((1) . -52.49551859406658d0) ((2) . -54.413751342408716d0)
             ((4) . 4105.748201166527d0) (NIL . 38.55264209425187d0))
            (2 ((0 1 3 4) . -61.045735197753956d0) ((0 3 4) . -38.95205302674637d0)
             ((0 1 3) . -61.79876827728732d0) ((0 1 4) . -50.96725959925447d0)
             ((1 3 4) . -46.12346476970712d0) ((3 4) . -41.04194088016231d0)
             ((1 3) . -41.70051198556541d0) ((1 4) . -47.42196927743376d0)
             ((0 1) . -63.36754972089352d0) ((0 3) . -47.11396245698578d0)
             ((0 4) . -39.616121315923166d0) ((0) . -40.147279824428566d0)
             ((1) . -37.387012910173404d0) ((3) . -31.083139791831925d0)
             ((4) . -18.910307964423012d0) (NIL . 63.90361483863597d0))
            (1 ((0 2 3 4) . 30.77728933247731d0) ((0 3 4) . 43.94855183380389d0)
             ((0 2 3) . 28.776134544214415d0) ((0 2 4) . 29.75686036268743d0)
             ((2 3 4) . 36.812498632777846d0) ((3 4) . 41.916399373915176d0)
             ((2 3) . 35.02086828714569d0) ((2 4) . 34.001581582399794d0)
             ((0 2) . 27.637890441714376d0) ((0 3) . 43.75540018325395d0)
             ((0 4) . 41.277556419427555d0) ((0) . 43.31227856140443d0)
             ((2) . 34.57651925431053d0) ((3) . 42.86230422925407d0)
             ((4) . 42.846211590429185d0) (NIL . 43.052588225290215d0))
            (0 ((1 2 3 4) . 52.77098167382405d0) ((1 3 4) . 75.13414800424643d0)
             ((1 2 3) . 50.77085751729405d0) ((1 2 4) . 52.681792707673814d0)
             ((2 3 4) . 60.48994635759845d0) ((3 4) . 71.86934064459871d0)
             ((2 3) . 58.6964609203246d0) ((2 4) . 59.00744223237982d0)
             ((1 2) . 50.657527645430704d0) ((1 3) . 74.46736738478742d0)
             ((1 4) . 73.18314966658644d0) ((1) . 72.97634343521777d0)
             ((2) . 56.89782892549552d0) ((3) . 76.34684313460292d0)
             ((4) . 71.59823263333121d0) (NIL . 74.0387744686359d0))))

    (setf times-alist
          '((4 ((0 1 2 3) . 240.446) ((0 2 3) . 99.033) ((0 1 2) . 40.394)
             ((0 1 3) . 30.961) ((1 2 3) . 115.957) ((2 3) . 197.194)
             ((1 2) . 176.503) ((1 3) . 9.164) ((0 1) . 35.723)
             ((0 2) . 90.749) ((0 3) . 17.73) ((0) . 19.677) ((1) . 14.123)
             ((2) . 9.666) ((3) . 43.574) (NIL . 0.0))
            (3 ((0 1 2 4) . 224.978) ((0 2 4) . 50.443) ((0 1 2) . 39.098)
             ((0 1 4) . 49.016) ((1 2 4) . 35.365) ((2 4) . 131.713)
             ((1 2) . 153.479) ((1 4) . 4.363) ((0 1) . 8.586)
             ((0 2) . 7.842) ((0 4) . 4.723) ((0) . 5.37)
             ((1) . 6.759) ((2) . 14.257) ((4) . 0.066) (NIL . 0.0))
            (2 ((0 1 3 4) . 25.169) ((0 3 4) . 57.733) ((0 1 3) . 33.521)
             ((0 1 4) . 93.872) ((1 3 4) . 13.889) ((3 4) . 56.103) ((1 3) . 26.182)
             ((1 4) . 55.963) ((0 1) . 27.74) ((0 3) . 33.266) ((0 4) . 5.703)
             ((0) . 11.207) ((1) . 6.858) ((3) . 21.975) ((4) . 7.936) (NIL . 0.0))
            (1 ((0 2 3 4) . 83.809) ((0 3 4) . 4.269) ((0 2 3) . 31.442)
             ((0 2 4) . 40.57) ((2 3 4) . 13.692) ((3 4) . 15.125) ((2 3) . 24.951)
             ((2 4) . 13.443) ((0 2) . 29.274) ((0 3) . 0.564) ((0 4) . 0.945)
             ((0) . 0.802) ((2) . 18.555) ((3) . 0.152) ((4) . 0.376) (NIL . 0.0))
            (0 ((1 2 3 4) . 78.165) ((1 3 4) . 0.876) ((1 2 3) . 37.394)
             ((1 2 4) . 15.539) ((2 3 4) . 3.926) ((3 4) . 4.592) ((2 3) . 23.997)
             ((2 4) . 37.638) ((1 2) . 9.979) ((1 3) . 0.554) ((1 4) . 0.779)
             ((1) . 0.665) ((2) . 17.408) ((3) . 0.245) ((4) . 2.249) (NIL . 0.0))))
            
    (setf consistent-scores-alist (make-scores-consistent scores-alist
                                                          dag-score)
          lik-alist (likelihood-from-score consistent-scores-alist
                                           dag-score))
    

    (loop for (variable . parent-scores) in consistent-scores-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-scores (acons variable hash all-scores)))
    
    (loop for (variable . parent-scores) in lik-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-lik (acons variable hash all-lik)))

    (setf initial-node (make-initial-search-node 5)
          compute-func (lambda (v p) (format t "computing ~a <- ~a~%" v p) (gethash p (cdr (assoc v all-scores))))
          lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
          out (abc-search initial-node dag-score lik-compute-func k-groups))

    (loop for (variable . var-times) in times-alist do
      (loop for (parents . time) in var-times do
        (incf all-scores-time time)))
    (loop for (variable . var-times) in times-alist
          for abc-parents = (cdr (assoc variable (third out) :test #'equalp))
          do (loop for abc-par being the hash-key of abc-parents do
            (incf abc-scores-time (cdr (assoc abc-par var-times :test #'equalp)))))

    (append out (list all-lik abc-scores-time all-scores-time))))


;; This was optimized with 5 latent
(defun deep-gp-test-structure-1-n-100-test-3 ()
  (let* (;(data (make-instance 'gp-data :path (asdf:system-relative-pathname :dag-search "./data/deep-gp-2.txt") :n 100 :noise-var 0.1))
         (dag-score (make-instance 'gp-aic-score :n-vars 5))
         scores-alist
         times-alist
         consistent-scores-alist
         ;; (pruned-scores-alist (calculate-all-scores data :score-function :aic :no-prune nil))
         lik-alist
         (all-scores nil)
         (all-scores-time 0)
         (abc-scores-time 0)
         solution-time
         (all-lik nil)
         hash
         initial-node
         compute-func
         lik-compute-func
         out
         (k-groups ;(list (list 0 1 2)
                   ;      (list 3 4))
           '((0) (1) (2) (3) (4))
           ;'((0 1 2 3 4))
           ))

    (setf scores-alist
          '((4 ((0 1 2 3) . -55.62895341518767d0) ((0 2 3) . -56.79319953339137d0)
             ((0 1 2) . -59.1603728313041d0) ((0 1 3) . -59.154804446859345d0)
             ((1 2 3) . -59.14918235982502d0) ((2 3) . -60.943027784894056d0)
             ((1 2) . -59.953225405350196d0) ((1 3) . -61.12334324201066d0)
             ((0 1) . -61.16132953849633d0) ((0 2) . -61.74708582117579d0)
             ((0 3) . -59.15346598995862d0) ((0) . -60.60998420305366d0)
             ((1) . -59.21185136620202d0) ((2) . 52.97896498570924d0)
             ((3) . -58.48428426527186d0) (NIL . -40.2678065312012d0))
            (3 ((0 1 2 4) . -45.827037198166835d0) ((0 2 4) . -47.82702467146646d0)
             ((0 1 2) . -47.94420064568336d0) ((0 1 4) . -43.44962991148351d0)
             ((1 2 4) . -47.05651160925352d0) ((2 4) . -48.7997509140157d0)
             ((1 2) . -48.04262375855444d0) ((1 4) . -45.74262505540088d0)
   ((0 1) . -48.0565641407137d0) ((0 2) . -48.062779035167445d0)
             ((0 4) . -42.650187911805475d0) ((0) . -42.27879492449499d0)
             ((1) . -8.534142527931794d0) ((2) . 48.532685995121895d0)
             ((4) . -31.0868901966423d0) (NIL . 87.93246346308537d0))
  (2 ((0 1 3 4) . -55.06673336136201d0) ((0 3 4) . -37.95677352435051d0)
   ((0 1 3) . -57.066866976297945d0) ((0 1 4) . -57.70341517522391d0)
   ((1 3 4) . -48.88277340136127d0) ((3 4) . -33.67265164350741d0)
   ((1 3) . -50.88727161173752d0) ((1 4) . -48.71081321856759d0)
   ((0 1) . -59.70471219079643d0) ((0 3) . -39.95734095795277d0)
   ((0 4) . -33.13317639143388d0) ((0) . -33.88182454139858d0)
   ((1) . 38.34011114880555d0) ((3) . -34.38220280107374d0)
   ((4) . -14.463733629318739d0) (NIL . 103.79491312596375d0))
            (1 ((0 2 3 4) . 19.198567797179013d0) ((0 3 4) . 34.389705549888475d0)
             ((0 2 3) . 13.303727356181728d0) ((0 2 4) . 14.05458087035652d0)
             ((2 3 4) . 24.841263393063514d0) ((3 4) . 35.93521769807788d0)
             ((2 3) . 23.439194938820037d0) ((2 4) . 22.669621469237423d0)
             ((0 2) . 12.069507092157687d0) ((0 3) . 33.69661945004842d0)
             ((0 4) . 38.29174630852686d0) ((0) . 37.851581413248994d0)
             ((2) . 29.59616172504505d0) ((3) . 36.96265281801913d0)
             ((4) . 132.8345288620971d0) (NIL . 39.059419562228726d0))
            (0 ((1 2 3 4) . 62.59735287313336d0) ((1 3 4) . 68.95836063521287d0)
             ((1 2 3) . 60.597175774588756d0) ((1 2 4) . 68.90685962998424d0)
             ((2 3 4) . 65.71263943480368d0) ((3 4) . 67.39213556504933d0)
             ((2 3) . 63.43730776945451d0) ((2 4) . 65.45038166877364d0)
             ((1 2) . 67.12785202816004d0) ((1 3) . 67.89954654108487d0)
             ((1 4) . 66.60584157505338d0) ((1) . 70.15798232163247d0)
             ((2) . 70.05781306562142d0) ((3) . 66.23706104259934d0)
             ((4) . 70.98730857729453d0) (NIL . 69.16998116453115d0))))
    
    (setf times-alist
          '((4 ((0 1 2 3) . 2.365) ((0 2 3) . 54.545) ((0 1 2) . 309.693)
             ((0 1 3) . 154.128) ((1 2 3) . 115.309) ((2 3) . 154.373)
             ((1 2) . 86.311) ((1 3) . 96.224) ((0 1) . 58.461)
             ((0 2) . 104.44) ((0 3) . 112.243) ((0) . 22.976) ((1) . 10.871)
             ((2) . 1.905) ((3) . 15.831) (NIL . 0.0))
            (3 ((0 1 2 4) . 63.025) ((0 2 4) . 46.486) ((0 1 2) . 68.943)
             ((0 1 4) . 105.282) ((1 2 4) . 80.717) ((2 4) . 43.492) ((1 2) . 22.298)
             ((1 4) . 40.376) ((0 1) . 28.422) ((0 2) . 2.835) ((0 4) . 18.232)
             ((0) . 11.187) ((1) . 4.785) ((2) . 5.642) ((4) . 8.724) (NIL . 0.0))
            (2 ((0 1 3 4) . 54.203) ((0 3 4) . 75.473) ((0 1 3) . 14.783)
             ((0 1 4) . 86.993) ((1 3 4) . 87.376) ((3 4) . 33.43) ((1 3) . 49.129)
             ((1 4) . 69.67) ((0 1) . 5.996) ((0 3) . 10.357) ((0 4) . 53.215)
             ((0) . 24.304) ((1) . 8.256) ((3) . 15.572) ((4) . 10.059) (NIL . 0.0))
            (1 ((0 2 3 4) . 50.059) ((0 3 4) . 13.205) ((0 2 3) . 30.403)
             ((0 2 4) . 24.632) ((2 3 4) . 10.754) ((3 4) . 3.749) ((2 3) . 4.641)
             ((2 4) . 10.703) ((0 2) . 23.495) ((0 3) . 4.59) ((0 4) . 1.298)
             ((0) . 0.215) ((2) . 11.944) ((3) . 1.521) ((4) . 1.363) (NIL . 0.0))
            (0 ((1 2 3 4) . 26.847) ((1 3 4) . 3.663) ((1 2 3) . 11.174)
             ((1 2 4) . 3.542) ((2 3 4) . 0.845) ((3 4) . 0.827) ((2 3) . 0.948)
             ((2 4) . 6.983) ((1 2) . 3.757) ((1 3) . 7.306) ((1 4) . 3.954)
             ((1) . 13.191) ((2) . 0.389) ((3) . 1.089) ((4) . 0.149) (NIL . 0.0))))
    
    (setf consistent-scores-alist (make-scores-consistent scores-alist
                                                          dag-score)
          lik-alist (likelihood-from-score consistent-scores-alist
                                           dag-score))
    

    (loop for (variable . parent-scores) in consistent-scores-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-scores (acons variable hash all-scores)))
    
    (loop for (variable . parent-scores) in lik-alist do
      (setf hash (make-hash-table :test #'equalp))
      (loop for (parent . score) in parent-scores do
        (setf (gethash parent hash) score))
      (setf all-lik (acons variable hash all-lik)))

    (setf initial-node (make-initial-search-node 5)
          compute-func (lambda (v p) (format t "computing ~a <- ~a~%" v p) (gethash p (cdr (assoc v all-scores))))
          lik-compute-func (lambda (v p) (gethash p (cdr (assoc v all-lik))))
          out (abc-search initial-node dag-score lik-compute-func k-groups))

    (loop for (variable . var-times) in times-alist do
      (loop for (parents . time) in var-times do
        (incf all-scores-time time)))
    (loop for (variable . var-times) in times-alist
          for abc-parents = (cdr (assoc variable (third out) :test #'equalp))
          do (loop for abc-par being the hash-key of abc-parents do
            (incf abc-scores-time (cdr (assoc abc-par var-times :test #'equalp)))))

    (append out (list all-lik abc-scores-time all-scores-time))))
