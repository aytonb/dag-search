(uiop:define-package #:dag-search/generate-discrete-data
  (:use #:cl)
  (:export )
  (:documentation "Functions to generate discrete data sets."))

(in-package #:dag-search/generate-discrete-data)


(defun generate-discrete-data (var-parents descriptor path n-data-points)
  "Creates a discrete data set at path.

var-parents is an a-list from var -> parent list.

descriptor is an a-list from var -> hashtable, where the hashtable is parent-combinations -> array of probabilities.

n-data-points is a count of how many data points should be generated."
  (let ((ordering nil)
        data-point
        (n-vars (list-length var-parents)))
    ;; Determine the order in which to generate variables.
    (dotimes (i n-vars)
      (loop for (var . parents) in var-parents do
        ;; var cannot already be ordered.
        (unless (member var ordering)
          ;; All parents must be ordered.
          (when (subsetp parents ordering)
            (setf ordering (append ordering (list var)))))))

    (with-open-file (s path :direction :output :if-does-not-exist :create :if-exists :supersede)
      ;; Generate the data.
      (dotimes (n n-data-points)
        ;; Generate a single data point.
        (setf data-point (make-array n-vars :initial-element -1))
        (loop for var in ordering
              for parents = (cdr (assoc var var-parents))
              for parent-values = (loop for parent in parents
                                        collect (aref data-point parent))
              for var-probs = (gethash parent-values (cdr (assoc var descriptor)))
              for r = (random 1.0)
              for cumulative-value = 0
              do (loop for prob across var-probs
                       for i from 0
                       do (incf cumulative-value prob)
                          (when (<= r cumulative-value)
                            (setf (aref data-point var) i)
                            (return))))
        (loop for var-val across data-point
              for i from 0
              do (if (equal i 0)
                     (format s "~a" var-val)
                     (format s " ~a" var-val)))
        (format s "~%")))
    ordering))


(defun three-variable-example ()
  "In this example, 1 is a child of 0 and 2. Each has values from 0 to 2."
  (let ((var-parents nil)
        (descriptor nil)
        hash)
    (setf var-parents (acons 0 nil var-parents)
          var-parents (acons 1 (list 0 2) var-parents)
          var-parents (acons 2 nil var-parents))
    ;; Variable 0 descriptor
    (setf hash (make-hash-table :test #'equalp)
          (gethash nil hash) (make-array 3 :initial-contents (list 0.4 0.3 0.3))
          descriptor (acons 0 hash descriptor))
    ;; Variable 1 descriptor
    (setf hash (make-hash-table :test #'equalp)
          (gethash (list 0 0) hash) (make-array 3 :initial-contents (list 0.9 0.1 0))
          (gethash (list 0 1) hash) (make-array 3 :initial-contents (list 0.6 0.3 0.1))
          (gethash (list 0 2) hash) (make-array 3 :initial-contents (list 0.3 0.5 0.2))
          (gethash (list 1 0) hash) (make-array 3 :initial-contents (list 0.2 0.1 0.7))
          (gethash (list 1 1) hash) (make-array 3 :initial-contents (list 0.2 0.3 0.5))
          (gethash (list 1 2) hash) (make-array 3 :initial-contents (list 0.2 0.5 0.3))
          (gethash (list 2 0) hash) (make-array 3 :initial-contents (list 0.5 0 0.5))
          (gethash (list 2 1) hash) (make-array 3 :initial-contents (list 0.2 0 0.8))
          (gethash (list 2 2) hash) (make-array 3 :initial-contents (list 0.6 0 0.4))
          descriptor (acons 1 hash descriptor))
    ;; Variable 2 descriptor
    (setf hash (make-hash-table :test #'equalp)
          (gethash nil hash) (make-array 3 :initial-contents (list 0.6 0.1 0.3))
          descriptor (acons 2 hash descriptor))
    (generate-discrete-data var-parents descriptor (asdf:system-relative-pathname :dag-search "./src/three-variable-example.txt") 400)))


(defun four-variable-linear-example ()
  "In this example, 0 -> 1 -> 2 -> 3."
  (let ((var-parents nil)
        (descriptor nil)
        hash)
    (setf var-parents (acons 0 nil var-parents)
          var-parents (acons 1 (list 0) var-parents)
          var-parents (acons 2 (list 1) var-parents)
          var-parents (acons 3 (list 2) var-parents))
    ;; Variable 0 descriptor
    (setf hash (make-hash-table :test #'equalp)
          (gethash nil hash) (make-array 3 :initial-contents (list 0.4 0.2 0.4))
          descriptor (acons 0 hash descriptor))
    ;; Variable 1 descriptor
    (setf hash (make-hash-table :test #'equalp)
          (gethash (list 0) hash) (make-array 3 :initial-contents (list 0.2 0.7 0.1))
          (gethash (list 1) hash) (make-array 3 :initial-contents (list 0.1 0.1 0.8))
          (gethash (list 2) hash) (make-array 3 :initial-contents (list 0.4 0.1 0.5))
          descriptor (acons 1 hash descriptor))
    ;; Variable 2 descriptor
    (setf hash (make-hash-table :test #'equalp)
          (gethash (list 0) hash) (make-array 3 :initial-contents (list 0.3 0.2 0.5))
          (gethash (list 1) hash) (make-array 3 :initial-contents (list 0.2 0.1 0.7))
          (gethash (list 2) hash) (make-array 3 :initial-contents (list 0.3 0.1 0.6))
          descriptor (acons 2 hash descriptor))
    ;; Variable 3 descriptor
    (setf hash (make-hash-table :test #'equalp)
          (gethash (list 0) hash) (make-array 3 :initial-contents (list 0.5 0.4 0.1))
          (gethash (list 1) hash) (make-array 3 :initial-contents (list 0.2 0.2 0.6))
          (gethash (list 2) hash) (make-array 3 :initial-contents (list 0.7 0.1 0.2))
          descriptor (acons 3 hash descriptor))
    (generate-discrete-data var-parents descriptor (asdf:system-relative-pathname :dag-search "./src/four-variable-linear-example.txt") 400)))


(defun condition-breast-cancer-simple ()
  (with-open-file (s (asdf:system-relative-pathname
                      :dag-search "./src/breast-cancer.data")
                     :direction :input
                     :if-does-not-exist :error)
    (with-open-file (sout (asdf:system-relative-pathname
                           :dag-search "./src/breast-cancer-simple.txt")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (let (line)
        (loop for raw-line = (read-line s nil nil)
              while raw-line do
                (setf line (uiop:split-string raw-line :separator (list #\,)))
                (cond
                  ((equalp (nth 0 line) "recurrence-events")
                   (format sout "0"))
                  ((equalp (nth 0 line) "no-recurrence-events")
                   (format sout "1"))
                  (t
                   (error "Error on 0")))
                (cond
                  ((equalp (nth 1 line) "10-19")
                   (format sout " 0"))
                  ((equalp (nth 1 line) "20-29")
                   (format sout " 1"))
                  ((equalp (nth 1 line) "30-39")
                   (format sout " 2"))
                  ((equalp (nth 1 line) "40-49")
                   (format sout " 3"))
                  ((equalp (nth 1 line) "50-59")
                   (format sout " 4"))
                  ((equalp (nth 1 line) "60-69")
                   (format sout " 5"))
                  ((equalp (nth 1 line) "70-79")
                   (format sout " 6"))
                  ((equalp (nth 1 line) "80-89")
                   (format sout " 7"))
                  ((equalp (nth 1 line) "90-99")
                   (format sout " 8"))
                  (t
                   (error "Error on 1")))
                (cond
                  ((equalp (nth 2 line) "lt40")
                   (format sout " 0"))
                  ((equalp (nth 2 line) "ge40")
                   (format sout " 1"))
                  ((equalp (nth 2 line) "premeno")
                   (format sout " 2"))
                  (t
                   (error "Error on 2")))
                (cond
                  ((equalp (nth 3 line) "0-4")
                   (format sout " 0"))
                  ((equalp (nth 3 line) "5-9")
                   (format sout " 1"))
                  ((equalp (nth 3 line) "10-14")
                   (format sout " 2"))
                  ((equalp (nth 3 line) "15-19")
                   (format sout " 3"))
                  ((equalp (nth 3 line) "20-24")
                   (format sout " 4"))
                  ((equalp (nth 3 line) "25-29")
                   (format sout " 5"))
                  ((equalp (nth 3 line) "30-34")
                   (format sout " 6"))
                  ((equalp (nth 3 line) "35-39")
                   (format sout " 7"))
                  ((equalp (nth 3 line) "40-44")
                   (format sout " 8"))
                  ((equalp (nth 3 line) "45-49")
                   (format sout " 9"))
                  ((equalp (nth 3 line) "50-54")
                   (format sout " 10"))
                  ((equalp (nth 3 line) "55-59")
                   (format sout " 11"))
                  (t
                   (error "Error on 3")))
                (cond
                  ((equalp (nth 4 line) "0-2")
                   (format sout " 0"))
                  ((equalp (nth 4 line) "3-5")
                   (format sout " 1"))
                  ((equalp (nth 4 line) "6-8")
                   (format sout " 2"))
                  ((equalp (nth 4 line) "9-11")
                   (format sout " 3"))
                  ((equalp (nth 4 line) "12-14")
                   (format sout " 4"))
                  ((equalp (nth 4 line) "15-17")
                   (format sout " 5"))
                  ((equalp (nth 4 line) "18-20")
                   (format sout " 6"))
                  ((equalp (nth 4 line) "21-23")
                   (format sout " 7"))
                  ((equalp (nth 4 line) "24-26")
                   (format sout " 8"))
                  ((equalp (nth 4 line) "27-29")
                   (format sout " 9"))
                  ((equalp (nth 4 line) "30-32")
                   (format sout " 10"))
                  ((equalp (nth 4 line) "33-35")
                   (format sout " 11"))
                  ((equalp (nth 4 line) "36-39")
                   (format sout " 12"))
                  (t
                   (error "Error on 4")))
                (cond
                  ((equalp (nth 5 line) "yes")
                   (format sout " 0~%"))
                  ((equalp (nth 5 line) "no")
                   (format sout " 1~%"))
                  ((equalp (nth 5 line) "?")
                   (format sout " 2~%"))
                  (t
                   (error "Error on 5, value is ~a" (nth 5 line))))
                
                (format t "~a~%" line))))))


(defun condition-breast-cancer-intermediate ()
  (with-open-file (s (asdf:system-relative-pathname
                      :dag-search "./src/breast-cancer.data")
                     :direction :input
                     :if-does-not-exist :error)
    (with-open-file (sout (asdf:system-relative-pathname
                           :dag-search "./src/breast-cancer-intermediate.txt")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (let (line)
        (loop for raw-line = (read-line s nil nil)
              while raw-line do
                (setf line (uiop:split-string raw-line :separator (list #\,)))
                (cond
                  ((equalp (nth 0 line) "recurrence-events")
                   (format sout "0"))
                  ((equalp (nth 0 line) "no-recurrence-events")
                   (format sout "1"))
                  (t
                   (error "Error on 0")))
                (cond
                  ((equalp (nth 1 line) "10-19")
                   (format sout " 0"))
                  ((equalp (nth 1 line) "20-29")
                   (format sout " 1"))
                  ((equalp (nth 1 line) "30-39")
                   (format sout " 2"))
                  ((equalp (nth 1 line) "40-49")
                   (format sout " 3"))
                  ((equalp (nth 1 line) "50-59")
                   (format sout " 4"))
                  ((equalp (nth 1 line) "60-69")
                   (format sout " 5"))
                  ((equalp (nth 1 line) "70-79")
                   (format sout " 6"))
                  ((equalp (nth 1 line) "80-89")
                   (format sout " 7"))
                  ((equalp (nth 1 line) "90-99")
                   (format sout " 8"))
                  (t
                   (error "Error on 1")))
                (cond
                  ((equalp (nth 2 line) "lt40")
                   (format sout " 0"))
                  ((equalp (nth 2 line) "ge40")
                   (format sout " 1"))
                  ((equalp (nth 2 line) "premeno")
                   (format sout " 2"))
                  (t
                   (error "Error on 2")))
                (cond
                  ((equalp (nth 3 line) "0-4")
                   (format sout " 0"))
                  ((equalp (nth 3 line) "5-9")
                   (format sout " 1"))
                  ((equalp (nth 3 line) "10-14")
                   (format sout " 2"))
                  ((equalp (nth 3 line) "15-19")
                   (format sout " 3"))
                  ((equalp (nth 3 line) "20-24")
                   (format sout " 4"))
                  ((equalp (nth 3 line) "25-29")
                   (format sout " 5"))
                  ((equalp (nth 3 line) "30-34")
                   (format sout " 6"))
                  ((equalp (nth 3 line) "35-39")
                   (format sout " 7"))
                  ((equalp (nth 3 line) "40-44")
                   (format sout " 8"))
                  ((equalp (nth 3 line) "45-49")
                   (format sout " 9"))
                  ((equalp (nth 3 line) "50-54")
                   (format sout " 10"))
                  ((equalp (nth 3 line) "55-59")
                   (format sout " 11"))
                  (t
                   (error "Error on 3")))
                (cond
                  ((equalp (nth 4 line) "0-2")
                   (format sout " 0"))
                  ((equalp (nth 4 line) "3-5")
                   (format sout " 1"))
                  ((equalp (nth 4 line) "6-8")
                   (format sout " 2"))
                  ((equalp (nth 4 line) "9-11")
                   (format sout " 3"))
                  ((equalp (nth 4 line) "12-14")
                   (format sout " 4"))
                  ((equalp (nth 4 line) "15-17")
                   (format sout " 5"))
                  ((equalp (nth 4 line) "18-20")
                   (format sout " 6"))
                  ((equalp (nth 4 line) "21-23")
                   (format sout " 7"))
                  ((equalp (nth 4 line) "24-26")
                   (format sout " 8"))
                  ((equalp (nth 4 line) "27-29")
                   (format sout " 9"))
                  ((equalp (nth 4 line) "30-32")
                   (format sout " 10"))
                  ((equalp (nth 4 line) "33-35")
                   (format sout " 11"))
                  ((equalp (nth 4 line) "36-39")
                   (format sout " 12"))
                  (t
                   (error "Error on 4")))
                (cond
                  ((equalp (nth 5 line) "yes")
                   (format sout " 0"))
                  ((equalp (nth 5 line) "no")
                   (format sout " 1"))
                  ((equalp (nth 5 line) "?")
                   (format sout " 2"))
                  (t
                   (error "Error on 5, value is ~a" (nth 5 line))))
                (cond
                  ((equalp (nth 6 line) "1")
                   (format sout " 0"))
                  ((equalp (nth 6 line) "2")
                   (format sout " 1"))
                  ((equalp (nth 6 line) "3")
                   (format sout " 2"))
                  (t
                   (error "Error on 6, value is ~a" (nth 6 line))))
                (cond
                  ((equalp (nth 7 line) "left")
                   (format sout " 0~%"))
                  ((equalp (nth 7 line) "right")
                   (format sout " 1~%"))
                  (t
                   (error "Error on 7, value is ~a" (nth 7 line))))
                
                (format t "~a~%" line))))))



(defun condition-breast-cancer ()
  (with-open-file (s (asdf:system-relative-pathname
                      :dag-search "./src/breast-cancer.data")
                     :direction :input
                     :if-does-not-exist :error)
    (with-open-file (sout (asdf:system-relative-pathname
                           :dag-search "./src/breast-cancer.txt")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (let (line)
        (loop for raw-line = (read-line s nil nil)
              while raw-line do
                (setf line (uiop:split-string raw-line :separator (list #\,)))
                (cond
                  ((equalp (nth 0 line) "recurrence-events")
                   (format sout "0"))
                  ((equalp (nth 0 line) "no-recurrence-events")
                   (format sout "1"))
                  (t
                   (error "Error on 0")))
                (cond
                  ((equalp (nth 1 line) "10-19")
                   (format sout " 0"))
                  ((equalp (nth 1 line) "20-29")
                   (format sout " 1"))
                  ((equalp (nth 1 line) "30-39")
                   (format sout " 2"))
                  ((equalp (nth 1 line) "40-49")
                   (format sout " 3"))
                  ((equalp (nth 1 line) "50-59")
                   (format sout " 4"))
                  ((equalp (nth 1 line) "60-69")
                   (format sout " 5"))
                  ((equalp (nth 1 line) "70-79")
                   (format sout " 6"))
                  ((equalp (nth 1 line) "80-89")
                   (format sout " 7"))
                  ((equalp (nth 1 line) "90-99")
                   (format sout " 8"))
                  (t
                   (error "Error on 1")))
                (cond
                  ((equalp (nth 2 line) "lt40")
                   (format sout " 0"))
                  ((equalp (nth 2 line) "ge40")
                   (format sout " 1"))
                  ((equalp (nth 2 line) "premeno")
                   (format sout " 2"))
                  (t
                   (error "Error on 2")))
                (cond
                  ((equalp (nth 3 line) "0-4")
                   (format sout " 0"))
                  ((equalp (nth 3 line) "5-9")
                   (format sout " 1"))
                  ((equalp (nth 3 line) "10-14")
                   (format sout " 2"))
                  ((equalp (nth 3 line) "15-19")
                   (format sout " 3"))
                  ((equalp (nth 3 line) "20-24")
                   (format sout " 4"))
                  ((equalp (nth 3 line) "25-29")
                   (format sout " 5"))
                  ((equalp (nth 3 line) "30-34")
                   (format sout " 6"))
                  ((equalp (nth 3 line) "35-39")
                   (format sout " 7"))
                  ((equalp (nth 3 line) "40-44")
                   (format sout " 8"))
                  ((equalp (nth 3 line) "45-49")
                   (format sout " 9"))
                  ((equalp (nth 3 line) "50-54")
                   (format sout " 10"))
                  ((equalp (nth 3 line) "55-59")
                   (format sout " 11"))
                  (t
                   (error "Error on 3")))
                (cond
                  ((equalp (nth 4 line) "0-2")
                   (format sout " 0"))
                  ((equalp (nth 4 line) "3-5")
                   (format sout " 1"))
                  ((equalp (nth 4 line) "6-8")
                   (format sout " 2"))
                  ((equalp (nth 4 line) "9-11")
                   (format sout " 3"))
                  ((equalp (nth 4 line) "12-14")
                   (format sout " 4"))
                  ((equalp (nth 4 line) "15-17")
                   (format sout " 5"))
                  ((equalp (nth 4 line) "18-20")
                   (format sout " 6"))
                  ((equalp (nth 4 line) "21-23")
                   (format sout " 7"))
                  ((equalp (nth 4 line) "24-26")
                   (format sout " 8"))
                  ((equalp (nth 4 line) "27-29")
                   (format sout " 9"))
                  ((equalp (nth 4 line) "30-32")
                   (format sout " 10"))
                  ((equalp (nth 4 line) "33-35")
                   (format sout " 11"))
                  ((equalp (nth 4 line) "36-39")
                   (format sout " 12"))
                  (t
                   (error "Error on 4")))
                (cond
                  ((equalp (nth 5 line) "yes")
                   (format sout " 0"))
                  ((equalp (nth 5 line) "no")
                   (format sout " 1"))
                  ((equalp (nth 5 line) "?")
                   (format sout " 2"))
                  (t
                   (error "Error on 5, value is ~a" (nth 5 line))))
                (cond
                  ((equalp (nth 6 line) "1")
                   (format sout " 0"))
                  ((equalp (nth 6 line) "2")
                   (format sout " 1"))
                  ((equalp (nth 6 line) "3")
                   (format sout " 2"))
                  (t
                   (error "Error on 6, value is ~a" (nth 6 line))))
                (cond
                  ((equalp (nth 7 line) "left")
                   (format sout " 0"))
                  ((equalp (nth 7 line) "right")
                   (format sout " 1"))
                  (t
                   (error "Error on 7, value is ~a" (nth 7 line))))
                (cond
                  ((equalp (nth 8 line) "left_up")
                   (format sout " 0"))
                  ((equalp (nth 8 line) "left_low")
                   (format sout " 1"))
                  ((equalp (nth 8 line) "right_up")
                   (format sout " 2"))
                  ((equalp (nth 8 line) "right_low")
                   (format sout " 3"))
                  ((equalp (nth 8 line) "central")
                   (format sout " 4"))
                  ((equalp (nth 8 line) "?")
                   (format sout " 5"))
                  (t
                   (error "Error on 8, value is ~a" (nth 8 line))))
                (cond
                  ((equalp (nth 9 line) "yes")
                   (format sout " 0~%"))
                  ((equalp (nth 9 line) "no")
                   (format sout " 1~%"))
                  (t
                   (error "Error on 9, value is ~a" (nth 9 line))))
                
                (format t "~a~%" line))))))
