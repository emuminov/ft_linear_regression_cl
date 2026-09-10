(load "./common.lisp")

(defun ask-user-for-mileage ()
  (format *query-io* "Please enter the mileage: ")
  (force-output *query-io*)
  (parse-integer (read-line *query-io*)))

(defun load-model ()
  (with-open-file (in "./model")
    (with-standard-io-syntax
      (read in))))

(defun main ()
  (let ((mileage (ask-user-for-mileage)))
    (destructuring-bind (&key theta0 theta1 min-mileage max-mileage) (load-model)
      (format t "Estimated price: ~,2f~%"
              (predict-price (normalize-param mileage min-mileage max-mileage)
                             theta0 theta1)))))

(defun run ()
  (exit :code
        (handler-case (progn (main) 0)
          (parse-error ()
            (format *error-output* "Not an integer.~%") 1)
          (file-error (c)
            (format *error-output* "~a~%" c) 2)
          (error (c)
            (format *error-output* "Unexpected: ~a~%" c) 3))))


(when (invoked-as-script-p)
  (run))
