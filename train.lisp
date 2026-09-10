#-quicklisp (load "~/quicklisp/setup.lisp")
(ql:quickload :split-sequence :silent t)
(load "./common.lisp")

(defvar *theta0* 0 "Intercept.")
(defvar *theta1* 0 "Slope.")
(defvar *learning-rate* 0.01)
(defvar *min-gradient-step-size* 1d-9 "Condition for gradient termination.")

(defmacro megabytes (n) `(* ,n 1024 1024))

(defun load-csv (&optional (path "./data.csv"))
  (with-open-file (input path)
    (let ((len (file-length input)))
      (when (>= len (megabytes 20))
        (error "File ~a exceeds the 20MB limit (~:d bytes)." path len))
      (let ((out (make-string len)))
        (read-sequence out input)
        out))))

(defun parse-csv (data)
  (loop for line in (butlast (rest (split-sequence:split-sequence #\newline data)))
        collect (mapcar #'parse-integer
                        (split-sequence:split-sequence #\, line))))

(defun normalize-data (data)
  "Normalizes the only independent variable, `mileage'. This helps to avoid overflow."
  (multiple-value-bind (min-mileage max-mileage)
      (loop for (mileage _) in data
            minimize mileage into min-mileage
            maximize mileage into max-mileage
            finally (return (values min-mileage max-mileage)))

    (values min-mileage max-mileage
            (loop for (mileage price) in data
                  collect (list (normalize-param mileage min-mileage max-mileage)
                                price)))))

(defun gradient-descent-step (data tmp-theta0 tmp-theta1)
  (loop with m = (length data)
        for (mileage price) in data
        for residual = (- (predict-price mileage tmp-theta0 tmp-theta1) price)
        sum residual into sum-of-residuals-intercept
        sum (* residual mileage) into sum-of-residuals-slope
        finally (return (values (* *learning-rate* (/ sum-of-residuals-intercept m))
                                (* *learning-rate* (/ sum-of-residuals-slope m))))))

(defun main ()
  (multiple-value-bind (min-mileage max-mileage normalized-data)
      (normalize-data (parse-csv (load-csv)))
    (loop for (derivative0 derivative1) = (multiple-value-list (gradient-descent-step normalized-data *theta0* *theta1*))
          until (and (< (abs derivative0) *min-gradient-step-size*)
                     (< (abs derivative1) *min-gradient-step-size*))
          do (decf *theta0* derivative0)
             (decf *theta1* derivative1)
          finally (with-open-file (output "model"
                                          :direction :output
                                          :if-exists :supersede)
                    (with-standard-io-syntax
                      (print (list :min-mileage min-mileage
                                   :max-mileage max-mileage
                                   :theta0 *theta0*
                                   :theta1 *theta1*) output))))))


(defun run ()
  (exit :code
        (handler-case (progn (main) 0)
          (file-error (c)
            (format *error-output* "~a~%" c) 1)
          (parse-error (c)
            (format *error-output* "Malformed data: ~a~%" c) 2)
          (error (c)
            (format *error-output* "Unexpected: ~a~%" c) 3))))

(when (invoked-as-script-p)
  (run))
