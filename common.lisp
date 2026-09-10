(defun predict-price (mileage theta0 theta1)
  (+ theta0 (* theta1 mileage)))

(defun normalize-param (param min max)
  (float (/ (- param min)
            (- max min)) 1d0))

(defun real-command-line ()
  "The process's actual command line. Unlike `sb-ext:*posix-argv*', it still
contains `--script' and the script name."
  #-linux '()
  #+linux
  (ignore-errors
    (with-open-file (in "/proc/self/cmdline")
      (let ((raw (read-line in)))
        (loop for start = 0 then (1+ end)
              for end = (position #\nul raw :start start)
              collect (subseq raw start (or end (length raw)))
              while end)))))

(defun invoked-as-script-p ()
  (member "--script" (real-command-line) :test #'string=))
