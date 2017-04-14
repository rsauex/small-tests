;;;; small testing framework as in PCL

(in-package #:small-tests)

(defparameter *tests* (make-hash-table)
  "Global table of registered tests.")

(defun register-test (test-name)
  (pushnew test-name (gethash (package-name (symbol-package test-name)) *tests*)))

(defmacro deftest (test-name &body body)
  "Defines a function with name 'test-name' which is puhed into the test table.
The test is concidered to be failing if it throws any error.
The body of the function can contain any forms and documentation string as an
ordinary function."
  `(progn
     (eval-when (:load-toplevel)
       (small-tests::register-test ',test-name))
     (defun ,test-name ()
       ,@body)))

(defun run-test (test)
  "Runs test named 'test' and handles the errors in it. If any error occured then
the test is concidered as failing, otherwise as passing."
  (handler-case
      (funcall test)
    (error () (return-from run-test nil)))
  t)

(defun run-tests (&key package)
  "Runs all the test in all the packages or in the package with name 'package-name'."
  (let (failed
        (failed-num 0)
        (all-num 0)
        (columns 0))
    (labels ((%report (test-res)
               (when (> columns 45)
                 (format t "~%")
                 (setq columns 0))
               (if test-res
                   (princ ".")
                   (princ "x"))
               (incf columns))
             (%run-tests (package tests)
               (format t "--- Tests from ~A:~%" package)
               (dolist (test tests)
                 (incf all-num)
                 (let ((test-res (run-test test)))
                   (%report test-res)
                   (unless test-res
                     (incf failed-num)
                     (push test failed))))
               (if (/= columns 0)
                   (format t "~%~%")
                   (format t "~%"))))
      (if package
          (let ((tests (gethash package *tests*)))
            (if tests
                (%run-tests package tests)
                (format t "No tests in package ~A~%" package)))
          (maphash #'%run-tests *tests*)))
    (when failed
      (format t "~%--- Failing tests (~A from ~A):~%" failed-num all-num)
      (dolist (test failed)
        (format t "~A::~A~%" (package-name (symbol-package test)) test))
      (return-from run-tests nil))
    t))
