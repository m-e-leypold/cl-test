;;;
;;;  cl-test -- another test framework for common lisp.
;;;  Copyright (C) 2022  M E Leypold
;;;  
;;;  This program is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation, either version 3 of the License, or
;;;  (at your option) any later version.
;;;  
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;  
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;;  For alternative licensing options, see README.md

(in-package :de.m-e-leypold.cl-test/loading-ramp)
(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Package definition  --------------------------------------------------------------------------

(define-package :de.m-e-leypold.cl-test/test-procedures
  
    "This package defines the `TEST' abstraction.

     TODO: Explain more, refer to other packages. No define-test, though.
    "
  (:export :register-test :get-tests :get-test-ids :do-tests)
  (:import-from :de.m-e-leypold.cl-test/test-suites
   :get-or-create-suite :add-test :get-suites))

(in-package :de.m-e-leypold.cl-test/test-procedures)

;;; * defclass TEST-DESCRIPTOR ---------------------------------------------------------------------

(defclass test-descriptor ()
  ((test-id
    :reader test-id
    :initarg :test-id
    :initform (error "TEST-ID is required for TEST-DESCRIPTOR")
    :documentation "ID of the test, a symbol")))

;;; * Test registration ----------------------------------------------------------------------------

(defun register-test (symbol)
  "
  Register symbol in `TEST-SUITE', implicitely define SYMBOL-PACKAGE to be a test suite.
"
  (let ((package (symbol-package symbol)))
    (assert (eq package *package*) nil
	    (format t "*** (SYMBOL-PACKAGE '~S) = ~S is not *PACKAGE* = ~S when registering test"
		    symbol package *package*))

    (let ((test-descriptor (make-instance 'test-descriptor :test-id symbol)))
      (setf (get symbol 'test-descriptor) test-descriptor))
    
    (let ((suite (get-or-create-suite package)))
      (add-test symbol suite))))


(defun get-test-descriptor (symbol)
  (get symbol 'test-descriptor))

;;; * Getting at the test list ---------------------------------------------------------------------

(defun get-tests ()
  (funcall #'concatenate 'cons
	   (mapcar #'(lambda (suite)
		       (mapcar #'(lambda (id) (get-test-descriptor id))
			       (de.m-e-leypold.cl-test/test-suites:get-test-ids suite)))
		   (get-suites))))


;; TODO: Call this get-all-* etc to enable debugging with slime ?!


(defun get-test-ids ()
  "Returns a new list of all test ids in the order of the definitions of the respective tests."
  (apply #'concatenate 'cons
	   (mapcar #'(lambda (suite)
		       (de.m-e-leypold.cl-test/test-suites:get-test-ids suite))
		   (get-suites))))

(defmacro do-tests ((test-var) &body body)
  )



;; TODO: TEST-DESCRIPTOR (mostly as container for flags)
;; TODO: get-suite (from some signifier)
;; TODO: get-tests (null/:all) => gets us all tests
;; TODO: do-tests (suite var ...)

