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

;;; * define-package -------------------------------------------------------------------------------

(define-package :de.m-e-leypold.cl-test/test-procedures

    "This package defines the `TEST' abstraction.

     TODO: Explain more, refer to other packages. No define-test, though.
    "

  (:export
   :register-test
   :test-id
   :get-tests :get-test-ids :do-tests :get-test-list
   :get-tags)

  (:import-from :de.m-e-leypold.cl-test/test-suites
   :get-or-create-suite :add-test :get-suites :do-suites :do-test-ids :get-suite
   :get-suite-tags))

(in-package :de.m-e-leypold.cl-test/test-procedures)

;;; * defclass TEST-DESCRIPTOR ---------------------------------------------------------------------

(defclass test-descriptor ()
  ((test-id
    :reader test-id
    :initarg :test-id
    :initform (error "TEST-ID is required for TEST-DESCRIPTOR")
    :documentation "ID of the test, a symbol")
   (tags
    :reader tags
    :initarg :tags
    :initform nil
    :documentation "Tags attacvhed to the test, e.g :SMOKE or :EXPENSIVE or :BETA")))

;;; * Test registration: REGISTER-TEST, GET-TEST-DESCRIPTOR ----------------------------------------

(defun register-test (symbol &key tags)
  "
  Register symbol in `TEST-SUITE', implicitely define SYMBOL-PACKAGE to be a test suite.
"
  (let ((package (symbol-package symbol)))
    (assert (eq package *package*) nil
	    (format t "*** (SYMBOL-PACKAGE '~S) = ~S is not *PACKAGE* = ~S when registering test"
		    symbol package *package*))

    (let ((test-descriptor (make-instance 'test-descriptor :test-id symbol :tags tags)))
      (setf (get symbol 'test-descriptor) test-descriptor))

    (let ((suite (get-or-create-suite package)))
      (add-test symbol suite))))


(defun get-test-descriptor (symbol)
  (get symbol 'test-descriptor))

;;; * Getting test, iterating: GET-TESTS, GET-TEST-IDS, DO-TESTS GET-TEST-LIST ---------------------

(defun get-tests ()
  (mapcar #'get-test-descriptor
	  (mapcan #'(lambda (suite)
		      (de.m-e-leypold.cl-test/test-suites:get-test-ids suite))
		  (get-suites))))


;; TODO: Call this get-all-* etc to enable debugging with slime ?!

(defun get-test-ids ()
  "Returns a new list of all test ids in the order of the definitions of the respective tests."
  (apply #'concatenate 'cons
	   (mapcar #'(lambda (suite)
		       (de.m-e-leypold.cl-test/test-suites:get-test-ids suite))
		   (get-suites))))

(defmacro do-tests ((test-var) &body body)
  (let ((suite-var (gensym "suite-var"))
	(id-var (gensym "id-var")))
    `(do-suites (,suite-var)
       (do-test-ids (,id-var ,suite-var)
	 (let ((,test-var (get-test-descriptor ,id-var)))
	   ,@body)))))

(defun get-test-list (&optional (sequence-type 'cons))
  (map sequence-type #'get-test-descriptor  (get-test-ids)))

;;; * Navigating to TEST-SUITE: GET-TEST-SUITE -----------------------------------------------------

(defgeneric get-test-suite (test)
  (:documentation "Get the `TEST-SUITE' of TEST"))

(defmethod get-test-suite ((test-id symbol))
  (get-suite (symbol-package test-id)))

(defmethod get-test-suite ((descriptor test-descriptor))
  (get-test-suite (test-id descriptor)))


;;; * Tags and getting tests by tags ---------------------------------------------------------------

(defgeneric get-tags (test)
  (:documentation "Get tags for TEST"))

(defmethod get-tags ((descriptor test-descriptor))
  (concatenate 'list
	       (get-suite-tags (get-test-suite descriptor))
	       (tags descriptor)))

(defmethod get-tags ((test-id symbol))
  (let ((descriptor (get-test-descriptor test-id)))
    (assert descriptor nil "~S does not have a TEST-DESCRIPTOR" test-id)
    (get-tags descriptor)))

  
