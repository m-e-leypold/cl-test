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

(define-package :de.m-e-leypold.cl-test/test-suites

    "This package defines the `TEST-SUITE' abstraction.

     TODO: Explain more, refer to other packages.
    "
  (:export :with-new-suite-registry))

(in-package :de.m-e-leypold.cl-test/test-suites)

;;; * Suite registry -------------------------------------------------------------------------------

(defparameter *suites-package*  (find-package "DE.M-E-LEYPOLD.CL-TEST/SUITES"))
(defparameter *suites*          '())
(defparameter *suite-instances* (make-hash-table))

(defmacro with-new-suite-registry ((&optional package) &body body)
  `(let ((*suites*          '())
	 (*suite-instances* (make-hash-table))
	 (*suites-package*  ,package))
     ,@body))

;;; * defclass TEST-SUITE  -------------------------------------------------------------------------

(defclass test-suite ()
  ((suite-id
    :type symbol
    :reader suite-id
    :initarg :suite-id
    :initform (error "SUITE-ID is required for TEST-SUITE")
    :documentation "ID of a suite, a symbol from package :KEYWORD")
   (suite-symbol
    :type symbol
    :reader suite-symbol
    :initarg :suite-symbol
    :initform (error "SUITE-SYMBOL is required for TEST-SUITE")
    :documentation "Symbol in *SUITE-PACKAGE* to which the suite is bound")
   (suite-tests
    :type (or (cons symbol) null)
    :accessor suite-tests
    :initform '()
    :documentation "Tests in the suite, as symbols")
   ))

;; TODO: Printing of test suites
;; TODO: Also capture package

(let ((keyword-package (find-package :keyword)))

  (defun get-or-create-suite (package)
    (let* ((package-name (package-name package))
	   (suite-id (intern package-name keyword-package))
	   (test-suite (gethash suite-id *suite-instances* nil)))

      ;; Do we have the suite already?

      (if test-suite
	  test-suite

	  ;; Otherwise we have to create a TEST-SUITE instance and bind it.

	  (let* ((package-symbol
		   (if *suites-package*
		       (intern package-name *suites-package*)
		       nil)))

	    (if package-symbol
		(progn
		  (proclaim `(special ,package-symbol))	   
		  (export (list package-symbol) *suites-package*)))
	    
	    #+nil (format t "Registering: ~S~%" package-symbol) ; TODO: Need a logging package
	    
	    (let ((test-suite
		    (make-instance 'test-suite :suite-id suite-id :suite-symbol package-symbol)))
	      (if package-symbol
		  (setf (symbol-value package-symbol) test-suite))
	      (pushnew suite-id *suites*)
	      (setf (gethash suite-id *suite-instances*) test-suite)
	      (format t "*suites* => ~S~%" *suites*) ; TODO: Need a logging package
	      test-suite))))))


(declaim (ftype (function (symbol test-suite)) add-test))

(defun add-test (test-symbol suite)
  (pushnew test-symbol (slot-value suite 'suite-tests)))

