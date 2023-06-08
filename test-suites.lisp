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
  (:export :register-test))

(in-package :de.m-e-leypold.cl-test/test-suites)

;;; * Suite registry -------------------------------------------------------------------------------

(defparameter *suites-package* (find-package "DE.M-E-LEYPOLD.CL-TEST/SUITES"))
(defparameter *suites*         '())

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

(let ((keyword-package (find-package :keyword)))
  (defun get-or-create-suite (package)
    (let* ((package-name (package-name package))
	   (package-symbol (find-symbol package-name *suites-package*)))

      ;; if we have the package symbol already registered in *SUITES-PACKAGE* and its bound,
      ;; it must be bound to the TEST-SUITE instance.
          
      (if (and package-symbol (boundp package-symbol))
	  (symbol-value package-symbol)

	  ;; Otherwise we have to create TEST-SUITE instance and bind it.
	  
	  (let* ((package-symbol (intern package-name *suites-package*))
		 (suite-id (intern package-name keyword-package)))

	    (proclaim `(special ,package-symbol))	    
	    (export (list package-symbol) *suites-package*) 
	    
	    #+nil (format t "Registering: ~S~%" package-symbol) ; TODO: Need a logging package

	    (let ((test-suite
		    (make-instance 'test-suite :suite-id suite-id :suite-symbol package-symbol)))
	      (setf (symbol-value package-symbol) test-suite)
	      test-suite))))))

;;; * Registering tests  ---------------------------------------------------------------------------

  ;; TODO: Make accessible as global var.
  
(defun register-test (symbol)
  "
  Register symbol in `TEST-SUITE', implicitelky define SYMBOL-PACKAGE to be a test suite.
"
  (let ((package (symbol-package symbol)))
    (assert (eq package *package*) nil
	    (format t "*** (SYMBOL-PACKAGE '~S) = ~S is not *PACKAGE* = ~S when registering test"
		    symbol package *package*))
    (let ((suite (get-or-create-suite package)))

      ;; TODO TEST-SUITE instance, bind
      ;; TODO Put suite in *SUITES*
      ;; TODO RUN-SUITE, bind run-suite wrapper
      ;; TODO: Register symbol with package/suite.         
      )))
