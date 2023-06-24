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

;;; * Package definition  --------------------------------------------------------------------------

(define-package :de.m-e-leypold.cl-test/assert-based
    "TODO assert-based"
  (:export :define-test* :skip-test)
  (:import-from :de.m-e-leypold.cl-test/test-suites
   :defvar-suite-symbol)
  (:import-from :de.m-e-leypold.cl-test/test-procedures
   :register-test)
  (:import-from :de.m-e-leypold.cl-test/execution
   :*force-debug*)
  (:use
   :de.m-e-leypold.cl-test/conditions
   :de.m-e-leypold.cl-test/macro-tools))

(in-package :de.m-e-leypold.cl-test/assert-based)

;;; * Defining tests  ------------------------------------------------------------------------------

(defun doc-string-&-body (maybe-doc-string body)
  (if (typep maybe-doc-string '(SIMPLE-ARRAY CHARACTER))
      (values maybe-doc-string body)
      (values nil (cons maybe-doc-string body))))

(define-condition failed-assertion (failed-check)
  ())

(defmacro define-test* (name (&rest empty-lambda-list)  &body maybe-docstring+body)

  (assert (not empty-lambda-list) nil
	  (format nil "lambda list for DEFINE-TEST* must be emtpy, but is: ~S"
		  empty-lambda-list))

  (multiple-value-bind (body docstring options)
      (split-docstring+options+body maybe-docstring+body :allowed-tags '(:tags))

    (let ((tags (get-option :tags options :concatenate t)))

      `(progn
	 ,(defvar-suite-symbol name)
	 (export (quote ,name))    ; TODO: Change: Do not export automatically.

	 (defun ,name ()
	   ,docstring

	   (handler-bind
	       ((error
		  #'(lambda (e)
		      (if (not *force-debug*)
			  (error 'failed-assertion :cause e)))))
	     ,@body))


	 ;; TODO: Pass tags into register-test
	 
	 (register-test (quote ,name) :tags (quote ,tags))))))
