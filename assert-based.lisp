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
  (:export :define-test*)
  (:import-from :de.m-e-leypold.cl-test/test-suites
   :defvar-suite-symbol)
  (:import-from :de.m-e-leypold.cl-test/test-procedures
   :register-test))		


(in-package :de.m-e-leypold.cl-test/assert-based)

;;; * Defining tests  ------------------------------------------------------------------------------

(defun doc-string-&-body (maybe-doc-string body)
  (if (typep maybe-doc-string '(SIMPLE-ARRAY CHARACTER))
      (values maybe-doc-string body)
      (values nil (cons maybe-doc-string body))))

(defmacro define-test*
    (name (&rest empty-lambda-list)  &body maybe-docstring+body)
  (assert (not empty-lambda-list) nil
	  (format nil "lambda list for DEFINE-TEST* must be emtpy, but is: ~S"
		  empty-lambda-list))
  (let* ((maybe-docstring (car maybe-docstring+body))
	 (body (cdr maybe-docstring+body))
	 (docstring maybe-docstring))
    (if (not (and (typep maybe-docstring 'string) body))
	(progn
	  (setf docstring nil)
	  (setf body maybe-docstring+body)))
    `(progn
       ,(defvar-suite-symbol name)
       (export (quote ,name))
       (defun ,name ()
	 ,docstring

	 ;; TODO: Convert errors in TEST-FAILURE (subclass of)
	 
	 (progn
	   ,@body
	   ))
       (register-test (quote ,name)))))

;;; TODO: Register package as suite
;;; TODO: Automatically export
;;; TODO: Abstract body parsing into parse-function-body and with-parsed-function-body
;;; TODO: Convert escaping conditions to test failures
