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
  (:export :define-test*))

(in-package :de.m-e-leypold.cl-test/assert-based)

;;; * Defining tests  ------------------------------------------------------------------------------

(defun doc-string-&-body (maybe-doc-string body)
  (if (typep maybe-doc-string '(SIMPLE-ARRAY CHARACTER))
      (values maybe-doc-string body)
      (values nil (cons maybe-doc-string body))))

(defmacro define-test*
    (name (&rest empty-list) maybe-doc-string &body body)
  (assert (not empty-list) nil
	  (format nil
		  "lambda list passed to (define-test ~A ...) must be empty, is ~S instead"
		  name
		  empty-list))
  (multiple-value-bind (doc-string body) (doc-string-&-body maybe-doc-string body)
    `(progn
       (defun ,name ()
	 ,doc-string
	 (progn           ;; will convert simple error to test failure later [TODO]
	   ,@body))
       )))
    
	 
	 

