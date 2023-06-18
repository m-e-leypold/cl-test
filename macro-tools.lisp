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

(define-package :de.m-e-leypold.cl-test/macro-tools
    "TODO doctools"

  (:export
   :package-documentation
   :define-package-documentation-anchor))
      
(in-package :de.m-e-leypold.cl-test/macro-tools)

;;; * Handling definition bodies -------------------------------------------------------------------

(defmacro trace-var (var)
  `(format t "~A => ~S~%" (quote ,var) ,var))

(defun split-docstring+options+body (forms &key no-options no-docstring)
  (let ((docstring nil)
	(options nil)
	(body forms))
    (let ((first-form (car body)))

      (if (stringp first-form)
	  (progn
	    (setf docstring first-form)
	    (setf body (cdr body)))))

    (do ((next-item (car body) (car rest))
	 (body body rest)
	 (rest (cdr body) (cdr rest)))
	
	((not (and (consp next-item) (keywordp (car next-item))))

	 (if no-docstring
	     (assert (not docstring) nil "This body cannot have a docstring"))

	 (if no-options
	     (assert (not options) nil "This body cannot have options"))
	 
	 (values
	  (if body body '(progn )) ;; avoid empty body FTM
	  docstring
	  (reverse options)))

      (trace-var next-item)
      (trace-var body)      
      (push next-item options))))

