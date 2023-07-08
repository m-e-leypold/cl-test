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

(define-package :de.m-e-leypold.cl-test/emacs-api

    "TODO: Explain logging."

  (:export
   :get-test-tags
   :run-tests))

(in-package :de.m-e-leypold.cl-test/emacs-api)

(defun get-test-tags (suites)
  '(:foo :bar :baz))

(defvar *status-output* nil)

(defun run-tests (&key status-output select-tags)
  (let ((*status-output*
	  (open status-output :direction :output :if-does-not-exist :error)))
    (format *status-output* "~&status-output => ~S~%select-tags => ~S~%"
	    status-output select-tags)
    (finish-output *status-output*)))
    


  
  
