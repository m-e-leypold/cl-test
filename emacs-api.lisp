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
   :run)

  (:import-from :de.m-e-leypold.cl-test/test-procedures
   :get-tags :do-tests)

  (:import-from :de.m-e-leypold.cl-test/execution
   :run-tests :with-new-excution-state)

  (:import-from :de.m-e-leypold.cl-test/test-logging
   :*test-console-stream*))

(in-package :de.m-e-leypold.cl-test/emacs-api)

(defun get-test-tags (&optional suites)
  (let ((tags '()))
    (do-tests (test)
      (do*
       ((test-tags (get-tags test) (cdr test-tags))
	(tag (car test-tags) (car test-tags)))
       ((not test-tags) tags)
	(pushnew tag tags)))
    tags))
	 
(defvar *status-output* nil)
(defvar *log-output* nil)

;; TODO: Install event handler, build display from test plan, update dependent on test results.

(defun run (&key status-output select-tags log-output)
  (let ((*status-output*
	  (open status-output :direction :output :if-does-not-exist :error)))
    (let ((*log-output*
	    (open log-output :direction :output :if-does-not-exist :error)))

      (dolist (output (list *status-output* *log-output*))
	(format output "~%")
	(format output "Selected: ~S~%" select-tags)
	(format output "Start:    ~A~%" (local-time:now))
	(format output "End:      ~A~%" "--")
	(format output "~%")      
	(finish-output output))

      (let ((*standard-output* *log-output*)
	    (*error-output* *log-output*)
	    (*test-console-stream* *log-output*))
	
	(format t "running tests now ...")
	(finish-output)
	(run-tests :select select-tags)
	
	;; TODO: Output approximate end time, requires "move-to-line"
	
	(close *standard-output*)
	(close *error-output*)))))
    
;; Note: Alternative output method for log: swank-buffer-streams, see cl-test-el demo.

#+nil (defun test-run (test-sets)
	(let ((*log-output* (swank-buffer-streams:make-buffer-output-stream :test-log)))
	  (format *log-output* "Tests sets ~A~%" test-sets)
	  (finish-output *log-output*)
	  (close *log-output*)))



  
  
