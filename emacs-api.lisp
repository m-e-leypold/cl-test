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
   :run-tests :with-new-excution-state :*test-plan* :run-results :nested-run-p)

  (:import-from :de.m-e-leypold.cl-test/test-logging
   :*test-console-stream* :with-added-test-event-hook))

(in-package :de.m-e-leypold.cl-test/emacs-api)

;;; * Output streams -------------------------------------------------------------------------------

(defvar *status-output* nil)
(defvar *log-output* nil)

;;; * Status display -------------------------------------------------------------------------------

(defparameter *status-display* '())
(defparameter *status-display-vertical-position* 8)
(defparameter *status-display-height* nil)                ;; to be set by build-display

;;; ** Output primitives ---------------------------------------------------------------------------

(defmacro escape (string-form)
  (if (typep string-form 'string)
      (format nil "~A[~A" (code-char 27) string-form)
      `(format nil "~A[~A" (code-char 27) ,string-form)))

(defun  cmd-cursor-position (line &optional (charpos 0))
  (format nil (escape "~a;~aH") line charpos))

(defun move-to (line charpos)
  (write-string (cmd-cursor-position line charpos) *status-output*))

(defun park-cursor ()
  (move-to (+ *status-display-vertical-position*
	      *status-display-height*
	      2)
	   0)
  (finish-output *status-output*))

;; ** Output style ---------------------------------------------------------------------------------

(defvar *status-style* :black&white )

(defgeneric status-string (status-selector style-selector))

(defun status (status)
  (status-string status *status-style*))

(defmacro defstatus ((status style) &body body)
  `(progn
     (let ((string (progn ,@body)))
       (defmethod status-string
	   ((status-selector (eql ,status))(style-selector (eql ,style)))
	 string))))

(defmacro defstatus.bw (status &body body)
    `(defstatus (,status :black&white) ,@body))

(defstatus.bw :queued  "  [      ]  ")
(defstatus.bw :passed  "✓ [PASSED]  ")
(defstatus.bw :failed  "⨯ [FAILED]  ")
(defstatus.bw :skipped "- [SKIPPED] ")
(defstatus.bw :error   "☇ [ERROR]   ")


;; ** Building the status display ------------------------------------------------------------------

(let ((keyword (find-package "KEYWORD")))
  
  (defun build-status-display ()
    (if (not *status-display*)
	(progn
	  (format *log-output* "test-plan => ~S~%" *test-plan*)
	  (let ((*print-case* :downcase))
	    (let ((last-package nil)
		  (position *status-display-vertical-position*))
	      (move-to *status-display-vertical-position* 0)
	      (dolist (test *test-plan*)
		(let ((package (symbol-package test)))
		  (if (not (eq package last-package))
		      (progn			
			(setf last-package package)
			(format *status-output* "  (in-package ~S)~%~%"
				(intern (package-name package) keyword))
			(incf position 2)))
		  (format *status-output* "~&~A ~A~%" (status :queued) test)
		  (push (cons test position) *status-display*)
		  (incf position)
		  (finish-output *status-output*)))
	      (setf *status-display-height* (- position *status-display-vertical-position*))
	      (format *status-output* "~%~%~%")))
	  (park-cursor)))))
	  
      
    
    

;;; * Event handling -------------------------------------------------------------------------------

(defun handle-event (event-type event)
  (if (not (nested-run-p))
      
      (progn
	(format *log-output* "~& => nested: ~A~%" (nested-run-p))
	(finish-output *log-output*)
	;; TODO: Split logging and display update => logging always on!

	
	(ecase event-type
	  (:run-begin
	   (format *log-output* "~&=> ~A ~A~%"
		   event-type event)
	   (build-status-display)
	   (format *log-output* "~&display => ~S~%" *status-display*))
	  (:suite-enter
	   (format *log-output* "~&   (in-package ~S)~%" (package-name (car event))))
	  (:test-begin)
	  ((:passed :failed :error :skipped)
	   (let ((test (cadr event)))
	     (format *log-output* "~&=> ~A ~A~%     ~S~%"
		     event-type (symbol-name test) (cddr event))
	     (move-to (cdr (assoc test *status-display*)) 0)
	     (format *status-output* "~A" (status event-type))
	     (park-cursor)))
	  (:test-end)
	  (:suite-exit)
	  (:run-end      
	   (format *log-output* "~&=> ~A ~A~%"
		   event-type event)
	   (move-to 5 11)
	   (format *status-output* "~A" (local-time:now))
	   (move-to 6 11)       
	   (let ((results (run-results (car event))))
	     (labels ((count-results (what) (length (getf results what))))
	       (format *status-output* "#passed=~A #failed=~A #errors=~A #skipped=~A"
		       (count-results :passed)
		       (count-results :failed)
		       (count-results :error)
		       (count-results :skipped))
	       (format *log-output* "~&~%     ~S~%"  results)))
	   (park-cursor)))
	
	(finish-output *status-output*)
	(finish-output *log-output*))))

;;; * CL-TEST-EL interface -------------------------------------------------------------------------

(defun get-test-tags (&optional suites)
  (let ((tags '()))
    (do-tests (test)
      (do*
       ((test-tags (get-tags test) (cdr test-tags))
	(tag (car test-tags) (car test-tags)))
       ((not test-tags) tags)
	(pushnew tag tags)))
    tags))
	 
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
	(format output "Results:  ~A~%" "--")	
	(format output "~%~%")      
	(finish-output output))

      (let ((discarding-stream (make-broadcast-stream)))
	
	
	(let ((*standard-output* discarding-stream)
	      (*error-output* discarding-stream)
	      (*test-console-stream* discarding-stream))

	  (let ((*status-display* nil)
		(*status-display-height* nil))

	    (with-added-test-event-hook ('handle-event)
	      (run-tests :select select-tags)))
	  
	  ;; TODO: Output approximate end time, requires "move-to-line"

	  (finish-output *log-output*)
	  (close discarding-stream))))))

    
;; Note: Alternative output method for log: swank-buffer-streams, see cl-test-el demo.

;;; (defun test-run (test-sets)
;;;   (let ((*log-output* (swank-buffer-streams:make-buffer-output-stream :test-log)))
;;;     (format *log-output* "Tests sets ~A~%" test-sets)
;;;     (finish-output *log-output*)
;;;     (close *log-output*)))



  
  
