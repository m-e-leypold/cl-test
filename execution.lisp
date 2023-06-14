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

(define-package :de.m-e-leypold.cl-test/execution

    "TODO: Explain execution."

  (:export :make-test-plan :with-new-excution-state)

  (:import-from :de.m-e-leypold.cl-test/test-suites)
  (:import-from :de.m-e-leypold.cl-test/test-procedures
   :do-tests :test-id :continue))

(in-package :de.m-e-leypold.cl-test/execution)


;;; * Execution state  -----------------------------------------------------------------------------

(defvar *test-plan* '())
(defvar *tests-continuation* '())
(defvar *tests-run* '())
(defvar *passed* '())
(defvar *failed* '())
(defvar *errors* '())
(defvar *skipped* '())
(defvar *current-test* '())

(defmacro with-new-execution-state (&body body)
  `(let ((*test-plan* '())
	 (*tests-continuation* '())
	 (*tests-run* '())
	 (*passed* '())
	 (*failed* '())
	 (*errors* '())
	 (*skipped* '())
	 (*current-test* '()))
     ,@body))


;;; * User parameters ------------------------------------------------------------------------------

(defvar *force-debug* nil)

;;; * defun MAKE-TEST-PLAN -------------------------------------------------------------------------

(defun make-test-plan ()
  (let ((tests '()))
    (do-tests (test)
      ;; Here we will select tests later
      (push test tests))
    (nreverse (mapcar #'test-id tests))))

;;; * defun RUN-TESTS -----------------------------------------------------------------------------

(defun get-results ()
  (list
   :passed  *passed*
   :failed  *failed*
   :errors  *errors*
   :skipped *skipped*   
   :total   *tests-run*
   ))


(defun log-test-as-error (test-symbol condition)
  (format t "error => ~S, ~S~%" test-symbol condition)
  (push (cons test-symbol condition) *errors*))


(defun log-test-as-failed (test-symbol condition)

  ;; TODO: Need to unpack real condition from some wrapper
  ;;       conditions.

  (format t "failed => ~S, ~S~%" test-symbol condition)
  (push (cons test-symbol condition) *failed*))

(defun log-test-as-skipped (test-symbol reason)

  ;; TODO: Need to unpack real condition from some wrapper
  ;;       conditions.

  (format t "skipped => ~S, ~S~%" test-symbol reason)
  (push (cons test-symbol reason) *skipped*))


(defun log-test-as-passed (test-symbol)
  (format t "passed => ~S~%" test-symbol)
  (push test-symbol *passed*))


(defun run-tests (&key restart debug)
  (setf *current-test* nil)
  (if (not restart)
      (setf *test-plan* (make-test-plan)))
  (if (not (eq restart :continue))
      (progn (setf *tests-continuation* *test-plan*)
	     (setf *tests-run* '())
	     (setf *passed* '())
	     (setf *failed* '())
  	     (setf *errors* '())))



  (do ()
      ((not *tests-continuation*) (get-results))

    (let* ((*current-test* (car *tests-continuation*))
	   (*force-debug* (or debug *force-debug*)))

      (handler-bind
	  ((error  #'(lambda (c)
		       (declare (ignore c))
		       (if (not *force-debug*)
			   (invoke-restart 'log-error))))

	   ;; TODO: Handler for actual TEST-FAILURE (does not exist yet)
	   )

	(do ((repeat t))
	    ((not repeat))
	  (let ((current-condition nil))
	    (restart-case
		(handler-bind
		    ((condition #'(lambda (c) (setf current-condition c))))

		  (funcall *current-test*)
		  (setf repeat nil)
		  (log-test-as-passed *current-test*))

	      (repeat () )
	      
	      (skip (&optional (origin :manually) reason)
		(setf repeat nil)
		(log-test-as-skipped *current-test* (cons origin (or reason current-condition))))
	      
	      (log-error ()
		(setf repeat nil)
		(log-test-as-error *current-test* current-condition))

	      (log-failure ()
		(setf repeat nil)
		(log-test-as-failed *current-test* current-condition))
	      
	    ))))

      (pushnew *current-test* *tests-run*)
      (setf *tests-continuation* (cdr *tests-continuation*)))))



;; TODO: Better readable restarts

;; TODO: Introduce *skipped*, e.g. ((foo :manually) (bar :rule "only in SBCL")), add skipped to
;; results

;; TODO: Do registration mostly in the restarts / loop not in the handlers. Handlers set policy,
;;       restarts execute it.

;; TODO: Introduce a flag for a "dirty" run, where the user has interfered. *TEST-RUN-DIRTY*
;; and (:dirty t)
