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

  (:export :run-tests :make-test-plan :with-new-excution-state)
  (:use :de.m-e-leypold.cl-test/conditions)
  (:import-from :de.m-e-leypold.cl-test/test-suites)
  (:import-from :de.m-e-leypold.cl-test/test-procedures
   :do-tests :test-id :continue :get-tags))

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

(defun compile-selector (selector)
  (if (not selector)
      #'(lambda (test) (declare (ignore test)) nil)
      (ecase (type-of selector)
	(compiled-function selector)
	(keyword #'(lambda (test) (find selector (get-tags test))))

	;; TEST and extend this: AND, OR
	
	)))

  
(defun compile-selectors (select)
  (if (eq select :all)
      (make-array 1 :initial-element #'(lambda (x) (declare (ignore x)) t))
      (progn
	(if (not (consp select))
	    (compile-selectors (list select))	   
	    (let* ((count (length select))
		   (compiled (make-array count)))
	      (do ((i 0 (1+ i))
		   (selector (car select) (car rest))
		   (rest (cdr select) (cdr rest)))
		  ((>= i count))
		(setf (aref compiled i) (compile-selector selector)))
	      compiled)))))

(defun make-test-plan (&optional select)
  
  (let* ((selectors (compile-selectors select))
	 (count (length selectors))
	 (tests (make-array count :initial-element '())))
    (do-tests (test)
      (let ((bucket nil))
	(do ((i 0 (1+ i)))
	    ((or (>= i count) bucket))
	  (if (funcall (aref selectors i) test)
	      (setf bucket i)))
	(if bucket
	    (push (test-id test) (aref tests bucket)))))
    (let ((tests (apply #'nconc (map 'cons #'(lambda (l) (nreverse l)) tests))))
      tests)))

;;; * defun RUN-TESTS -----------------------------------------------------------------------------

(defun get-results ()
  (list
   :passed  *passed*
   :failed  *failed*
   :errors  *errors*
   :skipped *skipped*
   :total   *tests-run*
   ))


(defun log-test-result (test-symbol result result-origin &rest more-info)
  (format t "~&~A (~S) => ~S: ~A~%" result result-origin test-symbol more-info)

  ;; TODO: We should print better errors...
  ;;       - convert more-info to s.th. readable.
  ;;       - Generalized "logging" instead of plain format (logging subsystem?)
  ;;
  ;; TODO: If origin :manually ...

  (ecase result
    (:passed
     (push test-symbol *passed*))
    (:failed
     (push (cons test-symbol (cons result-origin more-info)) *failed*))
    (:skipped
     (push (cons test-symbol (cons result-origin more-info)) *skipped*))
    (:error
     (push (cons test-symbol (cons result-origin more-info)) *errors*))))


(defun run-tests (&key restart debug (select :all))

  (setf *current-test* nil)

  (if (not restart)
      (setf *test-plan* (make-test-plan select)))

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

	  ((skip-request  #'(lambda (c)
			      (declare (ignore c))
			      (invoke-restart 'skip :run-test)))

	   (failed-check  #'(lambda (c)
			      (declare (ignore c))
			      (if (not *force-debug*)
				  (invoke-restart 'log-failure :run-test))))

	   (test-error  #'(lambda (c)
			    (declare (ignore c))
			    (if (not *force-debug*)
				(invoke-restart 'log-error :run-test))))

	   (error  #'(lambda (c)
		       (declare (ignore c))
		       (if (not *force-debug*)
			   (invoke-restart 'log-error :run-test)))))

	(do ((repeat t))
	    ((not repeat))
	  (let ((current-condition nil))
	    (restart-case
		(handler-bind

		    ((condition #'(lambda (c) (setf current-condition c))))

		  (funcall *current-test*)
		  (setf repeat nil)
		  (log-test-result *current-test* :passed :run-test))
	      (repeat () )

	      (skip (&optional (origin :manually) reason)
		(setf repeat nil)
		(log-test-result *current-test* :skipped origin (or reason current-condition)))

	      (log-error (&optional (origin :manually) reason)
		(setf repeat nil)
		(log-test-result *current-test* :error origin (or reason current-condition)))

	      ;; Better name for log-failure: Fail-test

	      (log-failure (&optional (origin :manually) reason)
		(setf repeat nil)
		(log-test-result *current-test* :failed origin (or reason current-condition)))
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
