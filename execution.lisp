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

  (:export
   :run-tests :re-run-test
   :make-test-plan
   :with-new-excution-state :with-current-test
   :current-test :current-suite-package :current-suite :current-test-run
   :run-results)

  (:use :de.m-e-leypold.cl-test/conditions)

  (:import-from :de.m-e-leypold.cl-test/test-suites)

  (:import-from :de.m-e-leypold.cl-test/test-procedures
   :do-tests :test-id :continue :get-tags :get-test-suite)

  (:use :de.m-e-leypold.cl-test/test-logging))

(in-package :de.m-e-leypold.cl-test/execution)

;;; * Execution state  -----------------------------------------------------------------------------
;;; ** Test results --------------------------------------------------------------------------------

(defvar *tests-run* '())
(defvar *passed* '())
(defvar *failed* '())
(defvar *errors* '())
(defvar *skipped* '())

;;; ** Test plan -----------------------------------------------------------------------------------

(defvar *test-plan* '())
(defvar *tests-continuation* '())


;;; ** WITH-NEW-EXECUTION-STATE --------------------------------------------------------------------

(defmacro with-new-execution-state* (&body body)  
  `(let ((*test-plan* '())
	 (*tests-run* '())
	 (*passed* '())
	 (*failed* '())
	 (*errors* '())
	 (*skipped* '())
	 (*current-test* '())
	 (*current-test-run* '())
	 (*force-debug* *force-debug*))
     ,@body))


(defmacro with-new-execution-state (&body body)
  `(let ((*tests-continuation* '())
	 (*test-plan* '()))
     (with-test-event-hooks ()       
       ,@body)))

;;; ** *CURRENT-TEST*, *CURRENT-TEST-RUN* -----------------------------------

(defvar *current-test* nil
  "
  Contains the current tests test id (the symbol of the test procedure) while the test runs.

  Running tests directly will set the variable, but never unset it, so after the test
  *CURRENT-TEST* will contain the most recently run test and `RUN-RECENT-TEST' can run the most
  recently run test.

  After `RUN-TESTS' on the other side the variable will have the value NIL.
")

(defmacro with-current-test ((symbol) &body body)
  `(let ((*current-test* ,symbol))
     ,@body))

(defvar *current-test-run* nil)

(defun current-test () *current-test*)
(defun current-suite-package () (symbol-package *current-test*))
(defun current-suite () (get-test-suite *current-test*))
(defun current-test-run () *current-test-run*)


;;; * User parameters: *FORCE-DEBUG* ---------------------------------------------------------------

(defvar *force-debug* nil)

;;; * defun MAKE-TEST-PLAN -------------------------------------------------------------------------

(defun compile-selector-or (selectors)
  (let ((predicates (mapcar #'compile-selector selectors)))
    #'(lambda (test)
	(dolist (p predicates)
	  (if (funcall p test)
	      (return t))))))


(defun compile-selector-and (selectors)
    (let ((predicates (mapcar #'compile-selector selectors)))
    #'(lambda (test)
	(not (dolist (p predicates)
	  (if (not (funcall p test))
	      (return t)))))))

(defun compile-selector-not (selectors)
  (let ((excluded (compile-selector-or selectors)))
    #'(lambda (test)
	(not (funcall excluded test)))))

(defun compile-selector (selector)
  (if (not selector)
      #'(lambda (test) (declare (ignore test)) nil)
      (if (eq selector t)
	  #'(lambda (test) (declare (ignore test)) t)

	  (ecase (type-of selector)

	    ;; TODO switch to typecase to avoid handling differences in type systems?

	    #+clasp (core:closure selector) ;; seriously?

	    (function selector)
	    (compiled-function selector)
	    (keyword #'(lambda (test) (find selector (get-tags test))))
	    (cons
	     (ecase (type-of (car selector))
	       (keyword (compile-selector-or selector))
	       (symbol
		(ecase (car selector)
		  (or (compile-selector-or (cdr selector)))
		  (and (compile-selector-and (cdr selector)))
		  (not (compile-selector-not (cdr selector)))))))))))


(defun compile-selectors (select)
  (if (not (consp select))
      (compile-selectors (list select))
      (let* ((count (length select))
	     (compiled (make-array count)))
	(do ((i 0 (1+ i))
	     (selector (car select) (car rest))
	     (rest (cdr select) (cdr rest)))
	    ((>= i count))
	  (setf (aref compiled i) (compile-selector selector)))
	compiled)))

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


;;; * defun RE-RUN-TEST ---------------------------------------------------------------------------

(defun re-run-test ()
  (if *current-test*
      (funcall *current-test*)
      (if (car *tests-continuation*)
	  (funcall (car *tests-continuation*))
	  (error "No current test"))))

;;; * defun RUN-TESTS -----------------------------------------------------------------------------

(defun get-results ()
  (list
   :passed  (reverse *passed*)
   :failed  (reverse *failed*)
   :errors  (reverse *errors*)
   :skipped (reverse *skipped*)
   :total   (reverse *tests-run*)))


(defun register-test-result (test-symbol result result-origin &rest more-info)
  (log-test-result test-symbol result result-origin more-info)

  (ecase result
    (:passed
     (push test-symbol *passed*))
    (:failed
     (push (cons test-symbol (cons result-origin more-info)) *failed*))
    (:skipped
     (push (cons test-symbol (cons result-origin more-info)) *skipped*))
    (:error
     (push (cons test-symbol (cons result-origin more-info)) *errors*))))



(defclass test-run ()
  ((start-time
    :reader start-time
    :initform (local-time:now))
   (end-time
    :accessor end-time
    :initform nil)
   (test-plan
    :initarg :test-plan
    :reader test-plan)
   (continued-p
    :initarg :continued-p
    :reader continued-p)
   (continued-from-plan
    :initarg :continued-from-plan
    :reader continued-from-plan)
   (run-results
    :accessor run-results
    :initform nil)))

(defmethod print-object ((run test-run) stream)
  (print-unreadable-object (run stream :type t)
    (let* ((results (run-results run))
	   (passed   (getf results :passed))
	   (failed   (getf results :failed))
	   (errors   (getf results :errors))
	   (skipped  (getf results :skipped)))

      (format stream "#passed=~A " (length passed))
      (format stream "#failed=~A " (length failed))
      (format stream "#errors=~A " (length errors))
      (format stream "#skipped=~A " (+ (length skipped)
				       (if nil
					   (- (length (continued-from-plan run))
					      (length (test-plan run)))
					   0)))
      (format stream "continued-p=~A " (continued-p run))
      (format stream "begin=~A " (start-time run))
      (format stream "end=~A"   (end-time run)))))

(defmethod encode-event-element ((r test-run))
  `(:test-run :begin ,(start-time r)
	      :end ,(end-time r)
	      :results ,(run-results r)))


(defun run-tests (&key restart debug (select t))

  (with-new-execution-state*

      (if (not restart)
	  (setf *test-plan* (make-test-plan select)))

    (let ((continued-p (eq restart :continue)))


      ;; TODO: Following SETFs are not required (on the one side), on the other side
      ;;       with-new-execution-state* now also isolates the result-part, which is not wquite
      ;;       intended.
      
      (if (not continued-p)
	  (progn (setf *tests-continuation* *test-plan*)
		 (setf *tests-run* '())
		 (setf *passed* '())
		 (setf *failed* '())
		 (setf *skipped* '())
  		 (setf *errors* '())))

      (let ((*current-test-run*
	      (make-instance 'test-run
			     :continued-p continued-p
			     :test-plan *tests-continuation*
			     :continued-from-plan (if continued-p *test-plan* nil)
			     ))
	    (last-suite-package nil) ; Just for isolation
	    (*standard-output* *test-console-stream*)
	    (*error-output* *test-console-stream*))
	
	(log-test-run-begin *current-test-run*)

	(let ((run-results

		(do ()
		    ((not *tests-continuation*)
		     (if last-suite-package
			 (log-suite-exit last-suite-package))
		     (get-results))

		  (let* ((*current-test* (car *tests-continuation*))
			 (current-suite-package  (symbol-package *current-test*))
			 (*force-debug* (or debug *force-debug*)))

		    (if (not (eq current-suite-package last-suite-package))
			(progn
			  (if last-suite-package
			      (log-suite-exit last-suite-package))
			  (setf last-suite-package current-suite-package)
			  (log-suite-enter current-suite-package)))

		    (handler-bind

			;; TODO: More logging here.

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

		      (log-test-begin *current-test*)

		      (do ((repeat t))
			  ((not repeat))
			(let ((current-condition nil))
			  (restart-case
			      (handler-bind

				  ((condition #'(lambda (c) (setf current-condition c))))

				(funcall *current-test*)
				(setf repeat nil)
				(register-test-result *current-test* :passed :run-test))

			    (repeat () )

			    (skip (&optional (origin :manually) reason)
			      (setf repeat nil)
			      (register-test-result *current-test* :skipped origin (or reason current-condition)))

			    (log-error (&optional (origin :manually) reason)
			      (setf repeat nil)
			      (register-test-result *current-test* :error origin (or reason current-condition)))

			    ;; Better name for log-failure: Fail-test

			    (log-failure (&optional (origin :manually) reason)
			      (setf repeat nil)
			      (register-test-result *current-test* :failed origin (or reason current-condition)))
			    )))

		      (log-test-end *current-test*))

		    (pushnew *current-test* *tests-run*)
		    (setf *tests-continuation* (cdr *tests-continuation*))))))
	  

	  (setf (end-time *current-test-run*) (local-time:now))
	  (setf (run-results *current-test-run*) run-results)
	  (log-test-run-end *current-test-run*)
	  *current-test-run*)))))

;; TODO: Better readable restarts

;; TODO: Introduce *skipped*, e.g. ((foo :manually) (bar :rule "only in SBCL")), add skipped to
;; results

;; TODO: Do registration mostly in the restarts / loop not in the handlers. Handlers set policy,
;;       restarts execute it.

;; TODO: Introduce a flag for a "dirty" run, where the user has interfered. *TEST-RUN-DIRTY*
;; and (:dirty t)
