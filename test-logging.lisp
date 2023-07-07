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

(define-package :de.m-e-leypold.cl-test/test-logging

    "TODO: Explain logging."

  (:export
   :log-test-run-begin
   :log-test-run-end
   :log-suite-enter
   :log-suite-exit
   :log-test-begin
   :log-test-end
   :log-test-result
   :*test-console-stream*
   :*test-event-stream*
   :encode-event-element
   :encode-test-event))

(in-package :de.m-e-leypold.cl-test/test-logging)

;;; * Streams --------------------------------------------------------------------------------------

(defvar *test-console-stream* *standard-output*)
(defvar *test-event-stream* *standard-output*)

;; TODO: TEST-OUTPUT-STREAM, TEST-ERROR-STREAM (must be handled by RUN-TEST)

;;; * General test events --------------------------------------------------------------------------

;;; ** Encoding test events ------------------------------------------------------------------------

(defgeneric encode-event-element (element)
  )

(defmethod encode-event-element ((element T))
  (format nil "~S" element))


(defmethod encode-event-element ((elements cons))
  (mapcar #'encode-event-element elements))

(defmethod encode-event-element ((p package))
  `(:package ,(package-name p)))

(defmethod encode-event-element ((s symbol))
  s)


(defun encode-test-event (event)
  (encode-event-element event))


;;; ** Handling test events ------------------------------------------------------------------------

;;; TODO:  with-added-test-event-hooks, with-added-test-event-hook, with-test-event-hooks

(defvar *test-event-hooks* nil)

(let ((keyword (find-package "KEYWORD")))

  (defun log-test-event (event-type &rest rest)

    (loop for hook in *test-event-hooks*
	  do (funcall hook event-type rest))	

    (let ((*package* keyword))
      (if *test-event-stream*
	  (format *test-event-stream* "~&*** Event: ~S~%" (encode-test-event (cons event-type rest)))))))

;;; * Logging run begin/end ------------------------------------------------------------------------

(defun log-test-run-begin (test-run)
  (if *test-console-stream*
      (format *test-console-stream* "~&=> Test run begins ...~%"))
  (log-test-event :RUN-BEGIN test-run))

(defun log-test-run-end (test-run)
  (if *test-console-stream*
      (format *test-console-stream* "~&=> End of test run.~%")) ; TODO: Also log end-time
  (log-test-event :RUN-END test-run))

;;; * Logging suite begin/end ----------------------------------------------------------------------

;; TODO: Use reflected suite identifiers or anchors here! We don't have any anchors yet, but
;; I'm now inclined to insist we need them and a DEFINE-SUITE instead of a defpackage. Later.

(defun log-suite-enter (suite-package)
  (if *test-console-stream*
      (format *test-console-stream* "~&=> Beginning suite ~A ...~%" (package-name suite-package)))
  (log-test-event :SUITE-ENTER suite-package))

(defun log-suite-exit (suite-package)
  (if *test-console-stream*
      (format *test-console-stream* "~&=> Leaving suite (~A).~%" (package-name suite-package)))
  (log-test-event :SUITE-EXIT suite-package))
  
;;; * Logging test begin/end ----------------------------------------------------------------------

(defun log-test-begin (test-id)
  (if *test-console-stream*
      (format *test-console-stream* "~&=> Testing ~A ...~%" test-id))
  (log-test-event :TEST-BEGIN test-id))

(defun log-test-end (test-id)
  (if *test-console-stream*
      (format *test-console-stream* "~&=> Done ~A ...~%" test-id))
  (log-test-event :TEST-END test-id))

;;; * Logging test results ------------------------------------------------------------------------

(defun log-test-result (test-symbol result result-origin more-info)

  ;; TODO: We should print better errors...
  ;;       - convert more-info to s.th. readable.
  ;;       - Generalized "logging" instead of plain format (logging subsystem?)
  ;;
  ;; TODO: If origin :manually ...

  (if *test-console-stream*
      (format *test-console-stream*
	      "~&~A (~S) => ~S: ~A~%"
	      result result-origin test-symbol more-info))
  (if more-info
      (log-test-event result :test test-symbol :origin result-origin :more-info more-info)
      (log-test-event result :test test-symbol :origin result-origin)))
      




