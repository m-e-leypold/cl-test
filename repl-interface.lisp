;;;                                              -*- mode: common-lisp -*-
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

(define-package :de.m-e-leypold.cl-test/repl-interface
    "A simple interface for running test from the REPL"
  (:export :set-default-run :run :force-debugging))

(in-package :de.m-e-leypold.cl-test/repl-interface)

(defvar *default-run* nil)

(defun set-default-run (symbol-or-function)
  (setf *default-run* symbol-or-function))

(defun run ()
  (apply *default-run* ()))

(defun force-debugging (&optional (force-p t))
  (setf de.m-e-leypold.cl-test/execution::*force-debug* force-p))

(in-package :cl-user)

(define-symbol-macro .debug
    (de.m-e-leypold.cl-test/repl-interface:force-debugging))

(define-symbol-macro .debug-off
    (de.m-e-leypold.cl-test/repl-interface:force-debugging nil))

(defun .debug (&optional (force-p t))
  (de.m-e-leypold.cl-test/repl-interface:force-debugging force-p))

(define-symbol-macro .t
    (de.m-e-leypold.cl-test/repl-interface:run))

(defmacro .t (form)
  `(progn 
     ,(ecase (type-of form)
	(symbol `(de.m-e-leypold.cl-test/repl-interface:set-default-run (quote ,form)))
	(cons   `(de.m-e-leypold.cl-test/repl-interface:set-default-run #'(lambda () ,form))))
     (de.m-e-leypold.cl-test/repl-interface:run)))

;; TODO more specifications for .t (like: Tags or suites).
;; TODO more error handling
;; .h?
