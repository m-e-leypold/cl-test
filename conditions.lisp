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

(define-package :de.m-e-leypold.cl-test/conditions
    "TODO conditions

     TODO: Convention tests signal conditions, perhaps converted
     TODO: With *FORCE-DEBUG* no conversion will take place

     When something fails when executing a test (which means it probably needs to abort
     immediately), conditions are signalled:

     - `FAILED-CHECK' :: A check (the assertion of some predicate on testee output or state)
       failed.

     - `TEST-ERROR' :: TODO

     - `SKIP-REQUEST' :: TODO

     The actual policy in force at any given moment is left to signal handlers established
     earlier on the call stack. For example `DE.M-E-LEYPOLD.CL-TEST/EXECUTION::RUN-TESTS'
     establishes such signal handlers. The options provided to the policies are restablished by
     restarts wrapped around the test procedures and around check macros (like EXPECT [not yet
     available]).

     To facilitate differentiated handling, the conditions are structured as a class hierarchy:

     ```classdiagram

        test-condition

        test-failure      --|> test-condition
        test-error        --|> test-failure
        failed-check      --|> test-failure

        skip-request      --|> test-condition
     ```
"  
  (:export :test-condition :test-failure :failed-check :test-error
	   :skip-test :skip-request))

(in-package :de.m-e-leypold.cl-test/conditions)

(define-condition test-condition ()
  ())

(define-condition test-failure (test-condition)
  ())

(define-condition test-error (test-failure)
  ())

(define-condition failed-check (test-failure)
  ())

(define-condition skip-request (test-failure)
  ((cause
    :reader cause
    :initarg :cause
    :documentation "Why the test must be skipped"
    )))

(defun skip-test (&optional cause)
  (signal 'skip-request :cause cause))
