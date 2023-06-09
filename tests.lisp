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

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))
(in-package :de.m-e-leypold.cl-test/loading-ramp)

;;; * Package definition  --------------------------------------------------------------------------

(define-package :de.m-e-leypold.cl-test/tests
    "TODO: Tests"
  (:use :common-lisp :de.m-e-leypold.cl-test/assert-based)
  (:import-from :de.m-e-leypold.cl-test/test-suites
   :with-new-suite-registry
   :get-suites :suite-id
   )
  (:import-from :de.m-e-leypold.cl-test/test-procedures
   :get-test-ids)
  (:export
   :examples-load-properly))

(in-package :de.m-e-leypold.cl-test/tests)


;;; * Tests  ---------------------------------------------------------------------------------------

(define-test* examples-load-properly ()
  "
  Try to load the examples. If this fails, we have a problem with the syntax macros.
"
  (with-new-suite-registry ()
    ;; TODO: Make this into a special assert "assert-asdf-loads"?
    (asdf:operate 'asdf:load-op "de.m-e-leypold.cl-test/examples" :force T)

    (let ((p1 (find-package :de.m-e-leypold.cl-test/example/assert-based))
	  (p2 (find-package :de.m-e-leypold.cl-test/example/assert-based-2)))

      (assert p1)  ;; packages are now loaded
      (assert p2)

      ;; cannot write the symbols verbatimely since the examples are (intentionally) not loaded
      ;; at read time.

      (let ((s1a (find-symbol "A-TEST-THAT-WILL-FAIL" p1))
	    (s1b (find-symbol "A-TEST-THAT-WILL-PASS" p1))
	    (s2a (find-symbol "A-TEST-THAT-WILL-FAIL" p2))
	    (s2b (find-symbol "A-TEST-THAT-WILL-PASS" p2)))

	(assert s1a)
	(assert s1b)
	(assert s2a)
	(assert s2b)


	(assert (equal de.m-e-leypold.cl-test/test-suites::*suites*
		       '(:de.m-e-leypold.cl-test/example/assert-based-2
			 :de.m-e-leypold.cl-test/example/assert-based)))

	(let ((suites (get-suites)))
	  (assert (equal (mapcar #'suite-id suites)
			 '(:de.m-e-leypold.cl-test/example/assert-based
			   :de.m-e-leypold.cl-test/example/assert-based-2))))

	(assert (equal (get-test-ids) (list s1a s1b s2a s2b)))))))
