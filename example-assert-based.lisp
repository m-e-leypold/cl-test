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

(defpackage :de.m-e-leypold.cl-test/example/assert-based
  (:use :common-lisp :de.m-e-leypold.cl-test/assert-based)
  (:export
   :a-test-that-will-fail
   :a-test-that-will-pass
   :a-test-that-will-be-skipped))

;; Note regarding (:export :test-a): Normally you do not *need* to export the tests, this is
;; done automatically and running a test with the framework function doesn't need the symbol
;; exported anyway (the framework functions us DE.M-E-LEYPOLD.CL-TEST/TEST-SUITES::*SUITES* to
;; translate selectors into a list of tests).
;;
;; In this case, though, in the tests of this package (see
;; DE.M-E-LEYPOLD.CL-TEST/TESTS:DOCUMENTATION*) we force-reload this package repeatedly in
;; DE.M-E-LEYPOLD.CL-TEST/TESTS:EXAMPLES-LOAD-PROPERLY. This will (at least under SBCL) lead to
;; discovery of package variance (the in-system package export list is different from that
;; given in defpackage) and ASDF seems to interprete the warning as an error (not so sure about
;; the latter, though).
;;
;; Anyway: Explicitely exporting the symbol fixes the error from force-reloading with ASDF.

(in-package :de.m-e-leypold.cl-test/example/assert-based)

(define-test* a-test-that-will-fail ()
  "An assertion based test which will fail"
  (:tags :smoke :experimental)
  (assert nil))

(define-test* a-test-that-will-pass ()
  "An assertion based test which will not fail"
  (assert t))

(define-test* a-test-that-will-be-skipped ()
  "An assertion based test tht will be skipped"
  (skip-test "Just to demonstrate test skipping"))


