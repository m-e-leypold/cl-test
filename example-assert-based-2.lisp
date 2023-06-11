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

(defpackage :de.m-e-leypold.cl-test/example/assert-based-2
  (:use :common-lisp :de.m-e-leypold.cl-test/assert-based)
  (:export
   :a-test-that-will-fail
   :a-test-that-will-pass))

;; Regarding (:export :test-a): See note in DE.M-E-LEYPOLD.CL-TEST/EXAMPLE/ASSERT-BASED-2.

(in-package :de.m-e-leypold.cl-test/example/assert-based-2)

(define-test* a-test-that-will-fail ()
  "An assertion based test which will fail"
  (assert nil))

(define-test* a-test-that-will-pass ()
  "An assertion based test which will not fail"
  (assert t))


