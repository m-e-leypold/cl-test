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

(defpackage :de.m-e-leypold.de.cl-test/test-suites
  
  (:documentation "
   This package defines the `TEST-SUITE' abstraction.

   TODO: Explain more, refer to other packages.
   ")

  (:export
   :*documentation*)
  
  (:use :common-lisp)
  (:import-from de.m-e-leypold.de.cl-test/doctools
   :package-documentation))

(in-package :de.m-e-leypold.de.cl-test/test-suites)

(package-documentation *documentation*)

