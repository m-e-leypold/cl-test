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

(define-package :de.m-e-leypold.cl-test/suites
    
    "This package serves as a kind of test suite registry. All symbols interned into this package are
     global variables, are bound to an instance of
     `DE.M-E-LEYPOLD.CL-TEST.TEST-SUITES:TEST-SUITE` that refers to a package that is a test
     suite and the `CL:SYMBOL-NAME` of those symbols are identical to the package names.

     The package is a pure storage container which is filled by other functions in the
     framework, while the package that contain tests are loaded.

     For more details see `DE.M-E-LEYPOLD.CL-TEST/TEST-SUITES:*DOCUMENTATION*`.
   ")

(in-package :de.m-e-leypold.cl-test/suites)



