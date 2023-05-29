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


(defsystem "de.m-e-leypold.cl-test"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Another Common Lisp testing framework"
  :depends-on ()
  :components ())

(defsystem "de.m-e-leypold.cl-test/assert-based"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "CL-TEST assertion based testing support"
  :depends-on ()
  :components ())

;;; * Tests  ---------------------------------------------------------------------------------------

(defsystem "de.m-e-leypold.cl-test/tests"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "CL-TEST tests"
  :depends-on ()
  :components ())

;;; * Examples  ------------------------------------------------------------------------------------

(defsystem "de.m-e-leypold.cl-test/examples"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "CL-TEST examples"
  :depends-on ()
  :components ())

;;; * Loader stubs  --------------------------------------------------------------------------------

(defsystem "de.m-e-leypold.cl-test/load"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Load all systems in CL-TEST"
  :depends-on ("de.m-e-leypold.cl-test"
	       "de.m-e-leypold.cl-test/assert-based"
	       "de.m-e-leypold.cl-test/tests"))


(defsystem "de.m-e-leypold.cl-test/prerequisites"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Load all prerequisites for CL-TEST, see doc/ProjectStructure.org"
  :depends-on ())
