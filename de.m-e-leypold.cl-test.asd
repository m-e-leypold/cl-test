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

;;  

(defsystem "de.m-e-leypold.cl-test"

  ;; Documentation see de.m-e-leypold.cl-test:*documentation*
  
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Another Common Lisp testing framework"
  :depends-on ("de.m-e-leypold.cl-test/loading-ramp"
	       "de.m-e-leypold.cl-test/common-layer")
  :components ((:file "cl-test")))

(defsystem "de.m-e-leypold.cl-test/assert-based"

  ;; Documentation see de.m-e-leypold.cl-test/assert-based:*documentation*
  
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "CL-TEST assertion based testing support"
  :depends-on ("de.m-e-leypold.cl-test/loading-ramp"
	       "de.m-e-leypold.cl-test/common-layer")
  :components ((:file "assert-based")))


(defsystem "de.m-e-leypold.cl-test/repl-interface"

  ;; Documentation see de.m-e-leypold.cl-test/repl-interface:*documentation*
  
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "CL-TEST interface for the REPL"
  :depends-on ("de.m-e-leypold.cl-test/loading-ramp"
	       "de.m-e-leypold.cl-test/common-layer")
  :components ((:file "repl-interface")))

(defsystem "de.m-e-leypold.cl-test/common-layer"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "CL-TEST common abstractions and utilities"
  :depends-on ("de.m-e-leypold.cl-test/loading-ramp" "local-time")
  :components ((:file "doctools")
	       (:file "macro-tools")	       
	       (:file "conditions")
	       (:file "test-logging")
	       (:file "suites")
	       (:file "test-suites")
	       (:file "test-procedures")
	       (:file "execution")))

(defsystem "de.m-e-leypold.cl-test/loading-ramp"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Loading ramp mechanism for CL-TEST"
  :depends-on ()
  :components ((:file "doctools-primitives")
	       (:file "loading-ramp")))

;;; * Tests  ---------------------------------------------------------------------------------------

(defsystem "de.m-e-leypold.cl-test/tests"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "CL-TEST tests"
  :depends-on ("de.m-e-leypold.cl-test/assert-based")
  :components ((:file "tests")))

;;; * Examples  ------------------------------------------------------------------------------------

(defsystem "de.m-e-leypold.cl-test/examples"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "CL-TEST examples"
  :depends-on ("de.m-e-leypold.cl-test"
	       "de.m-e-leypold.cl-test/assert-based")
  :components ((:file "example-assert-based")
	       (:file "example-assert-based-2")))

;;; * Loader stubs  --------------------------------------------------------------------------------

(defsystem "de.m-e-leypold.cl-test/load"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Load all systems in CL-TEST"
  :depends-on ("de.m-e-leypold.cl-test"
	       "de.m-e-leypold.cl-test/assert-based"
	       "de.m-e-leypold.cl-test/tests"
	       "de.m-e-leypold.cl-test/examples"
	       ))

(defsystem "de.m-e-leypold.cl-test/prerequisites"
  :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Load all prerequisites for CL-TEST, see doc/ProjectStructure.org"
  :depends-on ("local-time"))
