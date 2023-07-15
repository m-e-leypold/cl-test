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

(asdf:load-system "de.m-e-leypold.cl-test/repl-interface")
(asdf:load-system "de.m-e-leypold.cl-test/tests")
(asdf:load-system "de.m-e-leypold.cl-test/emacs-api")

(in-package :de.m-e-leypold.cl-test/loading-ramp)
(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;; For the moment: Run the test(s) explicitely as procedures

(de.m-e-leypold.cl-test/repl-interface:set-default-run 'de.m-e-leypold.cl-test/tests:run)
(de.m-e-leypold.cl-test/tests:run)







