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
   :get-suites :do-suites :get-suite :do-test-ids :suite-id)
  (:import-from :de.m-e-leypold.cl-test/test-procedures
   :get-test-ids :get-tests :test-id :do-tests
   :get-tags)
  (:import-from :de.m-e-leypold.cl-test/execution
   :make-test-plan :run-tests)

  (:import-from :de.m-e-leypold.cl-test/macro-tools
   :split-docstring+options+body)
  
  (:export
   :run
   :check-split-docstring+options+body
   :examples-load-properly))

(in-package :de.m-e-leypold.cl-test/tests)


;;; * Tests  ---------------------------------------------------------------------------------------

(defun run ()
  (de.m-e-leypold.cl-test/tests:check-split-docstring+options+body)
  (de.m-e-leypold.cl-test/tests:examples-load-properly))

;;; ** CHECK-SPLIT-DOCSTRING+OPTIONS+BODY  ---------------------------------------------------------

(define-test* check-split-docstring+options+body ()

  "This test checks if various permutations of calling `SPLIT-DOCSTRING+OPTIONS+BODY' actually work."
  
  (multiple-value-bind (body docstring options)
      (split-docstring+options+body
       '("a docstring"
	 (:opt1 1 2 3)
	 (:opt2 4)
	 (and a body form)
	 (and another form))
       :allowed-tags '(:opt1 :opt2))

    (assert (equal body
		   '((and a body form)
		     (and another form))))

    (assert (equal options
		   '((:opt1 1 2 3)
		     (:opt2 4))))

    (assert (equal docstring "a docstring")))

  (multiple-value-bind (body docstring options)
      (split-docstring+options+body
       '((:opt1 1 2 3)
	 (:opt2 4)
	 (and a body form)
	 (and another form))
       :allowed-tags '(:opt1 :opt2))

    (assert (equal body
		   '((and a body form)
		     (and another form))))

    (assert (equal options
		   '((:opt1 1 2 3)
		     (:opt2 4))))

    (assert (equal docstring nil)))

  (multiple-value-bind (body docstring options)
      (split-docstring+options+body
       '((:opt2 4)
	 (and a body form)
	 (and another form))
       :allowed-tags '(:opt1 :opt2))

    (assert (equal body
		   '((and a body form)
		     (and another form))))

    (assert (equal options
		   '((:opt2 4))))

    (assert (equal docstring nil)))

  
  (multiple-value-bind (body docstring options)
      (split-docstring+options+body
       '((and a body form)
	 (and another form))
       :allowed-tags '(:opt1 :opt2))

    (assert (equal body
		   '((and a body form)
		     (and another form))))

    (assert (equal options nil))
    (assert (equal docstring nil)))

  (multiple-value-bind (body docstring options)
      (split-docstring+options+body
       '("a docstring"
	 (and a body form)
	 (and another form))
       :allowed-tags '(:opt1 :opt2))
    
    (assert (equal body
		   '((and a body form)
		     (and another form))))
      
    (assert (equal options nil))
    (assert (equal docstring "a docstring")))

  (multiple-value-bind (body docstring options)
      (split-docstring+options+body
       '("a docstring")
       :allowed-tags '(:opt1 :opt2))
    
    (assert (equal body '(progn )))
    (assert (equal options nil))
    (assert (equal docstring "a docstring")))

  (multiple-value-bind (body docstring options)
      (split-docstring+options+body
       '()
       :allowed-tags '(:opt1 :opt2))
    
    (assert (equal body '(progn )))
    (assert (equal options nil))
    (assert (equal docstring nil))))

;;; ** EXAMPLES-LOAD-PROPERLY  ---------------------------------------------------------------------

(define-test* examples-load-properly ()
  "
  Try to load the examples. If this fails, we have a problem with the syntax macros.
"   
  ;; Effects of loading :DE.M-E-LEYPOLD.CL-TEST/TESTS which already occured.
  ;;
  ;; We phrase this "openly", leaving the possibility that other test suites where also already
  ;; loaded.

  ;; (1) Package name as a keyword has been entered into
  ;;     DE.M-E-LEYPOLD.CL-TEST/TEST-SUITES::*SUITES*
  ;;
  (assert (find :de.m-e-leypold.cl-test/tests de.m-e-leypold.cl-test/test-suites::*suites*))

  ;; (2) Descriptor for test has been bound to a symbol (whose name is the package name) in
  ;;     DE.M-E-LEYPOLD.CL-TEST/SUITES.
  ;;
  (assert (typep de.m-e-leypold.cl-test/suites::de.m-e-leypold.cl-test/tests
		 'de.m-e-leypold.cl-test/test-suites::test-suite))

  ;; (3) The descriptor has been stored in
  ;;     DE.M-E-LEYPOLD.CL-TEST/TEST-SUITES::*SUITE-INSTANCES*
  ;;
  (assert (equal de.m-e-leypold.cl-test/suites::de.m-e-leypold.cl-test/tests
		 (gethash :de.m-e-leypold.cl-test/tests
		  de.m-e-leypold.cl-test/test-suites::*suite-instances*)))

  ;; (4) DE.M-E-LEYPOLD.CL-TEST/TEST-SUITES::*SUITES-PACKAGE* points to package
  ;;     :de.m-e-leypold.cl-test/suites. This is the package where automatic symbols for test
  ;;     suites will be created.
  ;;
  (assert (equal de.m-e-leypold.cl-test/test-suites::*suites-package*
		 (find-package :de.m-e-leypold.cl-test/suites)))


  ;; We start be establishing a new registry context. This possibility exists solely for the
  ;; purpose of checking the test definition and loading: I don't see ATN any value in using
  ;; this in any day to day operation.

  (with-new-suite-registry ()

    ;; To avoid difficult to understand error downstream, we check if a new context has been
    ;; esatablished. Also count this as a basic test of with-new-suite-registry.

    (assert (equal nil de.m-e-leypold.cl-test/test-suites::*suites*))
    (assert (equal 0 (hash-table-count de.m-e-leypold.cl-test/test-suites::*suite-instances*)))
    (assert (equal nil de.m-e-leypold.cl-test/test-suites::*suites-package*))


    ;; Then we force-load the example suites via ASDF.
    
    (asdf:operate 'asdf:load-op "de.m-e-leypold.cl-test/examples" :force T)

    (let ((p1 (find-package :de.m-e-leypold.cl-test/example/assert-based))
	  (p2 (find-package :de.m-e-leypold.cl-test/example/assert-based-2)))

      ;; The example suites (which are packages) could have been loaded before, but now they
      ;; MUST have been loaded.
      
      (assert p1)  ;; packages are now loaded
      (assert p2)

      ;; Cannot write the symbols verbatimely since the examples are (intentionally) not loaded
      ;; at read time.
      ;;
      ;; This is a special quirk of this test where we dynamically load a system.

      (let ((s1a (find-symbol "A-TEST-THAT-WILL-FAIL" p1))
	    (s1b (find-symbol "A-TEST-THAT-WILL-PASS" p1))
	    (s1c (find-symbol "A-TEST-THAT-WILL-BE-SKIPPED" p1))
	    (s2a (find-symbol "A-TEST-THAT-WILL-FAIL" p2))
	    (s2b (find-symbol "A-TEST-THAT-WILL-PASS" p2)))

	(assert s1a)
	(assert s1b)
	(assert s1c)
	(assert s2a)
	(assert s2b)

	;; Suites registered properly?
	
	(assert (equal de.m-e-leypold.cl-test/test-suites::*suites*
		       '(:de.m-e-leypold.cl-test/example/assert-based-2
			 :de.m-e-leypold.cl-test/example/assert-based)))

	;; Suites have the proper IDs?
	
	(let ((suites (get-suites)))
	  (assert (equal (mapcar #'suite-id suites)
			 '(:de.m-e-leypold.cl-test/example/assert-based
			   :de.m-e-leypold.cl-test/example/assert-based-2))))

	;; Have the tags been recorded properly?

	(assert (equal (get-tags s1a)
		       '(:de.m-e-leypold.cl-test/example/assert-based
			 :smoke :experimental)))

	(assert (equal (get-tags s2a)
		       '(:de.m-e-leypold.cl-test/example/assert-based-2)))

	;; Does GET-TEST-IDs return the ids in order of their definition?
	
	(assert (equal (get-test-ids) (list s1a s1b s1c s2a s2b)))

	;; Does GET-TESTS return the proper test descriptors (in order of their definition)?
	
	(let ((tests (get-tests)))
	  (assert (equal (mapcar #'test-id tests)
			 (list s1a s1b s1c s2a s2b))))

	(let ((suite (get-suite :de.m-e-leypold.cl-test/example/assert-based)))
	  (let ((tests '()))
	    (do-test-ids (test-id suite)
	      (push test-id tests))
	    (assert (equal tests (list s1c s1b s1a)))))
	
	(let ((suites '()))
	  (do-suites (suite)
	    (push suite suites))
	  (assert (equal suites
			 (list
			  (get-suite :de.m-e-leypold.cl-test/example/assert-based-2)
			  (get-suite :de.m-e-leypold.cl-test/example/assert-based)))))


	(let ((tests '()))
	  (do-tests (test)
	    (assert (typep test 'de.m-e-leypold.cl-test/test-procedures::test-descriptor))
	    (push (test-id test) tests))
	  (assert (equal tests (list s2b s2a s1c s1b s1a))))
	
	(assert (equal (make-test-plan :all)
		       (list s1a s1b s1c s2a s2b)))

	(assert (equal (make-test-plan (list #'(lambda (test) (find :smoke (get-tags test)))))
		       (list s1a)))
       
	(let ((results (run-tests)))
	  (let ((*print-pretty* t))
	    (format t "results => ~S~%" results)))
	))))


