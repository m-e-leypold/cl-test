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


(require 'cl-test)

(defun load-it ()
  (interactive)
  (if (or (not (boundp 'slime-default-connection))
	  (not slime-default-connection))
      (progn
	(slime)
	(while (not (swank-call/ping))
	  (message "test.el: Slime not ready: waiting for response")
	  (sleep-for 0.2))))  
  (sleep-for 0.2)
  (slime-switch-to-output-buffer)
  (slime-load-file "load.lisp"))

(load-it)

(global-set-key (kbd "s-t") #'(lambda () (interactive) (load-file "test.el")))

