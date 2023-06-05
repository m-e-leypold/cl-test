(progn
  (setf *debugger-hook*
	#'(lambda (c f) (progn (format t "~A~%" c) (uiop:quit 1))))
  (load (car uiop:*command-line-arguments*)))
(quit)
