"Problem 2-2: Evaluate the following forms:"

(defconst ANSWERS
  '(
    ("(append '(a b c) '( ))"   "(a b c)")
    ("(list '(a b c) '( ))"     "((a b c) nil)")
    ("(cons '(a b c) '( ))"     "((a b c))")
    ))

(defun valid-ANSWER-p (answer)
  (equal
   (eval (first (read-from-string (first answer))))
   (first (read-from-string (first (rest answer))))))

(defun invalid-ANSWERS-iter (invalids answers)
  (if (null answers) invalids
    (let ((ans (first answers)))
      (if (valid-ANSWER-p ans)
	  (invalid-ANSWERS-iter invalids (rest answers))
	(invalid-ANSWERS-iter (cons ans invalids) (rest answers))))))

(defun invalid-ANSWERS ()
  (invalid-ANSWERS-iter () ANSWERS))

(if (invalid-ANSWERS)
    (message "FAIL !")
  (message "All Correct !"))
