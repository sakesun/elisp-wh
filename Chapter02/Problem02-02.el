"Problem 2-2: Evaluate the following forms:"

(defconst ANSWERS
  '(
    ("(first '(p h w))"                                     "p")
    ("(rest '(b k p h))"                                    "(k p h)")
    ("(first '((a b) (c d)))"                               "(a b)")
    ("(rest '((a b) (c d)))"                                "((c d))")
    ("(first (rest '((a b) (c d))))"                        "(c d)")
    ("(rest (first '((a b) (c d))))"                        "(b)")
    ("(rest (first (rest '((a b) (c d)))))"                 "(d)")
    ("(first (rest (first '((a b) (c d)))))"                "b")
    ))

(defun valid-ANSWER-p (answer)
  (equal
   (eval (first (read-from-string (first answer))))
   (first (read-from-string (first (rest answer))))))

(defun invalid-ANSWERS-iter (invalids answers)
  (if (null answers) invalids
    (let ((ans (first answers)))
      (if (and (consp ans) (valid-ANSWER-p ans))
	  (invalid-ANSWERS-iter invalids (rest answers))
	(invalid-ANSWERS-iter (cons ans invalids) (rest answers))))))

(defun invalid-ANSWERS ()
  (invalid-ANSWERS-iter () ANSWERS))

(if (invalid-ANSWERS)
    (message "FAIL !")
  (message "All Correct !"))
