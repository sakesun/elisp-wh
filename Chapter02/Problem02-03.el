"Problem 2-3: Evaluate the following forms:"

(defconst ANSWERS
  '(
    ("(first (rest (first (rest '((a b) (c d) (e f))))))"  "d")
    ("(first (first (rest (rest '((a b) (c d) (e f))))))"  "e")
    ("(first (first (rest '(rest ((a b) (c d) (e f))))))"  "(a b)")
    ("(first (first '(rest (rest ((a b) (c d) (e f))))))"  "ERROR")
    ("(first '(first (rest (rest ((a b) (c d) (e f))))))"  "first")
    ("'(first (first (rest (rest ((a b) (c d) (e f))))))"
     "(first (first (rest (rest ((a b) (c d) (e f))))))")
    ))

(defun valid-ANSWER-p (answer)
  (let ((q) (a))
    (setq q
      (condition-case nil
        (eval (first (read-from-string (first answer))))
        (error 'ERROR)))
    (setq a
      (first (read-from-string (first (rest answer)))))
    (equal q a)))

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
