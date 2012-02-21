"Problem 2-4: Write sequences of  FIRSTs and RESTs that will pick
the symbol PEAR out of the following expressions:"

(defconst ANSWERS
  '(
    (  (apple orange pear grapefruit)              (lambda (x) (first (rest (rest x))))  )
    (  ((apple orange) (pear grapefruit))          (lambda (x) (first (first (rest x))))  )
    (  (((apple) (orange) (pear) (grapefruit)))    (lambda (x) (first (first (rest (rest (first x))))))  )
    (  (apple (orange) ((pear)) (((grapefruit))))  (lambda (x) (first (first (first (rest (rest x))))))  )
    (  ((((apple))) ((orange)) (pear) grapefruit)  (lambda (x) (first (first (rest (rest x)))))  )
    (  ((((apple) orange) pear) grapefruit)        (lambda (x) (first (rest (first x))))  )
    ))

(defun valid-ANSWER-p (answer)
  (equal
   'pear
   (funcall (first (rest answer)) (first answer))))

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
