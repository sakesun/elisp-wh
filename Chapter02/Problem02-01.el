"Problem 2-1: Each of the following things may be an atom, a list, or neither. Identify each accordingly."

(atom (first (read-from-string "ATOM")))
(listp (first (read-from-string "(THIS IS AN ATOM)")))

(defconst ANSWERS
  '(
    ("ATOM"                     "atom")
    ("(THIS IS AN ATOM)"        "list")
    ("(THIS IS AN EXPRESSION)"  "list")
    ("((A B) (C D)) 3 (3)"      "neither")
    ("(LIST 3)"                 "list")
    ("(/ (+ 3 1) (- 3 1))"      "list")
    (")("                       "neither")
    ("((()))"                   "list")
    ("(() ())"                  "list")
    ("((())"                    "neither")
    ("())("                     "neither")
    ("((ABC"                    "neither")))

(defun get-checker (text)
  (cond
   ((string= text "nil")      'null)
   ((string= text "atom")     'atom)
   ((string= text "list")     'listp)
   ((string= text "neither")  'NEITHER-P)))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

(defun valid-ANSWER-p (answer)
  (let ((q nil)
	(neither nil))
    (condition-case nil
	(let* ((qstr (trim-string (first answer)))
	       (qstrlen (length qstr))
	       (qinfo (read-from-string qstr))
	       (qread (first qinfo))
	       (qreadlen (rest qinfo)))
	  (when (< qreadlen qstrlen) (signal 'invalid-read-syntax nil))
	  (setq q qread))
      (invalid-read-syntax (setq neither t))
      (end-of-file (setq neither t)))
    (let  ((checker (get-checker (first (rest answer)))))
      (cond
       ((eq checker 'NEITHER-P) neither)
       ((null q) nil)
       (t (funcall checker q))))))

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
