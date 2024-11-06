(defparameter *good* "[({(<(())[]>[[{[]{<()<>>")
(defparameter *bad* "{([(<{}[<>[]}>{[]{[(<()>")

(defun char-split (str &optional (pos 0) (subs ()))
  (if (< pos (length str))
    (char-split str (+ pos 1) (cons (char str pos) subs))
    (nreverse subs)))
(assert (equal (char-split "foo") '(#\f #\o #\o)))

(defun parse-block (left &optional right)
  (loop while (and left (listp left))
        do (setf left (cond
          ((eq #\( (car left)) (parse-block (cdr left) #\)))
          ((eq #\[ (car left)) (parse-block (cdr left) #\]))
          ((eq #\{ (car left)) (parse-block (cdr left) #\}))
          ((eq #\< (car left)) (parse-block (cdr left) #\>))
          ((eq (car left) right) (return-from parse-block (cdr left)))
          ((eq (car left) #\)) 3)
          ((eq (car left) #\]) 57)
          ((eq (car left) #\}) 1197)
          ((eq (car left) #\>) 25137)
        )))
  left)

(assert (equal (parse-block '(#\))) 3))
(assert (equal (parse-block '(#\[ #\))) 3))
(assert (equal (parse-block '(#\( #\))) nil))
(assert (equal (parse-block '(#\( #\) #\))) 3))
(assert (equal (parse-block '(#\( #\) #\( #\))) nil))
(assert (equal (parse-block '(#\( #\( #\) #\))) nil))
(assert (equal (parse-block (char-split *good*)) nil))
(assert (equal (parse-block (char-split *bad*)) 1197))

(defun get-file (filename)
(with-open-file (stream filename)
  (loop for line = (read-line stream nil)
        while line
        collect line)))

(print
  (apply '+
         (remove nil
                 (mapcar #'parse-block
                         (mapcar #'char-split
                                 (get-file "input.txt"))))))
