(defparameter *test-case* '(
   "3   4~%"
   "4   3~%"
   "2   5~%"
   "1   3~%"
   "3   9~%"
   "3   3~%"))

(defun str-split (str delim &optional (subs ()))
  (let ((pos (search delim str)))
    (if pos
      (str-split (subseq str (+ pos 1)) delim (cons (subseq str 0 pos) subs))
      (reverse (cons str subs)))))
(assert (equal (str-split "add b 7" " ") '("add" "b" "7")))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-line (line)
  (mapcar
    (lambda (v) (parse-integer (string-trim " ~%" v)))
    (str-split line "   ")))
(assert (equal (parse-line "3   4") '(3 4)))

(defun parse-lines (lines) (mapcar #'parse-line lines))
(assert (equal
          (parse-lines *test-case*)
          '((3 4) (4 3) (2 5) (1 3) (3 9) (3 3))))

(defun separate-item (next acc)
  (list
    (cons (car acc) (car next))
    (cons (cadr acc) (cadr next))))

(defun separate (lines)
  (reduce #'separate-item lines
          :initial-value '(() ())))

(defun sort-cols (cols)
  (mapcar (lambda (col) (sort col #'<)) cols))

(defun distance (row) (abs (- (car row) (cadr row))))

(defun total-distance (cols)
  (reduce #'+ (mapcar #'distance (mapcar
                                   #'list
                                   (car cols)
                                   (cadr cols)))))

(defun prepare (lines) (sort-cols (separate (parse-lines lines))))

(defun part-1 (lines)
  (total-distance (prepare lines)))

(defun alist-iter (acc next)
  (if (equal next (caar acc))
    (cons (cons next (+ 1 (cdar acc))) acc)
    (cons (cons next 1) acc)
    ))
(defun build-alist (vals)
  (reduce #'alist-iter vals :initial-value '()))

(defun similarity (left right)
  (reduce
    #'+
    (mapcar
      (lambda
        (v)
        (* v (if (assoc v right) (cdr (assoc v right)) 0)))
      left)))

(defun part-2 (lines)
  (let ((cols (prepare lines)))
    (let ((left (car cols)) (right (cadr cols)))
      (let ((alist (build-alist right)))
        (similarity left alist)))))

(assert (equal (part-1 *test-case*) 11))
(print (part-1 (get-file "input.txt")))

(assert (equal (part-2 *test-case*) 31))
(print (part-2 (get-file "input.txt")))
