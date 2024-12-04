(defparameter *test-case* '(
  "MMMSXXMASM~%"
  "MSAMXMSMSA~%"
  "AMXSXMAAMM~%"
  "MSAMASMSMX~%"
  "XMASAMXAMM~%"
  "XXAMMXXAMA~%"
  "SMSMSASXSS~%"
  "SAXAMASAAA~%"
  "MAMMMXMMMM~%"
  "MXMXAXMASX~%"))
(defparameter *target* '(#\X #\M #\A #\S))

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

(defun str-chars (s)
  (loop for i to (- (length s) 1)
  collect (char s i)))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun parse-row (line)
  (str-chars (string-trim " ~%" line)))

(defun parse-grid (lines)
  (list-to-2d-array (mapcar #'parse-row lines)))


(defun predicate-n (grid x y target max-x max-y)
  (if (> y 0)
    (check-cell #'predicate-n grid x (- y 1) (cdr target)) 0))

(defun predicate-s (grid x y target max-x max-y)
  (if (< y max-y)
    (check-cell #'predicate-s grid x (+ y 1) (cdr target)) 0))

(defun predicate-w (grid x y target max-x max-y)
  (if (> x 0)
    (check-cell #'predicate-w grid (- x 1) y (cdr target)) 0))

(defun predicate-e (grid x y target max-x max-y)
  (if (< x max-x)
    (check-cell #'predicate-e grid (+ x 1) y (cdr target)) 0))

(defun predicate-nw (grid x y target max-x max-y)
  (if (and (> y 0) (> x 0))
    (check-cell #'predicate-nw grid (- x 1) (- y 1) (cdr target)) 0))

(defun predicate-ne (grid x y target max-x max-y)
  (if (and (> y 0) (< x max-x))
    (check-cell #'predicate-ne grid (+ x 1) (- y 1) (cdr target)) 0))

(defun predicate-sw (grid x y target max-x max-y)
  (if (and (< y max-y) (> x 0))
    (check-cell #'predicate-sw grid (- x 1) (+ y 1) (cdr target)) 0))

(defun predicate-se (grid x y target max-x max-y)
  (if (and (< y max-y) (< x max-x))
    (check-cell #'predicate-se grid (+ x 1) (+ y 1) (cdr target)) 0))

(defun check-cell (predicate grid x y target)
  (let ((max-y (- (array-dimension grid 0) 1))
        (max-x (- (array-dimension grid 1) 1)))
    (if (equal (aref grid y x) (car target))
      (if (cdr target)
        (funcall predicate grid x y target max-x max-y)
        1)
      0)))

(defun check-cell-top (grid x y target)
  (let ((max-y (- (array-dimension grid 0) 1))
        (max-x (- (array-dimension grid 1) 1)))
    (if (equal (aref grid y x) (car target))
      (if (cdr target)
        (reduce #'+ (list
                ;; N
                (if (> y 0)
                  (check-cell #'predicate-n grid x (- y 1) (cdr target)) 0)
                ;; S
                (if (< y max-y)
                  (check-cell #'predicate-s grid x (+ y 1) (cdr target)) 0)
                ;; W
                (if (> x 0)
                  (check-cell #'predicate-w grid (- x 1) y (cdr target)) 0)
                ;; E
                (if (< x max-x)
                  (check-cell #'predicate-e grid (+ x 1) y (cdr target)) 0)
                ;; NW
                (if (and (> y 0) (> x 0))
                  (check-cell #'predicate-nw grid (- x 1) (- y 1) (cdr target)) 0)
                ;; NE
                (if (and (> y 0) (< x max-x))
                  (check-cell #'predicate-ne grid (+ x 1) (- y 1) (cdr target)) 0)
                ;; SW
                (if (and (< y max-y) (> x 0))
                  (check-cell #'predicate-sw grid (- x 1) (+ y 1) (cdr target)) 0)
                ;; SE
                (if (and (< y max-y) (< x max-x))
                  (check-cell #'predicate-se grid (+ x 1) (+ y 1) (cdr target)) 0)))
        1)
      0)))

;; (defun check-cell-m (grid x y)
;;   (let ((max-y (- (array-dimension grid 0) 1))
;;         (max-x (- (array-dimension grid 1) 1)))
;;     (if (equal #\M (car target))
;;       (if (cdr target)
;;         (reduce #'+ (list
;;                 ;; N
;;                 (if (> y 0)
;;                   (check-cell-a grid x (- y 1) (cdr target)) 0)
;;                 ;; S
;;                 (if (< y max-y)
;;                   (check-cell-a grid x (+ y 1) (cdr target)) 0)
;;                 ;; W
;;                 (if (> x 0)
;;                   (check-cell-a grid (- x 1) y (cdr target)) 0)
;;                 ;; E
;;                 (if (< x max-x)
;;                   (check-cell-a grid (+ x 1) y (cdr target)) 0)
;;                 ;; NW
;;                 (if (and (> y 0) (> x 0))
;;                   (check-cell-a grid (- x 1) (- y 1) (cdr target)) 0)
;;                 ;; NE
;;                 (if (and (> y 0) (< x max-x))
;;                   (check-cell-a grid (+ x 1) (- y 1) (cdr target)) 0)
;;                 ;; SW
;;                 (if (and (< y max-y) (> x 0))
;;                   (check-cell-a grid (- x 1) (+ y 1) (cdr target)) 0)
;;                 ;; SE
;;                 (if (and (< y max-y) (< x max-x))
;;                   (check-cell-a grid (+ x 1) (+ y 1) (cdr target)) 0)))
;;         1)
;;       0)))

(defun range (start stop step)
  (do (
    (i start (+ i step))
    (acc '() (push i acc)))
   ((>= i stop) (nreverse acc))))

(defun process-row (grid y max-x)
  (reduce #'+ (mapcar (lambda (x) (check-cell-top grid x y *target*)) (range 0 (+ max-x 1) 1))))

(defun part-1 (grid)
  (let ((max-y (- (array-dimension grid 0) 1))
        (max-x (- (array-dimension grid 1) 1)))
    (reduce #'+ (mapcar (lambda (y) (process-row grid y max-x)) (range 0 (+ max-y 1) 1)))))

(let ((grid (parse-grid *test-case*)))
  (assert (equal (check-cell-top grid 4 0 *target*) 1))
  (assert (equal (check-cell-top grid 5 0 *target*) 1))
  (assert (equal (check-cell-top grid 4 1 *target*) 1))
  (assert (equal (check-cell-top grid 9 3 *target*) 2))
  (assert (equal (check-cell-top grid 0 4 *target*) 1))
  (assert (equal (check-cell-top grid 6 4 *target*) 2))
  (assert (equal (check-cell-top grid 0 5 *target*) 1))
  (assert (equal (check-cell-top grid 6 5 *target*) 1))
  (assert (equal (check-cell-top grid 1 9 *target*) 1))
  (assert (equal (check-cell-top grid 3 9 *target*) 2))
  (assert (equal (check-cell-top grid 5 9 *target*) 3))
  (assert (equal (check-cell-top grid 9 9 *target*) 2)))

(assert (equal (part-1 (parse-grid *test-case*)) 18))
(print (part-1 (parse-grid (get-file "input.txt"))))
