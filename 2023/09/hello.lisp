(defparameter *test* '(
  (0 3 6 9 12 15)
  (1 3 6 10 15 21)
  (10 13 16 21 30 45)
))

(defun history (line &optional (out '()))
  (if (cdr line)
    (history (cdr line) (cons (- (cadr line) (car line)) out))
    (reverse out)))
(assert (equal (history '(0 3 6 9 12 15)) '(3 3 3 3 3)))
(assert (equal (history '(3 3 3 3 3)) '(0 0 0 0)))
(assert (equal (history '(10 13 16 21 30 45 68)) '(3 3 5 9 15 23)))

(defun check-all-zero (line)
  (if (not (eq (car line) 0))
    nil
    (if (cadr line)
      (check-all-zero (cdr line))
      t)))
(assert (equal (check-all-zero '(0 0 0 0)) t))
(assert (equal (check-all-zero '(0 1 0 0)) nil))
(assert (equal (check-all-zero '(0 0 0 1)) nil))

(defun histories (now &optional (past '()))
  (let ((hist (history now)))
    (if (check-all-zero hist)
      (cons hist (cons now past))
      (histories hist (cons now past)))))

(defun first-vals (lists &optional (acc '()))
  (let ((next (cons (car (car lists)) acc)))
    (if (cadr lists)
      (first-vals (cdr lists) next)
      (reverse next))))
(assert (equal
          (first-vals '((0 0) (2 2 2) (0 2 4 6) (3 3 5 9 15) (10 13 16 21 30 45)))
          '(0 2 0 3 10)))

(defun last-vals (lists &optional (acc '()))
  (let ((next (cons (car (reverse (car lists))) acc)))
    (if (cadr lists)
      (last-vals (cdr lists) next)
      (reverse next))))
(assert (equal
          (last-vals '((0 0) (2 2 2) (0 2 4 6) (3 3 5 9 15) (10 13 16 21 30 45)))
          '(0 2 6 15 45)))

(defun history-value (hist) (apply '+ hist))
(assert (eq (history-value '(0 2 6 15 45)) 68))

(defun history-value-l (hist &optional (acc 0))
  (if (cadr hist)
    (history-value-l (cdr hist) (- (car hist) acc))
    (- (car hist) acc)
   ))
(assert (eq (history-value-l  '(0 3 0)) -3))
(assert (eq (history-value-l  '(0 1 2 1)) 0))
(assert (eq (history-value-l  '(0 2 0 3 10)) 5))

(defun extrapolate-r (hists) (history-value (last-vals hists)))
(defun extrapolate-l (hists) (history-value-l (first-vals hists)))

(defun parse-line (string)
  (loop :for (integer position) := (multiple-value-list 
                                    (parse-integer string
                                                   :start (or position 0)
                                                   :junk-allowed t))
        :while integer
        :collect integer))

(defun parse-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))

(defun part-1 (hists) (apply '+ (mapcar #'extrapolate-r hists)))
(defun part-2 (hists) (apply '+ (mapcar #'extrapolate-l hists)))

(defun process (seqs)
  (let ((hists (mapcar #'histories seqs)))
    (list (part-1 hists) (part-2 hists))))

(assert (equal (process *test*) '(114 2)))
(print (process (parse-lines "input.txt")))
