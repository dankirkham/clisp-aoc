(defparameter *line* "absixcd5efg6eas7weoneightc")

(defparameter *map-1* (list
  '("0" #\0)
  '("1" #\1)
  '("2" #\2)
  '("3" #\3)
  '("4" #\4)
  '("5" #\5)
  '("6" #\6)
  '("7" #\7)
  '("8" #\8)
  '("9" #\9)
))

(defparameter *map-2* (concatenate 'list *map-1* (list
  '("zero" #\0)
  '("one" #\1)
  '("two" #\2)
  '("three" #\3)
  '("four" #\4)
  '("five" #\5)
  '("six" #\6)
  '("seven" #\7)
  '("eight" #\8)
  '("nine" #\9)
)))

(defun iter-substrs (str)
  (loop for i to (- (length str) 1)
        collect (subseq str i)))

(defun iter-substrs-rev (str)
  (loop for i
        from (- (length str) 1)
        downto 0
        collect (subseq str i)))

(defun check-map (line symbol-map)
  (loop for symbol
        in symbol-map
        when (eq (search (car symbol) line) 0)
        return (cadr symbol)))

(defun substrs (line iter-fun symbol-map)
  "Find the number on the left"
  (loop for substr
        in (funcall iter-fun line)
        when (check-map substr symbol-map)
        return (check-map substr symbol-map)
  )
)
(defun left-num (line symbol-map) (substrs line #'iter-substrs symbol-map))
(defun right-num (line symbol-map) (substrs line #'iter-substrs-rev symbol-map))

(defun process-line (line symbol-map)
  (parse-integer (coerce
    (list (left-num line symbol-map) (right-num line symbol-map))
    'string)))

(defun process-lines (filename symbol-map)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (process-line line symbol-map))))

(print (apply '+ (process-lines "input.txt" *map-1*)))
(print (apply '+ (process-lines "input.txt" *map-2*)))
