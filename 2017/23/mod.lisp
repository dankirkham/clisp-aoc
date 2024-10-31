(defparameter *muls* 0)
(defparameter *registers* '(
  ("a" . 0)
  ("b" . 0)
  ("c" . 0)
  ("d" . 0)
  ("e" . 0)
  ("f" . 0)
  ("g" . 0)
  ("h" . 0)
))

(defun fetch-register (key)
  (let ((entry (assoc key *registers* :test 'string=)))
    (when entry
      (cdr entry))))
(assert (eq (fetch-register "b") 0))

(defun fetch-src (y)
  (let ((imm (parse-integer y :junk-allowed t)))
    (if imm
      imm
      (fetch-register y))))
(assert (equal (fetch-src "10") 10))
(assert (equal (fetch-src "b") 0))

(defun my-set (x y)
    (push (cons x (fetch-src y)) *registers*)
    (lambda (pc) (+ pc 1)))

(defun my-sub (x y)
  (push (cons x (- (fetch-register x) (fetch-src y))) *registers*)
  (lambda (pc) (+ pc 1)))

(defun my-mul (x y)
  (setq *muls* (+ *muls* 1))
  (push (cons x (* (fetch-register x) (fetch-src y))) *registers*)
  (lambda (pc) (+ pc 1)))

(defun my-jnz (x y)
  (let ((offset (fetch-src y)))
    (if (eq (fetch-src x) 0)
      (lambda (pc) (+ pc 1))
      (lambda (pc) (+ pc offset)))))

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

(defun swap-symbols (val)
  (cond
    ((equal "set" (car val)) (cons #'my-set (cdr val)))
    ((equal "sub" (car val)) (cons #'my-sub (cdr val)))
    ((equal "mul" (car val)) (cons #'my-mul (cdr val)))
    ((equal "jnz" (car val)) (cons #'my-jnz (cdr val)))))

(defun read-instrs (filename)
  (mapcar (lambda (val) (swap-symbols (str-split val " "))) (get-file filename)))

(defun run-vm (instructions &optional (pc 0))
  (let ((instruction (nth pc instructions)))
    (if instruction
      (let ((pc-func (funcall
                         (car instruction)
                         (cadr instruction)
                         (cadr (cdr instruction)))))
        (run-vm instructions (funcall pc-func pc)))
      nil)))

(run-vm (read-instrs "input.txt"))
(print *muls*)
