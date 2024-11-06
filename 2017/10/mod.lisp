(defun twist (jl len)
  (let (
        (pl (cdr jl))
        (pr (nthcdr (- len 1) (cdr jl)))
        (jr (nthcdr len (cdr jl))))
    ;; j = jack
    ;; p = plug
    ;; ---jl-> <-pl-----pr-> <-jr---
    ;; break jl <-> pr
    (setf (cdr jl) nil)
    ;; break pr <-> jr
    (setf (cdr pr) nil)
    ;; reverse segment
    (nreverse pl)
    ;; connect jl <-> pr
    (setf (cdr jl) pr)
    ;; connect pl <-> jr
    (setf (cdr pl) jr)
    jl))
(assert (equal (twist '(1 2 3 4 5) 3) '(1 4 3 2 5)))

(defun circular! (items)
  "Modifies the last cdr of list ITEMS, returning a circular list"
  (setf (cdr (last items)) items))

