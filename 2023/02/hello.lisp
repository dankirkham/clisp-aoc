(ql:quickload "str")

(defparameter *test* "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(defun parse-game (game)
  (split ":" game)
  )

(print (parse-game *test*))
