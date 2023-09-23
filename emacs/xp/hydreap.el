
;;; -*- lexical-binding: t; -*-

(defun hydreap-send (code)
  (cider-interactive-eval (concat "(reaper/>> " (prin1-to-string code) ")")
                          nil nil
                          (cider--nrepl-pr-request-map)))

(defmacro ifn (&rest body)
  `(lambda () (interactive) (progn ,@body)))

(eval
 `(defhydra hydra-reaper (:color pink :columns 4)
    "Window Management"
    ("h" ,(ifn (ru.take.cursor.update T -1))
     "cursor bw")
    ("q" nil "quit" :exit t)
    ("<escape>" nil "quit" :exit t)))

(map! "C-s-r" 'hydra-reaper/body)

["C-c h" "root"
 (:color pink)
 ["a" (message "a")]
 ["b" "child" (:color pink)
  ["x" (message "x")]
  ["y" "i'm exiting" (:exit t) (message "y")]]]

(defhydra noon-hydra/root2 (:color teal)
  "main noon hydra"
  ("q" nil "quit" :exit t)
  ("f" #'noon-hydra/focus/body "focus")
  ("s" #'noon-hydra/selection/body "selection")
  ("S" #'noon-hydra/score/body "score"))

(defhydra noon-hydra/focus (:color teal)
  "act on focused note"
  ("q" nil "quit" :exit t)
  ("d" #'noon-hydra/focus/diatonic/body "diatonic step")
  ("s" #'noon-hydra/focus/structural/body "strutural step")
  ("c" #'noon-hydra/focus/chromatic/body "chromatic step"))

(defhydra noon-hydra/focus/diatonic (:color teal)
  ("q" nil "quit" :exit t)
  ("<escape>" #'noon-hydra/focus/body "focus")
  ("j" (my-cider/eval! "(noon.lib.reaper0/upd-focus! noon.score/d1)") "down" :color red)
  ("k" (my-cider/eval! "(noon.lib.reaper0/upd-focus! noon.score/d1-)") "up" :color red))

(defhydra noon-hydra/focus/structural (:color teal)
  ("q" nil "quit" :exit t)
  ("<escape>" #'noon-hydra/focus/body "focus")
  ("j" (my-cider/eval! "(noon.lib.reaper0/upd-focus! noon.score/s1)") "down" :color red)
  ("k" (my-cider/eval! "(noon.lib.reaper0/upd-focus! noon.score/s1-)") "up" :color red))

(defhydra noon-hydra/focus/chromatic (:color teal)
  ("q" nil "quit" :exit t)
  ("<escape>" #'noon-hydra/focus/body "focus")
  ("j" (my-cider/eval! "(noon.lib.reaper0/upd-focus! noon.score/c1)") "down" :color red)
  ("k" (my-cider/eval! "(noon.lib.reaper0/upd-focus! noon.score/c1-)") "up" :color red))

(map! "C-c v" #'noon-hydra/root2/body)
