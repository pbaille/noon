
;;; -*- lexical-binding: t; -*-

(defun hydreap-send (code)
  (cider-interactive-eval (concat "(reaper/>> " (prin1-to-string code) ")")
                          nil nil
                          (cider--nrepl-pr-request-map)))

(defmacro ifn (&rest body)
  `(lambda () (interactive) (progn ,@body)))

(defhydra hydra-reaper (:color pink :columns 4)
  "Window Management"
  ("h" (lambda () (interactive) (hydreap-send "(ru.take.cursor.update T -1)"))
   "cursor bw")
  ("l" (lambda () (interactive) (hydreap-send "(ru.take.cursor.update T 1)"))
   "cursor fw")
  ("j" (lambda () (interactive) (hydreap-send "(ru.midi-editor.pitch-cursor.update E -1)"))
   "cursor down")
  ("k" (lambda () (interactive) (hydreap-send "(ru.midi-editor.pitch-cursor.update E 1)"))
   "cursor up")
  ("q" nil "quit" :exit t)
  ("<escape>" nil "quit" :exit t))

(map! "C-s-r" 'hydra-reaper/body)
