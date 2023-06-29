;;; noon-modes.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 pierre baille
;;
;; Author: pierre baille <pierrebaille@MBP2>
;; Maintainer: pierre baille <pierrebaille@MBP2>
;; Created: June 14, 2023
;; Modified: June 14, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/pierrebaille/modes
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'pb-reaper)

(defvar reaper-osc-client (osc-make-client (pb/get-local-ip) 8001))

(progn :noon-mode

       (defvar noon-mode-map (make-sparse-keymap))

       (define-minor-mode noon-mode "noon mode" :init-value nil :lighter " Noon" :keymap noon-mode-map))

(progn :reaper-mode

       (defvar reaper-mode-map (make-sparse-keymap))

       (define-minor-mode reaper-mode
         "reaper mode"
         :init-value nil
         :lighter " Reaper"
         :keymap reaper-mode-map)

       (defun noon/toggle-reaper-mode-cursor-color ()
         (if reaper-mode
             (progn (setq evil-normal-state-cursor '(box "#f09383")) (evil-normal-state 1))
           (progn (setq evil-normal-state-cursor '(box "#e95678")) (evil-normal-state 1))))

       (add-hook 'reaper-mode-hook 'noon/toggle-reaper-mode-cursor-color))

(defun noon/install-reaper-actions! ()
  (interactive)
  (my-cider/eval! "(noon.utils.reaper/install-edn-actions!)"))

(defun noon/reload-bindings! ()
  (interactive)
  (load "/Users/pierrebaille/Code/WIP/noon/emacs/reaper-bindings.el")
  (load "/Users/pierrebaille/Code/WIP/noon/emacs/cider-bindings.el"))

(map! :leader
      (:map cider-mode-map
       :desc "toggle noon mode" "t n" #'noon-mode))

(map! (:map noon-mode-map
       ;; at system level H-* is rebound to H-C-* in order to avoid osx default bindings
       :desc "init reaper interop" "H-C-c" #'noon/init-reaper-interop!
       (:prefix ("H-C-r" . "reload")
        :desc "reload actions and bindings" "r" #'noon/init-actions-and-bindings!
        :desc "reload noon cider bindings" "b" #'noon/reload-bindings!
        :desc "reload reaper actions" "a" #'noon/install-reaper-actions!)
       (:prefix ("H-C-l" . "log")
        :desc "focus" "f" (lambda () (interactive) (print "focus")))))

(map! (:map noon-mode-map
            ;; at the system level the fn key pressed alone emits C-M-s-<return> (via karabiner)
            "C-M-s-<return>" (lambda () (interactive) (reaper-mode 1)))
      (:map reaper-mode-map
       :n "<escape>" (lambda () (interactive) (reaper-mode -1))))

(defun noon/connect-reaper! ()
  (interactive)
  ;; launch socket-repl script
  (osc-send-message reaper-osc-client "/action/_RSb0f401791ac0eba7140bad3965dc7833c18e650d"))

(defun noon/reset-reaper-connection! ()
  (interactive)
  (my-cider/eval! "(noon.utils.reaper/reset-reaper-input-chan!)")
  (noon/connect-reaper!))

(defun noon/init-actions-and-bindings! ()
  (interactive)
  ;; register actions and install keybindings
  (noon/install-reaper-actions!)

  (noon/reload-bindings!))

(defun noon/init-reaper-interop! ()
  (interactive)
  (noon/connect-reaper!)
  (sleep-for 1)
  (noon/init-actions-and-bindings!))

(provide 'noon-modes)
;;; noon-modes.el ends here
