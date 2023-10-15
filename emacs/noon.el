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

(defvar noon/reaper-osc-client (osc-make-client (pb/get-local-ip) 8001))
(defvar noon/source-path "/Users/pierrebaille/Code/WIP/noon")
(defvar noon/emacs-app-path "/usr/local/Cellar/emacs-mac/emacs-29.1-mac-10.0/Emacs.app")
(defvar noon/socket-repl-action-id "_RSb0f401791ac0eba7140bad3965dc7833c18e650d")

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
             (progn (setq-local evil-normal-state-cursor '(box "#f09383")) (evil-normal-state 1))
           (progn (setq-local evil-normal-state-cursor '(box "#e95678")) (evil-normal-state 1))))

       (add-hook 'reaper-mode-hook 'noon/toggle-reaper-mode-cursor-color))

(defun noon/install-reaper-actions! ()
  "Register actions from reaper-actions.edn to reaper and compile reaper-bindings.el."
  (interactive)
  (my-cider/eval! (concat "(noon.utils.reaper.actions/install-actions! \"" noon/source-path "/emacs/reaper-actions.edn\")")))

(defun noon/reload-bindings! ()
  "Reload noon and reaper bindings."
  (interactive)
  (load (concat noon/source-path "/emacs/compiled/reaper-bindings.el"))
  (load (concat noon/source-path "/emacs/compiled/cider-bindings.el")))

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
  "Start the socket-repl script in reaper."
  (interactive)
  ;; launch socket-repl script
  (osc-send-message noon/reaper-osc-client (concat "/action/" noon/socket-repl-action-id)))

(defun noon/reset-reaper-connection! ()
  "Reset the udp channel and relaunch the socket-repl script."
  (interactive)
  (my-cider/eval! "(noon.utils.reaper.interop/reset-reaper-input-chan!)")
  (noon/connect-reaper!))

(defun noon/launch-fluidsynth! ()
  "Start a fluidsynth process."
  (interactive)
  (when (not (get-process "fluidsynth1"))
      (start-process-shell-command "fluidsynth1" "*fluidsynth1*" (concat noon/source-path "/scripts/fluidsynth1"))
      (sleep-for 2)))

(defun noon/open-reaper! ()
  "Open reaper with default noon template that use fluidsynth."
  (interactive)
  (noon/launch-fluidsynth!)
  (when (not (get-process "reaper"))
    (start-process-shell-command "reaper" "*reaper*" (concat noon/source-path "/scripts/launch_reaper"))
    (shell-command (concat "open -a " noon/emacs-app-path))
    (sleep-for 6)))

(defun noon/init-actions-and-bindings! ()
  "Recompile reaper-actions.edn then reload all bindings."
  (interactive)
  ;; register actions and install keybindings
  (noon/install-reaper-actions!)

  (noon/reload-bindings!))

(defun noon/init-reaper-interop! ()
  "Launch socket-repl script and reinit bindings."
  (interactive)
  (noon/connect-reaper!)
  (sleep-for 1)
  (noon/init-actions-and-bindings!))

(defun noon/reaper! ()
  "Launch fluidsynth, reaper, load actions and bindings."
  (interactive)
  (noon/open-reaper!)
  (noon/init-actions-and-bindings!))

(provide 'noon)
;;; noon.el ends here
