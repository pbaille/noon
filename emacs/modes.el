;;; modes.el --- Description -*- lexical-binding: t; -*-
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

(defvar reaper-osc-client (osc-make-client "192.168.1.60" 8001))

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

       (defun toggle-reaper-mode-cursor-color ()
         (if reaper-mode
             (progn (setq evil-normal-state-cursor '(box "#f09383")) (evil-normal-state 1))
           (progn (setq evil-normal-state-cursor '(box "#e95678")) (evil-normal-state 1))))

       (add-hook 'reaper-mode-hook 'toggle-reaper-mode-cursor-color))

(defun my-cider/eval! (code)
  (interactive)
  (cider-interactive-eval code
                          nil nil
                          (cider--nrepl-pr-request-map)))

(defun reaper/install-actions! ()
  (interactive)
  ;; (my-cider/eval! "(noon.utils.reaper/install-edn-actions!)")
  ;;(load "/Users/pierrebaille/Code/WIP/noon/emacs/reaper-bindings.el")
  (load "/Users/pierrebaille/Code/WIP/noon/emacs/cider-bindings.el"))

(defun pb/current-s-expression-as-string ()
  (interactive)
  (buffer-substring-no-properties
   (point)
   (+ 1 (save-excursion (evil-jump-item) (point)))))

(defun reaper-noon/update-selection! ()
  (interactive)
  (my-cider/eval! (concat "(noon.lib.reaper/upd-selection! " (pb/current-s-expression-as-string) ")")))

(map! (:map cider-mode-map
            "C-M-s-n" #'noon-mode)
      (:map noon-mode-map
            "C-M-s-m" (lambda () (interactive) (reaper-mode 1))
            "C-M-s-z" (lambda () (interactive) (reaper-mode 1))
            "C-M-s-r" #'reaper/install-actions!
            "C-M-s-u" #'reaper-noon/update-selection!)
      (:map reaper-mode-map
            :n "<escape>" (lambda () (interactive) (reaper-mode -1))))

(reaper/install-actions!)

(provide 'noon-modes)
;;; modes.el ends here
