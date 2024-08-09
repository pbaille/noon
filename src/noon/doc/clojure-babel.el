;;; -*- lexical-binding: t; -*-

(require 'cider)
(defun pb-clojure-babel_refresh-dynamic-font-lock-keywords (buffer ns)
  "Install font-lock rules according to NS for BUFFER.
The *org-src-fontification:clojure-mode* buffer is used to fontify clojure code.
For blocks to be correctly fontified, we need to install those using cider."
  (with-current-buffer (get-buffer-create
                        buffer)
    (setq-local cider-buffer-ns ns)
    (font-lock-remove-keywords nil cider--dynamic-font-lock-keywords)
    (setq-local cider--dynamic-font-lock-keywords
                (cider--compile-font-lock-keywords
                 (cider-resolve-ns-symbols ns)
                 (cider-resolve-ns-symbols (cider-resolve-core-ns))))
    (font-lock-add-keywords nil cider--dynamic-font-lock-keywords 'end)
    (font-lock-flush)))

(defun pb-org-babel_get-clojure-namespace ()
  "Retreive the clojure namespace of the current org buffer.
The ns declaration is assumed to be the first clojure block of the file."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#\\+begin_src clojure" nil t)
      (let* ((element (org-element-context)))
        (when (eq (org-element-type element) 'src-block)
          (let ((block-content (org-element-property :value element)))
            (string-match "(ns \\([^ ]+\\)" block-content)
            (list :ns-name (string-trim-right (match-string 1 block-content))
                  :ns-form block-content)))))))

(defun pb-org-babel_edit-src-code-hook (fun &optional code buf-name)
  (let ((clojure-ns (km_get (pb-org-babel_get-clojure-namespace)
                            :ns-name)))
    (funcall fun code buf-name)
    (when clojure-ns
      (pb-clojure-babel_refresh-dynamic-font-lock-keywords
       (concat "*Org Src " (buffer-name (current-buffer)) "[ clojure ]*")
       clojure-ns)
      (flycheck-mode -1)
      (symex-mode-interface))))

(advice-add 'org-edit-src-code :around #'pb-org-babel_edit-src-code-hook)

(require 'pb-org-babel)
(require 'pb-cider)

(defun pb-org-babel_jack-in ()
  "Setup clojure literate org buffer.
- cider-jack-in-clj if necessary,
- send top block ns form to the repl,
- set the corresponding namespace for code blocks fontification."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (not (cider-connected-p))
        (if (yes-or-no-p "Start Cider repl? ")
            (progn
              (message "Starting cider repl")
              (call-interactively #'cider-jack-in-clj)))
      (with-current-buffer buffer
        (print (pb-org-babel_get-clojure-namespace))
        (pb_let [(km ns-name ns-form) (pb-org-babel_get-clojure-namespace)]
            (progn (print (list ns-name ns-form))
                   (pb-cider_eval! ns-form)
                   (sit-for 3)
                   (pb-clojure-babel_refresh-dynamic-font-lock-keywords
                    " *org-src-fontification:clojure-mode*"
                    ns-name)
                   (revert-buffer buffer)))))))


(pb-org-babel_add-custom-param
 :proll :clojure
 (km :content (lambda (content)
                (format "((requiring-resolve 'noon.doc.utils/->piano-roll) %s)"
                        content))
     :result (lambda (result)
               ;; (pp (cons :proll result))
               (with-current-buffer (get-buffer-create "*pr*")
                 (erase-buffer)
                 (insert (format "'%s" result))
                 (proll-mode 1))
               nil)))
