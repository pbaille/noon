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

;; evaluate this to fontify the guide.org buffer
(pb-clojure-babel_refresh-dynamic-font-lock-keywords
 " *org-src-fontification:clojure-mode*"
 "noon.doc.guide")
(pb-clojure-babel_refresh-dynamic-font-lock-keywords
 " *org-src-fontification:clojure-mode*"
 "noon.doc.examples")

(defun pb-org-babel_edit-src-code-hook (fun &optional code buf-name)
  (funcall fun code buf-name)
  (cond ((equal (buffer-name) "*Org Src guide.org[ clojure ]*")
         (pb-clojure-babel_refresh-dynamic-font-lock-keywords
          "*Org Src guide.org[ clojure ]*"
          "noon.doc.guide"))
        ((equal (buffer-name) "*Org Src examples.org[ clojure ]*")
         (pb-clojure-babel_refresh-dynamic-font-lock-keywords
          "*Org Src examples.org[ clojure ]*"
          "noon.doc.examples")))
  (flycheck-mode -1)
  (symex-mode-interface))

(advice-add 'org-edit-src-code :around #'pb-org-babel_edit-src-code-hook)

(require 'pb-org-babel)

(pb-org-babel_add-custom-param
 :proll :clojure
 (km :content (lambda (content)
                (format "((requiring-resolve 'noon.doc.utils/->piano-roll) %s)"
                        content))
     :result (lambda (result)
               (with-current-buffer (get-buffer-create "*pr*")
                (erase-buffer)
                (insert (format "'%s" result))
                (proll-mode 1))
               nil)))
