;;; -*- lexical-binding: t; -*-

(require 'cider)
(defun pb-clojure-babel_refresh-dynamic-font-lock-keywords (ns)
  "Install font-lock rules according to NS.
The *org-src-fontification:clojure-mode* buffer is used to fontify clojure code.
For blocks to be correctly fontified, we need to install those using cider."
  (with-current-buffer (get-buffer-create
                        " *org-src-fontification:clojure-mode*")
    (setq-local cider-buffer-ns ns)
    (font-lock-remove-keywords nil cider--dynamic-font-lock-keywords)
    (setq-local cider--dynamic-font-lock-keywords
                (cider--compile-font-lock-keywords
                 (cider-resolve-ns-symbols ns)
                 (cider-resolve-ns-symbols (cider-resolve-core-ns))))
    (font-lock-add-keywords nil cider--dynamic-font-lock-keywords 'end)
    (font-lock-flush)))

;; evaluate this to fontify the guide.org buffer
(pb-clojure-babel_refresh-dynamic-font-lock-keywords "noon.doc.intro")
