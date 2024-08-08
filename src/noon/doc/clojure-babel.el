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
'(pb-clojure-babel_refresh-dynamic-font-lock-keywords
 " *org-src-fontification:clojure-mode*"
 "noon.doc.guide")
'(pb-clojure-babel_refresh-dynamic-font-lock-keywords
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
(require 'pb-cider)

(defun pb-org-babel_jack-in ()
  "Setup clojure literate org buffer.
- cider-jack-in-clj if necessary,
- send top block ns form to the repl,
- set the corresponding namespace for code blocks fontification."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (not (cider-connected-p))
      '(print "starting cider repl")
      (call-interactively #'cider-jack-in-clj)
      (sit-for 5))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        '(print "will evaluate top clojure form")
        (when (re-search-forward "#\\+begin_src clojure" nil t)
          '(print "find first block")
          (let* ((element (org-element-context)))
            (when (eq (org-element-type element) 'src-block)
              '(print "code block found")
              (let ((block-content (org-element-property :value element)))
                '(print (concat "will eval:\n " block-content))
                (pb-cider_eval! block-content)
                (sit-for 3)
                (string-match "(ns \\([^ ]+\\)" block-content)
                '(print "will refresh fontification")
                '(print (concat "ns: " (string-trim-right (match-string 1 block-content))))
                (pb-clojure-babel_refresh-dynamic-font-lock-keywords
                 " *org-src-fontification:clojure-mode*"
                 (string-trim-right (match-string 1 block-content)))))))))))


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
