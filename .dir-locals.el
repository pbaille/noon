((nil . ((eval . (progn (require 'pb-org-babel)

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
                                       nil))))))))
