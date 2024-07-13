((nil . ((eval . (add-hook 'hack-local-variables-hook
                           (lambda ()
                             (load (concat (projectile-project-root)
                                           "src/noon/doc/clojure-babel.el")))
                           nil t)))))
