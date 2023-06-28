(map!
 (:map
  noon-mode-map
  :desc
  "score-upd"
  :n
  "H-C-u"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    (format "(do (upd-selection! %s) :ok)" (pb/thing-at-point))))))

(map!
 (:map
  reaper-mode-map
  :desc
  "score-upd"
  :n
  "U"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    (format "(do (reset-score! %s) :ok)" (pb/thing-at-point))))
  :desc
  "selection-upd"
  :n
  "u"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    (format "(do (upd-selection! %s) :ok)" (pb/thing-at-point))))))
