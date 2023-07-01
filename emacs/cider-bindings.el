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
    (format "(do (upd-selection! %s) :ok)" (pb/thing-at-point))))
  :desc
  "focus-d1"
  :n
  "M-k"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    (format
     "(do (upd-focus! noon.score/d1) :ok)"
     (pb/thing-at-point))))
  :desc
  "focus-d1-"
  :n
  "M-j"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    (format
     "(do (upd-focus! noon.score/d1-) :ok)"
     (pb/thing-at-point))))
  :desc
  "focus-s1"
  :n
  "M-s-k"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    (format
     "(do (upd-focus! noon.score/s1) :ok)"
     (pb/thing-at-point))))
  :desc
  "focus-s1-"
  :n
  "M-s-j"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    (format
     "(do (upd-focus! noon.score/s1-) :ok)"
     (pb/thing-at-point))))))
