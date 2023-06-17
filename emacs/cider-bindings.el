(map!
 (:map
  reaper-mode-map
  :desc
  "focus-closest"
  :n
  "F"
  (lambda () (interactive) (my-cider/eval! "(focus-closest!)"))
  :desc
  "focus-fw"
  :n
  "l"
  (lambda () (interactive) (my-cider/eval! "(focus-move! 1)"))
  :desc
  "focus-bw"
  :n
  "h"
  (lambda () (interactive) (my-cider/eval! "(focus-move! -1)"))
  :desc
  "focus-s1"
  :n
  "K"
  (lambda () (interactive) (my-cider/eval! "(focus-upd! noon/s1)"))
  :desc
  "focus-s1-"
  :n
  "J"
  (lambda () (interactive) (my-cider/eval! "(focus-upd! noon/s1-)"))
  :desc
  "cursor-fw"
  :n
  "f"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    "(upd-reaper! (fn [{:as reaper, :keys [grid focus]}] (update-in reaper [:focus 0] + grid)))"))
  :desc
  "cursor-bw"
  :n
  "b"
  (lambda
   ()
   (interactive)
   (my-cider/eval!
    "(upd-reaper! (fn [{:as reaper, :keys [grid focus]}] (update-in reaper [:focus 0] - grid)))"))))
