(map!
 (:map
  reaper-mode-map
  :n
  "<escape>"
  (lambda () (interactive) (reaper-mode -1))
  :desc
  "repeat-last-command"
  :n
  "."
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 2999))
  :desc
  "redo"
  :n
  "C-r"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 40014))
  :desc
  "time-selection-shift-fw"
  :n
  "M-l"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65833))
  :desc
  "time-selection-shift-bw"
  :n
  "M-h"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65834))
  :desc
  "time-selection-shrink-fw"
  :n
  "M-H"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65814))
  :desc
  "time-selection-shrink-bw"
  :n
  "M-L"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65815))
  :desc
  "time-selection-grow-fw"
  :n
  "L"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65816))
  :desc
  "time-selection-grow-bw"
  :n
  "H"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65817))
  :desc
  "time-selection-clear"
  :n
  "X"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65818))
  :desc
  "cursor-step-grid-fw"
  :n
  "l"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65819))
  :desc
  "cursor-step-grid-bw"
  :n
  "h"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65820))
  :desc
  "cursor-goto-beginning"
  :n
  "g g"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65821))
  :desc
  "note-insert"
  :n
  "i"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65822))
  :desc
  "note-step-fw"
  :n
  "f"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65839))
  :desc
  "note-step-bw"
  :n
  "b"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65835))
  :desc
  "note-toggle-selection"
  :n
  "t"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65837))
  :desc
  "note-channel-up"
  :n
  "c k"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65838))
  :desc
  "note-channel-down"
  :n
  "c j"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65840))
  :desc
  "note-velocity-up"
  :n
  "v k"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65841))
  :desc
  "note-velocity-down"
  :n
  "v j"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65842))
  :desc
  "undo"
  :n
  "u"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 40013))
  :desc
  "pitch-cursor-step-semitone-up"
  :n
  "k"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65843))
  :desc
  "pitch-cursor-step-semitone-down"
  :n
  "j"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65844))
  :desc
  "play"
  :n
  "P"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 40073))
  :desc
  "selection-unselect-all"
  :n
  "T"
  (lambda
   ()
   (interactive)
   (osc-send-message reaper-osc-client "/action" 65823))))
