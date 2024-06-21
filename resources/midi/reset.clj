(use 'noon.score)
(write {:midi true
        :filename "reset"}
       (mk (ef_ (dissoc _ :pitch :duration :velocity))
           (par* (map chan (range 0 16)))
           ($ (par (patch :electric-piano-1)
                   (cc 10 64)
                   (cc 7 100)
                   (cc 11 127)
                   (cc 100 0)
                   (cc 101 0)
                   (cc 6 2)))))
