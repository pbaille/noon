(ns noon.freeze
  (:require [noon.eval]
            #_[noon.utils.misc :refer [pretty-str]]
            [noon.utils.pseudo-random :as pseudo-random]
            [noon.freezer :refer [freezer]]
            #_(:cljs ["fs" :as fs]))
  #?(:cljs (:require-macros [noon.freeze :refer [freeze]])))

(def FREEZE_FILE "test/noon/freezer.cljc")

(defmacro freeze [form]
  (let [expr-hash (hash form)
        score-expr `(pseudo-random/with-rand 0
                      (noon.eval/eval-and-return '~form))]
    `(if-let [frozen# (get @freezer ~expr-hash)]
       (= frozen# (hash ~score-expr))
       (swap! freezer assoc ~expr-hash (hash ~score-expr)))))

#_(defn write-freezer []
  (let [spit-fn #?(:cljs (fn [file content]
                           (.writeFileSync fs file content "utf8"))
                   :clj spit)]
    (spit-fn FREEZE_FILE
             (str '(ns noon.freezer)
                  "\n"
                  (list 'def 'freezer
                        (list 'atom @freezer))))))

(comment
  (macroexpand '(freeze (play (tup s0 s1 s2))))
  (freeze (play (tup s0 s1 s3)))
  (swap! noon.output/options*
         assoc :mute false)
  @freezer
  (reset! freezer {}))
