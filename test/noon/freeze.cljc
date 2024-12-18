(ns noon.freeze
  (:require [noon.eval]
            #_[noon.utils.misc :refer [pretty-str]]
            [noon.utils.pseudo-random :as pseudo-random]
            [noon.freezer :refer [freezer]]
            #_(:cljs ["fs" :as fs]))
  #?(:cljs (:require-macros [noon.freeze :refer [freeze]])))

(def FREEZE_FILE "test/noon/freezer.cljc")

(defn top-level-form? [[v & _]]
  (#{"score" "play" "noon"} (name v)))

(defn top-level-form->score-form [form]
  (if-let [[v & args :as form] (if (list? form) form)]
    (case (name v)
      "play" `(noon.eval/score ~@args)
      "score" form
      "noon" (second args)
      "let" (if (top-level-form? (last args))
              (list `noon.eval/eval (list 'quote (concat (list v (first args))
                                                         (butlast args)
                                                         (list (top-level-form->score-form (last args)))))))
      nil)))

(defmacro freeze [form]
  (let [expr-hash (hash form)
        score-expr `(pseudo-random/with-rand 0
                      ~(or (top-level-form->score-form form)
                           `(noon.eval/eval '~form)))]
    (if-let [frozen (get @freezer expr-hash)]
      `(= ~frozen (hash ~score-expr))
      `(swap! freezer assoc ~expr-hash (hash ~score-expr)))))

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
  (macroexpand '(freeze (noon.eval/play (tup s0 s1 s2))))
  (macroexpand '(freeze (noon.eval/play (tup s0 s1 s2))))
  (freeze (noon.eval/play (tup s0 s1 s2)))
  (freeze (noon.eval/play (one-of s0 s1 s2)))
  (write-freezer))
