(ns noon.utils.keybindings
  "utility for defining keybinding in an edn way !"
  (:require [noon.utils.misc :as u]
            [backtick :refer [template]]
            [clojure.walk :as walk]))

"We will first define the data model, using exclusively maps, precision and extensibility > concision."

"the idea of key prefixes will be central"

(def sample-keybinding-spec
  {:keyspace true
   :name "foo"
   :keybinding "C-f"
   :description "do foo related things"
   :emacs-modes [:n]
   :children [{:action true
               :name "do something foolish"
               :keybinding "f"
               :elisp-var 'my-foolish-elisp-interactive-function}
              {:keyspace true
               :name "bar"
               :keybinding "b"
               :description "do some foobarish stuff"
               :children [{:action true
                           :name "qux"
                           :description "do something foobarquxish"
                           :keybinding "q"
                           :elisp-var 'my-foobarish-elisp-interactive-function}]}]})

"The too main type of objects are:"

:keyspace
:action

(do :basic-compiler

    "Our compilation target will be the `map!` form that doom-emacs provides."

    (defn compile-kbs
      [x]
      (cond (:keyspace x) [(list* :prefix (list (:keybinding x) '. (:name x))
                                  (mapcat compile-kbs (:children x)))]
            (:action x) (list :desc (:name x)
                              (:keybinding x)
                              (cond (:elisp-var x) (list 'var (:elisp x))
                                    (:elisp x) (:elisp x)))))

    (compile-kbs sample-keybinding-spec)

    (defn compile-doom-keybindings
      [spec]
      (cons 'map! (compile-kbs spec)))

    (defn compile-doom-keymaps
      [keymap->spec]
      (cons 'map! (map (fn [[keymap spec]] (list* :map keymap (compile-kbs spec)))
                       keymap->spec)))

    (compile-doom-keybindings sample-keybinding-spec))

(do :syntax
    "Now we will expose some macros to build the keybinding spec"

    (defmacro elisp>
      [& code]
      (list 'quote (template
                    (lambda ()
                            (interactive)
                            ~@(walk/prewalk-replace '{*expr* (pb/thing-at-point)} code)))))

    (defmacro clj>
      [& code]
      (let [expr (if (> (count code) 1)
                   (cons 'do code)
                   (first code))
            code-str (if (u/deep-find code '*expr*)
                       (template (format ~(str (seq (walk/prewalk-replace '{*expr* %s} expr)))
                                         (pb/thing-at-point)))
                       (str expr))]
        (list 'quote
              (template (lambda ()
                           (interactive)
                           (my-cider/eval! ~code-str))))))

    (comment
      (clj> (+ 2 3))
      (clj> (print *expr*) (+ 2 3)))

    (defmacro reaper>
      [& code]
      (template (clj> (requiring-resolve 'noon.utils.reaper/<<)
                      (noon.utils.reaper/<< ~@code))))

    (declare spec-vec->map)

    (defn action-vec->map
      [[name key code]]
      {:action true
       :name (clojure.core/name name)
       :keybinding key
       :elisp code})

    (defn keyspace-vec->map
      [[name key & children]]
      {:keyspace true
       :name (clojure.core/name name)
       :keybinding key
       :children (mapv spec-vec->map children)})

    (defn spec-vec->map
      [x]
      (if (vector? (nth x 2))
        (keyspace-vec->map x)
        (action-vec->map x)))

    (defn compile-tree
      [tree]
      (compile-doom-keybindings (spec-vec->map tree)))

    (defn spit-keymaps
      [filename & keymap-spec-pairs]
      (->> (map (fn [[keymap tree]] (spec-vec->map tree))
                (partition 2 keymap-spec-pairs))
           (into {})
           (compile-doom-keymaps)
           (u/pretty-str)
           (spit filename)))

    (defn spit-tree [filename tree]
      (spit filename (u/pretty-str (compile-tree tree))))

    (spit-tree "emacs/xp/try-keybindings-utils.el"
               [:foo "C-f"
                [:pouet "p" (reaper> (ru.take.get-active))]
                [:bar "b"
                 [:qux "q" (clj> (println *expr*))]]
                [:greet "g" (elisp> (print "hi!"))]]))



"there is several type of actions we want to support"

:clojure
:elisp
:osc
:reaper

"those actions will all compile to elisp actions of course"
"each type of action will in charge to implement its own ->elisp"
