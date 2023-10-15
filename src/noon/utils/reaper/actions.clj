(ns noon.utils.reaper.actions
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [noon.score :as noon]
   [noon.utils.fennel :as fennel]
   [noon.utils.misc :as u]
   [noon.utils.reaper.interop :as interop]))



(def REASCRIPT_PATH "/Users/pierrebaille/Code/Lua/reascripts")

(def *actions (atom {}))

(def action-prelude
  '[(global u (require :utils))
    (global ru (require :ruteal))
    (global T (ru.take.get-active))])

(defn action-name [x]
  (str "pb_"(cond (keyword? x) (name x)
                  (sequential? x) (str/join "_" (map name x)))))

(defn init-actions! [actions-file]
  (->> (slurp actions-file)
       (read-string)
       (u/all-paths)
       (map (fn [[path [binding code]]]
              (let [action-name (action-name path)
                    action-id (if (int? code) code)
                    code (if-not action-id code)]
                [(keyword action-name) {:action-name action-name
                                        :path path
                                        :binding binding
                                        :code code
                                        :action-id action-id}])))
       (into {})
       (reset! *actions)))

(defn compile-action! [{:keys [path code]}]
  (let [action-name (action-name path)
        lua-file (str REASCRIPT_PATH "/" action-name ".lua")
        lua-source (fennel/compile-string (clojure.string/join "\n" (concat action-prelude [code])))]
    (spit lua-file lua-source)
    (swap! *actions assoc-in [(keyword action-name) :lua-file] lua-file)))

(defn compile-actions!
      []
      (doseq [action (vals @*actions)]
        (if (:code action)
          (compile-action! action))))

(defn compile-actions-loading-script []
      (let [name->lua-file
            (->> (vals @*actions)
                 (keep (fn [{:keys [action-name lua-file]}] (when lua-file [action-name lua-file])))
                 (into {}))
            action-count (count name->lua-file)
            action-reg-forms (map-indexed (fn [i [name file]]
                                            (u/template  (tset actions ~name (reaper.AddRemoveReaScript true 0 ~file ~(= (inc i) action-count)))))
                                          name->lua-file)]
        (fennel/compile-string (str '(local actions {})
                                    (cons 'do action-reg-forms)
                                    'actions))))

(defn register-actions! [loading-script-path]
  (doseq [[name id] (interop/<< (dofile ~loading-script-path))]
    (swap! *actions assoc-in [name :action-id] id)))

(defn compile-emacs-bindings []
  (with-out-str
    (pp/pprint
     (u/template (map! (:map reaper-mode-map
                           :n "<escape>" (lambda () (interactive) (reaper-mode -1))
                           ~@(mapcat (fn [{:keys [path binding action-id]}]
                                       (if binding
                                         [:desc (str/join "-" (map name path)) :n binding
                                          (u/template (lambda ()
                                                            (interactive)
                                                            (osc-send-message noon/reaper-osc-client "/action" ~action-id)))]))
                                     (vals @*actions))))))))

(defn compile-hydra-bindings []
  ())

(defn install-actions!
  [actions-file]
  (let [loading-script-path (str REASCRIPT_PATH "/load-pb-actions.lua")]
    (init-actions! actions-file)
    (compile-actions!)
    (spit loading-script-path (compile-actions-loading-script))
    (register-actions! loading-script-path)
    (spit "emacs/compiled/reaper-bindings.el" (compile-emacs-bindings))))

(comment (def actions-file "emacs/reaper-actions.edn")
         (install-actions! "emacs/reaper-actions.edn")
         (compile-actions!)
         (init-actions! actions-file)
         (spit (str REASCRIPT_PATH "/load-pb-actions.lua") (compile-actions-loading-script)))
(comment
  (vals @*actions)
  (slurp (str REASCRIPT_PATH "/load-pb-actions.lua"))
  (u/all-paths (read-string (slurp "emacs/reaper-actions.edn"))))
