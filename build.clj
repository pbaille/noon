(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'pbaille/noon)
(def version-tic (b/git-count-revs nil))
(def version "0.1.0")
(def class-dir "target/classes")
(def jar-file (str "target/noon-" version ".jar"))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))
