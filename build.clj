(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as io]))

(def lib 'pbaille/noon)
(def version-tic (b/git-count-revs nil))
(def version "0.1.1-SNAPSHOT")
(def class-dir "target/classes")
(def jar-file (str "target/noon-" version ".jar"))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  ;; Delete the entire target directory
  (b/delete {:path "target"})
  ;; Remove files from the specific soundfonts directory
  (let [soundfonts-dir (io/file "resources/midi/soundfonts")]
    (when (.exists soundfonts-dir)
      (doseq [file (.listFiles soundfonts-dir)]
        (when (.isFile file)
          (.delete file))))))

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
