(ns noon.vst.vsl
  (:require [noon.score :as n]
            [noon.utils.misc :as u]))

(def TRACKS
  {0 :bus1
   1 :bus2
   2 :bus3
   3 :bus4})

(def PATCHES
  {:strings
   {:programs [0 16]
    :tree [[:short [:staccato :detache-short :detache :legato-rep :spiccato-rep]]
           [:long [:sustain :marcato :xf-tremolo]]
           [:legato [:legato :portamento]]
           [:dyn [:sforzato :sfz-xf-tremolo :forte-piano :fp-xf-tremolo]]
           [:fx [:tremolo :tremolo-marcato :half-tone-trill :whole-tone-trill :fast-rep-150 :fast-rep-170 :fast-rep-190]]
           [:pizzicato [:pizzicato :snap-pizzicato]]
           [:harmonics [:harmonics-staccato :harmonics :harmonics-marcato]]]}

   :woodwinds
   {:programs [0 16]
    :tree [[:short [:staccato :portato :legato-rep :staccato-rep]]
           [:long [:sustain :marcato]]
           [:legato [:legato :legato-sus]]
           [:dyn [:sforzato :forte-piano]]
           [:fx [:half-tone-trill :whole-tone-trill :fast-rep-150 :fast-rep-170 :fast-rep-190]]]}})

(def CONFIG
  {:strings
   {:track 0
    :category :strings
    :instruments [:solo-violin-1 :solo-violin-2 :solo-viola :solo-cello-1 :solo-cello-2 :solo-double-bass
                  :chamber-violins-1 :chamber-violins-2 :chamber-violas :chamber-cellos :chamber-double-basses
                  :orchestral-violins-1 :orchestral-violins-2 :orchestral-violas :orchestral-cellos :orchestral-double-basses]
    :patches (PATCHES :strings)}

   :woodwinds-1
   {:track 1
    :category :woodwinds-1
    :instruments [:flute-1 :flute-2 :alto-flute :flute-ensemble
                  :oboe-fr :oboe-vienna :oboe-amore :english-horn :oboe-ensemble
                  :eb-clarinet :clarinet-1 :clarinet-2 :bass-clarinet :basset-horn :clarinet-ensemble]
    :patches (PATCHES :woodwinds)}

   :woodwinds-2
   {:track 3
    :category :woodwinds-2
    :instruments [:bassoon :contra-bassoon :bassoon-ensemble
                  :alto-sax :tenor-sax :baritone-sax]
    :patches (PATCHES :woodwinds)}

   :brass-1
   {:track 4
    :category :brass-1
    :instruments [:piccolo-trumpet :trumpet :muted-trumpet :trumpet-ensemble :muted-trumpet-ensemble
                  :tenor-trombone :muted-tenor-trombone :bass-trombone :contrabass-trombone :trombone-ensemble :muted-trombone-ensemble]
    :patches (PATCHES :woodwinds)}

   :brass-2
   {:track 5
    :category :brass-2
    :instruments [:horn-1 :horn-2 :horn-ensemble-1 :horn-ensemble-2
                  :bass-tube :contrabass-tuba :wagner-tuba]
    :patches (PATCHES :woodwinds)}

   :keyboards
   {:track 6
    :category :keyboards
    :instruments [:bosendorfer :harpsichord :organ :harmonium :prepared-piano
                  :celesta :glockenspeil :xylophone :vibraphone :marimba
                  :harp :guitar :overdrive-guitar :upright-bass]
    :instrument->patches {}}})

(defn instrument [instrument-name]
  (or (first (keep (fn [[_ {:keys [category instruments track]}]]
                     (if-let [chan (u/index-of instruments instrument-name)]
                       (n/ef_ (merge _ {:track track :channel chan :vsl {:category category :instrument instrument-name}}))))
                   CONFIG))
      (if (nil? instrument-name)
        (n/ef_ _)
        (u/throw* "vsl-instrument: unknown instrument: " instrument-name))))

(defn patch [patch-name]
  (n/efn e (if-let [{:keys [instrument category]} (get e :vsl)]
             (let [{:keys [programs tree]} (get-in CONFIG [category :patches])]
               (or (first (keep-indexed (fn [idx [_ patches]]
                                          (if-let [patch-idx (u/index-of patches patch-name)]
                                            (-> (assoc e :pc [(+ idx (get programs 0))
                                                              (+ patch-idx (get programs 1))])
                                                (update :vsl assoc :patch patch-name))))
                                        tree))
                   (u/throw* "vsl-patch: unknown patch " patch-name)))
             (u/throw* "vsl-patch: vsl-instrument has to be set before patch."))))

(defn vsl
  ([x]
   (cond (sequential? x) (vsl (first x) (second x))
         (keyword? x) (instrument x)
         :else (u/throw* "vsl: bad argument " x)))
  ([instrument-name patch-name]
   (let  [patch-upd (patch patch-name)
          instrument-upd (instrument instrument-name)]
     (n/ef_ (-> _ instrument-upd patch-upd)))))

(defn noon [options score]
  (n/noon (merge {:tracks TRACKS}
                 options)
          score))

(comment :WIP-refactoring

         (def patchmap
           (memoize
            (fn ([{:keys [programs tree]}]
                 (reduce (fn [acc {:keys [program-changes name]}] (assoc acc name program-changes))
                         {}
                         (patchmap [] programs tree)))
              ([from offsets tree]
               (mapcat (fn [program x]
                         (if (next offsets)
                           (patchmap (conj from program) (next offsets) (second x))
                           [{:program-changes (conj from program)
                             :name x}]))
                       (range (first offsets) 128)
                       tree)))))

         (defn instruments [xs {:keys [track patches channel]}]
           (map-indexed (fn [i x]
                          {:name x
                           :channel (+ i channel)
                           :patches (patchmap patches)
                           :track track})
                        xs))

         (instruments [:solo-violin-1 :solo-violin-2 :solo-viola :solo-cello-1 :solo-cello-2 :solo-double-bass
                       :chamber-violins-1 :chamber-violins-2 :chamber-violas :chamber-cellos :chamber-double-basses
                       :orchestral-violins-1 :orchestral-violins-2 :orchestral-violas :orchestral-cellos :orchestral-double-basses]
                      {:channel 0
                       :track 0
                       :category :strings
                       :patches (PATCHES :strings)}))
