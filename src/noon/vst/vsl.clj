(ns noon.vst.vsl
  (:require [noon.score :as n]
            [noon.utils.misc :as u]))

(def TRACKS
  {0 :bus1
   1 :bus2
   2 :bus3
   3 :bus4})

(def CONFIG
  {:strings
   {:track 0
    :category :strings
    :instruments [:violin1 :violin2 :viola :cello :double-bass]
    :patches {:programs [0 16]
              :tree [[:short [:staccato :detache-short :detache :legato-rep :spiccato-rep]]
                     [:long [:sustain :marcato :xf-tremolo]]
                     [:legato [:legato :portamento]]
                     [:dyn [:sforzato :sfz-xf-tremolo :forte-piano :fp-xf-tremolo]]
                     [:fx [:tremolo :tremolo-marcato :half-tone-trill :whole-tone-trill :fast-rep-150 :fast-rep-170 :fast-rep-190]]
                     [:pizzicato [:pizzicato :snap-pizzicato]]
                     [:harmonics [:harmonics-staccato :harmonics :harmonics-marcato]]]}}

   :woodwinds
   {:track 1
    :category :woodwinds
    :instruments [:flute1 :flute2 :alto-flute :flute-ensemble]
    :patches {:programs [0 16]
              :tree [[:short [:staccato :portato :legato-rep :staccato-rep]]
                     [:long [:sustain :marcato]]
                     [:legato [:legato :legato-sus]]
                     [:dyn [:sforzato :forte-piano]]
                     [:fx [:half-tone-trill :whole-tone-trill :fast-rep-150 :fast-rep-170 :fast-rep-190]]]}}})

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
