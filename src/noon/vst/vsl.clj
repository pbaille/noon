(ns noon.vst.vsl
  (:require [noon.score :as n]
            [noon.utils.misc :as u]))

(def TRACKS
  {0 :bus1
   1 :bus2
   2 :bus3
   3 :bus4
   4 :bus5
   5 :bus6})

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

(def CATEGORIES
  [{:category :strings
    :track 0
    :instruments [:solo-violin-1 :solo-violin-2 :solo-viola :solo-cello-1 :solo-cello-2 :solo-double-bass
                  :chamber-violins-1 :chamber-violins-2 :chamber-violas :chamber-cellos :chamber-double-basses
                  :orchestral-violins-1 :orchestral-violins-2 :orchestral-violas :orchestral-cellos :orchestral-double-basses]
    :patches (PATCHES :strings)}

   {:category :woodwinds-1
    :track 1
    :instruments [:flute-1 :flute-2 :alto-flute :flute-ensemble
                  :oboe-fr :oboe-vienna :oboe-amore :english-horn :oboe-ensemble
                  :eb-clarinet :clarinet-1 :clarinet-2 :bass-clarinet :basset-horn :clarinet-ensemble]
    :patches (PATCHES :woodwinds)}

   {:category :woodwinds-2
    :track 2
    :instruments [:bassoon :contra-bassoon :bassoon-ensemble
                  :alto-sax :tenor-sax :baritone-sax]
    :patches (PATCHES :woodwinds)}

   {:category :brass-1
    :track 3
    :instruments [:piccolo-trumpet :trumpet :muted-trumpet :trumpet-ensemble :muted-trumpet-ensemble
                  :tenor-trombone :muted-tenor-trombone :bass-trombone :contrabass-trombone :trombone-ensemble :muted-trombone-ensemble]
    :patches (PATCHES :woodwinds)}

   {:category :brass-2
    :track 4
    :instruments [:horn-1 :horn-2 :horn-ensemble-1 :horn-ensemble-2
                  :bass-tube :contrabass-tuba :wagner-tuba]
    :patches (PATCHES :woodwinds)}

   {:category :keyboards
    :track 5
    :instruments [:bosendorfer :harpsichord :organ :harmonium :prepared-piano]
    :instrument->patches {:bosendorfer {:pedal-up [0 16]
                                        :pedal-down [1 16]}
                          :harpsichord {:normal [0 16]}
                          :organ {:plenum-manual [0 16]
                                  :plenum-pedal [0 17]
                                  :plenum-combined [0 18]
                                  :flutes-manual [1 16]
                                  :flutes-pedal [1 17]
                                  :flutes-combined [1 18]}
                          :harmonium {:aeoline [0 16]
                                      :clairon-fifre [1 16]
                                      :clarinet-bourdon [2 16]
                                      :grand-jeu [3 16]}
                          :prepared-piano {:chain [0 16]
                                           :harmonic [1 16]
                                           :glass [2 16]
                                           :glissandi [3 16]}}}
   {:category :mallets
    :track 5
    :channel 5
    :instruments [:celesta :glockenspeil :xylophone :vibraphone :marimba]
    :instrument->patches {:celesta {:normal [0 16]
                                    :loco [0 17]
                                    :soft [1 17]
                                    :soft-loco [1 17]}
                          :glockenspeil {:normal [0 16]
                                         :loco [1 16]}
                          :xylophone {:normal [0 16]
                                      :loco [1 16]}
                          :vibraphone {:motor-off [0 16]
                                       :motor-on [1 16]}
                          :marimba {:normal [0 16]}}}

   {:category :plucked
    :track 5
    :channel 10
    :instruments [:harp :guitar :overdrive-guitar :upright-bass]
    :instrument->patches {:harp {:regular [0 16]
                                 :harmonics [1 16]}
                          :guitar {:staccato [0 16]
                                   :sustain [1 16]
                                   :sustain-vibrato [1 17]
                                   :legato [2 16]}
                          :overdrive-guitar {:staccato [0 16]
                                             :sustain [1 16]
                                             :sustain-vibrato [1 17]
                                             :sustain-whammy [1 18]
                                             :legato [2 16]
                                             :legato-vibrato [2 17]
                                             :legato-whammy [2 18]
                                             :slide [3 16]
                                             :slide-vibrato [3 17]
                                             :slide-whammy [3 18]}
                          :upright-bass {:staccato [0 16]
                                         :portato [0 17]
                                         :sustain [1 16]
                                         :sustain-snap [1 17]
                                         :legato [2 16]
                                         :legato-snap [2 17]
                                         :slide [3 16]
                                         :slide-snap [3 17]}}}])

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

(defn category->instruments [{:keys [category instruments track patches channel instrument->patches]
                              :or {channel 0}}]
  (->> instruments
       (map-indexed (fn [i x]
                      [x {:name x
                          :channel (+ i channel)
                          :patches (if patches
                                     (patchmap patches)
                                     (instrument->patches x))
                          :track track
                          :category category}]))
       (into {})))

(def INSTRUMENTS
  (apply merge
         (mapv category->instruments
               CATEGORIES)))

(defn instrument [instrument-name]
  (if-let [{:keys [category channel track]} (INSTRUMENTS instrument-name)]
    (n/ef_ (merge _ {:track track :channel channel :vsl {:category category :instrument instrument-name}}))
    (if (nil? instrument-name)
      (n/ef_ _)
      (u/throw* "vsl-instrument: unknown instrument: " instrument-name))))

(defn patch [patch-name]
  (n/efn e (if-let [{:keys [instrument]} (get e :vsl)]
             (if-let [program-changes (get-in INSTRUMENTS [instrument :patches patch-name])]
               (or (-> (assoc e :pc program-changes)
                       (update :vsl assoc :patch patch-name))
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
