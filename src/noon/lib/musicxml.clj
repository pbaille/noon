(ns noon.lib.musicxml
  (:require [noon.score :as n]
            [noon.harmony :as hc]
            [noon.constants :as k]
            [clojure.data.xml :as xml]
            [clojure.math.numeric-tower :as math]
            [noon.utils.misc :as u]))

(def pitch-class->notation-infos
  (into {}
        (for [[dval cval] (map-indexed vector (butlast (reductions + 0 k/major-scale-steps)))
              [_altstr altval] k/alt-sym->alt-val]
          [{:d dval :c (+ cval altval)} {:step (k/natural-pitch-class-syms dval) :alter altval}])))

(do :help

    (defn safe-denominator [n]
      (if (ratio? n) (denominator n) 1))

    (defn num->numden [n]
      (cond (ratio? n)
            [(numerator n) (denominator n)]
            (integer? n) [n 1]
            :else (assert nil (str "num->numden do not accept float: " n))))

    (defn minimal-division [numbers]
      (if (empty? numbers)
        1                              ; GCD of an empty sequence is 1
        (let [numdens (map num->numden numbers)]
          (/ (reduce math/gcd (map first numdens))
             (reduce math/lcm (map second numdens)))))))

(defn time->bar-length [[num den]]
  (* num (get {2 2 4 1 8 1/2 16 1/4} den)))

(defn score->beat-divisions [score]
  (safe-denominator (minimal-division (mapcat (juxt :position :duration) score))))

(defn split-bars [score & {:keys [time]}]
  (let [len (time->bar-length time)
        duration (n/score-duration score)]
    (loop [bars []
           remaining score]
      (if (empty? remaining)
        bars
        (recur (conj bars (n/upd remaining (n/trim 0 len)))
               (n/upd remaining (n/lin (n/trim len duration) (n/start-from len))))))))

(defn note-type [_duration [_num den :as _time]]
  (assert (= 4 den)
          "only n/4 time sig are supported for now"))

(defn sorted-notes [score]
  (sort-by (juxt :position (comp hc/hc->chromatic-value :pitch)) (filter :pitch score)))

(defn add-chord-tags
  "musicxml use the <chord/> element to mark chords,
  the first appear as is and other chord notes are marked"
  [score]
  (reduce (fn [score [_ block]]
            (let [[n1 & ns] (sorted-notes block)]
              (into (conj score n1)
                    (map #(assoc % :chord true) ns))))
          #{} (sort (group-by :position score))))

(defn insert-rests
  "given a length and a score, find the gaps and fill them with rest notes
   for now it do not support overlapping notes, just chords with notes of equal length."
  [score & {:keys [time]}]
  (let [blocks (sort (group-by :position score))]
    (assert (every? (fn [[_ xs]] (apply = (map :duration xs))) blocks)
            "every chord notes should have same duration")
    (let [end-pos (n/score-duration score)
          remaining-rest-duration  (- (time->bar-length time) end-pos)
          rests (keep (fn [[[p1 xs] [p2 _]]]
                        (let [p1-end (+ p1 (:duration (first xs)))
                              dur (- p2 p1-end)]
                          (assert (>= dur 0)
                                  "no overlap allowed for now")
                          (if (pos? dur)
                            {:rest true :position p1-end :duration dur})))
                      (partition 2 1 blocks))]
      (into score (if (pos? remaining-rest-duration)
                    (conj rests {:rest true :position end-pos :duration remaining-rest-duration})
                    rests)))))

(defn note [{:as n :keys [chord _rest trimed-fw trimed-bw]} & {:as _options :keys [beat-divisions]}]
  (u/template
   [:note
    ~(if (:rest n)
       [:rest]
       (let [pitch (-> n :pitch hc/hc->pitch)
             {:keys [step alter]} (-> pitch k/pitch->pitch-class pitch-class->notation-infos)]
         [:pitch
          [:step step]
          [:alter alter]
          [:octave (quot (:d pitch) 7)]]))
    ~@(if chord [[:chord]])
    [:duration ~(quot (:duration n) (/ 1 beat-divisions))]
    ~@(if trimed-bw [[:tie {:type "stop"}]])
    ~@(if trimed-fw [[:tie {:type "start"}]])
    [:type "whole"]
    [:notations
     ~@(if trimed-bw [[:tied {:type "stop"}]])
     ~@(if trimed-fw [[:tied {:type "start"}]])]]))

(defn measure [score & {:as options :keys [beat-divisions time number]}]
  (let [notes (-> (add-chord-tags score) (insert-rests options) sorted-notes)]
    (-> [:measure
         {:number number}
         (into [:attributes
                [:divisions beat-divisions]]
               (when (= 1 number)
                 [[:key [:fifths 0]]
                  [:time [:beats (time 0)] [:beat-type (time 1)]]
                  [:clef [:sign "G"] [:line 2]]]))]
        (into (map (fn [n] (note n options)) notes)))))

(defn part [score & {:as options :keys [_time name]}]
  (into [:part {:id name}]
        (map-indexed (fn [i b] (measure b :number (inc i) options))
                     (split-bars score options))))

(defn emit [score & {:as options :keys [_time file]}]
  (let [beat-divisions (score->beat-divisions score)

        parts (->> (group-by :channel score)
                   (map (fn [[chan events]] (part (set events) (assoc options :name (str "chan-" chan) :beat-divisions beat-divisions)))))
        part-list (map (fn [[_ {id :id}]] [:score-part {:id id} [:part-name id]]) parts)]
    (spit file
          (xml/emit-str (xml/sexp-as-element
                         (into [:score-partwise {:version "4.0"}
                                (into [:part-list] part-list)]
                               parts))))))

(comment

  '(use 'noon.score)
  (def sample-score (mk o1- dur8 dur2))
  (split-bars sample-score {:time [4 4]})
  (emit sample-score
        :file "notation/test.musicxml"
        :time [4 4])
  (n/open dur4 (rup 16 (one-of d1 d3 d4 d1- d3- d4-))))
