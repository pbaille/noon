(ns noon.doc.intro
  (:use noon.score)
  (:refer-clojure :exclude [cat struct while])
  (:require [clojure.pprint :as pprint]
            [noon.lib.harmony :as h]
            [noon.lib.melody :as m]
            [noon.lib.rythmn :as r]
            [noon.utils.sequences :as seqs]))

(def guide

  '[:noon

    "This library provides a way to compose, play and export music as MIDI."

    (ns noon.doc.intro
      (:use noon.score))

    [:building-blocks

     [:events
      "An event is the basic building block we are dealing with."
      "It can represent any MIDI event, a note, a control change etc..."
      "It is represented using a clojure map"]

     [:score
      "A score is a collection of events, represented using a clojure set."]]

    [:elements

     [:top-forms

      "We can create a score with the =mk= function"
      "With no arguments it simply returns the default score containing only a middle C."
      (= (mk)
         #{{:position 0,
            :channel 0,
            :track 0,
            :duration 1,
            :pitch <C0>
            :velocity 80}})

      "The =mk= function can take any number of arguments, each one being a score transformation."
      "Those transformations are applied in order to the default score."

      '(mk transformation1 transformation2 ...)

      "the =play= macro acts exactly like =mk= except it plays the resulting score with general MIDI."

      '(play transformation1 transformation2 ...)]

     [:transformations-1

      "There is a bunch of transformations available, let's see the basics."

      [:pitches
       "We can set the current pitch by using pitch vars."

       "Pitch vars names consist of a pitch-class name followed by an octave offset.
(pitch classes are simply musical notes names like C, Db, F#, E, Bbb, Fx (x mean double sharp))"

       "The middle C is named C0, the C above is C1, the C below is C-1"

       "Here some examples of pitches:"
       (play Eb0)
       (play F#-1)
       (play Gb2)

       "Pitches are not often used as is, we will prefer more relative constructs like intervals, patterns etc...
      But it may be a little overwhelming to start with, so for now we will use them to introduce the basics building blocks of the system."]

      [:first-melody
       "Using the =cat= function we can create our first melody."
       (play (cat C0 E0 G0 B0))]

      [:first-chord
       "Using the =par= function we can stack things up."
       (play (par C0 Eb0 G0)) "a C minor chord."
       "It can also be notated using clojure set litteral:"
       (play #{C0 Eb0 G0})]

      [:durations
       "We can operate on durations by multiplying or dividing them."
       (play dur2) "Multiplying the duration of our middle C by 2"
       (play dur:3) "Dividing it by 3"

       "There is also a more verbose and flexible way to build duration transformations."
       (dur 2) "sets the duration to 2"
       (dur 1/4) "sets the duration to 1/4"
       (dur #(* % 2)) "multiply by 2 the current duration."

       "Those 3 forms return a transformation that can be used in =mk= or =play="
       (play (dur 1/4))]

      [:composing-transformations
       "We can compose any number of transformations together using a clojure vector."
       (play [Eb0 dur:2]) "This plays a Eb of half duration"
       (play [F#-1 dur4]) "the F# above the middle C with quadruple duration."
       (play [dur4 F#-1]) "the order do not matter here."

       "For simple cases like this don't forget that the =play= macro and the =mk= function variadic arities are doing the same thing."
       (play F#-1 dur4)

       "But in a =cat= form for instance we need the vectors (square brackets)"
       (play (cat [C0 dur:2] [Eb0 dur:4] [G0 dur:4] C1))]

      [:velocities
       "Velocity is the force with which a note is played, and it is vitally important in making MIDI performances sound human."
       "In midi the velocity is a number between 0 and 127."

       "For easing notation, 12 levels of velocity are defined as vars."
       (play vel0) "silent"
       (play vel3) "piano"
       (play vel8) "forte"
       (play vel12) "fortissimo"

       "Like for duration there is also a more flexible form:"
       (play (vel 100)) "sets the midi velocity of the current event to 100 (forte)."
       (play (vel #(/ % 2))) "divide the current velocity by 2 (by default the velocity is 80)"]

      [:sounds
       "By default, we are using general MIDI to emit sounds, it is not the most exciting way to play MIDI but it is everywhere and gives you a rapid feedback without extra setup."
       "Of course if you want to use fancy VSTs in a proper DAW you can, one of the feature of this library is to export MIDI files after all."
       "Here how you can leverage general MIDI sounds:"

       (play (patch :clarinet)
             (cat C0 E0 G#0 B0))

       (play (patch :vibraphone)
             [dur:4 (cat C0 E0 G0 #{B0 D1})])

       "You can look at what is available here"
       (do noon.vst.general-midi/summary)]

      [:channels
       "In most of the tunes we write, we want several instruments playing together."
       "In MIDI there is this concept of channel, it serve the purpose to separate different streams of events."

       (play (chans [(patch :ocarina) dur:2
                     (cat G0 Eb0 C0 G-1 F0 D0 A-1 F-1)]

                    [(patch :vibraphone) dur2 vel3
                     (cat #{C0 Eb0 G0} #{A-1 D0 F0})]

                    [(patch :acoustic-bass)
                     (cat [dur3 C-2] G-2)])

             (dup 4))]]

     [:transformations-2

      [:intervals-1
       "It is now time to brings intervals into the equation, pitches were nice for introduction purposes but lacks the flexibility that intervals have."
       "When a musician thinks about music, he do not think in precise pitches most of the time, he more often thinks of scales, intervals, degrees, melodic contour etc..."
       "Those higher level abstractions are available in this system and in fact it is the whole point of it."
       "Some really nice libraries already exists to deal with low levels aspects of music notation and sound synthesis."

       "In noon there is two types of intervals: steps and shifts."

       [:steps
        "Steps are the most commonly used type of interval."
        "The 2 most common type of steps are chromatic steps and diatonic steps"

        [:chromatic
         "A chromatic step is a movement by semitones."
         (c-step 3) "going up 3 semitones from wherever we are."
         (c-step -1) "going down one semitone."

         "Those kind of transformation are so common that they are available as vars:"
         c1 "is equivalent to" (c-step 1)
         c2- "is equivalent to" (c-step -2)
         "all chromatic steps from c36 to c36- are available."

         "If we apply the =c3= step to the default score, it transpose the default middle C (C0) 3 semitones up to Eb0 (or D#0)."
         (play c3)

         (play (c-step -3)) "going down 3 semitones to A-1"
         (play c12-) "going 12 semitones down (one octave) to C-1"]

        [:diatonic
         "A diatonic step is a movement toward a note that belong to the current scale."
         (d-step 1) "move to the upper scale note (or degree)."
         (d-step -1) "moves to the above scale note (or degree)."
         (d-step 4) "moves four scale degree up..."

         "Those kind of transformation are so common that they are available as vars:"
         d1 "is equivalent to" (d-step 1)
         d2- "is equivalent to" (d-step -2)
         "all diatonic steps from d21 to d21- are available."

         [:example
          "An ascending scale"
          (play dur:4 (cat d0 d1 d2 d3 d4 d5 d6 d7))
          "A broken scale pattern"
          (play dur:4 (cat d0 d2 d1 d3 d2 d4 d3 d5 d4))
          "The same downward"
          (play dur:4 (cat d0 d2- d1- d3- d2- d4- d3- d5- d4-))]

         "By default, we are in the C major scale, but of course it can be changed. (see :harmony section)"

         "As a quick example, pretty self explanatory (but explained in more details later)."
         (play dur:4
               (root :Eb) (scale :hungarian)
               (cat d0 d1 d2 d3 d4 d5 d6 d7))]

        "There is 2 more type of steps: structural and tonic, but we will see them later."]

       [:octaves
        "paraphrasing wiki:
       In music, an octave is the interval between one musical pitch and another with double its frequency.
       The octave relationship is a natural phenomenon that has been referred to as 'the basic miracle of music'.
       The interval between the first and second harmonics of the harmonic series is an octave."

        "In noon, octaves are a different kind of interval, they are belonging to the =shift= family."
        "The nuance will appear more clearly later... Until then, let see how to use them:"
        (play (t-shift 1)) "one octave up."
        (play (t-shift -1)) "one octave down."
        (play o2-) "2 octaves down in var notation"]]

      [:cat
       "As we have seen, =cat= let you create a succession of events:"
       (play (cat C0 E0 G0 B0))

       "Let's try to go further with it by composing it with another =cat=:"
       (play dur:8
             (cat c0 c3 c6)
             (cat c0 c2 c3 c5))

       "Let see what happens here:"
       "3 transformations are chained:"
       [1 "We are dividing the duration of our base note by 8."
        2 "We are creating a series of 3 notes using chromatic intervals (diminished triad C,Eb,Gb)."
        3 "Then this 3 notes score is passed to each member of the second =cat= expression, each one transposing it from the indicated chromatic interval."]]

      [:tup
       "=tup= stands for tuplet and is analogous to =cat= but keep the duration of the given score unchanged."
       (play (tup c1 c2 c3 c4 c5 c6 c7 c8))
       "the resulting notes are fitted into the duration of the base note."

       "Like =cat= it can of course be chained with other transformations, as an example, here is a classic jazz melodic pattern."
       (play (tup c0 c2 c4 c7)
             (tup c0 c3)
             (rep 3 c4-))]

      [:dup
       "=dup= stands for duplicate and let you repeat a score n times."
       (play (tup c0 c3 c6 c9)
             (dup 3))]

      [:rep
       "rep let you apply a transformation several times in a row accumulating intermediate results."
       (play dur:4
             (rep 8 c4)) "a melody of 8 successive major thirds (4 semitones)."

       (play (rep 6 (tup c5 c10))) "be careful, with more complex transformations it can quickly become hairy."

       "You can remove the input score at the start of the result by giving an extra argument:"
       (play (rep 3 o1 :skip-first))]

      [:fit
       "fit is used to make a transformation fit the current duration of the score."
       "The 2 previous transformations introduced: =dup= and =rep=, are actually changing the score length."
       "Sometimes we want to transform our score in place, stretching or compressing it, in the same way =tup= is acting."

       (play (tup c0 c4)
             (fit (rep 4 c2)))

       "In fact =tup= is just a composition of fit and cat"

       (= (mk (tup c0 c3 c8))
          (mk (fit (cat c0 c3 c8))))

       "The composition of =fit= and =rep= is also defined as =rup= for lack of a better name:"
       (play (rup 15 d1))

       "A fitted version of =dup= also exists under the name =dupt="
       (play (tup d0 d3 d6 d7) (dupt 3))]

      [:catn
       "concat the results of the given transformation n times"
       (play (catn 4 (tup d0 d1 d2 d3)))
       "it is the same thing as:"
       (play (tup d0 d1 d2 d3) (dup 4))]

      [:tupn
       "the fitted version of =catn="
       (play (tupn 4 (tup d0 d1 d2 d3)))]

      [:cat>
       "cat> stands for accumulative concatenation, it accumulates the given transformations concatenating the intermediate results."
       (play (cat> c0 c2 c2 c2 c2 c2 c2))]

      [:tup>
       "tup> is doing the same as cat>, except it maintains the score original duration."
       (play (tup> d0 d1 d1 d1 d1 d1 d1 d1))]]

     [:polyphony

      "As we have seen, we can parallelize things with the =par= function."

      (play (par c0 c3 c7 c9 c14)) "a Cm69 chord."

      (play #{c0 c5 c10 c16}) "a C7sus4add10 using set literal"

      "But we are not limited to use simple intervals, we can use any score transformations."

      (play (patch :electric-piano-1)
            (par (tup d0 d2 d4 o1)
                 [vel3 (par> o1 d4) (fit (rep 8 d1))]
                 o1-))

      "parallels transformations can be used anywhere of course. Here inside a tup."

      (play o1
            (tup c0 #{c10 c15} c9 #{c4 c6})
            (rep 3 c3))

      (play (par (rep 12 c1)
                 (rep 12 c1-)))

      "Like =cat= and =tup=, =par= has its accumulative counterpart:"
      (play (par> d0 d2 d2 d2 d2)) "piling diatonic thirds."
      (play (patch :string-ensemble-1) o2-
            (par> c0 c7 c7 c7 c7 c7)) "piling perfect fifths."

      [:channels
       "the =chans= function is doing the same thing as =par= except that it put each element on a separate MIDI channel."
       (play (chans c0 c3 c7))
       "To be more precise it put each of its argument on subsequent midi channels starting at the current one."
       "By default, we are on channel 0, so here the C will stay on channel 0, the Eb will go on channel 1 and the G on channel 2."

       "When we want more fine control we can use the =chan= function, that works like =vel= and =dur="
       (chan 1) "set midi channel to 1"
       (chan 3) "set midi channel to 3"
       (chan inc) "increment the current midi channel."

       "We can achieve the same thing as the first expression of the section using =par= and =chan= like this:"
       (play (par [(chan 0) c0]
                  [(chan 1) c3]
                  [(chan 2) c7]))]

      [:tracks
       "Tracks are a way of not be limited to only 16 channels, you can create virtually as many as you want."
       "Most of the time, 16 channels are enough but who knows..."
       "The =tracks= function works exactly like the =chans= function, except that it operates on the :track entry of events."
       (play (patch :flute)
             (tracks (tup> c0 c5 c5 c5- c2- c7-)
                     (tup> c0 c2- c5 c5))
             (dup 4))

       "By default we are on track 0. So the second argument of tracks goes on track 1."

       "Like with channels we can be more precise by using the =track= function"
       (track 1)
       (track 12)
       (track #(+ % 3))]]

     [:mapping

      "All the transformation we've seen so far are acting on a score to produce another score."
      "But sometimes what we need is to apply a transformation on each event of a score, for this we are using the =$= function."

      "As an illustration, here those two fragments:"

      (play (cat c0 c1 c2 c3)
            (tup c0 o1)) "each member of this =tup= form receives and operate on the whole score"

      (play (cat c0 c1 c2 c3)
            ($ (tup c0 o1))) "each event of the score is transformed using this tup transformation."

      "One important thing to be aware of is that events will be mapped in place. So if the given transformation expand the score, some superposition will occur."

      (play (cat c0 o1)
            ($ [dur:4 (rep 8 c1-)]))

      "Some others functions exists to transform only subparts of the score, if interested you can look at =$by= and/or =parts=."]

     [:dynamism

      "For now our scores are pretty static, and don't use the power of clojure much."
      "Since this library is built out of just simple functions it should be a easy to do so."

      "There is a bunch of things to know in order to ease things."

      [:star-functions
       "variadic functions have a 'star' counterpart that accepts a sequence instead of variadic args."
       (tup c1 c2 c3)
       "is similar to"
       (tup* [c1 c2 c3])
       "or"
       (tup* (list c1 c2 c3))

       "It ease things a bit when using clojure to generate arguments of those functions. Avoiding to write apply everywhere."]

      [:map-functions
       "maps can be used to compose event transformations"
       (play {:velocity #(/ % 2)
              :duration #(* % 2)})]

      "Some examples:"

      (play (tup* (shuffle [c0 c3 c7 c9])))

      (play (patch :electric-piano-1)
            (tup* (map (fn [v] {:velocity v})
                       (range 0 127 15))))]

     [:non-determinism

      "It is quite fun to insert a bit of randomness in our scores."

      (play
       ;; pick either major or minor arpegio
       (rand-nth [(tup c0 c4 c7)
                  (tup c0 c3 c7)])
       ;; repeating it 4 times transposing it by some third interval
       (rep 4 (rand-nth [c3 c4 c3- c4-])))

      "We can use some great available tools like =test.check.generators= to handle non determinism."
      "That being said, some commonly used non-deterministic functions are available directly."

      [:one-of
       "pick randomly one of the given transformations and apply it."
       (play (one-of o1- o1))
       (play dur:8 (rep 50 (one-of c1 c1-)))]

      [:maybe
       "maybe is very similar to one-of except it has a chance to do nothing (identity transformation)."
       (play (maybe o1 o2)) "may do nothing, or one octave up, or two octave up"
       (play (one-of same o1 o2)) "the equivalent =one=-of form"
       (play dur:8 (rep 50 (maybe c1 c1-))) "you can notice melodic repetitions unlike with the corresponding one-of example."]

      [:probs
       "=probs= gives you more control over the probability of occurence of the given transformations"
       (play (probs {o1 4 o1- 1})) "4/5 to go up one octave, 1/5 chance to go down one octave"

       (play dur:4
             (rep 24 (probs {c1 6 c6- 1 (par c0 o1-) 1})))]

      [:any-that
       "=any=-that is similar to one-of except it takes an extra first argument that check if the picked transformation is valid."
       (play dur:8
             (rep 60 (any-that (within-pitch-bounds? :C-1 :C1)
                              ;; 6 chromatic intervals to choose from
                               c2 c5 c7 c2- c5- c7-)))
       "a melody of 60 notes using the 6 given intervals but remaining in the given pitch bounds. "

       "The =within=-pitch-bounds? is just a score transformation that return the score unchanged if it is within the given bounds, else it returns nil."
       "Any function of this kind can be used has first argument to =any-that=."]

      [:!
       "the ! macro can be useful to deal with raw non deterministic expressions. here the docstring:"
       "Takes a non deterministic expression resulting in a score transformation.
       return a score transformation that wraps the expression so that it is evaluated each time the transformation is used."
       (play (catn 4 (! (tup* (shuffle [d0 d2 d4 d6])))))
       (play (catn 4 (tup* (shuffle [d0 d2 d4 d6])))) "without the bang the shuffle expression is executed only one time."]

      [:shuffling
       "As in the previews example, building a tup or a cat with shuffled sequence of transformation is quite fun."
       "So two shortcuts are defined"
       (play (shuftup d0 d2 d4 d6))
       (play (shufcat d0 d2 d4 d6))]]

     [:harmony

      "It is time to enter more deeply into the harmonic system."
      "In this part we will see how to deal with scales, modes, chords, modulations and more..."

      [:intervals-2

       "So far we've seen 3 types of intervals, chromatic steps, diatonic steps and octaves (aka tonic shifts)."
       "Let see the two remaining kinds of steps."

       [:steps
        [:structural
         "Most of the time, our music is based on chords."
         "Structural steps are targeting chord notes."
         "By default the harmony is set to C Major scale, and C Major chord (C major triad)."
         {:ascending-third (play (s-step 1))
          :ascending-fifth (play (s-step 2))}

         "As other steps corresponding vars are defined:"
         (play s1)
         (play s2)
         (play s1-)

         [:examples
          {:arpegios
           [(play (tup s0 s1 s2 s3))
            (play (rup 6 s1))
            (play (rep 4 s1-)
                  ($ (tup> s2 s2 s2 s1- s2- s1-)))]

           :passing-tones
           (play (scale :eolian)
                 dur:2 o2
                 (rep 12 s1-)
                 ($ (tup s0 c1- d1 s0)))}]]

        [:tonic
         "The last kind of step is the tonic one."
         "It let you jump to the root of the tonality."
         {:upper-tonic (play (t-step 1))
          :above-tonic (play (t-step -1))}

         "As other steps corresponding vars are defined:"
         (play t1)
         (play t2)
         (play t1-)

         [:examples
          (play (rup 4 t1))
          (play (rep 3 t1)
                ($ (tup> s0 s1 s1 d1-)))]]]]

      [:implementation

       "Those four types of steps can be seen as belonging to 4 successive layers build on each others."

       {:chromatic [0 1 2 3 4 5 6 7 8 9 10 11] ; the chromatic layer, 12 successive semitones
        :diatonic [0 2 4 5 7 9 11] ; we select indexes from the above layer (chromatic) to form the diatonic layer (here the major scale)
        :structural [0 2 4] ; same here but based on the diatonic layer to form the structural layer (here the basic triad)
        :tonic [0]}         ; again

       "As you see, the chromatic layers and tonic layers are trivial, so they are omitted in the harmonic context representation."
       "The harmonic context can be found under the :pitch key of any event."
       (:pitch DEFAULT_EVENT)
       {:scale [0 2 4 5 7 9 11],
        :struct [0 2 4],
        :origin {:d 35, :c 60},
        :position {:t 0, :s 0, :d 0, :c 0}}

       "The :origin key hold the pitch from where our layers starts (in both directions)."
       "The :position key holds a map with the 4 layers indexes"
       {:t "tonic" :s "structural" :d "diatonic" :c "chromatic"}]

      [:shifts
       "At least we will understand the nuance between steps and shifts"
       "To do so let's compare tonic steps and tonic shifts (aka octaves)."
       "At first glance they seems to be equivalent:"
       (play (t-shift 1))
       (play (t-step 1))
       "In this case they are indeed equivalent, in each case a C1 is played."
       "But how about this"
       (play s1 (t-shift 1)) "plays a E1"
       (play s1 (t-step 1)) "plays a C1"
       "In the first expression (the shift) we have transposed the score (a E0 note) by 1 tonic layer index."
       "In the second one (the step) we have stepped to the next tonic layer index."

       "In practice, apart for octaves, shifts are not used so often, thats the reason why they don't have defined vars as steps have."
       "They are mainly used in more complex harmonic operations (voice leading etc...)."]

      [:tonality

       [:scale
        "By default the major scale is used, but it can be changed."
        "Most of the known scales and modes are available via the =scale= function or directly by name."
        "see: =noon.constants/modes= for full list"
        {:dorian-scale (play (scale :dorian) dur:4 (rep 8 d1))
         :set-scale-to-harmonic-minor (mk harmonic-minor)}]

       [:struct
        "By default we use the triad structure (tonic, third, fifth), but it can be changed"
        "Some common structures are predefined and available by name."
        "see: =noon.constants/structs= for full list"
        {:set-structure-to-tetrad (mk (struct :tetrad))
         :set-structure-to-sus47 (mk sus47)}]

       [:origin
        "The last thing we need to setup an harmonic context is an origin pitch."
        "By default the origin is setup to middle C."
        "We can use the =origin= function to change this"
        (mk (origin :Eb0))
        [:examples
         (play (cat (origin :C0) (origin :E0) (origin :G#0))
               ($ (rup 6 s1)))]]

       [:root
        "The root update works a bit like =origin= but it takes a pitch-class instead of a pitch"
        "It moves the :origin of the harmonic context to the closest pitch matching the given pitch class."
        "For instance if the origin is on C0, (root :B) will put the origin on B-1 because B-1 is closer to C0 than B0."
        (mk (root :D))
        (mk (root :B))
        [:examples
         (play (cat* (map root [:C :E :G#]))
               ($ (chans (par d0 d3 d6 d9)
                         [(rup 4 d3) (rup 3 d2)]))
               (rep 4 s1))]]

       [:transpose
        "the transpose update takes an interval or a position and use it to update the origin of the harmonic context"
        (play (scale :lydianb7)
              (rup 6 d2)
              (rep 4 (transpose c3-)))]

       [:rebase
        "Sometimes when changing the harmonic context, we want to stay on the same pitch, the =rebase= function let you do that."
        (mk (rebase (root :E)))
        "Here we are modulating to E major, but we are staying on the pitch we were on (C0)."
        (= (get-in (mk (rebase (root :E)))
                   [:pitch :position])
           {:t 0, :s -1, :d 0, :c 1})
        "This position points to C0 but in the context of E major."

        "The =rebase= function can take several harmonic context transformations."
        (mk (rebase (root :E) (scale :mixolydianb6)))]

       [:degree
        "Move to the nth degree of the current scale (mode), negative indexes are allowed"
        {"move to the 3rd degree of C major, E phrygian" (mk (degree 2))
         "move to the 7th degree of C melodic minor, B superlocrian." (mk (scale :melodic-minor) (degree -1))}

        "Roman numeral vars are also available to change degree."
        (play (patch :trumpet)
              (cat I IV V I)
              ($ (tup s0 s1 s2)))]]]]

    [:composing

     "When composing music, 4 major aspects are considered: melody, rythmn, harmony and tone."
     "In this section some tools to deal with those aspects will be introduced."

     (require '[noon.lib.harmony :as h]
              '[noon.lib.melody :as m]
              '[noon.lib.rythmn :as r]
              '[noon.utils.sequences :as seqs])

     [:melody
      "Let see some ways to deal with melodies."
      [:bounding
       "One of the most common things we want to be able to control when generating melodies is the range."
       [:within-pitch-bounds?
        "this function returns nil if any event of the score is not in the given pitch bounds"
        (= (mk Eb0 (within-pitch-bounds? :C-1 :C0))
           nil)

        (= (mk Eb0 (within-pitch-bounds? :C0 :C1))
           (mk Eb0))

        "This function is handy in conjuction with the =any-that= or =fst-that= forms"
        (play (patch :electric-piano-1) dur:8
              (rep 60 (any-that (within-pitch-bounds? :C0 :C1)
                                c1 c1- c5 c5-)))

        "the =fst-that= form takes a test and any number of update that will be tried in order until one pass the test."
        (play dur:8
              (rep 60 (fst-that (within-pitch-bounds? :C0 :C1)
                                (one-of c5 c5-)
                                c2 c2-)))]]

      "Random melodies are nice at first but can quickly become boring."
      "It is often more pleasing to develop one or more ideas gradually via simple transformations."

      [:rotations
       "Rotating a melody is a way to evolve it while preserving its identity."
       [:example
        (play (fit (rep 8 d1))
              (m/rotation 3))]
       [:forms
        "The =noon.lib.melody/rotation= accepts several types of argument:"
        {(m/rotation 2) "rotate two notes forward"
         (m/rotation -3) "rotate three notes backward"
         (m/rotation 1/2) "rotate half the size forward"
         (m/rotation -1/3) "rotate third the size backward"
         (m/rotation :rand) "random rotation"
         (m/rotation [0 1/2]) "random rotation between first and half the size"}

        "This kind of argument (that I will call a 'member-pick') will be used at many other places within this section,
       it came from the =noon.utils.sequences/member= function, here the docstring:"

        "Find or pick an element within a sequence 's.
       available forms:
        (member s <integer>) normal nth like get
        (member s <negative-integer>) nth from the end of the list
        (member s <float-or-rational>) a non integer between -1 and 1, is picking a member relatively to the length of the list, forward if positive, backward if negative.
        (member s <[min max]>) picks a member randomly between the given idxs (every type of index allowed)
        (member s <:rand|:random>) picks a random member"]

       [:chords
        "Not only pure melodies can be rotated, if we feed chords into the =rotation= transformation it behaves as intended"
        (play (fit (rep 8 d1))
              ($ (par d0 d3 d6))
              (m/rotation 1/4))]]

      [:permutations
       "Another way to transform a melody while preserving a bit of its identity is to permute it."
       "But for long melody, a random permutation can make it so distant to the original that it miss the point."
       "For this reason, permutations are ordered and requested by complexity (similarity degree with the original)"

       [:forms
        "Like the rotation function, the =permutation= function uses a 'member-pick argument."
        {(m/permutation 2) "the second most similar permutation"
         (m/permutation -1) "the less similar permutation"
         (m/permutation 1/2) "half way between most similar and most different"
         (m/permutation -1/4) "one quite distant permutation"
         (m/permutation :rand) "random permutation"
         (m/permutation [1/4 -1/4]) "a not too much similar nor different permutation"}]

       [:example
        (let [space [vel0 dur:8]]
          (play (patch :electric-piano-1)
                (tup d0 d2 d1 d3 d2 d4 d3 d5)
                (cat same
                     space
                     (m/permutation 1)
                     space
                     (m/permutation 2)
                     space
                     (m/permutation -1/4))))]

       [:options
        [:grade
         "The permutations are categorised by grade, the grade of a permutation correspond to the number of splits
        that has to be made on the original seq to obtain it."
         "For instance a grade 1 permutation is one that we can obtain by splitting our original sequence in 2 parts."
         (= (seqs/grade-permutations [0 1 2 3] 1)
            '((2 3 0 1)
              (1 2 3 0)
              (3 0 1 2)))
         "This way to categorise permutations can be helpful to have more control over the similarity of the resulting permutation"
         "In addition to this the returned permutations for a given grade are ordered starting from the more balanced splits."
         "As you can see in the previous example, (2 3 0 1) is the first permutation of grade 1, and contains 2 splits of size 2: (2 3) and (0 1)."

         "We can leverage those grades via our =m/permutation= function like this:"
         (m/permutation 0 {:grade 1}) "get the first grade 1 permutation."
         (m/permutation -1 {:grade [1 3]}) "get the last permutation for a randomly picked grade between 1 and 3."]

        [:layers
         "As we've seen, our melodies are built on different harmonic layers (chromatic, diatonic, structural and tonic),
        the =m/permutation= function is letting you act on or inside a particular layer."
         "As an example for this, please consider this kind of melody."
         (play dur2
               (tup s0 s1 s2 s3)
               ($ (tup d1 d1- d0)))
         "We start with an ascension on the structural layer, then adding some diatonic ornementation on each structural degree."
         "Those diatonic notes have meaning relatively to the structural degrees they are based upon."
         "If we do a raw permutation on this melodic line we are losing those relations."
         "With the :layer option we can permute only the structural layer keeping those diatonic ornementations untouched."
         (play dur2
               (tup s0 s1 s2 s3)
               ($ (tup d1 d1- d0))
               (m/permutation 1 {:layer :s}))]

        [:split-sizes
         "TODO"]]]

      [:mixed-example

       "In the following example you can get a sense of the effect of deriving a melody from simple transformations."

       (play {:description "rand harmonic seq using IV II and VI degrees on vibraphone,
                          ocarina melody derives using transposition, rotation and permutation."}

             (chans

              [(patch :vibraphone)
               vel3
               (tupn 4 [(one-of IV II VI) tetrad (par [t2- vel5] s0 s1 s2 s3)])]

              [(patch :ocarina)
               vel5
               (shuftup d1 d2 d3 d4 d5)
               ($ (maybe (par d0 d3)))
               (rup 16
                    (probs {(m/permutation :rand) 1
                            (m/rotation :rand) 3
                            (one-of* (map d-step (range -3 4))) 5}))])

             (adjust 10)
             (append [d2- (transpose c3)]
                     [d2 (transpose c3-)]
                     same))]

      [:contour
       "The idea of contour is quite simple. When you see a melody on a score or a pianoroll, by linking the successive notes you can make a line."
       "This line has a certain shape, some melodies with different notes share the same shape (contour)."
       "The contour of a melody greatly participate to its identification by the listener."
       "So by keeping a contour and changing the notes, we can ensure a kind of continuity in our melodic developments."

       "For instance those different melodies are all sharing the same contour: [0 2 1 2]"
       (play (tup s0 s2 s1 s2))
       (play (tup s0 s3 s2 s3))
       (play (tup d0 d2 d1 d2))
       (play (tup d1 d5 d2 d5))
       (play (tup s2 s4 d8 s4))
       "You can clearly hear the similarity between those"

       [:noon.lib.melody/contour

        {:docstring
         "changing the melodic contour of a score.

        forms:
        (contour :mirror <options>) : mirror the contour of the score.
        (contour :rotation <options>) : rotate the contour of the score.
        (contour :similar <options>) : get a different score with the same contour.

        <options>
        a map that may contain some of those keys:

        :layer : (all commands, default to score's lowest harmonic layer)
            The harmonic layer on which the contour transformation is performed

        :pick | :nth : (:rotation and :similar commands, default to :random)
            A 'member-pick (see `member function) to select one particular outcome.

        :extent : (:similar command only)
            A vector of min and max amount of deformation that we want to apply to the score.

        :delta : (:similar command only)
            The amount of shrinking or growing we want to apply to the score."}]

       [:demo

        "Let's take this simple arpegio to start"
        (play (tup s0 s1 s2 s3 s1 s2))
        {:contour [0 1 2 3 1 2]}

        "Here the way to obtain the mirror contour of the previous arpegio."
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :mirror))
        {:contour [3 2 1 0 3 2]}

        "Next let's try contour rotations:"
        "Here we are picking the first rotation (with the option :nth)"
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :rotation {:nth 1}))
        {:contour [1 2 3 0 2 3]}
        "Every contour index has been shifted one step up, the highest one returning all the way down."

        "Lets get the last rotation using a 'member-pick argument."
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :rotation {:pick -1}))
        {:contour [3 0 1 2 0 1]}

        "If no :pick or :nth option is given, select a random one."
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :rotation))

        "One of the nice things with contours is that it can serve to generate many melodies."
        "Using the :similar commands we can do this."
        "Here we are randomly picking a similar score that is one structural step wider (:delta 1) that the original one."
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :similar {:delta 1}))

        "In all the previous exemples, the contour was computed over the structural layer."
        "When the layer is not specified, the score's lowest harmonic layer is used, here the structural layer."

        "As an illustration let's look at the effect of specifying the layer within the :mirror contour operation:"
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :mirror)) "Original example"
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :mirror {:layer :d})) "Mirrored diatonically, resulting in a F major arpegio"
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :mirror {:layer :c})) "Mirror chromatically, resulting in a F minor arpegio (it can help with 'negative harmony')"

        "One of the similar scores between those shrinked by 2 diatonic step and those expanded by 3 diatonic steps (:extent [-2 3] :layer :d)."
        (play (tup s0 s1 s2 s3 s1 s2)
              (m/contour :similar {:extent [-2 3] :layer :d}))]]

      [:line
       "One simple way to build a melody is to concatenate some little fragments one after another, building the next fragment on the last note of the previous one."
       "There are several ways to do this:"
       (play {:description
              "building a melodic line of 32 notes by chaining fragments of differerent lengths."}
             (patch :ocarina)
             dur:4
             (m/simple-line 32
                            (one-of (catn> 4 (one-of d1- d1))
                                    (tup d1 d1- s0)
                                    (cat s2 s1 s1-)
                                    (catn> 4 (one-of s1- s1)))))

       "The =simple-line= function is built on top of the more general function =noon.lib.melody/line="

       (play {:description
              "another way to build a melodic line from a bunch of randomly chosen transformations."}
             (patch :acoustic-guitar-nylon)
             (while (within-time-bounds? 0 24)
               (append [start-from-last
                        (any-that (within-pitch-bounds? :C-1 :C2)
                                  (rep 3 d3)
                                  (rep 3 d3-)
                                  d1 d1-)]))
             (adjust 3))]]

     [:rythmn
      "So far we havn't discuss rythmn so much, let see what we have at our disposal to deal with it."
      [:simple
       "As we've seen earlier, we can use the =duration= related transformations to write simple rythmns"
       (play (patch :woodblock)
             dur:4
             (cat same dur:2 dur:2 same dur2 same same))
       "This is not a pretty way to write it !"
       "We can use the _ shortcut instead of =same=, and the =tup= function for making this a bit more readable."
       (play (patch :woodblock)
             dur:4
             (cat _ (tup _ _) _ dur2 _ _))
       "We can also use the =dupt= function if we prefer."
       (play (patch :woodblock)
             dur:4
             (cat _ (dupt 2) _ dur2 _ _))
       "We could have done it like so too:"
       (play (patch :woodblock)
             dur2
             (tup* (map dur [1 1/2 1/2 1 2 1 1])))
       "There is a function to help writing a rythmn this way:"
       (play dur2 (r/durtup 1 1/2 1/2 1 2 1 1))
       (play dur2 (r/durtup* [1 1/2 1/2 1 2 1 1]))]

      "Writing those kind of rythmns is not the funniest thing to do of course, let see how we can generate and transform rythmns."

      [:generation

       "The main tool we have at our disposal to create a rythmn is the noon.lib.melody/gen-tup"

       [r/gen-tup

        "form:
       (gen-tup resolution size & options)

       Generates a rythmic tup based on the given arguments:
       resolution: the number of subdivisions that we will use.
       size: the number of notes that the generated tup will contain.
       options:
         euclidean: generates an euclydean tup.
         durations: the multiples of =resolution= that we are allowed to use (fractionals allowed).
         shifted: the possibility for the generated tup to not begin on beat.
         "]

       [:examples
        "randomly dispose 5 notes into 8 subdivisions."
        (play (patch :woodblock)
              (r/gen-tup 8 5)
              (dup 4))

        "Lets add a metronome"
        (play (chans
               [(patch :tinkle-bell) o1-]
               [(patch :woodblock) (r/gen-tup 8 5)])
              (dup 4))

        "A bit slower"
        (play dur2
              (chans
               [(patch :tinkle-bell) (tup o1- o1)]
               [(patch :woodblock) (r/gen-tup 16 8)])
              (dup 4))

        "Let's try 12/8"
        (play dur2
              (chans
               [(patch :tinkle-bell) (tup o1- o1)]
               [(patch :woodblock) (r/gen-tup 12 6) ($ (maybe o1 o1-))])
              (dup 4))

        "Using the :shifted keyword you can give your tup a chance to not start on beat."
        (play dur2
              (chans
               [(patch :tinkle-bell) (tup o1- o1)]
               [(patch :woodblock) (r/gen-tup 16 7 :shifted) ($ (maybe o1 o1-))])
              (dup 4))

        "You can specifies which durations are allowed with the :durations option"
        "here we are generating a tuple of resolution 12 and size 5, using only 2/12 and 3/12 durations."
        (play dur2
              (chans
               [(patch :tinkle-bell) (tup o1- o1)]
               [(patch :woodblock) (r/gen-tup 12 5 :durations [2 3])])
              (dup 4))

        "A 3 voices example:"
        (play (patch :tinkle-bell)
              dur2
              (par [o1- (dupt 2)]
                   (r/gen-tup 12 5 :shifted :durations [1 2 3])
                   [o1 (r/gen-tup 12 7 :shifted :durations [2 1 3])])
              (dup 4))

        "The :euclidean flag let you generate euclidean rythmns:"
        "https://blog.landr.com/euclidean-rhythms/"

        (play {:description "~trésillo"}
              (chans
               (patch :tinkle-bell)
               [(patch :woodblock) (r/gen-tup 8 3 :euclidean)])
              (dup 4))

        (play {:description "~bembé"}
              dur2
              (chans
               [(patch :tinkle-bell) (tup o1- _)]
               [(patch :woodblock) (r/gen-tup 12 7 :euclidean)])
              (dup 4))

        (play {:description "~bossa"}
              dur2
              (chans
               [(patch :tinkle-bell) (tup o1- _)]
               [(patch :woodblock) (r/gen-tup 16 5 :euclidean)])
              (dup 4))

        "2 more examples:"
        (let [rtup (! (r/gen-tup 16 5 :euclidean :shifted))]
          (play (patch :tinkle-bell)
                (chans (tupn 2 o1-)
                       rtup
                       [o1 rtup]
                       [o2 rtup]
                       [o3 rtup])

                (dup 4)
                (adjust {:duration 8})))

        "fancy variation"
        (let [rtup (! [(r/gen-tup 16 5 :euclidean :shifted)
                       ($ [(maybe o1 o2) (one-of vel4 vel6 vel8)])])]
          (play mixolydian
                (patch :vibraphone)
                (cat same (transpose c4-))
                (h/align-contexts)
                ($ (chans [(patch :tinkle-bell) o1-]
                          [(patch :acoustic-bass) t1- (tup same s1-)]
                          rtup
                          [d4 rtup]
                          [d6 rtup]
                          [d10 rtup]))
                (dup 8)
                (adjust {:duration 32})))]]

      [:transformation
       "Once we have written or generated a rythmn we may want to make it evolve, here is some functions that can help."
       [:lib.melody
        "We can use the previously seen functions from lib.melody to permute or rotate a rythmn."
        (play dur2
              (chans [(patch :tinkle-bell) o1- (tup same [vel5 o1]) (dup 8)]
                     [(patch :woodblock)
                      (r/gen-tup 12 5 :euclidean)
                      (rep 8
                           (probs {(m/permutation :rand) 1
                                   (m/rotation :rand) 3}))]))]
       [r/rotation
        "Unlike m/rotation this function do not operates on a note basis"
        [:example
         "Rotating a score by the given duration"
         (play (chans [(patch :tinkle-bell) o1- (dup 4)]
                      [(patch :woodblock)
                       (r/durtup 2 1 1 4)
                       (cat _
                            (r/rotation 1/2)
                            (r/rotation 1/4)
                            (r/rotation -1/4))]))

         "You can rotate by any duration, even if it do not really make sense "
         (play (chans [(patch :tinkle-bell) o1-]
                      [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation -1/5)])
               (dup 4))

         "You can also rotate relatively to score duration"
         "Here we are starting with a score of duration 2. With the form (r/rotation :relative -1/4) we are rotating it a quarter of its duration backward."
         (play dur2
               (chans [(patch :tinkle-bell) o1-]
                      [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation :relative -1/4)])
               (dup 4))

         "There is also forms to randomly pick a rotation"
         "(rotation :rand-by <increment>) : pick a random rotation using increment as resolution.
        (rotation :rand-sub <n>) : split the score in 'n parts and rotate to a randomly picked one."

         (play dur2
               (chans [(patch :tinkle-bell) o1-]
                      [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation :rand-by 1/2)])
               (dup 4))

         (play dur2
               (chans [(patch :tinkle-bell) o1-]
                      [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation :rand-sub 4)])
               (dup 4))]]

       [r/permutation
        "Like r/rotation, r/permutation do not operate on a note basis like m/permutation."
        "It operates on even time splits"

        [:example
         "Let's start with this tup"
         (play (patch :woodblock)
               (r/durtup 2 1 1 4)
               (dup 4))

         "Here we are picking a random permutation of our score splitted in 4 equal parts."
         (play (chans [(patch :tinkle-bell) o1-]
                      [(patch :woodblock) (r/durtup 2 1 1 4) (r/permutation 4)])
               (dup 4))

         "Like we've seen with m/permutation, there is several way to choose a particular permutation."
         "With the second argument we can specify how to pick one."

         "picking the most similar base 4 permutation"
         (r/permutation 4 1)

         "picking the least similar base 4 permutation"
         (r/permutation 4 -1)

         "picking one of the most similar base 8 permutation"
         (r/permutation 8 [0 1/2])

         "picking a random base 8 permutation"
         (r/permutation 8 :rand)

         "fun"
         (play {:description "rythmic permutation demo"}
               (chans
               ;; beat
                [(patch :taiko-drum) vel5 (dup 4)]
               ;; rythmic permutations
                [(patch :woodblock)
                 (r/durtup 2 1 1 1/2 1/2)
                 ($ (maybe o1 o1-))
                 (catn 4 (r/permutation 5))]
               ;; chords
                [(patch :electric-piano-1)
                 o1- vel4 lydian
                 (par> d0 d3 d3 d3 d3)
                 (cat (root :C) (root :Eb) (root :Ab) (root :Db))])
              ;; loop 4
               (dup 4))]]]]

     [:harmony
      "Within the lib.harmony module you will find some tools to deal with chords"
      [:voicings
       "In musical terms, a voicing is a particular agencement of a chord."
       "When we speak of a chord like for instance G7, we are not specifying the precise way we will dispose its components."
       "It can be played in closed position"
       (play (patch :electric-piano-1)
             V tetrad ;; we put ourselves on the fifth degree (G) with the tetrad structure (G7)
             (par s0 s1 s2 s3))

       "Inverted (first inversion)"
       (play (patch :electric-piano-1)
             V tetrad
             (par [o1 s0] s1 s2 s3))

       "Or dropped (drop 2)"
       (play (patch :electric-piano-1)
             V tetrad
             (par s1 [o1 s2] s3 s4)) ;; the second note is sent one octave up

       "and many other ways..."

       [:inversions

        "upward inversions"
        (play (patch :vibraphone)
              (par s0 s1 s2)
              (rep 4 (h/inversion 1)))

        "downward double inversions"
        (play (patch :vibraphone)
              o1 (par s0 s1 s2)
              (rep 4 (h/inversion -2)))

        "In those particular exemples we could have done the same using s1 and s2-, here the equivalent of the first example:"
        (play (patch :vibraphone)
              (par s0 s1 s2)
              (rep 4 s1))

        "But it is not always the case with more complex chords"
        (play {:description "4 successive double inversions upward on a Cmaj79 "}
              (patch :vibraphone)
              o1- (par d0 d2 d4 d6 d8)
              (rep 4 (h/inversion 2)))]

       [:drops
        "A drop is voicing where some notes have been sent into upper octaves."
        "Here some common drops:"

        (let [closed (par s0 s1 s2 s3)
              drop2 (par s0 [o1 s1] s2 s3)
              drop3 (par s0 s1 [o1 s2] s3)
              drop23 (par s0 [o1 s1] [o1 s2] s3)]

          (play (patch :vibraphone) tetrad
                (cat closed drop2 drop3 drop23)
                ($ dur:2)))

        [h/drop
         "This function help you to drop a voicing"
         "It takes the same polymorphic kind of argument (called a 'member-pick') that we've seen with m/permutation and m/rotation."

         [:examples
          "pick a random drop of Cmaj7"
          (play (patch :vibraphone) tetrad
                (par s0 s1 s2 s3)
                (h/drop :rand))
          "first drop"
          (play (patch :vibraphone) tetrad
                (par s0 s1 s2 s3)
                (h/drop 1))
          "last drop"
          (play (patch :vibraphone) tetrad
                (par s0 s1 s2 s3)
                (h/drop -1))
          "one-of the least wide drop"
          (play (patch :vibraphone) tetrad
                (par s0 s1 s2 s3)
                (h/drop [0 1/2]))]]]]

      [:chord-progressions
       "A chord progression is simply a succession of different chords, cyclic or not."
       [:voice-leading
        "When dealing with chord progression one of the first thing to consider is called voice leading, it is the way voicings succession is handled."
        "Let's start with a very common chord progression"
        (play (patch :electric-piano-1)
              (cat I VI IV V)
              ($ (par s0 s1 s2))
              (dup 2))
        "It do not sound bad but it can arguably be better"
        (play (patch :electric-piano-1)
              (cat I VI II V)
              ($ [(par s0 s1 s2) (h/drop -1)])
              h/voice-led
              (dup 2))
        "The =voice-led= transformation is using inversions and drops in order to minimize voices motion between successive chords."

        "It is a really smooth way to transition between voicings but it would be nice to get the original bass motion back."
        (play (cat I VI II V)
              (chans [(patch :acoustic-bass) C-2 ($ t-round)]
                     [(patch :electric-piano-1) ($ (par s0 s1 s2)) h/voice-led])
              (dup 2))

        "It works on any voicings"
        (play (struct :tetrad)
              (cat I VI II V)
              (chans [(patch :acoustic-bass) C-2 ($ [t-round (tup _ s2-)])]
                     [(patch :electric-piano-1) ($ [(par s0 s1 s2 s3) (h/inversion -3) (h/drop 1/2)]) h/voice-led])
              (dup 2))

        "The voice-led function is quite resource consuming and remain to be optimized..."]

       [:melodies
        "Once you have a chord progression, you may want to apply a melody on it."
        "One way to do so is to use the =h/align-contexts= transformation"
        [h/align-contexts
         "Let's start with a simple chord progression in minor."
         (play (patch :clarinet)
               (scale :harmonic-minor)
               (cat I IV VII I)
               ($ (tup s0 s1 s2)))
         "the tup is applied on each chord without any inversion."
         "With h/align-contexts we can connect contexts together with minimal offsets, resulting in more conjoint motions."
         (play (patch :clarinet)
               (scale :harmonic-minor)
               (cat I IV VII I)
               (h/align-contexts :s)
               ($ (tup s0 s1 s2)))

         "The word 'context' may seem a bit confusing, what it really stands for is 'harmonic context', the harmonic context can be found under the :pitch key of any event."

         "A more elaborated example"
         (play dur2
               (scale :harmonic-minor)
               (cat I IV VII I)
               (h/align-contexts :s)
               (cat same (transpose c3) same)

               (chans

                [(patch :choir-aahs) vel4
                 ($ [(par s0 s1 s2)
                     (maybe (tup s0 s1-) (tup s0 s1))])]

                [(patch :ocarina) vel6
                 ($ [(shuftup s0 s1 s2)
                     ($ (one-of (tup s0 (shufcat (one-of c1- s-) s+) s0)
                                (tup s0 c1- s0 (one-of s2- s2))))])]

                [(patch :acoustic-bass) vel3
                 o2-]))]

        [h/grid-zipped
         "This transformation helps you to zip a melody on the current chord progression."
         "This way you don't have to worry at all about the chords, just write a melody it will be adjusted to chord changes."
         "Let's first write a simple melodic pattern."
         (play (patch :ocarina)
               (tup s0 s1 [s2 (cat d1 d1- _)] s1)
               (dupt 4)
               (adjust {:duration 4}))
         "Now let's use the =h/grid-zipped= function to apply this to a chord progression"
         (play [(scale :harmonic-minor)
                (tup I IV VII I)
                (h/align-contexts)]
               (h/grid-zipped
                [(patch :ocarina)
                 (tup s0 s1 [s2 (cat d1 d1- _)] s1)
                 (dupt 4)])
               (dup 2)
               (adjust {:duration 6}))
         "almost the same with comping"
         (play [(scale :harmonic-minor)
                (tup I IV VII I)
                (h/align-contexts)]
               (chans
                (h/grid-zipped
                 [(patch :ocarina)
                  (tup s0 s1 [s2 (cat d1 d1- _)] s1)
                  (tup s0 s2 s1)
                  (tupn> 2 s1-)])
                [(patch :acoustic-bass) ($ t2-)]
                [(patch :choir-aahs) vel4 ($ [(par s0 s2 s4)])])
               (dup 2)
               (adjust {:duration 12}))]]]]]])

(defn org-code-block [x]
  (str "#+begin_src clojure :results none\n"
       (with-out-str (pprint/pprint x))
       "#+end_src\n\n"))

(defn org-str [x level]
  (cond
    (string? x) (str x "\n\n")
    (and (vector? x)
         (or (ident? (first x))
             (string? (first x))))
    (let [header (str (apply str (repeat (inc level) "*")) " " (name (first x)))
          subblocks (map #(org-str % (inc level)) (next x))]
      (str header "\n\n" (apply str subblocks)))
    :else (org-code-block x)))

(spit "./src/noon/doc/intro.org"
      (org-str guide 0))
