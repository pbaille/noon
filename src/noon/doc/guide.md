
# Noon

Noon is a clojure library that helps with **musical composition and exploration**.
It provides a way to compose, play and export music as **MIDI**.

The initial goal was to be able to define musical fragments in a very declarative way.
Leveraging clojure and functional programming powers in order to **generate, manipulate and organize** musical content.

    (ns noon.doc.guide
      ;; main building blocks
      (:use noon.updates)
      (:require
    
       ;; entry points
       [noon.score :refer [mk]]
       [noon.output :refer [noon play #_write]]
    
       ;; lib
       [noon.lib.harmony :as h]
       [noon.lib.melody :as m]
       [noon.lib.rythmn :as r]
    
       ;; extra deps (for low level explanations)
       [noon.events :as events]
       [noon.midi]
       [noon.vst.general-midi]
       [noon.constants]
       [noon.utils.sequences :as seqs]))


## Motivation

Firstly, I was somewhat frustrated by the experience of computer-assisted music composition.
The tools commonly used for this, called digital audio workstations (DAWs), which include for example Logic Audio or Cubase, are certainly powerful, but quite laborious in use in my opinion.
The feedback loop seems a bit long to me. Changes are repetitive and quite slow, we spend a lot of time clicking, drag and dropping, and little time actually listening.

On the other hand, having primarily studied jazz, I would have liked to find the same kind of approach when composing with a computer.
In jazz (or other non classical music genres), a composition often consists of a melody and a chord grid. Its realization is largely left to the musician who interprets it.
This allows a composition to be relatively different from one version to another, compared to a piece composed in MIDI in a digital audio workstation which is quite rigid.

The improvisation and interpretation techniques used by musicians can be partially emulated by a computer.
Of course, what makes the essence of a very beautiful improvisation or interpretation will probably never be artificially reproduced, nevertheless it would be interesting to see how far we can go. Certain musical contexts and techniques require great speed of execution or calculation, and for that the machine is unbeatable.

Trying to use already existing musical libraries, I felt that this level of abstraction was not accessible to a degree that suits my needs.

-   The [overtone library](https://github.com/overtone/overtone) is focusing mainly on sound synthesis, but not much on high level musical abstraction.
-   The [alda library](https://github.com/daveyarwood/alda-clj) is low level, close to the MIDI representation in terms of abstraction level.
-   The [leipzig library](https://github.com/ctford/leipzig) is aiming in this direction but I think we can go even further.

To sum up a bit, we could say that the level of musical abstraction currently in force in digital audio workstations is quite low.
In contrast to the level of abstraction generally used by musicians to think about music.
A level of abstraction that is too low will require a lot of work to realize ideas that are thought of quickly, and this leads to the relative slowness of the feedback cycle.

One of the objectives of this language will therefore be to allow the user to manipulate higher level musical abstractions, such as for example:

-   a modulation
-   a melodic contour
-   a melodic or harmonic, diatonic or chromatic approach or embellishment.
-   voice leading
-   an inversion of a chord or melody.

The kind of ideas that musicians use to talk about music in a more concise and above all more general way.


## Elements


### Building blocks


#### Events

An event is the basic building block we are dealing with.

It can represent any MIDI event, a note, a control change etc&#x2026;

It is represented using a clojure map

    events/DEFAULT_EVENT


#### Score

A score is a collection of events, represented using a clojure set.

    (mk) ; the `mk` function is used to build score, more on it later


### Top form

`noon.output/noon` is the top level form of the library.

    '(noon <option-map> <score>)

Here a minimal example:

    (noon
     ;; the play option let you play the given score
     {:play true}
     ;; calling mk without argument just build the default score (middle C)
     (mk))


#### Options

The option map let you stay what you want to do with your score.

-   `:midi true`
    Write the score as MIDI file.
    Default to false.

-   `:bpm <number>`
    Set the tempo to <number> beats per minute.
    Default value is 60.

-   `:play true`
    Play the score.
    Default to false.

-   `:filename <path>`
    Setting the name and location of emitted files.
    File extensions will be appended to this filename.

-   `:tracks {<track-idx> <sequencer> ...}`
    Set each track idxs to a `javax.sound.midi.Sequencer`
    This library comes with 4 nice soundfonts that sounds way better than builtin one (`:default`):
    1.  `:squid`
    2.  `:chorium`
    3.  `:fluid`
    4.  `:airfont`

example:

    (noon {:play true
           :tracks {0 :chorium}} ;; try to change soundfont here
    
          ;; this will be explained later
          ;; it repeats an ascending scale with different patches
          ;; in order to demonstrate the soundfont
          (mk dur2
              (rup 8 d1)
              (lin (patch :clarinet)
                   (patch :electric-piano-1)
                   (patch :trumpet)
                   (patch :ocarina))))

In addition to those soundfonts, you can send the output of noon to any output device available on your machine.

    (require 'noon.midi)
    ;; retrieve a device by name
    (def bus1 (noon.midi/get-output-device "Bus 1"))
    ;; build a sequencer from it
    (def bus1-sequencer (noon.midi/init-device-sequencer bus1))
    ;; use it to play a score
    (noon {:play true
           :tracks {0 bus1-sequencer}}
          (mk (par s0 s1 s2)))


##### Musescore options

If you have [musecore](https://musescore.org/en) installed on your machine, you can emit music XML and pdf score.

-   `:xml true`
    write the score as musicXML file.

-   `:pdf true`
    write the score pdf file.


##### mp3 export

It is possible to create an mp3 file by passing this option:

`:mp3 true`

    (noon {:mp3 true}
          (mk (tup s0 s1 s2)))

[FFmpeg](https://ffmpeg.org/) and [FluidSynth](https://www.fluidsynth.org/) have to be installed on your machine.


#### score

As we&rsquo;ve just seen, we can create a score with the `mk` function.
With no arguments it simply returns the default score containing only a middle C.

    (mk)

The `mk` function can take any number of arguments, each one being a score transformation.

Those transformations are applied in order to the default score.

    '(mk transformation1 transformation2 ...)


#### shorthands

As a convenience, some thin `noon.output/noon` wrappers are defined:


##### `noon.output/play`

    '(play transformation1 transformation2 ...)

Which is is roughly equivalent to:

    '(noon {:play true}
           (mk transformation1 transformation2 ...))

More concretly:

    (play dur2
          (tup s0 s1 s2 s3))


##### `noon.output/write`

TODO


### Transformations 1

There is a bunch of transformations available, let&rsquo;s see the basics.


#### Pitches

We can set the current pitch by using pitch vars.

Pitch vars names consist of a pitch-class name followed by an octave offset.
(pitch classes are simply musical notes names like C, Db, F#, E, Bbb, Fx (x mean double sharp)).
The middle C is named C0, the C above is C1, the C below is C-1.

Here some examples of pitches:

    (play Eb0)

    (play F#-1)

    (play Gb2)

Pitches are not often used as is, we will prefer more relative constructs like intervals, patterns etc&#x2026;
But it may be a little overwhelming to start with, so for now we will use them to introduce the basics building blocks of the system.


#### Durations

We can operate on durations by multiplying or dividing them.

    (play dur2) ; multiplies the duration of our middle C by 2

    (play dur:3) ; divides it by 3

There is also a more flexible (and verbose) way to build duration transformations.

    (dur 2) ; sets the duration to 2

    (dur 1/4) ; sets the duration to 1/4

    (dur (fn [x] (* x 2))) ; multiply by 2 the current duration.

Those 3 forms return a transformation that can be used in `mk` or `play`

    (play (dur 1/4))


#### Velocities

Velocity is the force with which a note is played, and it is vitally important in making MIDI performances sound human.

In midi the velocity is a number between 0 and 127.

For easing notation, 12 levels of velocity are defined as vars.

    (play vel0) ; silent

    (play vel3) ; piano

    (play vel8) ; forte

    (play vel12) ; fortissimo

Like for duration there is also a more flexible form:

    (play (vel 100)) ; sets the midi velocity of the current event to 100 (forte).

    (play (vel (fn [x] (/ x 2)))) ; divide the current velocity by 2 (by default the velocity is 80)


#### Composition

We can compose any number of transformations together using a clojure vector.

    (play [Eb0 dur:2]) ; plays a Eb of half duration

    (play [F#-1 dur4 (vel 127)]) ; F# above the middle C with quadruple duration and max velocity.

    (play [(vel 127) dur4 F#-1]) ; the order do not matter in this case.

The `noon.output/play` and the `noon.score/mk` functions, when given several arguments are doing exactly this

    (play F#-1 dur4) ; is the same as (play [F#-1 dur4])


#### Concatenation

Using the `lin` function we can create our first melody.
The `lin` function takes an arbitrary number of transformations and concatenate their results into one score.

    (play (lin C0 E0 G0 B0))

`lin` accept any valid transformation, here we are using composite transformations.

    (play (lin [C0 dur:2]
               [Eb0 dur:4]
               [G0 dur:4]
               C1))


#### Superposition

Using the `par` function we can stack things up.

    (play (par C0 Eb0 G0)) ; a C minor chord.

A pianissimo, double duration, Csus4 chord:

    (play vel2
          dur2
          (par C0 F0 G0))


#### Sounds

By default, we are using general MIDI to emit sounds, it is not the most exciting way to play MIDI but it is everywhere and gives you a rapid feedback without extra setup.

Of course if you want to use fancy VSTs in a proper DAW you can, one of the feature of this library is to export MIDI files after all.

Here how you can leverage general MIDI sounds:

    (play (patch :clarinet) (lin C0 E0 G#0 B0))

    (play (patch :vibraphone) [dur:4 (lin C0 E0 G0 (par D1 B0))])

You can look at what is available here

    noon.vst.general-midi/summary


#### Channels

In most of the tunes we write, we want several instruments playing together.

In MIDI there is this concept of channel, it serve the purpose to separate different streams of events.

    (play
     (chans
      [(patch :ocarina) dur:2 (lin G0 Eb0 C0 G-1 F0 D0 A-1 F-1)]
      [(patch :vibraphone) dur2 vel3 (lin (par C0 Eb0 G0) (par A-1 F0 D0))]
      [(patch :acoustic-bass) (lin [dur3 C-2] G-2)])
     (dup 4))


### Transformations 2


#### Intervals 1

It is now time to brings intervals into the equation, pitches were nice for introduction purposes but lacks the flexibility that intervals have. When musicians think about music, they do not think in precise pitches most of the time, they more often thinks of scales, intervals, degrees, melodic contour etc&#x2026; Those higher level abstractions are available in this system and in fact it is the whole point of it. Some really nice libraries already exists to deal with low levels aspects of music notation and sound synthesis.

In noon there is two types of intervals: **steps** and **shifts**.


##### Steps

Steps are the most commonly used type of interval.

The 2 most common types of steps are chromatic steps and diatonic steps


###### Chromatic

A chromatic step is a movement by semitones.

    (c-step 3) ; going up 3 semitones from wherever we are.

    (c-step -1) ; going down one semitone

Those kind of transformation are so common that they are available as vars:

    c1 ; equivalent to (c-step 1)

    c2- ; equivalent to (c-step -2)

All chromatic steps from `c36` to `c36-` are available.

If we apply the `c3` step to the default score, it transpose the default middle C (`C0`) 3 semitones up to `Eb0` (or `D#0`).

    (play c3)

    (play (c-step -3)) ; going down 3 semitones to A-1

    (play c12-) ; going 12 semitones down (one octave) to C-1


###### Diatonic

A diatonic step is a movement toward a note that belong to the current scale.

    (d-step 1) ; move to the upper scale note (or degree).

    (d-step -1) ; moves to the above scale note (or degree).

    (d-step 4) ; moves four scale degree up...

Those kind of transformation are so common that they are available as vars:

    d1 ; is equivalent to (d-step 1)

    d2- ; is equivalent to (d-step -2)

all diatonic steps from `d21` to `d21-` are available.

1.  Example

        (play dur:4 (lin d0 d1 d2 d3 d4 d5 d6 d7)) ; ascending scale
    
        (play dur:4 (lin d0 d2 d1 d3 d2 d4 d3 d5 d4)) ; broken scale pattern
    
        (play dur:4 (lin d0 d2- d1- d3- d2- d4- d3- d5- d4-)) ; same downward
    
    By default, we are in the C major scale, but of course it can be changed. (see [Harmony](#org89d5cf8) section)
    
    As a quick example, pretty self explanatory (but explained in more details later).
    
        (play dur:4 (root :Eb) (scale :hungarian) (lin d0 d1 d2 d3 d4 d5 d6 d7))
    
    There is 2 more type of steps: **structural** and **tonic**, but we will see them later.


##### Octaves

Paraphrasing wiki:

> In music, an octave is the interval between one musical pitch and another with double its frequency. The octave relationship is a natural phenomenon that has been referred to as **the basic miracle of music**. The interval between the first and second harmonics of the harmonic series is an octave.

In noon, octaves are a different kind of interval, they belong to the `shift` family.

The nuance will appear more clearly later&#x2026; Until then, let see how to use them:

    (play (t-shift 1)) ; one octave up.

    (play (t-shift -1)) ; one octave down.

    (play o2-) ; 2 octaves down in var notation


#### lin

As we have seen, `lin` let you create a succession of events:

    (play (lin C0 E0 G0 B0))

Let&rsquo;s try to go further with it by composing it with another `lin`:

    (play dur:8 (lin c0 c3 c6) (lin c0 c2 c3 c5))

Let see what happens here:

3 transformations are chained:

1.  We are dividing the duration of our base note by 8.
2.  We are creating a series of 3 notes using chromatic intervals (diminished triad C,Eb,Gb).
3.  Then this 3 notes score is passed to each member of the second `lin` expression, each one transposing it from the indicated chromatic interval.


#### tup

`tup` stands for tuplet and is analogous to `lin` but keep the duration of the given score unchanged.

    (play (tup c1 c2 c3 c4 c5 c6 c7 c8))

The resulting notes are fitted into the duration of the base note.

Like `lin` it can of course be chained with other transformations, as an example, here is a classic jazz melodic pattern.

    (play (tup c0 c2 c4 c7) (tup c0 c3) (rep 3 c4-))


#### dup

`dup` stands for duplicate and let you repeat a score n times.

    (play (tup c0 c3 c6 c9) (dup 3))


#### rep

`rep` let you apply a transformation several times in a row accumulating intermediate results.

A melody of 8 successive major thirds (4 semitones):

    (play dur:4 (rep 8 c4))

Be careful, with more complex transformations it can quickly become hairy:

    (play (rep 6 (tup c5 c10)))

You can remove the input score at the start of the result by giving an extra argument:

    (play (rep 3 o1 :skip-first))


#### fit

`fit` is used to make a transformation fit the current duration of the score.
The 2 previous transformations introduced: `dup` and `rep`, are changing the score duration, but sometimes we want to transform our score in place, stretching or compressing it, in the same way `tup` is acting.

    (play (tup c0 c4) (fit (rep 4 c2)))

In fact `tup` is just a composition of `fit` and `lin`.

    (= (mk (tup c0 c3 c8)) (mk (fit (lin c0 c3 c8))))

The composition of `fit` and `rep` is also defined as `rup` for lack of a better name:

    (play (rup 15 d1))

A fitted version of `dup` also exists under the name `dupt`

    (play (tup d0 d3 d6 d7) (dupt 3))


#### nlin

concat the results of the given transformation n times

    (play (nlin 4 (tup d0 d1 d2 d3)))

it is the same thing as:

    (play (tup d0 d1 d2 d3) (dup 4))


#### ntup

the fitted version of `nlin`

    (play (ntup 4 (tup d0 d1 d2 d3)))


#### lin>

`lin>` stands for accumulative concatenation, it accumulates the given transformations concatenating the intermediate results.

    (play (lin> c0 c2 c2 c2 c2 c2 c2))


#### tup>

`tup>` is doing the same as `lin>`, except it maintains the score original duration.

    (play (tup> d0 d1 d1 d1 d1 d1 d1 d1))


### Polyphony

As we have seen, we can parallelize things with the `par` function.

    (play (par c0 c3 c7 c9 c14)) ; a Cm69 chord.

    (play (par c10 c0 c16 c5)) ; a C7sus4add10 using set literal

But we are not limited to use simple intervals, we can use any score transformations.

    (play
     (patch :electric-piano-1)
     (par (tup d0 d2 d4 o1)
          [vel3 (par> o1 d4) (fit (rep 8 d1))]
          o1-))

Parallels transformations can be used anywhere of course. Here inside a `tup`.

    (play o1
          (tup c0 (par c15 c10)
               c9 (par c6 c4))
          (rep 3 c3))

    (play (par (rep 12 c1)
               (rep 12 c1-)))

Like `lin` and `tup`, `par` has its accumulative counterpart:

    (play (par> d0 d2 d2 d2 d2)) ; piling diatonic thirds.

    (play (patch :string-ensemble-1)
          o2-
          (par> c0 c7 c7 c7 c7 c7)) ; piling perfect fifths.


#### Channels

the `chans` function is doing the same thing as `par` except that it put each element on a separate MIDI channel.

    (play (chans c0 c3 c7))

To be more precise it put each of its argument on subsequent midi channels starting at the current one. By default, we are on channel 0, so here the C will stay on channel 0, the Eb will go on channel 1 and the G on channel 2.

When we want more fine control we can use the `chan` function, that works like `vel` and `dur`

    (chan 1) ; set midi channel to 1

    (chan 3) ; set midi channel to 3

    (chan inc) ; increment the current midi channel.

We can achieve the same thing as the first expression of the section using `par` and `chan` like this:

    (play (par [(chan 0) c0]
               [(chan 1) c3]
               [(chan 2) c7]))


#### Tracks

Tracks are a way of not be limited to only 16 channels, you can create virtually as many as you want. Most of the time, 16 channels are enough but who knows&#x2026; The `tracks` function works exactly like the `chans` function, except that it operates on the `:track` entry of events.

    (play
     (patch :flute)
     (tracks (tup> c0 c5 c5 c5- c2- c7-)
             (tup> c0 c2- c5 c5))
     (dup 4))

By default we are on track 0. So the second argument of tracks goes on track 1. Like with channels we can be more precise by using the `track` function.

    (track 1)

    (track 12)

    (track (fn [x] (+ x 3)))


### Mapping

All the transformations we&rsquo;ve seen so far are acting on a score to produce another score, but sometimes what we need is to apply a transformation on each event of a score, for this we are using the `each` function.

As an illustration, here those two fragments:

    (play (lin c0 c1 c2 c3)
          (tup c0 o1)) ; each member of this `tup` form receives and operate on the whole score

    (play (lin c0 c1 c2 c3)
          (each (tup c0 o1))) ; each event of the score is transformed using this `tup` transformation.

One important thing to be aware of is that events will be mapped in place, so if the given transformation expand the score, some superposition will occur.

    (play (lin c0 o1)
          (each [dur:4 (rep 8 c1-)]))

Some others functions exists to transform only subparts of the score, if interested you can look at `$by` and/or `parts`.


### Dynamism

For now our scores are pretty static, and don&rsquo;t use the power of clojure much. Since this library is built out of simple functions it should be a easy to do so.

There is a bunch of things to know in order to ease things.


#### Star functions

Variadic functions have a &rsquo;star&rsquo; counterpart that accepts a sequence instead of variadic args.

    (tup c1 c2 c3)

Is similar to:

    (tup* [c1 c2 c3])

or

    (tup* (list c1 c2 c3))

It ease things a bit when using clojure to generate arguments of those functions. Avoiding to write `apply` everywhere.


#### Map functions

maps can be used to compose event transformations

    (play {:velocity (fn [x] (/ x 2)), :duration (fn [x] (* x 2))})


#### Examples

    (play (tup* (shuffle [c0 c3 c7 c9])))

    (play
     (patch :electric-piano-1)
     (tup* (map (fn [v] {:velocity v}) (range 0 127 15))))


### Non determinism

It is quite fun to insert a bit of randomness in our scores.

    (play
     (rand-nth [(tup c0 c4 c7) (tup c0 c3 c7)])
     (rep 4 (rand-nth [c3 c4 c3- c4-])))

We can use some great available tools like `test.check.generators` to handle non determinism. That being said, some commonly used non-deterministic functions are available directly.


#### one-of

`one-of` picks randomly one of the given transformations and apply it.

    (play (one-of o1- o1))

    (play dur:8 (rep 50 (one-of c1 c1-)))


#### maybe

`maybe` is very similar to `one-of` except it has a chance to do nothing (identity transformation).

    (play (maybe o1 o2)) ; may do nothing, or one octave up, or two octave up

    (play (one-of same o1 o2)) ; the equivalent `one-of` form

    (play dur:8 (rep 50 (maybe c1 c1-))) ; you can notice melodic repetitions unlike with the corresponding one-of example.


#### probs

`probs` gives you more control over the probability of occurence of the given transformations.

    (play (probs {o1 4, o1- 1})) ; 4/5 to go up one octave, 1/5 chance to go down one octave

    (play dur:4 (rep 24 (probs {c1 6, c6- 1, (par c0 o1-) 1})))


#### any-that

`any-that` is similar to `one-of` except it takes an extra first argument that check if the picked transformation is valid.

A melody of 60 notes using the 6 given intervals but remaining in the given pitch bounds:

    (play dur:8
          (rep 60
               (any-that (within-pitch-bounds? :C-1 :C1)
                         c2 c5 c7 c2- c5- c7-)))

The `within-pitch-bounds?` is just a score transformation that return the score unchanged if it is within the given bounds, else it returns `nil`. Any function of this kind can be used has first argument to `any-that`.


#### !

the `!` macro can be useful to deal with raw non deterministic expressions. here the docstring:

> Takes a non deterministic expression resulting in a score transformation. return a score transformation that wraps the expression so that it is evaluated each time the transformation is used.

    (play (nlin 4 (! (tup* (shuffle [d0 d2 d4 d6])))))

    (play (nlin 4 (tup* (shuffle [d0 d2 d4 d6])))) ; without the bang the shuffle expression is executed only one time.


#### Shuffling

As in the previews example, building a `tup` or a `lin` with shuffled sequence of transformation is quite fun.

So two shortcuts are defined:

    (play (shuftup d0 d2 d4 d6))

    (play (shuflin d0 d2 d4 d6))


### Harmony

It is time to enter more deeply into the harmonic system. In this part we will see how to deal with scales, modes, chords, modulations and more&#x2026;


#### Intervals 2

So far we&rsquo;ve seen 3 types of intervals, chromatic steps, diatonic steps and octaves (aka tonic shifts). Let see the two remaining kinds of steps.


##### Steps


###### Structural

Most of the time, our music is based on chords.

Structural steps are targeting chord notes. By default the harmony is set to C Major scale, and C Major chord (C major triad).

    (play (s-step 1)) ; ascending third

    (play (s-step 2)) ; ascending fifth

As other steps, corresponding vars are defined:

    (play s1)

    (play s2)

    (play s1-)

1.  Examples

    1.  Arpegios
    
            (play (tup s0 s1 s2 s3))
        
            (play (rup 6 s1))
        
            (play (rep 4 s1-) (each (tup> s2 s2 s2 s1- s2- s1-)))
    
    2.  Passing tones
    
            (play (scale :eolian) dur:2 o2 (rep 12 s1-) (each (tup s0 c1- d1 s0)))


###### Tonic

The last kind of step is the tonic one.

It let you jump to the root of the tonality.

    (play (t-step 1)) ; upper tonic

    (play (t-step -1)) ; above tonic

As other steps corresponding vars are defined:

    (play t1)

    (play t2)

    (play t1-)

1.  Examples

        (play (rup 4 t1))
    
        (play (rep 3 t1) (each (tup> s0 s1 s1 d1-)))


#### Implementation

Those four types of steps can be seen as belonging to 4 successive layers build on each others.

1.  chromatic: `[0 1 2 3 4 5 6 7 8 9 10 11]`
    the chromatic layer, 12 successive semitones
2.  diatonic: `[0 2 4 5 7 9 11]`
    we select indexes from the above layer (chromatic) to form the diatonic layer (here the major scale)
3.  structural: `[0 2 4]`
    same here but based on the diatonic layer to form the structural layer (here the basic triad)
4.  tonic: `[0]`
    the root

As you see, the chromatic layers and tonic layers are trivial, so they are omitted in the harmonic context representation.

The harmonic context can be found under the :pitch key of any event.

    (=
     (:pitch events/DEFAULT_EVENT)
     {:scale [0 2 4 5 7 9 11],
      :structure [0 2 4],
      :origin {:d 35, :c 60},
      :position {:t 0, :s 0, :d 0, :c 0}})

The :origin key hold the pitch from where our layers starts (in both directions).

The :position key holds a map with the 4 layers indexes

    :t ; tonic

    :s ; structural

    :d ; diatonic

    :c ; chromatic


#### Shifts

At least we will understand the nuance between steps and shifts. To do so let&rsquo;s compare tonic steps and tonic shifts (aka octaves).

At first glance they seems to be equivalent:

    (play (t-shift 1))

    (play (t-step 1))

In this case they are indeed equivalent, in each case a C1 is played. But how about this ?

    (play s1 (t-shift 1)) ; plays a E1

    (play s1 (t-step 1)) ; plays a C1

In the first expression (the shift) we have transposed the score (a E0 note) by 1 tonic layer index. In the second one (the step) we have stepped to the next tonic layer index.

In practice, apart for octaves, shifts are not used so often, thats the reason why they don&rsquo;t have defined vars as steps have. They are mainly used in more complex harmonic operations (voice leading etc&#x2026;).


#### Tonality


##### scale

By default the major scale is used, but it can be changed. Most of the known scales and modes are available via the `scale` function or directly by name.

    noon.constants/modes ; modes full list

    (play (scale :dorian) dur:4 (rep 8 d1)) ; dorian scale

    (mk harmonic-minor) ; sets scale to harmonic-minor


##### structure

By default we use the triad structure (tonic, third, fifth), but it can be changed. Some common structures are predefined and available by name.

    noon.constants/structures ; full structure list

    (mk (structure :tetrad)) ; sets structure to tetrad

    (mk sus47) ; set-structure-to-sus47


##### origin

The last thing we need to setup an harmonic context is an origin pitch.

By default the origin is setup to middle C.

We can use the `origin` function to change this

    (mk (origin :Eb0))


###### Examples

    (play (lin (origin :C0) (origin :E0) (origin :G#0)) (each (rup 6 s1)))


##### root

The root update works a bit like `origin` but it takes a pitch-class instead of a pitch. It moves the :origin of the harmonic context to the closest pitch matching the given pitch class.

For instance if the origin is on `C0`, `(root :B)` will put the origin on `B-1` because `B-1` is closer to `C0` than `B0`.

    (mk (root :D))

    (mk (root :B))


###### Examples

    (play
     (lin* (map root [:C :E :G#]))
     (each (chans (par d0 d3 d6 d9) [(rup 4 d3) (rup 3 d2)]))
     (rep 4 s1))


##### transpose

the transpose update takes an interval or a position and use it to update the origin of the harmonic context

    (play (scale :lydianb7) (rup 6 d2) (rep 4 (transpose c3-)))


##### rebase

Sometimes when changing the harmonic context, we want to stay on the same pitch, the `rebase` function let you do that.

    (mk (rebase (root :E)))

Here we are modulating to E major, but we are staying on the pitch we were on (`C0`).

    (=
     (get-in (mk (rebase (root :E))) [:pitch :position])
     {:t 0, :s -1, :d 0, :c 1})

This position points to `C0` but in the context of E major.

The `rebase` function can take several harmonic context transformations.

    (mk (rebase (root :E) (scale :mixolydianb6)))


##### degree

Move to the nth degree of the current scale (mode), negative indexes are allowed.

    (mk (degree 2)) ; move to the 3rd degree of C major, E phrygian

    (mk (scale :melodic-minor) (degree -1)) ; move to the 7th degree of C melodic minor, B superlocrian.

Roman numeral vars are also available to change degree.

    (play (patch :trumpet) (lin I IV V I) (each (tup s0 s1 s2)))


## Composing

When composing music, 4 major aspects are considered: melody, rythmn, harmony and tone. In this section some tools to deal with those aspects will be introduced.

    (require
     '[noon.lib.harmony :as h]
     '[noon.lib.melody :as m]
     '[noon.lib.rythmn :as r]
     '[noon.utils.sequences :as seqs])


### Melody

Let see some ways to deal with melodies.


#### Bounding

One of the most common things we want to be able to control when generating melodies is the range.


##### within-pitch-bounds?

This function returns nil if any event of the score is not in the given pitch bounds.

    (= (mk Eb0 (within-pitch-bounds? :C-1 :C0)) nil)

    (= (mk Eb0 (within-pitch-bounds? :C0 :C1)) (mk Eb0))

This function is handy in conjuction with the `any-that` or `fst-that` forms.

    (play
     (patch :electric-piano-1)
     dur:8
     (rep 60 (any-that (within-pitch-bounds? :C0 :C1) c1 c1- c5 c5-)))

The `fst-that` form takes a test and any number of update that will be tried in order until one pass the test.

    (play
     dur:8
     (rep
      60
      (fst-that (within-pitch-bounds? :C0 :C1) (one-of c5 c5-) c2 c2-)))

Random melodies are nice at first but can quickly become boring. It is often more pleasing to develop one or more ideas gradually via simple transformations.


#### Rotations

Rotating a melody is a way to evolve it while preserving its identity.


##### Example

    (play (fit (rep 8 d1)) (m/rotation 3))


##### Forms

The `noon.lib.melody/rotation` accepts several types of argument:

    (m/rotation 2) ; rotate two notes forward

    (m/rotation -3) ; rotate three notes backward

    (m/rotation 1/2) ; rotate half the size forward

    (m/rotation -1/3) ; rotate third the size backward

    (m/rotation :rand) ; random rotation

    (m/rotation [0 1/2]) ; random rotation between first and half the size

This kind of argument (that I will call a &rsquo;member-pick&rsquo;) will be used at many other places within this section, it came from the `noon.utils.sequences/member` function, here the docstring:

Find or pick an element within a sequence &rsquo;s.
available forms:

-   `(member s <integer>)` normal nth like get
-   `(member s <negative-integer>)` nth from the end of the list
-   `(member s <float-or-rational>)` a non integer between -1 and 1, is picking a member relatively to the length of the list, forward if positive, backward if negative.
-   `(member s <[min max]>)` picks a member randomly between the given idxs (every type of index allowed)
-   `(member s <:rand|:random>)` picks a random member


##### Chords

Not only pure melodies can be rotated, if we feed chords into the `rotation` transformation it behaves as intended.

    (play (fit (rep 8 d1)) (each (par d0 d3 d6)) (m/rotation 1/4))


#### Permutations

Another way to transform a melody while preserving a bit of its identity is to permute it. But for long melody, a random permutation can make it so distant to the original that it miss the point. For this reason, permutations are ordered and requested by complexity (similarity degree with the original)


##### Forms

Like the rotation function, the `permutation` function uses a &rsquo;member-pick argument.

    (m/permutation 2) ; the second most similar permutation

    (m/permutation -1) ; the less similar permutation

    (m/permutation 1/2) ; half way between most similar and most different

    (m/permutation -1/4) ; one quite distant permutation

    (m/permutation :rand) ; random permutation

    (m/permutation [1/4 -1/4]) ; a not too much similar nor different permutation


##### Example

    (let
     [space [vel0 dur:8]]
     (play
      (patch :electric-piano-1)
      (tup d0 d2 d1 d3 d2 d4 d3 d5)
      (lin
       same
       space
       (m/permutation 1)
       space
       (m/permutation 2)
       space
       (m/permutation -1/4))))


##### Options


###### Grade

The permutations are categorised by grade, the grade of a permutation correspond to the number of splits that has to be made on the original seq to obtain it. For instance a grade 1 permutation is one that we can obtain by splitting our original sequence in 2 parts.

    (require '[noon.utils.sequences :as seqs])
    
    (=
     (seqs/grade-permutations [0 1 2 3] 1)
     '((2 3 0 1) (1 2 3 0) (3 0 1 2)))

This way to categorise permutations can be helpful to have more control over the similarity of the resulting permutation. In addition to this the returned permutations for a given grade are ordered starting from the more balanced splits. As you can see in the previous example, (2 3 0 1) is the first permutation of grade 1, and contains 2 splits of size 2: (2 3) and (0 1).

We can leverage those grades via our `m/permutation` function like this:

    (m/permutation 0 {:grade 1}) ; get the first grade 1 permutation.

    (m/permutation -1 {:grade [1 3]}) ; get the last permutation for a randomly picked grade between 1 and 3.


###### Layers

As we&rsquo;ve seen, our melodies are built on different harmonic layers (chromatic, diatonic, structural and tonic), the `m/permutation` function is letting you act on or inside a particular layer.

As an example for this, please consider this kind of melody.

    (play dur2 (tup s0 s1 s2 s3) (each (tup d1 d1- d0)))

We start with an ascension on the structural layer, then adding some diatonic ornementation on each structural degree. Those diatonic notes have meaning relatively to the structural degrees they are based upon. If we do a raw permutation on this melodic line we are losing those relations. With the :layer option we can permute only the structural layer keeping those diatonic ornementations untouched.

    (play
     dur2
     (tup s0 s1 s2 s3)
     (each (tup d1 d1- d0))
     (m/permutation 1 {:layer :s}))


###### Split sizes

TODO


#### Mixed example

In the following example you can get a sense of the effect of deriving a melody from simple transformations.

    (play
     {:description
      "rand harmonic seq using IV II and VI degrees on vibraphone,
       ocarina melody derives using transposition, rotation and permutation."}
     (chans
      [(patch :vibraphone)
       vel3
       (ntup 4 [(one-of IV II VI) tetrad (par [t2- vel5] s0 s1 s2 s3)])]
      [(patch :ocarina)
       vel5
       (shuftup d1 d2 d3 d4 d5)
       (each (maybe (par d0 d3)))
       (rup
        16
        (probs
         {(m/permutation :rand) 1,
          (m/rotation :rand) 3,
          (one-of* (map d-step (range -3 4))) 5}))])
     (adjust 10)
     (append [d2- (transpose c3)] [d2 (transpose c3-)] same))


#### Contour

The idea of contour is quite simple. When you see a melody on a score or a pianoroll, by linking the successive notes you can make a line. This line has a certain shape, some melodies with different notes share the same shape (contour). The contour of a melody greatly participate to its identification by the listener. So by keeping a contour and changing the notes, we can ensure a kind of continuity in our melodic developments.

For instance those different melodies are all sharing the same contour: [0 2 1 2]

    (play (tup s0 s2 s1 s2))

    (play (tup s0 s3 s2 s3))

    (play (tup d0 d2 d1 d2))

    (play (tup d1 d5 d2 d5))

    (play (tup s2 s4 d8 s4))

You can clearly hear the similarity between those


##### contour

:docstring

changing the melodic contour of a score.

forms:
(contour :mirror <options>) : mirror the contour of the score.
(contour :rotation <options>) : rotate the contour of the score.
(contour :similar <options>) : get a different score with the same contour.

<options>
a map that may contain some of those keys:

:layer : (all commands, default to score&rsquo;s lowest harmonic layer)
    The harmonic layer on which the contour transformation is performed

:pick | :nth : (:rotation and :similar commands, default to :random)
    A &rsquo;member-pick (see \`member function) to select one particular outcome.

:extent : (:similar command only)
    A vector of min and max amount of deformation that we want to apply to the score.

:delta : (:similar command only)
    The amount of shrinking or growing we want to apply to the score.


##### Demo

Let&rsquo;s take this simple arpegio to start

    (play (tup s0 s1 s2 s3 s1 s2)) ; {:contour [0 1 2 3 1 2]}

Here the way to obtain the mirror contour of the previous arpegio.

    (play (tup s0 s1 s2 s3 s1 s2) (m/contour :mirror)) ; {:contour [3 2 1 0 3 2]}

Next let&rsquo;s try contour rotations:

Here we are picking the first rotation (with the option `:nth`)

    (play (tup s0 s1 s2 s3 s1 s2) (m/contour :rotation {:nth 1})) ; {:contour [1 2 3 0 2 3]}

Every contour index has been shifted one step up, the highest one returning all the way down.

Lets get the last rotation using a &rsquo;member-pick argument.

    (play (tup s0 s1 s2 s3 s1 s2) (m/contour :rotation {:pick -1})) ; {:contour [3 0 1 2 0 1]}

If no :pick or :nth option is given, select a random one.

    (play (tup s0 s1 s2 s3 s1 s2) (m/contour :rotation))

One of the nice things with contours is that it can serve to generate many melodies. Using the `:similar` commands we can do this.

Here we are randomly picking a similar score that is one structural step wider (:delta 1) that the original one.

    (play (tup s0 s1 s2 s3 s1 s2) (m/contour :similar {:delta 1}))

In all the previous exemples, the contour was computed over the structural layer. When the layer is not specified, the score&rsquo;s lowest harmonic layer is used, here the structural layer.

As an illustration let&rsquo;s look at the effect of specifying the layer within the :mirror contour operation:

    (play (tup s0 s1 s2 s3 s1 s2) (m/contour :mirror)) ; Original example

    (play (tup s0 s1 s2 s3 s1 s2) (m/contour :mirror {:layer :d})) ; Mirrored diatonically, resulting in a F major arpegio

    (play (tup s0 s1 s2 s3 s1 s2) (m/contour :mirror {:layer :c})) ; Mirror chromatically, resulting in a F minor arpegio (it can help with 'negative harmony')

One of the similar scores between those shrinked by 2 diatonic step and those expanded by 3 diatonic steps (:extent [-2 3] :layer :d).

    (play
     (tup s0 s1 s2 s3 s1 s2)
     (m/contour :similar {:extent [-2 3], :layer :d}))


#### Line

One simple way to build a melody is to concatenate some little fragments one after another, building the next fragment on the last note of the previous one.

There are several ways to do this:

    (play
     {:description
      "building a melodic line of 32 notes by chaining fragments of differerent lengths."}
     (patch :ocarina)
     dur:4
     (m/simple-line
      32
      (one-of
       (nlin> 4 (one-of d1- d1))
       (tup d1 d1- s0)
       (lin s2 s1 s1-)
       (nlin> 4 (one-of s1- s1)))))

The `simple-line` function is built on top of the more general function `noon.lib.melody/line`

    (play
     {:description
      "another way to build a melodic line from a bunch of randomly chosen transformations."}
     (patch :acoustic-guitar-nylon)
     (repeat-while
      (within-time-bounds? 0 24)
      (append
       [start-from-last
        (any-that
         (within-pitch-bounds? :C-1 :C2)
         (rep 3 d3)
         (rep 3 d3-)
         d1
         d1-)]))
     (adjust 3))


### Rythmn

So far we havn&rsquo;t discuss rythmn so much, let see what we have at our disposal to deal with it.


#### Simple

As we&rsquo;ve seen earlier, we can use the `duration` related transformations to write simple rythmns

    (play
     (patch :woodblock)
     dur:4
     (lin same dur:2 dur:2 same dur2 same same))

This is not a pretty way to write it ! We can use the `_` shortcut instead of `same`, and the `tup` function for making this a bit more readable.

    (play (patch :woodblock) dur:4 (lin _ (tup _ _) _ dur2 _ _))

We can also use the `dupt` function if we prefer.

    (play (patch :woodblock) dur:4 (lin _ (dupt 2) _ dur2 _ _))

We could have done it like so too:

    (play (patch :woodblock) dur2 (tup* (map dur [1 1/2 1/2 1 2 1 1])))

There is a function to help writing a rythmn this way:

    (play dur2 (r/durtup 1 1/2 1/2 1 2 1 1))

    (play dur2 (r/durtup* [1 1/2 1/2 1 2 1 1]))

Writing those kind of rythmns is not the funniest thing to do of course, let see how we can generate and transform rythmns.


#### Generation

The main tool we have at our disposal to create a rythmn is the noon.lib.melody/gen-tup


##### gen-tup

form:
       (gen-tup resolution size & options)

Generates a rythmic tup based on the given arguments:
resolution: the number of subdivisions that we will use.
size: the number of notes that the generated tup will contain.
options:
  euclidean: generates an euclydean tup.
  durations: the multiples of `resolution` that we are allowed to use (fractionals allowed).
  shifted: the possibility for the generated tup to not begin on beat.


##### Examples

randomly dispose 5 notes into 8 subdivisions.

    (play (patch :woodblock) (r/gen-tup 8 5) (dup 4))

Lets add a metronome

    (play
     (chans
      [(patch :tinkle-bell) o1-]
      [(patch :woodblock) (r/gen-tup 8 5)])
     (dup 4))

A bit slower

    (play
     dur2
     (chans
      [(patch :tinkle-bell) (tup o1- o1)]
      [(patch :woodblock) (r/gen-tup 16 8)])
     (dup 4))

Let&rsquo;s try 12/8

    (play
     dur2
     (chans
      [(patch :tinkle-bell) (tup o1- o1)]
      [(patch :woodblock) (r/gen-tup 12 6) (each (maybe o1 o1-))])
     (dup 4))

Using the `:shifted` keyword you can give your tup a chance to not start on beat.

    (play
     dur2
     (chans
      [(patch :tinkle-bell) (tup o1- o1)]
      [(patch :woodblock) (r/gen-tup 16 7 :shifted) (each (maybe o1 o1-))])
     (dup 4))

You can specifies which durations are allowed with the `:durations` option

here we are generating a tuple of resolution 12 and size 5, using only 2/12 and 3/12 durations.

    (play
     dur2
     (chans
      [(patch :tinkle-bell) (tup o1- o1)]
      [(patch :woodblock) (r/gen-tup 12 5 :durations [2 3])])
     (dup 4))

A 3 voices example:

    (play
     (patch :tinkle-bell)
     dur2
     (par
      [o1- (dupt 2)]
      (r/gen-tup 12 5 :shifted :durations [1 2 3])
      [o1 (r/gen-tup 12 7 :shifted :durations [2 1 3])])
     (dup 4))

The `:euclidean` flag let you generate euclidean rythmns: <https://blog.landr.com/euclidean-rhythms/>

    (play
     {:description "~trésillo"}
     (chans
      (patch :tinkle-bell)
      [(patch :woodblock) (r/gen-tup 8 3 :euclidean)])
     (dup 4))

    (play
     {:description "~bembé"}
     dur2
     (chans
      [(patch :tinkle-bell) (tup o1- _)]
      [(patch :woodblock) (r/gen-tup 12 7 :euclidean)])
     (dup 4))

    (play
     {:description "~bossa"}
     dur2
     (chans
      [(patch :tinkle-bell) (tup o1- _)]
      [(patch :woodblock) (r/gen-tup 16 5 :euclidean)])
     (dup 4))

2 more examples:

    (let
     [rtup (! (r/gen-tup 16 5 :euclidean :shifted))]
     (play
      (patch :tinkle-bell)
      (chans (ntup 2 o1-) rtup [o1 rtup] [o2 rtup] [o3 rtup])
      (dup 4)
      (adjust {:duration 8})))

Fancy variation:

    (let
     [rtup
      (!
       [(r/gen-tup 16 5 :euclidean :shifted)
        (each [(maybe o1 o2) (one-of vel4 vel6 vel8)])])]
     (play
      mixolydian
      (patch :vibraphone)
      (lin same (transpose c4-))
      (h/align-contexts)
      (each
       (chans
        [(patch :tinkle-bell) o1-]
        [(patch :acoustic-bass) t1- (tup same s1-)]
        rtup
        [d4 rtup]
        [d6 rtup]
        [d10 rtup]))
      (dup 8)
      (adjust {:duration 32})))


#### Transformation

Once we have written or generated a rythmn we may want to make it evolve, here is some functions that can help.


##### noon.lib.melody

We can use the previously seen functions from `noon.lib.melody` to permute or rotate a rythmn.

    (play
     dur2
     (chans
      [(patch :tinkle-bell) o1- (tup same [vel5 o1]) (dup 8)]
      [(patch :woodblock)
       (r/gen-tup 12 5 :euclidean)
       (rep 8 (probs {(m/permutation :rand) 1, (m/rotation :rand) 3}))]))


##### r/rotation

Unlike `noon.lib.melody/rotation` this function do not operates on a note basis


###### Example

Rotating a score by the given duration

    (play
     (chans
      [(patch :tinkle-bell) o1- (dup 4)]
      [(patch :woodblock)
       (r/durtup 2 1 1 4)
       (lin _ (r/rotation 1/2) (r/rotation 1/4) (r/rotation -1/4))]))

You can rotate by any duration, even if it do not really make sense.

    (play
     (chans
      [(patch :tinkle-bell) o1-]
      [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation -1/5)])
     (dup 4))

You can also rotate relatively to score duration. Here we are starting with a score of duration 2. With the form (r/rotation :relative -1/4) we are rotating it a quarter of its duration backward.

    (play
     dur2
     (chans
      [(patch :tinkle-bell) o1-]
      [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation :relative -1/4)])
     (dup 4))

There is also forms to randomly pick a rotation (rotation :rand-by <increment>) : pick a random rotation using increment as resolution. (rotation :rand-sub <n>) : split the score in &rsquo;n parts and rotate to a randomly picked one.

    (play
     dur2
     (chans
      [(patch :tinkle-bell) o1-]
      [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation :rand-by 1/2)])
     (dup 4))

    (play
     dur2
     (chans
      [(patch :tinkle-bell) o1-]
      [(patch :woodblock) (r/durtup 2 1 1 4) (r/rotation :rand-sub 4)])
     (dup 4))


##### r/permutation

Like `noon.lib.rythmn/rotation`, `noon.lib.rythmn/permutation` do not operate on a note basis like `noon.lib.melody/permutation`. It operates on even time splits


###### Example

Let&rsquo;s start with this tup:

    (play (patch :woodblock) (r/durtup 2 1 1 4) (dup 4))

Here we are picking a random permutation of our score splitted in 4 equal parts.

    (play
     (chans
      [(patch :tinkle-bell) o1-]
      [(patch :woodblock) (r/durtup 2 1 1 4) (r/permutation 4)])
     (dup 4))

Like we&rsquo;ve seen with `noon.lib.melody/permutation`, there is several way to choose a particular permutation. With the second argument we can specify how to pick one.

    (r/permutation 4 1) ; picking the most similar base 4 permutation

    (r/permutation 4 -1) ; picking the least similar base 4 permutation

    (r/permutation 8 [0 1/2]) ; picking one of the most similar base 8 permutation

    (r/permutation 8 :rand) ; picking a random base 8 permutation

fun:

    (play
     {:description "rythmic permutation demo"}
     (chans
      [(patch :taiko-drum) vel5 (dup 4)]
      [(patch :woodblock)
       (r/durtup 2 1 1 1/2 1/2)
       (each (maybe o1 o1-))
       (nlin 4 (r/permutation 5))]
      [(patch :electric-piano-1)
       o1-
       vel4
       lydian
       (par> d0 d3 d3 d3 d3)
       (lin (root :C) (root :Eb) (root :Ab) (root :Db))])
     (dup 4))


### Harmony

Within the lib.harmony module you will find some tools to deal with chords.


#### Voicings

In musical terms, a voicing is a particular agencement of a chord. When we speak of a chord like for instance G7, we are not specifying the precise way we will dispose its components.

It can be played in closed position

    (play (patch :electric-piano-1) V tetrad (par s0 s1 s2 s3))

Inverted (first inversion)

    (play (patch :electric-piano-1) V tetrad (par [o1 s0] s1 s2 s3))

Or dropped (drop 2)

    (play (patch :electric-piano-1) V tetrad (par s1 [o1 s2] s3 s4))

and many other ways&#x2026;


##### Inversions

upward inversions

    (play (patch :vibraphone) (par s0 s1 s2) (rep 4 (h/inversion 1)))

downward double inversions

    (play (patch :vibraphone) o1 (par s0 s1 s2) (rep 4 (h/inversion -2)))

In those particular exemples we could have done the same using s1 and s2-, here the equivalent of the first example:

    (play (patch :vibraphone) (par s0 s1 s2) (rep 4 s1))

But it is not always the case with more complex chords

    (play
     {:description "4 successive double inversions upward on a Cmaj79 "}
     (patch :vibraphone)
     o1-
     (par d0 d2 d4 d6 d8)
     (rep 4 (h/inversion 2)))


##### Drops

A drop is voicing where some notes have been sent into upper octaves.

Here some common drops:

    (let
     [closed
      (par s0 s1 s2 s3)
      drop2
      (par s0 [o1 s1] s2 s3)
      drop3
      (par s0 s1 [o1 s2] s3)
      drop23
      (par s0 [o1 s1] [o1 s2] s3)]
     (play
      (patch :vibraphone)
      tetrad
      (lin closed drop2 drop3 drop23)
      (each dur:2)))


###### drop

This function help you to drop a voicing. It takes the same polymorphic kind of argument (called a &rsquo;member-pick&rsquo;) that we&rsquo;ve seen with `noon.lib.melody/permutation` and `noon.lib.melody/rotation`.

1.  Examples

    pick a random drop of Cmaj7
    
        (play (patch :vibraphone) tetrad (par s0 s1 s2 s3) (h/drop :rand))
    
    first drop
    
        (play (patch :vibraphone) tetrad (par s0 s1 s2 s3) (h/drop 1))
    
    last drop
    
        (play (patch :vibraphone) tetrad (par s0 s1 s2 s3) (h/drop -1))
    
    one-of the least wide drop
    
        (play (patch :vibraphone) tetrad (par s0 s1 s2 s3) (h/drop [0 1/2]))


#### Chord progressions

A chord progression is simply a succession of different chords, cyclic or not.


##### Voice leading

When dealing with chord progression one of the first thing to consider is called voice leading, it is the way voicings succession is handled.

Let&rsquo;s start with a very common chord progression.

    (play
     (patch :electric-piano-1)
     (lin I VI IV V)
     (each (par s0 s1 s2))
     (dup 2))

It do not sound bad but it can arguably be better.

    (play
     (patch :electric-piano-1)
     (lin I VI II V)
     (each [(par s0 s1 s2) (h/drop -1)])
     h/voice-led
     (dup 2))

The `voice-led` transformation is using inversions and drops in order to minimize voices motion between successive chords.

It is a really smooth way to transition between voicings but it would be nice to get the original bass motion back.

    (play
     (lin I VI II V)
     (chans
      [(patch :acoustic-bass) C-2 (each t-round)]
      [(patch :electric-piano-1) (each (par s0 s1 s2)) h/voice-led])
     (dup 2))

It works on any voicings.

    (play
     (structure :tetrad)
     (lin I VI II V)
     (chans
      [(patch :acoustic-bass) C-2 (each [t-round (tup _ s2-)])]
      [(patch :electric-piano-1)
       (each [(par s0 s1 s2 s3) (h/inversion -3) (h/drop 1/2)])
       h/voice-led])
     (dup 2))

The voice-led function is quite resource consuming and remain to be optimized&#x2026;


##### Melodies

Once you have a chord progression, you may want to apply a melody on it.

One way to do so is to use the `noon.lib.harmony/align-contexts` transformation


###### align-contexts

Let&rsquo;s start with a simple chord progression in minor.

    (play
     (patch :clarinet)
     (scale :harmonic-minor)
     (lin I IV VII I)
     (each (tup s0 s1 s2)))

the tup is applied on each chord without any inversion.

With `noon.lib.harmony/align-contexts` we can connect contexts together with minimal offsets, resulting in more conjoint motions.

    (play
     (patch :clarinet)
     (scale :harmonic-minor)
     (lin I IV VII I)
     (h/align-contexts :s)
     (each (tup s0 s1 s2)))

The word &rsquo;context&rsquo; may seem a bit confusing, what it really stands for is &rsquo;harmonic context&rsquo;, the harmonic context can be found under the `:pitch` key of any event.

A more elaborated example

    (play
     dur2
     (scale :harmonic-minor)
     (lin I IV VII I)
     (h/align-contexts :s)
     (lin same (transpose c3) same)
     (chans
      [(patch :choir-aahs)
       vel4
       (each [(par s0 s1 s2) (maybe (tup s0 s1-) (tup s0 s1))])]
      [(patch :ocarina)
       vel6
       (each
        [(shuftup s0 s1 s2)
         (each
          (one-of
           (tup s0 (shuflin (one-of c1- s-) s+) s0)
           (tup s0 c1- s0 (one-of s2- s2))))])]
      [(patch :acoustic-bass) vel3 o2-]))


###### harmonic-zip

This transformation helps you to zip a melody on a chord progression. This way you don&rsquo;t have to worry at all about the chords, just write a melody it will be adjusted to chord changes.

Let&rsquo;s first write a simple melodic pattern.

    (play
     (patch :ocarina)
     (tup s0 s1 [s2 (lin d1 d1- _)] s1)
     (dupt 4)
     (adjust {:duration 4}))

Now let&rsquo;s use the `h/harmonic-zip` function to apply this to a chord progression.

    (play
      (h/harmonic-zip
       [(scale :harmonic-minor) (tup I IV VII I) (h/align-contexts :s)]
       [(patch :ocarina) (tup s0 s1 [s2 (lin d1 d1- _)] s1) (dupt 4)])
     (dup 2)
     (adjust {:duration 6}))

Almost the same with comping.

    (play
      (h/harmonic-zip
       [(scale :harmonic-minor) (tup I IV VII I) (h/align-contexts :s)]
       (chans
        [(patch :ocarina)
         (tup s0 s1 [s2 (lin d1 d1- _)] s1)
         (dupt 4)]
        [(patch :acoustic-bass) t2-]
        [(patch :choir-aahs) vel4 (par s0 s2 s4)]))
      (dup 2)
      (adjust {:duration 12}))

