
# Table of Contents

1.  [Transformations 1](#org3353ab6)
    1.  [Pitches](#org7696d2a)
    2.  [Durations](#org376d752)
    3.  [Velocities](#orgf4774c5)
    4.  [Composition](#org3c259d1)
    5.  [Concatenation](#org6e07535)
    6.  [Superposition](#org2a21af2)
    7.  [Sounds](#org01231f9)
    8.  [Channels](#orgbc0d095)


<a id="org3353ab6"></a>

# Transformations 1

There is a bunch of transformations available, let&rsquo;s see the basics.


<a id="org7696d2a"></a>

## Pitches

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


<a id="org376d752"></a>

## Durations

We can operate on durations by multiplying or dividing them.

    (play dur2) ; multiplies the duration of our middle C by 2

    (play dur:3) ; divides it by 3

There is also a more flexible (and verbose) way to build duration transformations.

    (dur 2) ; sets the duration to 2

    (dur 1/4) ; sets the duration to 1/4

    (dur (fn [x] (* x 2))) ; multiply by 2 the current duration.

Those 3 forms return a transformation that can be used in `mk` or `play`

    (play (dur 1/4))


<a id="orgf4774c5"></a>

## Velocities

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


<a id="org3c259d1"></a>

## Composition

We can compose any number of transformations together using a clojure vector.

    (play [Eb0 dur:2]) ; plays a Eb of half duration

    (play [F#-1 dur4 (vel 127)]) ; F# above the middle C with quadruple duration and max velocity.

    (play [(vel 127) dur4 F#-1]) ; the order do not matter in this case.

The `play` and the `mk` functions, when given several arguments are doing exactly this

    (play F#-1 dur4) ; is the same as (play [F#-1 dur4])


<a id="org6e07535"></a>

## Concatenation

Using the `lin` function we can create our first melody.
The `lin` function takes an arbitrary number of transformations and concatenate their results into one score.

    (play (lin C0 E0 G0 B0))

`lin` accept any valid transformation, here we are using composite transformations.

    (play (lin [C0 dur:2]
               [Eb0 dur:4]
               [G0 dur:4]
               C1))


<a id="org2a21af2"></a>

## Superposition

Using the `par` function we can stack things up.

    (play (par C0 Eb0 G0)) ; a C minor chord.

    (play #{C0 Eb0 G0}) ; it can also be notated using clojure set litteral:

A pianissimo, double duration, Csus4 chord:

    (play vel2
          dur2
          (par C0 F0 G0))


<a id="org01231f9"></a>

## Sounds

By default, we are using general MIDI to emit sounds, it is not the most exciting way to play MIDI but it is everywhere and gives you a rapid feedback without extra setup.

Of course if you want to use fancy VSTs in a proper DAW you can, one of the feature of this library is to export MIDI files after all.

Here how you can leverage general MIDI sounds:

    (play (patch :clarinet) (lin C0 E0 G#0 B0))

    (play (patch :vibraphone) [dur:4 (lin C0 E0 G0 #{D1 B0})])

You can look at what is available here

    noon.vst.general-midi/summary


<a id="orgbc0d095"></a>

## Channels

In most of the tunes we write, we want several instruments playing together.

In MIDI there is this concept of channel, it serve the purpose to separate different streams of events.

    (play
     (chans
      [(patch :ocarina) dur:2 (lin G0 Eb0 C0 G-1 F0 D0 A-1 F-1)]
      [(patch :vibraphone) dur2 vel3 (lin #{C0 Eb0 G0} #{A-1 F0 D0})]
      [(patch :acoustic-bass) (lin [dur3 C-2] G-2)])
     (dup 4))

