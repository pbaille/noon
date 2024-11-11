
# Polyphony

As we have seen, we can parallelize things with the `par` function.

    (play (par c0 c3 c7 c9 c14)) ; a Cm69 chord.

    (play #{c10 c0 c16 c5}) ; a C7sus4add10 using set literal

But we are not limited to use simple intervals, we can use any score transformations.

    (play
     (patch :electric-piano-1)
     (par (tup d0 d2 d4 o1)
          [vel3 (par> o1 d4) (fit (rep 8 d1))]
          o1-))

Parallels transformations can be used anywhere of course. Here inside a `tup`.

    (play o1
          (tup c0 #{c15 c10}
               c9 #{c6 c4})
          (rep 3 c3))

    (play (par (rep 12 c1)
               (rep 12 c1-)))

Like `lin` and `tup`, `par` has its accumulative counterpart:

    (play (par> d0 d2 d2 d2 d2)) ; piling diatonic thirds.

    (play (patch :string-ensemble-1)
          o2-
          (par> c0 c7 c7 c7 c7 c7)) ; piling perfect fifths.


## Channels

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


## Tracks

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

