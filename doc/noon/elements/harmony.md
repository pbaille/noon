
# Harmony

It is time to enter more deeply into the harmonic system. In this part we will see how to deal with scales, modes, chords, modulations and more&#x2026;


## Intervals 2

So far we&rsquo;ve seen 3 types of intervals, chromatic steps, diatonic steps and octaves (aka tonic shifts). Let see the two remaining kinds of steps.


### Steps

1.  Structural

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

2.  Tonic

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


## Implementation

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
     (:pitch DEFAULT_EVENT)
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


## Shifts

At least we will understand the nuance between steps and shifts. To do so let&rsquo;s compare tonic steps and tonic shifts (aka octaves).

At first glance they seems to be equivalent:

    (play (t-shift 1))

    (play (t-step 1))

In this case they are indeed equivalent, in each case a C1 is played. But how about this ?

    (play s1 (t-shift 1)) ; plays a E1

    (play s1 (t-step 1)) ; plays a C1

In the first expression (the shift) we have transposed the score (a E0 note) by 1 tonic layer index. In the second one (the step) we have stepped to the next tonic layer index.

In practice, apart for octaves, shifts are not used so often, thats the reason why they don&rsquo;t have defined vars as steps have. They are mainly used in more complex harmonic operations (voice leading etc&#x2026;).


## Tonality


### scale

By default the major scale is used, but it can be changed. Most of the known scales and modes are available via the `scale` function or directly by name.

    noon.constants/modes ; modes full list

    (play (scale :dorian) dur:4 (rep 8 d1)) ; dorian scale

    (mk harmonic-minor) ; sets scale to harmonic-minor


### structure

By default we use the triad structure (tonic, third, fifth), but it can be changed. Some common structures are predefined and available by name.

    noon.constants/structures ; full structure list

    (mk (structure :tetrad)) ; sets structure to tetrad

    (mk sus47) ; set-structure-to-sus47


### origin

The last thing we need to setup an harmonic context is an origin pitch.

By default the origin is setup to middle C.

We can use the `origin` function to change this

    (mk (origin :Eb0))

1.  Examples

        (play (lin (origin :C0) (origin :E0) (origin :G#0)) (each (rup 6 s1)))


### root

The root update works a bit like `origin` but it takes a pitch-class instead of a pitch. It moves the :origin of the harmonic context to the closest pitch matching the given pitch class.

For instance if the origin is on `C0`, `(root :B)` will put the origin on `B-1` because `B-1` is closer to `C0` than `B0`.

    (mk (root :D))

    (mk (root :B))

1.  Examples

        (play
         (lin* (map root [:C :E :G#]))
         (each (chans (par d0 d3 d6 d9) [(rup 4 d3) (rup 3 d2)]))
         (rep 4 s1))


### transpose

the transpose update takes an interval or a position and use it to update the origin of the harmonic context

    (play (scale :lydianb7) (rup 6 d2) (rep 4 (transpose c3-)))


### rebase

Sometimes when changing the harmonic context, we want to stay on the same pitch, the `rebase` function let you do that.

    (mk (rebase (root :E)))

Here we are modulating to E major, but we are staying on the pitch we were on (`C0`).

    (=
     (get-in (mk (rebase (root :E))) [:pitch :position])
     {:t 0, :s -1, :d 0, :c 1})

This position points to `C0` but in the context of E major.

The `rebase` function can take several harmonic context transformations.

    (mk (rebase (root :E) (scale :mixolydianb6)))


### degree

Move to the nth degree of the current scale (mode), negative indexes are allowed.

    (mk (degree 2)) ; move to the 3rd degree of C major, E phrygian

    (mk (scale :melodic-minor) (degree -1)) ; move to the 7th degree of C melodic minor, B superlocrian.

Roman numeral vars are also available to change degree.

    (play (patch :trumpet) (lin I IV V I) (each (tup s0 s1 s2)))

