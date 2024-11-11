
# Melody

Let see some ways to deal with melodies.


## Bounding

One of the most common things we want to be able to control when generating melodies is the range.


### within-pitch-bounds?

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


## Rotations

Rotating a melody is a way to evolve it while preserving its identity.


### Example

    (play (fit (rep 8 d1)) (m/rotation 3))


### Forms

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
        (member s <integer>) normal nth like get
        (member s <negative-integer>) nth from the end of the list
        (member s <float-or-rational>) a non integer between -1 and 1, is picking a member relatively to the length of the list, forward if positive, backward if negative.
        (member s <[min max]>) picks a member randomly between the given idxs (every type of index allowed)
        (member s <:rand|:random>) picks a random member


### Chords

Not only pure melodies can be rotated, if we feed chords into the `rotation` transformation it behaves as intended.

    (play (fit (rep 8 d1)) (each (par d0 d3 d6)) (m/rotation 1/4))


## Permutations

Another way to transform a melody while preserving a bit of its identity is to permute it. But for long melody, a random permutation can make it so distant to the original that it miss the point. For this reason, permutations are ordered and requested by complexity (similarity degree with the original)


### Forms

Like the rotation function, the `permutation` function uses a &rsquo;member-pick argument.

    (m/permutation 2) ; the second most similar permutation

    (m/permutation -1) ; the less similar permutation

    (m/permutation 1/2) ; half way between most similar and most different

    (m/permutation -1/4) ; one quite distant permutation

    (m/permutation :rand) ; random permutation

    (m/permutation [1/4 -1/4]) ; a not too much similar nor different permutation


### Example

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


### Options

1.  Grade

    The permutations are categorised by grade, the grade of a permutation correspond to the number of splits that has to be made on the original seq to obtain it. For instance a grade 1 permutation is one that we can obtain by splitting our original sequence in 2 parts.
    
        (require '[noon.utils.sequences :as seqs])
        
        (=
         (seqs/grade-permutations [0 1 2 3] 1)
         '((2 3 0 1) (1 2 3 0) (3 0 1 2)))
    
    This way to categorise permutations can be helpful to have more control over the similarity of the resulting permutation. In addition to this the returned permutations for a given grade are ordered starting from the more balanced splits. As you can see in the previous example, (2 3 0 1) is the first permutation of grade 1, and contains 2 splits of size 2: (2 3) and (0 1).
    
    We can leverage those grades via our `m/permutation` function like this:
    
        (m/permutation 0 {:grade 1}) ; get the first grade 1 permutation.
    
        (m/permutation -1 {:grade [1 3]}) ; get the last permutation for a randomly picked grade between 1 and 3.

2.  Layers

    As we&rsquo;ve seen, our melodies are built on different harmonic layers (chromatic, diatonic, structural and tonic), the `m/permutation` function is letting you act on or inside a particular layer.
    
    As an example for this, please consider this kind of melody.
    
        (play dur2 (tup s0 s1 s2 s3) (each (tup d1 d1- d0)))
    
    We start with an ascension on the structural layer, then adding some diatonic ornementation on each structural degree. Those diatonic notes have meaning relatively to the structural degrees they are based upon. If we do a raw permutation on this melodic line we are losing those relations. With the :layer option we can permute only the structural layer keeping those diatonic ornementations untouched.
    
        (play
         dur2
         (tup s0 s1 s2 s3)
         (each (tup d1 d1- d0))
         (m/permutation 1 {:layer :s}))

3.  Split sizes

    TODO


## Mixed example

In the following example you can get a sense of the effect of deriving a melody from simple transformations.

    (play
     {:description
      "rand harmonic seq using IV II and VI degrees on vibraphone,\n                          ocarina melody derives using transposition, rotation and permutation."}
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


## Contour

The idea of contour is quite simple. When you see a melody on a score or a pianoroll, by linking the successive notes you can make a line. This line has a certain shape, some melodies with different notes share the same shape (contour). The contour of a melody greatly participate to its identification by the listener. So by keeping a contour and changing the notes, we can ensure a kind of continuity in our melodic developments.

For instance those different melodies are all sharing the same contour: [0 2 1 2]

    (play (tup s0 s2 s1 s2))

    (play (tup s0 s3 s2 s3))

    (play (tup d0 d2 d1 d2))

    (play (tup d1 d5 d2 d5))

    (play (tup s2 s4 d8 s4))

You can clearly hear the similarity between those


### contour

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


### Demo

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


## Line

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

