
# Table of Contents

1.  [Transformations 2](#orgc84eafb)
    1.  [Intervals 1](#org3e81fb2)
        1.  [Steps](#org112261f)
        2.  [Octaves](#org8e17738)
    2.  [lin](#orga38682c)
    3.  [tup](#org1e4f7f3)
    4.  [dup](#org24dfc85)
    5.  [rep](#org9c9850c)
    6.  [fit](#org32270ba)
    7.  [nlin](#org433d593)
    8.  [ntup](#orgb0a9789)
    9.  [lin>](#org084ed5c)
    10. [tup>](#org07d6983)


<a id="orgc84eafb"></a>

# Transformations 2


<a id="org3e81fb2"></a>

## Intervals 1

It is now time to brings intervals into the equation, pitches were nice for introduction purposes but lacks the flexibility that intervals have. When musicians think about music, they do not think in precise pitches most of the time, they more often thinks of scales, intervals, degrees, melodic contour etc&#x2026; Those higher level abstractions are available in this system and in fact it is the whole point of it. Some really nice libraries already exists to deal with low levels aspects of music notation and sound synthesis.

In noon there is two types of intervals: **steps** and **shifts**.


<a id="org112261f"></a>

### Steps

Steps are the most commonly used type of interval.

The 2 most common types of steps are chromatic steps and diatonic steps

1.  Chromatic

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

2.  Diatonic

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
        
        By default, we are in the C major scale, but of course it can be changed. (see section)
        
        As a quick example, pretty self explanatory (but explained in more details later).
        
            (play dur:4 (root :Eb) (scale :hungarian) (lin d0 d1 d2 d3 d4 d5 d6 d7))
        
        There is 2 more type of steps: **structural** and **tonic**, but we will see them later.


<a id="org8e17738"></a>

### Octaves

Paraphrasing wiki:

> In music, an octave is the interval between one musical pitch and another with double its frequency. The octave relationship is a natural phenomenon that has been referred to as **the basic miracle of music**. The interval between the first and second harmonics of the harmonic series is an octave.

In noon, octaves are a different kind of interval, they belong to the `shift` family.

The nuance will appear more clearly later&#x2026; Until then, let see how to use them:

    (play (t-shift 1)) ; one octave up.

    (play (t-shift -1)) ; one octave down.

    (play o2-) ; 2 octaves down in var notation


<a id="orga38682c"></a>

## lin

As we have seen, `lin` let you create a succession of events:

    (play (lin C0 E0 G0 B0))

Let&rsquo;s try to go further with it by composing it with another `lin`:

    (play dur:8 (lin c0 c3 c6) (lin c0 c2 c3 c5))

Let see what happens here:

3 transformations are chained:

1.  We are dividing the duration of our base note by 8.
2.  We are creating a series of 3 notes using chromatic intervals (diminished triad C,Eb,Gb).
3.  Then this 3 notes score is passed to each member of the second `lin` expression, each one transposing it from the indicated chromatic interval.


<a id="org1e4f7f3"></a>

## tup

`tup` stands for tuplet and is analogous to `lin` but keep the duration of the given score unchanged.

    (play (tup c1 c2 c3 c4 c5 c6 c7 c8))

The resulting notes are fitted into the duration of the base note.

Like `lin` it can of course be chained with other transformations, as an example, here is a classic jazz melodic pattern.

    (play (tup c0 c2 c4 c7) (tup c0 c3) (rep 3 c4-))


<a id="org24dfc85"></a>

## dup

`dup` stands for duplicate and let you repeat a score n times.

    (play (tup c0 c3 c6 c9) (dup 3))


<a id="org9c9850c"></a>

## rep

`rep` let you apply a transformation several times in a row accumulating intermediate results.

A melody of 8 successive major thirds (4 semitones):

    (play dur:4 (rep 8 c4))

Be careful, with more complex transformations it can quickly become hairy:

    (play (rep 6 (tup c5 c10)))

You can remove the input score at the start of the result by giving an extra argument:

    (play (rep 3 o1 :skip-first))


<a id="org32270ba"></a>

## fit

`fit` is used to make a transformation fit the current duration of the score.
The 2 previous transformations introduced: `dup` and `rep`, are changing the score duration, but sometimes we want to transform our score in place, stretching or compressing it, in the same way `tup` is acting.

    (play (tup c0 c4) (fit (rep 4 c2)))

In fact `tup` is just a composition of `fit` and `lin`.

    (= (mk (tup c0 c3 c8)) (mk (fit (lin c0 c3 c8))))

The composition of `fit` and `rep` is also defined as `rup` for lack of a better name:

    (play (rup 15 d1))

A fitted version of `dup` also exists under the name `dupt`

    (play (tup d0 d3 d6 d7) (dupt 3))


<a id="org433d593"></a>

## nlin

concat the results of the given transformation n times

    (play (nlin 4 (tup d0 d1 d2 d3)))

it is the same thing as:

    (play (tup d0 d1 d2 d3) (dup 4))


<a id="orgb0a9789"></a>

## ntup

the fitted version of `nlin`

    (play (ntup 4 (tup d0 d1 d2 d3)))


<a id="org084ed5c"></a>

## lin>

`lin>` stands for accumulative concatenation, it accumulates the given transformations concatenating the intermediate results.

    (play (lin> c0 c2 c2 c2 c2 c2 c2))


<a id="org07d6983"></a>

## tup>

`tup>` is doing the same as `lin>`, except it maintains the score original duration.

    (play (tup> d0 d1 d1 d1 d1 d1 d1 d1))

