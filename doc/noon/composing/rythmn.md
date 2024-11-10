
# Table of Contents

1.  [Rythmn](#org2290498)
    1.  [Simple](#org4c7b4f7)
    2.  [Generation](#org63a1a19)
        1.  [gen-tup](#orgf888b41)
        2.  [Examples](#orgb58dd40)
    3.  [Transformation](#org8fb9f45)
        1.  [noon.lib.melody](#org207dac8)
        2.  [r/rotation](#org197442d)
        3.  [r/permutation](#orgd350e72)


<a id="org2290498"></a>

# Rythmn

So far we havn&rsquo;t discuss rythmn so much, let see what we have at our disposal to deal with it.


<a id="org4c7b4f7"></a>

## Simple

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


<a id="org63a1a19"></a>

## Generation

The main tool we have at our disposal to create a rythmn is the noon.lib.melody/gen-tup


<a id="orgf888b41"></a>

### gen-tup

form:
       (gen-tup resolution size & options)

Generates a rythmic tup based on the given arguments:
resolution: the number of subdivisions that we will use.
size: the number of notes that the generated tup will contain.
options:
  euclidean: generates an euclydean tup.
  durations: the multiples of `resolution` that we are allowed to use (fractionals allowed).
  shifted: the possibility for the generated tup to not begin on beat.


<a id="orgb58dd40"></a>

### Examples

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


<a id="org8fb9f45"></a>

## Transformation

Once we have written or generated a rythmn we may want to make it evolve, here is some functions that can help.


<a id="org207dac8"></a>

### noon.lib.melody

We can use the previously seen functions from `noon.lib.melody` to permute or rotate a rythmn.

    (play
     dur2
     (chans
      [(patch :tinkle-bell) o1- (tup same [vel5 o1]) (dup 8)]
      [(patch :woodblock)
       (r/gen-tup 12 5 :euclidean)
       (rep 8 (probs {(m/permutation :rand) 1, (m/rotation :rand) 3}))]))


<a id="org197442d"></a>

### r/rotation

Unlike `noon.lib.melody/rotation` this function do not operates on a note basis

1.  Example

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


<a id="orgd350e72"></a>

### r/permutation

Like `noon.lib.rythmn/rotation`, `noon.lib.rythmn/permutation` do not operate on a note basis like `noon.lib.melody/permutation`. It operates on even time splits

1.  Example

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

