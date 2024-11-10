
# Table of Contents

1.  [Harmony](#org28a0526)
    1.  [Voicings](#orgc22a241)
        1.  [Inversions](#org237f02a)
        2.  [Drops](#orga43205b)
    2.  [Chord progressions](#org68ec94f)
        1.  [Voice leading](#org3f4d7a0)
        2.  [Melodies](#orgd8828f2)


<a id="org28a0526"></a>

# Harmony

Within the lib.harmony module you will find some tools to deal with chords.


<a id="orgc22a241"></a>

## Voicings

In musical terms, a voicing is a particular agencement of a chord. When we speak of a chord like for instance G7, we are not specifying the precise way we will dispose its components.

It can be played in closed position

    (play (patch :electric-piano-1) V tetrad (par s0 s1 s2 s3))

Inverted (first inversion)

    (play (patch :electric-piano-1) V tetrad (par [o1 s0] s1 s2 s3))

Or dropped (drop 2)

    (play (patch :electric-piano-1) V tetrad (par s1 [o1 s2] s3 s4))

and many other ways&#x2026;


<a id="org237f02a"></a>

### Inversions

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


<a id="orga43205b"></a>

### Drops

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

1.  drop

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


<a id="org68ec94f"></a>

## Chord progressions

A chord progression is simply a succession of different chords, cyclic or not.


<a id="org3f4d7a0"></a>

### Voice leading

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


<a id="orgd8828f2"></a>

### Melodies

Once you have a chord progression, you may want to apply a melody on it.

One way to do so is to use the `noon.lib.harmony/align-contexts` transformation

1.  align-contexts

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

2.  harmonic-zip

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

