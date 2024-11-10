
# Table of Contents

1.  [Top form](#org9a9e68f)
    1.  [Options](#org678b216)
        1.  [Musescore options](#orgf2ebc82)
        2.  [mp3 export](#orge5f7c32)
    2.  [score](#orgac0cc36)
    3.  [shorthands](#orgdfb5344)
        1.  [`noon.score/play`](#orgdc133ab)
        2.  [`noon.score/write`](#org12d28c3)


<a id="org9a9e68f"></a>

# Top form

`noon.score/noon` is the top level form of the library.

    '(noon <option-map> <score>)

Here a minimal example:

    (noon
     ;; the play option let you play the given score
     {:play true}
     ;; calling mk without argument just build the default score (middle C)
     (mk))


<a id="org678b216"></a>

## Options

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


<a id="orgf2ebc82"></a>

### Musescore options

If you have [musecore](https://musescore.org/en) installed on your machine, you can emit music XML and pdf score.

-   `:xml true`
    write the score as musicXML file.

-   `:pdf true`
    write the score pdf file.


<a id="orge5f7c32"></a>

### mp3 export

It is possible to create an mp3 file by passing this option:

`:mp3 true`

[FFmpeg](https://ffmpeg.org/) and [FluidSynth](https://www.fluidsynth.org/) have to be installed on your machine.


<a id="orgac0cc36"></a>

## score

As we&rsquo;ve just seen, we can create a score with the `mk` function.
With no arguments it simply returns the default score containing only a middle C.

    (mk)

The `mk` function can take any number of arguments, each one being a score transformation.

Those transformations are applied in order to the default score.

    '(mk transformation1 transformation2 ...)


<a id="orgdfb5344"></a>

## shorthands

As a convenience, some thin `noon.score/noon` wrappers are defined:


<a id="orgdc133ab"></a>

### `noon.score/play`

    (play transformation1 transformation2 ...)

Which is is roughly equivalent to:

    (noon {:play true}
          (mk transformation1 transformation2 ...))

More concretly:

    (play dur2
          (tup s0 s1 s2 s3))


<a id="org12d28c3"></a>

### `noon.score/write`

TODO

