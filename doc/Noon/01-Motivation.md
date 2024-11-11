
# Motivation

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

