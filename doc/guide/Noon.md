
# Noon

Noon is a clojure library that helps with **musical composition and exploration**.
It provides a way to compose, play and export music as **MIDI**.

The initial goal was to be able to define musical fragments in a very declarative way.
Leveraging clojure and functional programming powers in order to **generate, manipulate and organize** musical content.

    (ns noon.doc.guide
      ;; core
      (:use noon.score)
      ;; lib
      (:require
        [noon.lib.harmony :as h]
        [noon.lib.melody :as m]
        [noon.lib.rythmn :as r]))


## [Motivation](Noon/01-Motivation.md)


## [Elements](Noon/02-Elements.md)


## [Composing](Noon/03-Composing.md)

