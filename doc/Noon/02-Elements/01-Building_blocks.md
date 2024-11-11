
# Building blocks


## Events

An event is the basic building block we are dealing with.

It can represent any MIDI event, a note, a control change etc&#x2026;

It is represented using a clojure map

    DEFAULT_EVENT


## Score

A score is a collection of events, represented using a clojure set.

    (mk) ; the `mk` function is used to build score, more on it later

