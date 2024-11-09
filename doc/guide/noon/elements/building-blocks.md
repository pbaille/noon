
# Table of Contents

1.  [Building blocks](#org33d7b9f)
    1.  [Events](#org8dda9af)
    2.  [Score](#org93f19c2)


<a id="org33d7b9f"></a>

# Building blocks


<a id="org8dda9af"></a>

## Events

An event is the basic building block we are dealing with.

It can represent any MIDI event, a note, a control change etc&#x2026;

It is represented using a clojure map

    DEFAULT_EVENT


<a id="org93f19c2"></a>

## Score

A score is a collection of events, represented using a clojure set.

    (mk) ; the `mk` function is used to build score, more on it later

