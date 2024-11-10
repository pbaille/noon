
# Table of Contents

1.  [Building blocks](#orgb112e4b)
    1.  [Events](#orge0fc496)
    2.  [Score](#org3c21226)


<a id="orgb112e4b"></a>

# Building blocks


<a id="orge0fc496"></a>

## Events

An event is the basic building block we are dealing with.

It can represent any MIDI event, a note, a control change etc&#x2026;

It is represented using a clojure map

    DEFAULT_EVENT


<a id="org3c21226"></a>

## Score

A score is a collection of events, represented using a clojure set.

    (mk) ; the `mk` function is used to build score, more on it later

