
# Dynamism

For now our scores are pretty static, and don&rsquo;t use the power of clojure much. Since this library is built out of simple functions it should be a easy to do so.

There is a bunch of things to know in order to ease things.


## Star functions

Variadic functions have a &rsquo;star&rsquo; counterpart that accepts a sequence instead of variadic args.

    (tup c1 c2 c3)

Is similar to:

    (tup* [c1 c2 c3])

or

    (tup* (list c1 c2 c3))

It ease things a bit when using clojure to generate arguments of those functions. Avoiding to write `apply` everywhere.


## Map functions

maps can be used to compose event transformations

    (play {:velocity (fn [x] (/ x 2)), :duration (fn [x] (* x 2))})


## Examples

    (play (tup* (shuffle [c0 c3 c7 c9])))

    (play
     (patch :electric-piano-1)
     (tup* (map (fn [v] {:velocity v}) (range 0 127 15))))

