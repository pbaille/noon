
# Non determinism

It is quite fun to insert a bit of randomness in our scores.

    (play
     (rand-nth [(tup c0 c4 c7) (tup c0 c3 c7)])
     (rep 4 (rand-nth [c3 c4 c3- c4-])))

We can use some great available tools like `test.check.generators` to handle non determinism. That being said, some commonly used non-deterministic functions are available directly.


## one-of

`one-of` picks randomly one of the given transformations and apply it.

    (play (one-of o1- o1))

    (play dur:8 (rep 50 (one-of c1 c1-)))


## maybe

`maybe` is very similar to `one-of` except it has a chance to do nothing (identity transformation).

    (play (maybe o1 o2)) ; may do nothing, or one octave up, or two octave up

    (play (one-of same o1 o2)) ; the equivalent `one-of` form

    (play dur:8 (rep 50 (maybe c1 c1-))) ; you can notice melodic repetitions unlike with the corresponding one-of example.


## probs

`probs` gives you more control over the probability of occurence of the given transformations.

    (play (probs {o1 4, o1- 1})) ; 4/5 to go up one octave, 1/5 chance to go down one octave

    (play dur:4 (rep 24 (probs {c1 6, c6- 1, (par c0 o1-) 1})))


## any-that

`any-that` is similar to `one-of` except it takes an extra first argument that check if the picked transformation is valid.

A melody of 60 notes using the 6 given intervals but remaining in the given pitch bounds:

    (play dur:8
          (rep 60
               (any-that (within-pitch-bounds? :C-1 :C1)
                         c2 c5 c7 c2- c5- c7-)))

The `within-pitch-bounds?` is just a score transformation that return the score unchanged if it is within the given bounds, else it returns `nil`. Any function of this kind can be used has first argument to `any-that`.


## !

the `!` macro can be useful to deal with raw non deterministic expressions. here the docstring:

> Takes a non deterministic expression resulting in a score transformation. return a score transformation that wraps the expression so that it is evaluated each time the transformation is used.

    (play (nlin 4 (! (tup* (shuffle [d0 d2 d4 d6])))))

    (play (nlin 4 (tup* (shuffle [d0 d2 d4 d6])))) ; without the bang the shuffle expression is executed only one time.


## Shuffling

As in the previews example, building a `tup` or a `lin` with shuffled sequence of transformation is quite fun.

So two shortcuts are defined:

    (play (shuftup d0 d2 d4 d6))

    (play (shuflin d0 d2 d4 d6))

