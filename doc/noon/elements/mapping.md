
# Mapping

All the transformations we&rsquo;ve seen so far are acting on a score to produce another score, but sometimes what we need is to apply a transformation on each event of a score, for this we are using the `each` function.

As an illustration, here those two fragments:

    (play (lin c0 c1 c2 c3)
          (tup c0 o1)) ; each member of this `tup` form receives and operate on the whole score

    (play (lin c0 c1 c2 c3)
          (each (tup c0 o1))) ; each event of the score is transformed using this `tup` transformation.

One important thing to be aware of is that events will be mapped in place, so if the given transformation expand the score, some superposition will occur.

    (play (lin c0 o1)
          (each [dur:4 (rep 8 c1-)]))

Some others functions exists to transform only subparts of the score, if interested you can look at `$by` and/or `parts`.

