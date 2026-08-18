# Strip a frame back to its dplyr classes

The structural steps operate on a plain frame, so they neither dispatch
back into the class-preserving methods nor trigger the `ungroup()` "use
with care" warning when a declaration leaves nothing to group by.

## Usage

``` r
strip_animovement_class(data)
```

## Arguments

- data:

  An aniframe or anievent object.

## Value

`data` with the animovement classes removed.
