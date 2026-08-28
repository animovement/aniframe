# Identity variable names recognised across the animovement classes

The identity (`what`) columns auto-detection looks for, shared by
[`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
and
[`as_anievent()`](https://animovement.dev/anicore/reference/as_anievent.md).
Only the names present in the data are used, and any other column can be
declared explicitly via `variables_what`.

## Usage

``` r
list_recognised_variables_what()
```

## Value

Character vector of column names.

## Details

The names are listed coarsest first, which reads naturally for the ones
that do nest — a `subject` has `track`s, a track has `keypoint`s. **That
is the order detection emits, not a hierarchy a frame asserts.**
Identity variables need not nest at all: `sex`, `treatment` and
`genotype` partition a population without containing one another, and
there is no sense in which one of them is finer than the next.

So nothing should read a position in `variables_what` as meaning a
level. Where a function needs to know which variable to operate on, it
asks — `animetric::add_centroid()` takes `across`,
`anispace::translate_coords()` takes `level` — rather than inferring
one. The order does still carry through to column order and grouping,
which is presentation: grouping by `(a, b)` and `(b, a)` gives the same
groups.

`subject` and `individual` name the same kind of thing; both are
recognised because behavioural coding tools (BORIS and its kin) speak of
subjects where tracking tools speak of individuals.
