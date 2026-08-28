# The column an aniframe is indexed by

Exactly one column, of any name, holding the position of each row within
its temporal context. It is declared separately from `variables_when`,
which holds the context itself — session, trial, observation — and
which, with `variables_what`, is what the frame is grouped by. The index
is never a grouping variable.

## Usage

``` r
get_index(data)
```

## Arguments

- data:

  An aniframe object.

## Value

Length-one character vector naming the index column.

## Details

An [`anievent()`](https://animovement.dev/anicore/reference/anievent.md)
has none: a bout spans an interval rather than sitting at a point, so it
is delimited by `start` and `stop`, which are declared temporal columns.
Its `variables_index` is `NA`, and asking for it here is an error rather
than a guess.

## See also

[`set_index()`](https://animovement.dev/anicore/reference/set_index.md)
to change it,
[`get_variables_when()`](https://animovement.dev/anicore/reference/variables.md)
for the full set of temporal columns.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
get_index(af)
#> [1] "time"
```
