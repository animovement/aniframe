# Warn when the declaration does not identify one observation per row

Identity plus temporal context plus the index is meant to be a composite
key: one entity, in one context, at one position. When it repeats, some
variable that distinguishes the rows is undeclared, and every grouped
operation silently folds those rows together — a trajectory with two `x`
values at the same instant is not a trajectory.

## Usage

``` r
warn_duplicate_observations(data)
```

## Arguments

- data:

  An aniframe object.

## Value

`TRUE`, invisibly.

## Details

A warning rather than an error. The state is reachable part-way through
honest work — a frame read before its identity column is declared, say —
and nothing in the class is broken by it (#49).
