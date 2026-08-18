# Restructure a frame to match a declaration

Dispatches to the per-class restructure. The two classes share the
metadata substrate but not their layout: an aniframe is grouped and
ordered by identity then time, an anievent is ordered by identity then
bout start and is never grouped.

## Usage

``` r
restructure_frame(data, variables_what, variables_when, variables_where)
```

## Arguments

- data:

  An aniframe or anievent object.

- variables_what, variables_when, variables_where:

  The full declaration to apply.

## Value

`data`, restructured, with the declaration recorded.
