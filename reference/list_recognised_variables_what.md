# Identity variable names recognised across the animovement classes

The identity (`what`) columns auto-detection looks for, shared by
[`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
and
[`as_anievent()`](https://animovement.dev/anicore/reference/as_anievent.md).
The order is coarse to fine — a `subject` or `individual` has `track`s,
a track has `keypoint`s — and it carries through to column order and
grouping. Only the names present in the data are used, and any other
column can be declared explicitly via `variables_what`.

## Usage

``` r
list_recognised_variables_what()
```

## Value

Character vector of column names.

## Details

`subject` and `individual` name the same kind of thing; both are
recognised because behavioural coding tools (BORIS and its kin) speak of
subjects where tracking tools speak of individuals.
