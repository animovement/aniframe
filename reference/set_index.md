# Declare which column an aniframe is indexed by

Changing the index changes the order the rows come in, so — like the
`variables_*` declarations — it is not reachable through
[`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md)
and has its own setter, which does the restructuring too.

## Usage

``` r
set_index(data, column)
```

## Arguments

- data:

  An aniframe object.

- column:

  Length-one character vector naming the index column. It must exist in
  `data` and be numeric.

## Value

`data`, re-indexed and restructured.

## Details

If the column was declared as temporal context it stops being so: a
variable cannot be both the position within a context and part of it.
The column the frame was previously indexed by becomes an ordinary
undeclared column rather than being promoted to a grouping variable —
which, holding one value per row, would put every row in its own group.

## See also

[`get_index()`](https://animovement.dev/anicore/reference/get_index.md)

## Examples

``` r
df <- data.frame(frame = 1:3, individual = "a", x = c(1, 2, 3), y = c(0, 1, 0))
af <- as_aniframe(df, index = "frame")
get_index(af)
#> [1] "frame"
```
