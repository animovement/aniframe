# Add connections to an aniframe

**\[experimental\]**

Append one or more `from`/`to` pairs to the existing connections for a
variable. `from` and `to` may be either single strings or character
vectors of equal length (one connection per element).

## Usage

``` r
add_connections(data, from, to, variable = "keypoint")
```

## Arguments

- data:

  An aniframe object.

- from:

  Character vector of source endpoints.

- to:

  Character vector of target endpoints. Must be the same length as
  `from`.

- variable:

  Character scalar. Name of the variable the connections relate to (must
  be in `variables_what` or `variables_when`). Defaults to `"keypoint"`.

## Value

The aniframe with the new connections appended.

## Details

No deduplication is performed — duplicates of an existing pair will
appear twice in the resulting table. Endpoints not found in
`data[[variable]]` produce a warning but are still appended.

## See also

[`set_connections()`](http://animovement.dev/aniframe/reference/set_connections.md),
[`get_connections()`](http://animovement.dev/aniframe/reference/get_connections.md),
[`remove_connections()`](http://animovement.dev/aniframe/reference/remove_connections.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- example_aniframe()
data <- add_connections(data, from = "head", to = "neck")
data <- add_connections(
  data,
  from = c("neck", "neck"),
  to = c("shoulder_right", "shoulder_left")
)
} # }
```
