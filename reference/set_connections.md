# Set the connections for a variable

**\[experimental\]**

Replace the connections (e.g. skeleton edges between keypoints, edges of
a social network between individuals) for a single variable. Connections
are stored as a 2-column `from`/`to` tibble; storage preserves the order
supplied so downstream consumers can interpret the table as either
directed or undirected.

## Usage

``` r
set_connections(data, connections, variable = "keypoint")
```

## Arguments

- data:

  An aniframe object.

- connections:

  One of:

  - a 2-column data.frame with columns `from` and `to`,

  - a list of length-2 character vectors (each `c(from, to)`),

  - `NULL` to clear the connections for `variable`.

- variable:

  Character scalar. Name of the identity (`variables_what`) or temporal
  (`variables_when`) variable the connections relate to. Defaults to
  `"keypoint"`.

## Value

The aniframe with updated `connections` metadata.

## Details

If any `from`/`to` value isn't found in the corresponding column of
`data`, a warning is emitted but the connection is kept — the value may
legitimately be missing in this particular recording while being valid
elsewhere.

## See also

[`get_connections()`](https://animovement.dev/aniframe/reference/get_connections.md),
[`add_connections()`](https://animovement.dev/aniframe/reference/add_connections.md),
[`remove_connections()`](https://animovement.dev/aniframe/reference/remove_connections.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- example_aniframe()

# Implicit by position (element[1] = from, element[2] = to)
data <- set_connections(
  data,
  list(
    c("head", "neck"),
    c("neck", "shoulder_right"),
    c("neck", "shoulder_left"),
    c("shoulder_right", "hip_right"),
    c("shoulder_left", "hip_left")
  )
)

# Explicit names within each pair
data <- set_connections(
  data,
  list(
    c(from = "head", to = "neck"),
    c(from = "neck", to = "shoulder_right")
  )
)

# Or as a 2-column data.frame
data <- set_connections(
  data,
  data.frame(
    from = c("head", "neck"),
    to   = c("neck", "shoulder_right")
  )
)
} # }
```
