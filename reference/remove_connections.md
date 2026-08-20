# Remove connections from an aniframe

**\[experimental\]**

Remove `from`/`to` pairs from the connections of a variable. Matching is
exact and order-sensitive: `remove_connections(data, "a", "b")` removes
the pair `(from = "a", to = "b")` but does *not* remove
`(from = "b", to = "a")`. Call twice with swapped arguments if you want
both directions gone.

## Usage

``` r
remove_connections(data, from, to, variable = "keypoint")
```

## Arguments

- data:

  An aniframe object.

- from:

  Character vector of source endpoints to remove.

- to:

  Character vector of target endpoints to remove. Must be the same
  length as `from`.

- variable:

  Character scalar. Name of the variable. Defaults to `"keypoint"`.

## Value

The aniframe with matching connections removed.

## See also

[`set_connections()`](https://animovement.dev/aniframe/reference/set_connections.md),
[`get_connections()`](https://animovement.dev/aniframe/reference/get_connections.md),
[`add_connections()`](https://animovement.dev/aniframe/reference/add_connections.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- example_aniframe() |>
  add_connections(from = c("head", "neck"), to = c("neck", "shoulder_right"))
data <- remove_connections(data, from = "head", to = "neck")
} # }
```
