# Get connections from an aniframe

**\[experimental\]**

Read the connections currently stored on an aniframe. Returns the full
named list of `from`/`to` tibbles by default, or a single tibble when
`variable` is supplied.

## Usage

``` r
get_connections(data, variable = NULL)
```

## Arguments

- data:

  An aniframe object.

- variable:

  Optional character scalar. When `NULL` (default), returns the full
  named list of connection tables (one per variable). When a variable
  name, returns just that variable's `from`/`to` tibble (an empty tibble
  if no connections are set for that variable).

## Value

A named list of tibbles (when `variable` is NULL), or a single 2-column
tibble.

## See also

[`set_connections()`](http://animovement.dev/aniframe/reference/set_connections.md),
[`add_connections()`](http://animovement.dev/aniframe/reference/add_connections.md),
[`remove_connections()`](http://animovement.dev/aniframe/reference/remove_connections.md)
