# Connections

``` r

library(aniframe)
```

## What `connections` are for

Many movement datasets need to record relationships *between* the values
of an identity or temporal variable. The most common case is a
**skeleton** — edges between keypoints — but the same shape works for a
social network (edges between individuals) or a session graph.

`aniframe` stores this as a single metadata field, `connections`, plus a
small family of functions for managing it. This article covers that
field and those functions.

For the rest of the metadata see [Metadata on an
aniframe](https://animovement.dev/aniframe/articles/aniframe-metadata.md);
for the data-column structure see [The aniframe data
structure](https://animovement.dev/aniframe/articles/aniframe-structure.md).

## Storage shape

`connections` is a **named list keyed by the variable** the
relationships describe. Each entry is a 2-column `from`/`to` tibble. The
order of `from` / `to` is preserved as supplied, so downstream consumers
can treat the table as either directed (parent → child kinematic chains)
or undirected (skeleton edges).

``` r

fish <- example_aniframe(n_keypoints = 7) |>
  set_connections(
    list(
      c("head", "neck"),
      c("neck", "shoulder_right"),
      c("neck", "shoulder_left"),
      c("shoulder_right", "abdomen"),
      c("shoulder_left", "abdomen")
    )
  )

get_connections(fish, "keypoint")
#> # A tibble: 5 × 2
#>   from           to            
#>   <chr>          <chr>         
#> 1 head           neck          
#> 2 neck           shoulder_right
#> 3 neck           shoulder_left 
#> 4 shoulder_right abdomen       
#> 5 shoulder_left  abdomen
```

## The four functions

``` r

set_connections(data, connections, variable = "keypoint") # replace
get_connections(data, variable = NULL) # full list, or one entry
add_connections(data, from, to, variable = "keypoint") # append (vectorised)
remove_connections(data, from, to, variable = "keypoint") # exact match
```

### `set_connections()`

Replaces the connection table for a single variable. Three input shapes
are accepted — pick whichever is most natural:

``` r

# Implicit by position: element[1] is `from`, element[2] is `to`
set_connections(data, list(c("head", "neck"), c("neck", "shoulder_right")))

# Named within each pair — use when direction should be obvious in the call
set_connections(data, list(c(from = "head", to = "neck")))

# 2-column data.frame — useful when connections come from another tabular source
set_connections(
  data,
  data.frame(
    from = c("head", "neck"),
    to   = c("neck", "shoulder_right")
  )
)
```

### `get_connections()`

Returns the full named list when `variable` is `NULL`, or a single
tibble when a variable name is given.

``` r

get_connections(fish, "keypoint")
#> # A tibble: 5 × 2
#>   from           to            
#>   <chr>          <chr>         
#> 1 head           neck          
#> 2 neck           shoulder_right
#> 3 neck           shoulder_left 
#> 4 shoulder_right abdomen       
#> 5 shoulder_left  abdomen
```

### `add_connections()` / `remove_connections()`

These mirror each other: both take `from` and `to` as either single
strings or vectors of equal length, so multiple edges go in or out in
one call.

``` r

fish <- add_connections(
  fish,
  from = c("abdomen", "abdomen"),
  to   = c("hip_right", "hip_left")
)
nrow(get_connections(fish, "keypoint"))
#> [1] 7

# Same shape, in reverse — remove three pairs at once:
fish <- remove_connections(
  fish,
  from = c("abdomen", "abdomen", "head"),
  to   = c("hip_right", "hip_left", "neck")
)
nrow(get_connections(fish, "keypoint"))
#> [1] 4
```

[`remove_connections()`](https://animovement.dev/aniframe/reference/remove_connections.md)
matches exactly on `from`/`to`. Direction matters: removing `(a, b)`
won’t strip `(b, a)`. If you stored an edge in one direction but want it
gone regardless of orientation, include both pairs in the call (or
invoke twice with swapped arguments).

## Connections on other variables

`connections` is keyed by *any* identity or temporal variable, not just
keypoints. A social-network study, for example, might record edges
between individuals:

``` r

social <- example_aniframe(n_individuals = 3) |>
  set_connections(
    list(c("1", "2"), c("2", "3")),
    variable = "individual"
  )

get_connections(social)
#> $individual
#> # A tibble: 2 × 2
#>   from  to   
#>   <chr> <chr>
#> 1 1     2    
#> 2 2     3
```

The `variable` argument must be a column listed in `variables_what` or
`variables_when` —
[`set_connections()`](https://animovement.dev/aniframe/reference/set_connections.md)
errors otherwise to catch typos.

## Catching typos

When a `from`/`to` value isn’t found in the corresponding column,
[`set_connections()`](https://animovement.dev/aniframe/reference/set_connections.md)
and
[`add_connections()`](https://animovement.dev/aniframe/reference/add_connections.md)
emit a warning but **keep the connection** — the value may legitimately
be missing in this particular recording while being valid elsewhere.

``` r

example_aniframe(n_keypoints = 5) |>
  set_connections(list(c("head", "necc"))) # typo
#> Warning: Some connection endpoints are not present in the "keypoint" column: "necc".
#> ℹ Keeping them in case the value is recorded in another file or video.
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right, shoulder_left, abdomen
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1  0.493  -0.995      0.676
#>  2          1 head           1     1     2 -0.865  -0.674      0.599
#>  3          1 head           1     1     3 -2.03   -0.985      0.571
#>  4          1 head           1     1     4 -0.973   0.234      0.702
#>  5          1 head           1     1     5  0.232  -0.683      0.798
#>  6          1 head           1     1     6  0.677  -1.38       0.944
#>  7          1 head           1     1     7  0.204   1.85       0.705
#>  8          1 head           1     1     8 -0.289  -0.219      0.874
#>  9          1 head           1     1     9  0.0532  1.18       0.696
#> 10          1 head           1     1    10 -0.660   0.534      0.572
#> # ℹ 740 more rows
```
