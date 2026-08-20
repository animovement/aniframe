# The aniframe data structure

``` r

library(aniframe)
```

## What is an `aniframe`?

The *aniframe* package defines `aniframe` — the foundational data
structure of the *animovement* ecosystem. Every sister package
(*aniread* for I/O, *animetric* for kinematics, …) is built around
consuming and producing `aniframe` objects, so the shape that an
`aniframe` takes is what holds the ecosystem together.

The shape itself follows the principles of *tidy movement data* (laid
out in an upcoming paper). The short version: every row records one
entity at one timepoint at one position, and the columns split into
three semantic slots that answer **what** is moving, **when**, and
**where**.

This article covers the data shape. Companion articles cover the
[metadata
attribute](https://animovement.dev/aniframe/articles/aniframe-metadata.md)
and the [connections
field](https://animovement.dev/aniframe/articles/aniframe-connections.md)
for skeletons and networks.

## Anatomy at a glance

The cheapest way to see an `aniframe` is to print one.

``` r

data <- example_aniframe(
  n_obs = 5,
  n_individuals = 2,
  n_keypoints = 3,
  n_dims = 2
)
data
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1  0.895  -0.937      0.724
#>  2          1 head           1     1     2  0.684   1.23       0.659
#>  3          1 head           1     1     3  1.58   -1.44       0.894
#>  4          1 head           1     1     4  0.0415  0.435      0.933
#>  5          1 head           1     1     5 -1.00   -1.89       0.796
#>  6          1 neck           1     1     1 -0.797  -1.32       0.909
#>  7          1 neck           1     1     2 -0.686   1.49       0.772
#>  8          1 neck           1     1     3 -1.96    1.22       0.750
#>  9          1 neck           1     1     4 -0.764  -0.937      0.658
#> 10          1 neck           1     1     5  0.0517  2.49       0.576
#> # ℹ 20 more rows
```

Two things to spot:

1.  The **header rows** prefixed with `#` come from the metadata
    attribute and summarise what the object contains — individuals,
    keypoints, recording duration when known, sampling rate, etc.
2.  The **data columns** form a tidy table where every row records one
    entity at one timepoint at one position.

`aniframe` inherits from `tbl_df` and `data.frame`, so dplyr verbs,
ggplot2, and base subsetting all work on it directly.

The rest of this article unpacks the columns.

## The three slots: what / when / where

Every row of an `aniframe` answers three questions:

| Question | Slot | Resolves | Default columns |
|----|----|----|----|
| **What** is moving? | Identity (`variables_what`) | An entity | `individual`, `keypoint` |
| **When**? | Temporal (`variables_when`) | A timepoint | `time` |
| **Where**? | Spatial (`variables_where`) | A position | `x`, `y` |

The combination of all identity and temporal columns forms a composite
key that uniquely identifies each row; the spatial columns are what that
row *records* about that entity at that timepoint.

`aniframe` stores which columns play which role in the metadata, so
downstream code can introspect:

``` r

md <- get_metadata(data)
md$variables_what
#> [1] "individual" "keypoint"
md$variables_when
#> [1] "session" "trial"   "time"
md$variables_where
#> [1] "x" "y"
```

### Customising the slots

Most readers populate these from the source data, but you can override
them when constructing manually. By default,
[`as_aniframe()`](https://animovement.dev/aniframe/reference/as_aniframe.md)
recognises `c("model", "individual", "track", "keypoint")` as identity
columns and `c("session", "trial", "time")` as temporal columns. Pass
the slot arguments explicitly to use other names:

``` r

df <- data.frame(
  track = rep(c("A", "B", "C"), each = 4),
  trial = rep(1:2, each = 6),
  time = rep(1:2, 6),
  x = runif(12),
  y = runif(12)
)

custom <- as_aniframe(
  df,
  variables_what = "track",
  variables_when = c("trial", "time")
)
custom
#> # Tracks: A, B, C
#> # Trials: 1, 2
#>    track trial  time      x     y
#>    <fct> <int> <int>  <dbl> <dbl>
#>  1 A         1     1 0.800  0.907
#>  2 A         1     1 0.718  0.117
#>  3 A         1     2 0.143  0.191
#>  4 A         1     2 0.465  0.629
#>  5 B         1     1 0.920  0.661
#>  6 B         1     2 0.627  0.792
#>  7 B         2     1 0.0671 0.897
#>  8 B         2     2 0.744  0.672
#>  9 C         2     1 0.220  0.903
#> 10 C         2     1 0.486  0.487
#> 11 C         2     2 0.468  0.361
#> 12 C         2     2 0.925  0.606
```

`time` is the only temporal column with a special status: it must always
be present, and downstream operations (smoothing, derivatives) assume
it’s monotonically ordered within each entity.

## Coordinate systems

The set of spatial columns determines the coordinate system. `aniframe`
recognises four families:

| System      | Columns                  |
|-------------|--------------------------|
| Cartesian   | `x`, `y`, optionally `z` |
| Polar       | `rho`, `phi`             |
| Cylindrical | `rho`, `phi`, `z`        |
| Spherical   | `rho`, `phi`, `theta`    |

The number of Cartesian columns picks the dimensionality
(`cartesian_1d`, `cartesian_2d`, `cartesian_3d`):

``` r

cart <- aniframe(
  individual = 1L, time = 1:3,
  x = c(0, 1, 2), y = c(0, 1, 4), z = c(0, 0, 1)
)
get_metadata(cart, "coordinate_system")
#> [1] cartesian_3d
#> 7 Levels: unknown cartesian_1d cartesian_2d cartesian_3d polar ... spherical
```

`rho` + `phi` indicates a polar-family system; the third column (if any)
distinguishes the variant:

``` r

pol <- aniframe(
  individual = 1L, time = 1:3,
  rho = c(1, 1, 1), phi = c(0, pi / 2, pi)
)
get_metadata(pol, "coordinate_system")
#> [1] polar
#> 7 Levels: unknown cartesian_1d cartesian_2d cartesian_3d polar ... spherical

cyl <- aniframe(
  individual = 1L, time = 1:3,
  rho = c(1, 1, 1), phi = c(0, pi / 2, pi), z = c(0, 1, 2)
)
get_metadata(cyl, "coordinate_system")
#> [1] cylindrical
#> 7 Levels: unknown cartesian_1d cartesian_2d cartesian_3d polar ... spherical

sph <- aniframe(
  individual = 1L, time = 1:3,
  rho = c(1, 1, 1), phi = c(0, pi / 2, pi), theta = c(0, pi / 4, pi / 2)
)
get_metadata(sph, "coordinate_system")
#> [1] spherical
#> 7 Levels: unknown cartesian_1d cartesian_2d cartesian_3d polar ... spherical
```

For programmatic checks, every coordinate system has both an
`is_<system>()` predicate (returns logical) and an
`ensure_is_<system>()` guard (errors otherwise):

``` r

is_polar(pol)
#> [1] TRUE
is_cartesian_2d(cart)
```

## Where to next?

- [The metadata
  attribute](https://animovement.dev/aniframe/articles/aniframe-metadata.md)
  — units, origin, sampling rate, and the setters that keep them
  consistent with the data.
- [Connections](https://animovement.dev/aniframe/articles/aniframe-connections.md)
  — recording skeleton edges or other relationships between values of an
  identity / temporal variable.
