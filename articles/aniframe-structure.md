# The aniframe data structure

``` r

library(anicore)
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
attribute](https://animovement.dev/anicore/articles/aniframe-metadata.md)
and the [connections
field](https://animovement.dev/anicore/articles/aniframe-connections.md)
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
#>  1          1 head           1     1     1  0.719  -0.379      0.589
#>  2          1 head           1     1     2 -0.683   0.689      0.354
#>  3          1 head           1     1     3  0.0598 -0.640      0.809
#>  4          1 head           1     1     4  0.0879  0.481      0.648
#>  5          1 head           1     1     5 -1.07   -0.867      0.765
#>  6          1 neck           1     1     1 -1.36   -1.63       0.921
#>  7          1 neck           1     1     2  0.531  -1.31       0.914
#>  8          1 neck           1     1     3  2.04    0.836      0.506
#>  9          1 neck           1     1     4 -0.357  -0.908      0.556
#> 10          1 neck           1     1     5  1.05    0.178      0.840
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
| **When**? | Temporal context (`variables_when`) | A context — which session, which trial | `session`, `trial` |
| **Where**? | Spatial (`variables_where`) | A position | `x`, `y` |

“When” is really two questions, and only the first is answered here:
which recording session this row belongs to, and where it sits inside
it. The first is context the row shares with its neighbours, and it is
what the frame groups by; the second is what tells the row apart from
them, and it orders rather than groups. That second half is the
**index**, declared on its own in `variables_index` and covered in [its
own section](#the-index) below.

The combination of all identity and temporal columns forms a composite
key that uniquely identifies each row; the spatial columns are what that
row *records* about that entity at that timepoint.

`aniframe` stores which columns play which role in the metadata, so
downstream code can introspect:

``` r

md <- get_metadata(data)
md$variables_index
#> [1] "time"
md$variables_what
#> [1] "individual" "keypoint"
md$variables_when
#> [1] "session" "trial"
md$variables_where
#> [1] "x" "y"
```

### Customising the slots

Most readers populate these from the source data, but you can override
them when constructing manually. By default,
[`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
recognises `c("model", "individual", "track", "keypoint")` as identity
columns and `c("session", "trial")` as temporal context. Pass the slot
arguments explicitly to use other names:

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
  variables_when = "trial"
)
custom
#> # Tracks: A, B, C
#> # Trials: 1, 2
#>    track trial  time      x      y
#>    <fct> <int> <int>  <dbl>  <dbl>
#>  1 A         1     1 0.439  0.331 
#>  2 A         1     1 0.958  0.0498
#>  3 A         1     2 0.942  0.172 
#>  4 A         1     2 0.766  0.530 
#>  5 B         1     1 0.0391 0.644 
#>  6 B         1     2 0.270  0.148 
#>  7 B         2     1 0.342  0.941 
#>  8 B         2     2 0.781  0.226 
#>  9 C         2     1 0.926  0.604 
#> 10 C         2     1 0.580  0.497 
#> 11 C         2     2 0.746  0.350 
#> 12 C         2     2 0.963  0.237
```

## The index

One column has a status of its own: the **index**, which gives each row
its position within its temporal context. Read it with
[`get_index()`](https://animovement.dev/anicore/reference/get_index.md):

``` r

get_index(data)
#> [1] "time"
```

It defaults to `time`, but nothing requires that name. A frame straight
off a camera is often indexed by frame number, and can say so:

``` r

frames <- data.frame(
  individual = 1L,
  frame = 1:6,
  x = runif(6),
  y = runif(6)
)

af <- as_aniframe(frames, index = "frame")
get_index(af)
#> [1] "frame"
```

No column called `time` is involved, and none is required:

``` r

names(af)
#> [1] "individual" "frame"      "x"          "y"
```

Two rules follow from what the index *is*, and both are worth knowing.

**A frame has exactly one.** The index is where a row sits within its
context, so a second one would have nothing left to mean. Declaring more
than one is an error rather than a silent pick between them:

``` r

as_aniframe(frames, index = c("frame", "individual"))
#> Error in `ensure_index_name()`:
#> ! `index` must be a single column name.
#> ℹ A frame has exactly one index.
```

**It is never a grouping variable.** The frame is grouped by identity
plus temporal *context*; the index orders rows inside each of those
groups. Grouping by it as well would put every row in a group of its
own, so it stays out:

``` r

dplyr::group_vars(data)
#> [1] "individual" "keypoint"   "session"    "trial"
```

[`set_index()`](https://animovement.dev/anicore/reference/set_index.md)
changes it, re-ordering the frame to match. The column that was
previously the index becomes an ordinary, undeclared one — it is not
promoted to temporal context, for the reason just given:

``` r

timestamped <- data |>
  dplyr::mutate(timestamp = time / 30) |>
  set_index("timestamp")

get_index(timestamped)
#> [1] "timestamp"
get_variables_when(timestamped)
#> [1] "session" "trial"
dplyr::group_vars(timestamped)
#> [1] "individual" "keypoint"   "session"    "trial"
```

`time` is still there as a column; it is simply no longer declared as
anything.

Downstream operations (smoothing, derivatives) assume the index is
monotonically ordered within each entity, which is how
[`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
arranges the frame.

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
#> [1] FALSE
```

## Where to next?

- [The metadata
  attribute](https://animovement.dev/anicore/articles/aniframe-metadata.md)
  — units, axis directions, sampling rate, and the setters that keep
  them consistent with the data.
- [Connections](https://animovement.dev/anicore/articles/aniframe-connections.md)
  — recording skeleton edges or other relationships between values of an
  identity / temporal variable.
