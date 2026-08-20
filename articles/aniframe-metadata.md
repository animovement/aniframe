# Metadata on an aniframe

``` r

library(aniframe)
```

## Why an attribute, not columns?

Every `aniframe` carries a metadata list as an R attribute alongside the
data columns. The metadata records the things that are true of the
recording as a whole rather than of any single observation: the source
software, the sampling rate, what units the spatial coordinates are in,
where the coordinate origin sits, and so on.

Keeping this information attached to the object — rather than living in
a separate file or being passed around as extra arguments — is what lets
the rest of the *animovement* ecosystem stay loosely coupled. A reader
(in *aniread*) populates the metadata at load time, and any downstream
tool can read it back without a hand-off.

This article covers the metadata attribute and the functions that read
and update it. For the data-column structure see [The aniframe data
structure](http://animovement.dev/aniframe/articles/aniframe-structure.md);
for the `connections` field specifically see
[Connections](http://animovement.dev/aniframe/articles/aniframe-connections.md).

## The metadata attribute

You can see the full metadata by printing it directly:

``` r

data <- example_aniframe()
get_metadata(data)
#> ── animovement metadata ────────────────────────────────────────────────────────
#> source            (character) : <NA>
#> source_version    (character) : <NA>
#> filename          (character) : <NA>
#> sampling_rate     (numeric)   : <NA>
#> start_datetime    (POSIXct)   : <NA>
#> variables_what    (character) : "individual, keypoint"
#> variables_when    (character) : "session, trial, time"
#> variables_where   (character) : "x, y"
#> variables_event   (list)      : "character(0), character(0)"
#> unit_space        (factor)    : "px"
#>                                 [levels: px, none, nm, um, mm, cm, m, km]
#> unit_angle        (factor)    : "rad"
#>                                 [levels: rad, deg, none]
#> unit_time         (factor)    : "frame"
#>                                 [levels: unknown, frame, ns, us, ms, s, m, h]
#> reference_frame   (factor)    : "allocentric"
#>                                 [levels: allocentric, egocentric, none]
#> coordinate_system (factor)    : "cartesian_2d"
#>                                 [levels: unknown, cartesian_1d, cartesian_2d, cartesian_3d, polar, cylindrical, spherical]
#> origin            (factor)    : "bottom_left"
#>                                 [levels: bottom_left, top_left, none]
#> y_height          (numeric)   : 3.064683
#> connections       (list)      : 
#> spec_version      (list)      : "1.1.0, 0.2.0"
```

The fields and their defaults are defined in one place,
[`default_metadata()`](http://animovement.dev/aniframe/reference/default_metadata.md)
— that’s the canonical source of truth for what an `aniframe`’s metadata
looks like.

``` r

str(default_metadata(), max.level = 1)
#> List of 18
#>  $ source           : chr NA
#>  $ source_version   : chr NA
#>  $ filename         : chr NA
#>  $ sampling_rate    : num NA
#>  $ start_datetime   : POSIXct[1:1], format: NA
#>  $ variables_what   : chr [1:2] "individual" "keypoint"
#>  $ variables_when   : chr "time"
#>  $ variables_where  : chr [1:2] "x" "y"
#>  $ variables_event  :List of 2
#>  $ unit_space       : Factor w/ 8 levels "px","none","nm",..: 1
#>  $ unit_angle       : Factor w/ 3 levels "rad","deg","none": 1
#>  $ unit_time        : Factor w/ 8 levels "unknown","frame",..: 2
#>  $ reference_frame  : Factor w/ 3 levels "allocentric",..: 1
#>  $ coordinate_system: Factor w/ 7 levels "unknown","cartesian_1d",..: 3
#>  $ origin           : Factor w/ 3 levels "bottom_left",..: 1
#>  $ y_height         : num NA
#>  $ connections      : list()
#>  $ spec_version     :List of 2
#>  - attr(*, "class")= chr "aniframe_metadata"
```

The fields fall into a few groups:

| Group | Fields |
|----|----|
| **Provenance** | `source`, `source_version`, `filename`, `start_datetime` |
| **Sampling** | `sampling_rate` |
| **Units** | `unit_space`, `unit_time`, `unit_angle` |
| **Frame of reference** | `reference_frame`, `coordinate_system`, `origin`, `y_height` |
| **Slot vocabulary** | `variables_what`, `variables_when`, `variables_where` |
| **Relationships** | `connections` |

`filename` accepts a character vector — readers like
`aniread::read_trackball()` populate it with all source paths.

## Reading and writing metadata

[`get_metadata()`](http://animovement.dev/aniframe/reference/get_metadata.md)
and
[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
are the workhorses.

``` r

get_metadata(data, "sampling_rate")
#> [1] NA

data <- set_metadata(data, sampling_rate = 30, source = "deeplabcut")
get_metadata(data, "sampling_rate")
#> [1] 30
get_metadata(data, "source")
#> [1] "deeplabcut"
```

[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
validates the input — factor fields are checked against their permitted
levels, and unknown fields are rejected.

For fields whose update has side effects on the data columns (or on
related fields), prefer the dedicated setters listed below.

| Setter | Touches |
|----|----|
| [`set_unit_space()`](http://animovement.dev/aniframe/reference/set_unit_space.md) | converts `x`/`y`/`z` between length units |
| [`set_unit_time()`](http://animovement.dev/aniframe/reference/set_unit_time.md) | converts `time` between time units |
| [`set_unit_angle()`](http://animovement.dev/aniframe/reference/set_unit_angle.md) | converts `phi`/`theta` (auto) and any extra `cols` you supply |
| [`set_sampling_rate()`](http://animovement.dev/aniframe/reference/set_sampling_rate.md) | flips `unit_time` from frames to seconds and rescales `time` |
| [`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md) | flips the y-axis around `y_height` when changing convention |
| [`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md) | sets the recorded frame height used by [`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md) |

## Declaring the slot vocabulary

`variables_what`, `variables_when`, `variables_where` and
`variables_event` are a special case: they name columns rather than
describing values, so a name that matches nothing is a promise the frame
can’t keep. The first three go further — they are not a description of
the frame, they *are* its structure.
[`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
uses them to coerce column types, order columns and rows, group the
frame, and derive `coordinate_system`. Writing them without redoing that
work would leave the frame and its own metadata disagreeing — the print
header would update while the grouping still reflected the old
declaration.

[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
therefore refuses all four, and points you at the setters that do the
whole job:

``` r

data |> set_metadata(variables_what = "id")
#> Error in `ensure_no_declaration_fields()`:
#> ! `set_metadata()` cannot write variables_what directly.
#> ℹ This field declares which columns carry identity, time, position and events.
#>   Writing it here would leave the metadata naming columns the frame may not
#>   have, and the frame ordered and grouped as it was before.
#> ℹ Use `set_variables_what()` instead, which validate the columns exist and
#>   restructure the frame to match.
#> ℹ A complete metadata object can still be restored wholesale, as in
#>   `set_metadata(data, metadata = get_metadata(x))`.
```

Each role has the same four verbs as `connections`: `get_variables_*()`,
`set_variables_*()`, `add_variables_*()` and `remove_variables_*()`. The
column has to exist before it can be declared, so the order is always
create-then-declare:

``` r

tagged <- data |>
  dplyr::mutate(id = "trial_1") |>
  add_variables_what("id")

get_variables_what(tagged)
#> [1] "individual" "keypoint"   "id"
dplyr::group_vars(tagged)
#> [1] "individual" "keypoint"   "id"         "session"    "trial"
```

`add_variables_*()` appends to the declaration, so you don’t have to
restate what is already there — forgetting to would quietly demote an
existing identity variable and regroup the frame without it.

Declaring a spatial column refreshes the fields derived from it:

``` r

data |>
  dplyr::mutate(z = 0) |>
  add_variables_where("z") |>
  get_metadata("coordinate_system")
#> [1] cartesian_3d
#> 7 Levels: unknown cartesian_1d cartesian_2d cartesian_3d polar ... spherical
```

`variables_event` is the fourth role, and the one that doesn’t change
the frame’s shape: it declares which columns carry per-frame event
labels, split into interval-valued `state` columns and instantaneous
`point` columns.
[`to_anievent()`](http://animovement.dev/aniframe/reference/to_anievent.md)
reads it to know what to encode.

``` r

data |>
  dplyr::mutate(behaviour = factor("rest")) |>
  set_variables_event(state = "behaviour") |>
  get_variables_event()
#> $state
#> [1] "behaviour"
#> 
#> $point
#> character(0)
```

## Coordinate origin

For 2D image-derived data there’s an annoying convention split: most
image / video tooling uses the **top-left** corner as `(0, 0)` (y
increases downward), while plotting and most maths uses the
**bottom-left** corner (y increases upward). `aniframe` records which
one your data uses in the `origin` field, with permitted values
`c("bottom_left", "top_left")`.

[`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)
does the actual flip when you change convention. It needs the frame
height to compute `y_new = y_height - y_old`, so `y_height` must be set
first. Readers populate it automatically; for manually-constructed
`aniframe` objects,
[`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
falls back to `max(y)`, and
[`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md)
lets you override that with the true value.

``` r

img <- aniframe(
  individual = 1L, time = 1:4,
  x = c(0, 10, 20, 30),
  y = c(50, 100, 150, 200)
) |>
  set_y_height(1080)

img$y
#> [1]  50 100 150 200
img <- set_origin(img, "top_left")
img$y # reflected: 1080 - original y
#> [1] 1030  980  930  880
```

## Units

Spatial, temporal, and angular units each have their own setter.
Conversions between standard units are automatic; conversions from
unknown / `frame` / `px` units require an explicit calibration factor
(or, for time, a `sampling_rate`).

``` r

data <- example_aniframe(n_dims = 2) |>
  set_metadata(unit_space = "mm")

data_cm <- set_unit_space(data, to_unit = "cm")
get_metadata(data_cm, "unit_space")
#> [1] cm
#> Levels: px none nm um mm cm m km
```

``` r

data <- example_aniframe() # default unit_time = "frame"
data_s <- set_sampling_rate(data, sampling_rate = 30)
get_metadata(data_s, "unit_time") # now "s"
#> [1] s
#> Levels: unknown frame ns us ms s m h
range(data_s$time) # frames divided by fps
#> [1] 0.03333333 1.66666667
```

Spatial angular columns (`phi`, `theta`) are converted automatically by
[`set_unit_angle()`](http://animovement.dev/aniframe/reference/set_unit_angle.md)
whenever they’re present. Pass `cols` only for non-spatial angular
columns (e.g. heading direction).

``` r

pol <- aniframe(
  individual = 1L, time = 1:3,
  rho = c(1, 1, 1), phi = c(0, pi / 2, pi)
)
pol$phi
#> [1] 0.000000 1.570796 3.141593
pol_deg <- set_unit_angle(pol, to_unit = "deg")
pol_deg$phi
#> [1]   0  90 180
```
