# Metadata on an aniframe

``` r

library(anicore)
```

## Why an attribute, not columns?

Every `aniframe` carries a metadata list as an R attribute alongside the
data columns. The metadata records the things that are true of the
recording as a whole rather than of any single observation: the source
software, the sampling rate, what units the spatial coordinates are in,
which way its axes point, and so on.

Keeping this information attached to the object — rather than living in
a separate file or being passed around as extra arguments — is what lets
the rest of the *animovement* ecosystem stay loosely coupled. A reader
(in *aniread*) populates the metadata at load time, and any downstream
tool can read it back without a hand-off.

This article covers the metadata attribute and the functions that read
and update it. For the data-column structure see [The aniframe data
structure](https://animovement.dev/anicore/articles/aniframe-structure.md);
for the `connections` field specifically see
[Connections](https://animovement.dev/anicore/articles/aniframe-connections.md).

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
#> sampling_interval (numeric)   : 1
#> start_datetime    (POSIXct)   : <NA>
#> variables_index   (character) : "time"
#> variables_what    (character) : "individual, keypoint"
#> variables_when    (character) : "session, trial"
#> variables_where   (character) : "x, y"
#> variables_event   (list)      : "character(0), character(0)"
#> axes              (character) : "x, y"
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
#> axis_directions   (character) : 
#> axis_extents      (numeric)   : 
#> handedness        (factor)    : "unknown"
#>                                 [levels: right, left, unknown]
#> connections       (list)      : 
#> spec_version      (list)      : "2.0.0, 0.3.0"
```

The fields and their defaults are defined in one place,
[`list_default_metadata()`](https://animovement.dev/anicore/reference/list_default_metadata.md)
— that’s the canonical source of truth for what an `aniframe`’s metadata
looks like.

``` r

str(list_default_metadata(), max.level = 1)
#> List of 22
#>  $ source           : chr NA
#>  $ source_version   : chr NA
#>  $ filename         : chr NA
#>  $ sampling_rate    : num NA
#>  $ sampling_interval: num NA
#>  $ start_datetime   : POSIXct[1:1], format: NA
#>  $ variables_index  : chr "time"
#>  $ variables_what   : chr [1:2] "individual" "keypoint"
#>  $ variables_when   : chr(0) 
#>  $ variables_where  : chr [1:2] "x" "y"
#>  $ variables_event  :List of 2
#>  $ axes             : Named chr [1:2] "x" "y"
#>   ..- attr(*, "names")= chr [1:2] "x" "y"
#>  $ unit_space       : Factor w/ 8 levels "px","none","nm",..: 1
#>  $ unit_angle       : Factor w/ 3 levels "rad","deg","none": 1
#>  $ unit_time        : Factor w/ 8 levels "unknown","frame",..: 2
#>  $ reference_frame  : Factor w/ 3 levels "allocentric",..: 1
#>  $ coordinate_system: Factor w/ 7 levels "unknown","cartesian_1d",..: 3
#>  $ axis_directions  : Named chr(0) 
#>   ..- attr(*, "names")= chr(0) 
#>  $ axis_extents     : Named num(0) 
#>   ..- attr(*, "names")= chr(0) 
#>  $ handedness       : Factor w/ 3 levels "right","left",..: 3
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
| **Frame of reference** | `reference_frame`, `coordinate_system`, `axes`, `axis_directions`, `axis_extents`, `handedness` |
| **Slot vocabulary** | `variables_index`, `variables_what`, `variables_when`, `variables_where` |
| **Relationships** | `connections` |

`filename` accepts a character vector — readers like
`aniread::read_trackball()` populate it with all source paths.

## Reading and writing metadata

[`get_metadata()`](https://animovement.dev/anicore/reference/get_metadata.md)
and
[`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md)
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

[`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md)
validates the input — factor fields are checked against their permitted
levels, and unknown fields are rejected.

For fields whose update has side effects on the data columns (or on
related fields), prefer the dedicated setters listed below.

| Setter | Touches |
|----|----|
| [`set_unit_space()`](https://animovement.dev/anicore/reference/set_unit_space.md) | converts `x`/`y`/`z` between length units |
| [`set_unit_time()`](https://animovement.dev/anicore/reference/set_unit_time.md) | converts the index column between time units |
| [`set_unit_angle()`](https://animovement.dev/anicore/reference/set_unit_angle.md) | converts `phi`/`theta` (auto) and any extra `cols` you supply |
| [`set_sampling_rate()`](https://animovement.dev/anicore/reference/set_sampling_rate.md) | flips `unit_time` from frames to seconds and rescales the index |
| [`set_index()`](https://animovement.dev/anicore/reference/set_index.md) | changes which column the frame is indexed by, and re-orders it |
| [`set_axes()`](https://animovement.dev/anicore/reference/set_axes.md) | declares which column carries which axis role |
| [`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md) | says which way each axis points, reflecting one turned over |
| [`set_axis_extents()`](https://animovement.dev/anicore/reference/set_axis_extents.md) | says how far each axis runs, which a reflection turns around |
| [`set_handedness()`](https://animovement.dev/anicore/reference/set_handedness.md) | says whether the frame is right- or left-handed |

The temporal setters follow the frame’s own declaration rather than a
column called `time` — see [the
index](https://animovement.dev/anicore/articles/aniframe-structure.html#the-index).

## Declaring the slot vocabulary

`variables_index`, `variables_what`, `variables_when`,
`variables_where`, `variables_event` and `axes` are a special case: they
name columns rather than describing values, so a name that matches
nothing is a promise the frame can’t keep. All but `variables_event` go
further — they are not a description of the frame, they *are* its
structure.
[`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
uses them to coerce column types, order columns and rows, group the
frame, and derive `coordinate_system`. Writing them without redoing that
work would leave the frame and its own metadata disagreeing — the print
header would update while the grouping still reflected the old
declaration.

[`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md)
therefore refuses all six, and points you at the setters that do the
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

The three structural roles have the same four verbs as `connections`:
`get_variables_*()`, `set_variables_*()`, `add_variables_*()` and
`remove_variables_*()`. `variables_index` has only two —
[`get_index()`](https://animovement.dev/anicore/reference/get_index.md)
and
[`set_index()`](https://animovement.dev/anicore/reference/set_index.md)
— because a frame has exactly one index, leaving nothing for `add_` and
`remove_` to do. The column has to exist before it can be declared, so
the order is always create-then-declare:

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
[`to_anievent()`](https://animovement.dev/anicore/reference/to_anievent.md)
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

## Which way the axes point

For 2D image-derived data there’s an annoying convention split: most
image and video tooling counts y **downward** from the top of the frame,
while plotting and most maths counts it **upward**. What differs is not
where `(0, 0)` sits — it is a corner either way — but the direction y
increases in, so that is what `aniframe` records.

`axis_directions` maps each axis role to one of six words, in three
opposed pairs, read from where the recording was made: `right`/`left`
across the view, `up`/`down` within it, and `back`/`forward` toward and
away from the viewer.

Turning an axis to its opposite reflects that column, so the data ends
up expressed the way you just declared. An axis runs from zero to its
`axis_extents` value, so the reflection is `extent - old` — and an axis
with no declared extent is centred on its origin instead, so turning it
over negates it.

``` r

img <- aniframe(
  individual = 1L, time = 1:4,
  x = c(0, 10, 20, 30),
  y = c(50, 100, 150, 200)
) |>
  set_axis_extents(c(y = 1080)) |>
  set_axis_directions(c(x = "right", y = "down"))

img$y
#> [1]  50 100 150 200
img <- set_axis_directions(img, c(y = "up"))
img$y # reflected: 1080 - original y
#> [1] 1030  980  930  880
```

### What follows from the directions

Two things are read off them rather than recorded separately, so they
cannot go on claiming a convention the axes no longer have.

[`get_angle_direction()`](https://animovement.dev/anicore/reference/get_angle_direction.md)
says which way angles run. `atan2(y, x)` counts counter-clockwise, so
the same physical heading comes out mirrored between a y-down and a y-up
frame — which is exactly the comparison that goes wrong silently without
this.

[`get_handedness()`](https://animovement.dev/anicore/reference/get_handedness.md)
needs three axes. Two directions leave it open, so a frame with only `x`
and `y` has none until it says which side it was observed from — and
that is not a detail. A rodent filmed from above and the same rodent
filmed from below through a glass floor give images whose `x` and `y`
are declared identically, but whose rotations run opposite ways. The
depth axis is the only thing that tells them apart.

``` r

above <- set_axis_directions(img, c(z = "back"))
below <- set_axis_directions(img, c(z = "forward"))

c(above = get_angle_direction(above), below = get_angle_direction(below))
#>               above               below 
#> "counter_clockwise"         "clockwise"
c(above = get_handedness(above), below = get_handedness(below))
#>   above   below 
#> "right"  "left"
```

Since `det[x y z]` is `(x × y) · z`, a right-handed frame counts
counter-clockwise about its own depth axis — always. The two answers are
one fact seen twice.

A frame can also state the convention without spelling the axes out,
which is how most 3D recordings are described:

``` r

set_handedness(img) |> get_handedness() # right-handed by default
#> [1] "right"
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
[`set_unit_angle()`](https://animovement.dev/anicore/reference/set_unit_angle.md)
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
