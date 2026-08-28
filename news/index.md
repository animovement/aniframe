# Changelog

## anicore 0.8.0 (2026-08-28)

### Changed

- **The package is renamed from `aniframe` to `anicore`**
  ([\#84](https://github.com/animovement/anicore/issues/84)). It is no
  longer one class’s home: it declares `aniframe`, `anievent` and — as
  [\#84](https://github.com/animovement/anicore/issues/84) settles — the
  types the domain packages build on, while producing almost none of
  them. A package named after one of its entries was going to keep
  getting stranger as angles
  ([\#83](https://github.com/animovement/anicore/issues/83)),
  orientation ([\#46](https://github.com/animovement/anicore/issues/46))
  and masks ([\#11](https://github.com/animovement/anicore/issues/11))
  arrive.

  **The `aniframe` class keeps its name**, as do
  [`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md),
  [`is_aniframe()`](https://animovement.dev/anicore/reference/is_aniframe.md)
  and every other function. Only the package changes:
  [`library(anicore)`](https://animovement.dev/anicore/), `anicore::`,
  and `install.packages("anicore")`.

  Done now rather than at 1.0.0 because a rename only ever gets more
  expensive, and there are no external users yet to carry the cost.

- [`?aniframe`](https://animovement.dev/anicore/reference/aniframe.md)
  is the constructor again. The package documentation had claimed the
  same help topic, so the two were merged into one page; it is now
  [`?anicore`](https://animovement.dev/anicore/reference/anicore.md).

### Added

- An aniframe can be indexed by a column that is not called `time`
  ([\#109](https://github.com/animovement/anicore/issues/109)). A
  `variables_index` metadata field names the single column each row is
  positioned by;
  [`get_index()`](https://animovement.dev/anicore/reference/get_index.md)
  reads it,
  [`set_index()`](https://animovement.dev/anicore/reference/set_index.md)
  changes it and re-orders the frame, and
  [`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
  and
  [`aniframe()`](https://animovement.dev/anicore/reference/aniframe.md)
  take an `index` argument. A frame has exactly one index, and it is
  never a grouping variable.

  [`set_unit_time()`](https://animovement.dev/anicore/reference/set_unit_time.md),
  [`set_sampling_rate()`](https://animovement.dev/anicore/reference/set_sampling_rate.md)
  and
  [`to_anievent()`](https://animovement.dev/anicore/reference/to_anievent.md)
  act on the declared index rather than a column named `time`, and
  [`validate_aniframe()`](https://animovement.dev/anicore/reference/validate_aniframe.md)
  checks it is present and numeric.

  An `anievent` has no index, since a bout is delimited by `start` and
  `stop`. Its `variables_index` is `NA` and
  [`get_index()`](https://animovement.dev/anicore/reference/get_index.md)
  errors on it.

- [`get_sampling_interval()`](https://animovement.dev/anicore/reference/get_sampling_interval.md)
  reports the spacing of the index, derived from the data at
  construction rather than declared, and
  [`is_sampling_regular()`](https://animovement.dev/anicore/reference/is_sampling_regular.md)
  says whether that spacing is even
  ([\#114](https://github.com/animovement/anicore/issues/114)). Nothing
  in the stack could previously tell whether a frame was regularly
  sampled, which several downstream functions need — interpolating on
  row position rather than on time is only correct when it is.

  Measured per key, since the index restarts in each group and a frame
  regular within every track can look irregular pooled. Regularity is
  computed on demand rather than stored, because dropping rows changes
  the answer; its `tolerance` is an argument, and relative, so
  floating-point timestamps are not called irregular over the last
  decimal place.

  [`validate_aniframe()`](https://animovement.dev/anicore/reference/validate_aniframe.md)
  warns when a declared `sampling_rate` disagrees with the measured
  spacing — the same shape as
  [\#98](https://github.com/animovement/anicore/issues/98), where the
  metadata claimed a unit the data was not in.

- [`get_sampling_rate()`](https://animovement.dev/anicore/reference/get_sampling_rate.md),
  [`get_unit_space()`](https://animovement.dev/anicore/reference/get_unit_space.md),
  [`get_unit_time()`](https://animovement.dev/anicore/reference/get_unit_time.md)
  and
  [`get_unit_angle()`](https://animovement.dev/anicore/reference/get_unit_angle.md)
  read the fields that already had setters
  ([\#121](https://github.com/animovement/anicore/issues/121)). Every
  field with a dedicated setter now has a dedicated getter, so reading
  one no longer means naming it as a string. The factor-backed ones
  return a bare character vector, which is what callers were doing with
  [`as.character()`](https://rdrr.io/r/base/character.html) anyway.

- [`get_coordinate_system()`](https://animovement.dev/anicore/reference/get_coordinate_system.md)
  reads the coordinate system a frame is in
  ([\#109](https://github.com/animovement/anicore/issues/109)). There is
  deliberately no setter: the field is derived from the axis roles, so
  [`set_axes()`](https://animovement.dev/anicore/reference/set_axes.md)
  says what the columns mean and `anispace`’s `map_to_*()` functions
  convert the coordinates.

- An `axes` metadata field records which column carries which axis role,
  so coordinates may be carried by columns of any name
  ([\#109](https://github.com/animovement/anicore/issues/109)).
  [`get_axes()`](https://animovement.dev/anicore/reference/get_axes.md)
  reads it,
  [`set_axes()`](https://animovement.dev/anicore/reference/set_axes.md)
  changes it, and
  [`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
  and
  [`set_variables_where()`](https://animovement.dev/anicore/reference/variables.md)
  accept the same mapping — `c(x = "u", y = "v")`. `coordinate_system`
  follows from it, and
  [`set_unit_space()`](https://animovement.dev/anicore/reference/set_unit_space.md)
  and the axis extents resolve through it. The roles are a closed set:
  `x`, `y`, `z`, `rho`, `phi`, `theta`, and one that forms no coordinate
  system is rejected by name at declaration. Declaring spatial columns
  without roles keeps its old meaning, the column name being the role.

- Axis directions and extents record how a frame is laid out, replacing
  `origin` and `y_height`
  ([\#124](https://github.com/animovement/anicore/issues/124)).
  `axis_directions` maps each axis role to one of `right`, `left`, `up`,
  `down`, `back` or `forward`, read from where the recording was made;
  `axis_extents` maps each to how far it runs.
  [`get_axis_directions()`](https://animovement.dev/anicore/reference/get_axis_directions.md),
  [`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md),
  [`get_axis_extents()`](https://animovement.dev/anicore/reference/get_axis_extents.md)
  and
  [`set_axis_extents()`](https://animovement.dev/anicore/reference/set_axis_extents.md)
  read and write them. Turning an axis to its opposite reflects that
  column: around the axis extent where one is declared, around zero
  where none is. The column is found by role, so a frame whose vertical
  axis is called something else is handled, and an angular frame refuses
  rather than leaving every stored angle facing the wrong way.

- [`get_angle_direction()`](https://animovement.dev/anicore/reference/get_angle_direction.md)
  says which way angles run, derived from the axis directions rather
  than recorded
  ([\#124](https://github.com/animovement/anicore/issues/124)).
  `atan2(y, x)` counts counter-clockwise, so the same physical heading
  comes out mirrored between a y-down and a y-up frame; nothing said
  which convention a number was in.

- [`get_handedness()`](https://animovement.dev/anicore/reference/get_handedness.md)
  and
  [`set_handedness()`](https://animovement.dev/anicore/reference/set_handedness.md)
  say whether a frame is right- or left-handed
  ([\#124](https://github.com/animovement/anicore/issues/124)). Three
  declared axis directions determine it and are read in preference to
  the `handedness` field, which carries the convention for a frame that
  states one without spelling the axes out.
  [`set_handedness()`](https://animovement.dev/anicore/reference/set_handedness.md)
  defaults to right-handed and completes the third axis when two are
  declared.

- [`set_angle_direction()`](https://animovement.dev/anicore/reference/set_angle_direction.md)
  asks for a sense of rotation and declares the axis directions that
  give it ([\#124](https://github.com/animovement/anicore/issues/124)),
  turning the vertical axis over when both are already declared.

- [`wrap_angle()`](https://animovement.dev/anicore/reference/wrap_angle.md)
  and
  [`unwrap_angle()`](https://animovement.dev/anicore/reference/unwrap_angle.md)
  move here from `anispace`
  ([\#128](https://github.com/animovement/anicore/issues/128)). They are
  angle arithmetic rather than coordinate transformation, and belong
  beside
  [`deg_to_rad()`](https://animovement.dev/anicore/reference/deg_to_rad.md)
  and
  [`rad_to_deg()`](https://animovement.dev/anicore/reference/rad_to_deg.md),
  which were already here. `animetric` re-exported both from `anispace`,
  which is the sign of a primitive sitting one layer above the packages
  that need it.

- Turning an axis over on a frame that stores angles recomputes them,
  rather than refusing
  ([\#134](https://github.com/animovement/anicore/issues/134)). No
  column carries `x`, `y` or `z` on a polar, cylindrical or spherical
  frame, but the angles are measured from those axes: turning `x` over
  takes the supplement of `phi`, turning `y` over negates it, and
  turning `z` over takes the supplement of `theta`. The result comes
  back in the unit and range the frame keeps its angles in, and `rho`
  never moves. An axis with a declared extent still refuses, because
  reflecting around it would move every point’s distance from the
  origin.

- Every exported function now has a runnable example
  ([\#106](https://github.com/animovement/anicore/issues/106)).

### Changed

- Declaring an axis role that is carried by one column while a
  different, undeclared column has that role’s name now warns
  ([\#119](https://github.com/animovement/anicore/issues/119)). The
  frame is legal and the mapping is right, but `.data$x` then returns a
  column that is not the x axis. Silence it with
  `options(aniframe.quiet = TRUE)`.

- `coordinate_system` follows from which axis roles are declared rather
  than from column names
  ([\#109](https://github.com/animovement/anicore/issues/109)). A frame
  whose coordinates are named something else is now inferred correctly,
  where it degraded to `unknown` and was refused by every spatial
  function.

- [`is_cartesian()`](https://animovement.dev/anicore/reference/is_cartesian.md),
  [`is_polar()`](https://animovement.dev/anicore/reference/is_polar.md),
  [`is_cylindrical()`](https://animovement.dev/anicore/reference/is_cylindrical.md),
  [`is_spherical()`](https://animovement.dev/anicore/reference/is_spherical.md),
  the `is_cartesian_*d()` variants and their `ensure_` guards read
  `coordinate_system` rather than matching column names, and require an
  aniframe ([\#107](https://github.com/animovement/anicore/issues/107),
  [\#109](https://github.com/animovement/anicore/issues/109)). A frame
  whose coordinates are called something else now satisfies the
  predicate for the system it is in, and an undeclared column no longer
  decides the answer — a spherical frame that has dropped `rho` from its
  declaration is no longer reported as spherical. The guards say which
  system the frame is in and how to get to the one you need.

- [`add_variables_where()`](https://animovement.dev/anicore/reference/variables.md)
  and
  [`remove_variables_where()`](https://animovement.dev/anicore/reference/variables.md)
  carry the axis roles through
  ([\#109](https://github.com/animovement/anicore/issues/109)). They
  combined bare column names, so on a frame with declared roles every
  addition or removal reduced it to `unknown`. Removing an axis until
  the remainder forms no coordinate system warns rather than aborting;
  declaring such a set outright still aborts.

- [`validate_aniframe()`](https://animovement.dev/anicore/reference/validate_aniframe.md)
  warns when identity, temporal context and the index together do not
  name one observation per row
  ([\#49](https://github.com/animovement/anicore/issues/49)). A repeat
  means some variable that tells the rows apart is undeclared, and every
  grouped operation folds them together.

- `variables_when` no longer names the column the frame is indexed by;
  read that from `variables_index`, via
  [`get_index()`](https://animovement.dev/anicore/reference/get_index.md)
  ([\#109](https://github.com/animovement/anicore/issues/109)). It holds
  only the temporal context, so a frame with none has
  [`character()`](https://rdrr.io/r/base/character.html) where it had
  `c("time")`, as does
  [`list_default_metadata()`](https://animovement.dev/anicore/reference/list_default_metadata.md).
  `aniprocess::filter_across()` and `filter_na_across()` take
  `variables_when[1]` as the time column and must swap to
  `aniframe::get_index()`. Code reading the grouping columns is
  unaffected, and no longer has anything to exclude: they are
  `c(variables_what, variables_when)`.

- `default_metadata()` is renamed
  [`list_default_metadata()`](https://animovement.dev/anicore/reference/list_default_metadata.md)
  ([\#121](https://github.com/animovement/anicore/issues/121)).

- `origin` and `y_height` are removed, along with `set_origin()` and
  `set_y_height()`
  ([\#124](https://github.com/animovement/anicore/issues/124)). Use
  [`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md)
  and
  [`set_axis_extents()`](https://animovement.dev/anicore/reference/set_axis_extents.md).
  `origin` recorded a corner, but the origin is `(0, 0)` in both of its
  values — what differed was the direction y increases in, which is what
  is recorded now. It also had nothing to say for 3D data or for a
  recording with no frame corners at all. The deprecated
  `point_of_reference` alias goes with the field it aliased.

- [`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
  no longer fills in an axis extent from the data
  ([\#124](https://github.com/animovement/anicore/issues/124)).
  `y_height` fell back to `max(y)`, which is the highest tracked point
  rather than the frame height, so a frame that was never told its
  height reflected around the wrong place. A frame now declares no
  extent until given one, and an axis with no extent is negated when
  turned over rather than reflected around a guess. `aniread`’s readers
  supply the video height they know about.

- `spec_version` moves to `aniframe = "2.0.0"` and `anievent = "0.3.0"`
  ([\#109](https://github.com/animovement/anicore/issues/109)). Major
  for `aniframe`: `variables_when` no longer names the index, which
  breaks a consumer reading it from there. Minor for `anievent`, which
  gains `variables_index` as `NA`.

### Fixed

- [`set_unit_space()`](https://animovement.dev/anicore/reference/set_unit_space.md)
  converts the axis extents along with the coordinates
  ([\#124](https://github.com/animovement/anicore/issues/124)). An
  extent is a length, so converting cm to m left the frame claiming a
  height in the unit it no longer used.

- [`set_unit_space()`](https://animovement.dev/anicore/reference/set_unit_space.md)
  converts the length axes of the frame’s coordinate system rather than
  whichever of `x`, `y` and `z` are present
  ([\#98](https://github.com/animovement/anicore/issues/98)). `rho` is a
  length on polar, cylindrical and spherical frames and was never
  converted, while the metadata was updated to claim the new unit.
  Angular axes remain
  [`set_unit_angle()`](https://animovement.dev/anicore/reference/set_unit_angle.md)’s
  to convert. Where the coordinate system is `unknown` a length cannot
  be told from an angle, and the function now warns rather than silently
  converting nothing.

- [`set_unit_space()`](https://animovement.dev/anicore/reference/set_unit_space.md),
  [`set_unit_angle()`](https://animovement.dev/anicore/reference/set_unit_angle.md),
  [`set_unit_time()`](https://animovement.dev/anicore/reference/set_unit_time.md)
  and
  [`set_sampling_rate()`](https://animovement.dev/anicore/reference/set_sampling_rate.md)
  no longer re-inject a `keypoint` column and overwrite `variables_what`
  with it ([\#96](https://github.com/animovement/anicore/issues/96)). A
  frame given a custom identity such as `id` was silently regrouped on a
  constant column.

- [`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
  keeps the roles a frame already declares rather than re-deriving them,
  so casting an aniframe is no longer destructive
  ([\#96](https://github.com/animovement/anicore/issues/96)). A
  declaration whose columns have since been dropped still falls through
  to detection, so a cast continues to repair a drifted frame.
