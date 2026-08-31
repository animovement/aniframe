# Package index

## Creating and converting aniframe objects

Functions that create an aniframe, coerce other objects to aniframe, or
provide example data.

- [`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
  : Convert a data frame to aniframe
- [`aniframe()`](https://animovement.dev/anicore/reference/aniframe.md)
  : Create an aniframe data frame
- [`example_aniframe()`](https://animovement.dev/anicore/reference/example_aniframe.md)
  : Create example aniframe data
- [`is_aniframe()`](https://animovement.dev/anicore/reference/is_aniframe.md)
  : Check if object is an aniframe
- [`ensure_is_aniframe()`](https://animovement.dev/anicore/reference/ensure_is_aniframe.md)
  : Ensure object is an aniframe
- [`validate_aniframe()`](https://animovement.dev/anicore/reference/validate_aniframe.md)
  : Validate an aniframe

## Creating and converting anievent objects

Functions that create an anievent (long-format behavioural event
records) or coerce other objects to anievent.

- [`to_anievent()`](https://animovement.dev/anicore/reference/to_anievent.md)
  : Encode per-frame data into an anievent
- [`as_anievent()`](https://animovement.dev/anicore/reference/as_anievent.md)
  : Cast a data frame to an anievent
- [`anievent()`](https://animovement.dev/anicore/reference/anievent.md)
  : Create an anievent data frame
- [`is_anievent()`](https://animovement.dev/anicore/reference/is_anievent.md)
  : Check if object is an anievent
- [`ensure_is_anievent()`](https://animovement.dev/anicore/reference/ensure_is_anievent.md)
  : Ensure object is an anievent
- [`validate_anievent()`](https://animovement.dev/anicore/reference/validate_anievent.md)
  : Validate an anievent

## Metadata handling

Functions for reading, setting, and retrieving the metadata attached to
aniframe or anievent objects.

- [`get_metadata()`](https://animovement.dev/anicore/reference/get_metadata.md)
  : Get metadata
- [`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md)
  : Set metadata
- [`get_unit_time()`](https://animovement.dev/anicore/reference/get_unit_time.md)
  : The unit the index or bout boundaries are in
- [`set_unit_time()`](https://animovement.dev/anicore/reference/set_unit_time.md)
  : Set the temporal unit of an aniframe or anievent
- [`get_unit_space()`](https://animovement.dev/anicore/reference/get_unit_space.md)
  : The unit the spatial coordinates are in
- [`set_unit_space()`](https://animovement.dev/anicore/reference/set_unit_space.md)
  : Set the spatial unit of an aniframe object
- [`get_unit_angle()`](https://animovement.dev/anicore/reference/get_unit_angle.md)
  : The unit the angular axes are in
- [`set_unit_angle()`](https://animovement.dev/anicore/reference/set_unit_angle.md)
  : Set the angular unit of an aniframe object
- [`get_sampling_rate()`](https://animovement.dev/anicore/reference/get_sampling_rate.md)
  : The sampling rate, in Hz
- [`set_sampling_rate()`](https://animovement.dev/anicore/reference/set_sampling_rate.md)
  : Set the sampling rate of an aniframe or anievent
- [`get_sampling_interval()`](https://animovement.dev/anicore/reference/get_sampling_interval.md)
  : The interval between consecutive observations
- [`is_sampling_regular()`](https://animovement.dev/anicore/reference/is_sampling_regular.md)
  : Is the frame regularly sampled?
- [`get_axis_directions()`](https://animovement.dev/anicore/reference/get_axis_directions.md)
  : Get the direction each axis points
- [`set_axis_directions()`](https://animovement.dev/anicore/reference/set_axis_directions.md)
  : Say which way an axis points
- [`get_axis_extents()`](https://animovement.dev/anicore/reference/get_axis_extents.md)
  : Get how far each axis runs
- [`set_axis_extents()`](https://animovement.dev/anicore/reference/set_axis_extents.md)
  : Say how far each axis runs
- [`get_angle_direction()`](https://animovement.dev/anicore/reference/get_angle_direction.md)
  : Which way angles run
- [`set_angle_direction()`](https://animovement.dev/anicore/reference/set_angle_direction.md)
  : Say which way angles run
- [`get_handedness()`](https://animovement.dev/anicore/reference/get_handedness.md)
  : Whether the frame is right- or left-handed
- [`set_handedness()`](https://animovement.dev/anicore/reference/set_handedness.md)
  : Say whether the frame is right- or left-handed
- [`list_default_metadata()`](https://animovement.dev/anicore/reference/list_default_metadata.md)
  : Default metadata structure

## Declaring variables

Declare which columns carry identity, time and position. These are the
frame’s structure rather than a description of it, so the setters
restructure the frame to match — coercing column types, relocating,
reordering, regrouping, and refreshing derived fields.
[`set_metadata()`](https://animovement.dev/anicore/reference/set_metadata.md)
refuses these fields for that reason.

- [`get_variables_what()`](https://animovement.dev/anicore/reference/variables.md)
  [`get_variables_when()`](https://animovement.dev/anicore/reference/variables.md)
  [`get_variables_where()`](https://animovement.dev/anicore/reference/variables.md)
  [`set_variables_what()`](https://animovement.dev/anicore/reference/variables.md)
  [`set_variables_when()`](https://animovement.dev/anicore/reference/variables.md)
  [`set_variables_where()`](https://animovement.dev/anicore/reference/variables.md)
  [`add_variables_what()`](https://animovement.dev/anicore/reference/variables.md)
  [`add_variables_when()`](https://animovement.dev/anicore/reference/variables.md)
  [`add_variables_where()`](https://animovement.dev/anicore/reference/variables.md)
  [`remove_variables_what()`](https://animovement.dev/anicore/reference/variables.md)
  [`remove_variables_when()`](https://animovement.dev/anicore/reference/variables.md)
  [`remove_variables_where()`](https://animovement.dev/anicore/reference/variables.md)
  : Declare which columns carry identity, time and position
- [`get_variables_event()`](https://animovement.dev/anicore/reference/variables_event.md)
  [`set_variables_event()`](https://animovement.dev/anicore/reference/variables_event.md)
  [`add_variables_event()`](https://animovement.dev/anicore/reference/variables_event.md)
  [`remove_variables_event()`](https://animovement.dev/anicore/reference/variables_event.md)
  : Declare which columns carry per-frame event labels
- [`get_index()`](https://animovement.dev/anicore/reference/get_index.md)
  : The column an aniframe is indexed by
- [`set_index()`](https://animovement.dev/anicore/reference/set_index.md)
  : Declare which column an aniframe is indexed by
- [`get_axes()`](https://animovement.dev/anicore/reference/get_axes.md)
  : The axis roles of an aniframe, and the columns carrying them
- [`set_axes()`](https://animovement.dev/anicore/reference/set_axes.md)
  : Declare which column carries which axis role
- [`get_coordinate_system()`](https://animovement.dev/anicore/reference/get_coordinate_system.md)
  : The coordinate system an aniframe is in

## Connections

Manage connections between identity or temporal variables (e.g. skeleton
edges between keypoints, or social-network edges between individuals).
Stored as a named list of `from`/`to` tibbles in `metadata$connections`.

- [`set_connections()`](https://animovement.dev/anicore/reference/set_connections.md)
  : Set the connections for a variable
- [`get_connections()`](https://animovement.dev/anicore/reference/get_connections.md)
  : Get connections from an aniframe
- [`add_connections()`](https://animovement.dev/anicore/reference/add_connections.md)
  : Add connections to an aniframe
- [`remove_connections()`](https://animovement.dev/anicore/reference/remove_connections.md)
  : Remove connections from an aniframe

## Spatial checks

These functions provide checks for your coordinate system.
[`is_spatial()`](https://animovement.dev/anicore/reference/is_spatial.md)
and
[`ensure_is_spatial()`](https://animovement.dev/anicore/reference/ensure_is_spatial.md)
check that the columns named in `variables_where` are present and
numeric; the `is_cartesian*()` family reports which coordinate system
the frame is in, which follows from the axis roles it declares.

- [`is_spatial()`](https://animovement.dev/anicore/reference/is_spatial.md)
  : Test whether the spatial columns match the metadata
- [`ensure_is_spatial()`](https://animovement.dev/anicore/reference/ensure_is_spatial.md)
  : Ensure the spatial columns match the metadata
- [`is_cartesian()`](https://animovement.dev/anicore/reference/is_cartesian.md)
  : Test whether an aniframe uses a Cartesian coordinate system
- [`is_cartesian_1d()`](https://animovement.dev/anicore/reference/is_cartesian_1d.md)
  : Test for a 1-D Cartesian coordinate system
- [`is_cartesian_2d()`](https://animovement.dev/anicore/reference/is_cartesian_2d.md)
  : Test for a 2-D Cartesian coordinate system
- [`is_cartesian_3d()`](https://animovement.dev/anicore/reference/is_cartesian_3d.md)
  : Test for a 3-D Cartesian coordinate system
- [`is_polar()`](https://animovement.dev/anicore/reference/is_polar.md)
  : Test whether an aniframe uses a polar coordinate system
- [`is_cylindrical()`](https://animovement.dev/anicore/reference/is_cylindrical.md)
  : Test whether an aniframe uses a cylindrical coordinate system
- [`is_spherical()`](https://animovement.dev/anicore/reference/is_spherical.md)
  : Test whether an aniframe uses a spherical coordinate system
- [`ensure_is_cartesian()`](https://animovement.dev/anicore/reference/ensure_is_cartesian.md)
  : Internal guard for Cartesian checks
- [`ensure_is_cartesian_1d()`](https://animovement.dev/anicore/reference/ensure_is_cartesian_1d.md)
  : Internal guard for 1-D Cartesian checks
- [`ensure_is_cartesian_2d()`](https://animovement.dev/anicore/reference/ensure_is_cartesian_2d.md)
  : Internal guard for 2-D Cartesian checks
- [`ensure_is_cartesian_3d()`](https://animovement.dev/anicore/reference/ensure_is_cartesian_3d.md)
  : Internal guard for 3-D Cartesian checks
- [`ensure_is_polar()`](https://animovement.dev/anicore/reference/ensure_is_polar.md)
  : Internal guard for polar checks
- [`ensure_is_cylindrical()`](https://animovement.dev/anicore/reference/ensure_is_cylindrical.md)
  : Internal guard for cylindrical checks
- [`ensure_is_spherical()`](https://animovement.dev/anicore/reference/ensure_is_spherical.md)
  : Internal guard for spherical checks

## Angles

Two families, and the difference matters. `*_angle()` and the `x_to_y()`
conversions manipulate how an angle is written. The `circ_*()` functions
compute with the wraparound: an ordinary mean or median of angles gives
the wrong answer, since the mean of 350 and 10 degrees is 0, not 180.

### Handling angles

- [`rad_to_deg()`](https://animovement.dev/anicore/reference/rad_to_deg.md)
  : Convert radians to degrees
- [`deg_to_rad()`](https://animovement.dev/anicore/reference/deg_to_rad.md)
  : Convert degrees to radians
- [`wrap_angle()`](https://animovement.dev/anicore/reference/wrap_angle.md)
  : Constrain angles to a standard range
- [`unwrap_angle()`](https://animovement.dev/anicore/reference/unwrap_angle.md)
  : Remove wrapping from a sequence of angles

### Circular statistics

- [`circ_difference()`](https://animovement.dev/anicore/reference/circ_difference.md)
  : Shortest signed distance between two angles
- [`circ_successive_difference()`](https://animovement.dev/anicore/reference/circ_successive_difference.md)
  : Differences between successive angles in a series
- [`circ_mean()`](https://animovement.dev/anicore/reference/circ_mean.md)
  : Circular mean
- [`circ_median()`](https://animovement.dev/anicore/reference/circ_median.md)
  : Circular median
- [`circ_sd()`](https://animovement.dev/anicore/reference/circ_sd.md) :
  Circular standard deviation
- [`circ_mad()`](https://animovement.dev/anicore/reference/circ_mad.md)
  : Circular median absolute deviation

## Helpers

- [`convert_nan_to_na()`](https://animovement.dev/anicore/reference/convert_nan_to_na.md)
  : Convert NaN to NA in numeric columns
- [`convert_inf_to_na()`](https://animovement.dev/anicore/reference/convert_inf_to_na.md)
  : Convert Inf to NA in numeric columns
