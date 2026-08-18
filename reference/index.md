# Package index

## Creating and converting aniframe objects

Functions that create an aniframe, coerce other objects to aniframe, or
provide example data.

- [`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
  : Convert a data frame to aniframe
- [`aniframe()`](http://animovement.dev/aniframe/reference/aniframe.md)
  : aniframe package
- [`example_aniframe()`](http://animovement.dev/aniframe/reference/example_aniframe.md)
  : Create example aniframe data
- [`is_aniframe()`](http://animovement.dev/aniframe/reference/is_aniframe.md)
  : Check if object is an aniframe
- [`ensure_is_aniframe()`](http://animovement.dev/aniframe/reference/ensure_is_aniframe.md)
  : Ensure object is an aniframe
- [`validate_aniframe()`](http://animovement.dev/aniframe/reference/validate_aniframe.md)
  : Validate an aniframe

## Creating and converting anievent objects

Functions that create an anievent (long-format behavioural event
records) or coerce other objects to anievent.

- [`to_anievent()`](http://animovement.dev/aniframe/reference/to_anievent.md)
  : Encode per-frame data into an anievent
- [`as_anievent()`](http://animovement.dev/aniframe/reference/as_anievent.md)
  : Cast a data frame to an anievent
- [`anievent()`](http://animovement.dev/aniframe/reference/anievent.md)
  : Create an anievent data frame
- [`is_anievent()`](http://animovement.dev/aniframe/reference/is_anievent.md)
  : Check if object is an anievent
- [`ensure_is_anievent()`](http://animovement.dev/aniframe/reference/ensure_is_anievent.md)
  : Ensure object is an anievent
- [`validate_anievent()`](http://animovement.dev/aniframe/reference/validate_anievent.md)
  : Validate an anievent

## Metadata handling

Functions for reading, setting, and retrieving the metadata attached to
aniframe or anievent objects.

- [`get_metadata()`](http://animovement.dev/aniframe/reference/get_metadata.md)
  : Get metadata
- [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  : Set metadata
- [`set_unit_time()`](http://animovement.dev/aniframe/reference/set_unit_time.md)
  : Set the temporal unit of an aniframe or anievent
- [`set_unit_space()`](http://animovement.dev/aniframe/reference/set_unit_space.md)
  : Set the spatial unit of an aniframe object
- [`set_unit_angle()`](http://animovement.dev/aniframe/reference/set_unit_angle.md)
  : Set the angular unit of an aniframe object
- [`set_sampling_rate()`](http://animovement.dev/aniframe/reference/set_sampling_rate.md)
  : Set the sampling rate of an aniframe or anievent
- [`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)
  : Set the coordinate origin
- [`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md)
  : Set the y-axis frame height
- [`default_metadata()`](http://animovement.dev/aniframe/reference/default_metadata.md)
  : Default metadata structure

## Declaring variables

Declare which columns carry identity, time and position. These are the
frame’s structure rather than a description of it, so the setters
restructure the frame to match — coercing column types, relocating,
reordering, regrouping, and refreshing derived fields.
[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
refuses these three fields for that reason.

- [`get_variables_what()`](http://animovement.dev/aniframe/reference/variables.md)
  [`get_variables_when()`](http://animovement.dev/aniframe/reference/variables.md)
  [`get_variables_where()`](http://animovement.dev/aniframe/reference/variables.md)
  [`set_variables_what()`](http://animovement.dev/aniframe/reference/variables.md)
  [`set_variables_when()`](http://animovement.dev/aniframe/reference/variables.md)
  [`set_variables_where()`](http://animovement.dev/aniframe/reference/variables.md)
  [`add_variables_what()`](http://animovement.dev/aniframe/reference/variables.md)
  [`add_variables_when()`](http://animovement.dev/aniframe/reference/variables.md)
  [`add_variables_where()`](http://animovement.dev/aniframe/reference/variables.md)
  [`remove_variables_what()`](http://animovement.dev/aniframe/reference/variables.md)
  [`remove_variables_when()`](http://animovement.dev/aniframe/reference/variables.md)
  [`remove_variables_where()`](http://animovement.dev/aniframe/reference/variables.md)
  : Declare which columns carry identity, time and position

## Connections

Manage connections between identity or temporal variables (e.g. skeleton
edges between keypoints, or social-network edges between individuals).
Stored as a named list of `from`/`to` tibbles in `metadata$connections`.

- [`set_connections()`](http://animovement.dev/aniframe/reference/set_connections.md)
  **\[experimental\]** : Set the connections for a variable
- [`get_connections()`](http://animovement.dev/aniframe/reference/get_connections.md)
  **\[experimental\]** : Get connections from an aniframe
- [`add_connections()`](http://animovement.dev/aniframe/reference/add_connections.md)
  **\[experimental\]** : Add connections to an aniframe
- [`remove_connections()`](http://animovement.dev/aniframe/reference/remove_connections.md)
  **\[experimental\]** : Remove connections from an aniframe

## Spatial checks

These functions provide checks for your coordinate system.
[`is_spatial()`](http://animovement.dev/aniframe/reference/is_spatial.md)
and
[`ensure_is_spatial()`](http://animovement.dev/aniframe/reference/ensure_is_spatial.md)
check the spatial columns against the `variables_where` metadata; the
`is_cartesian*()` family checks for particular column names.

- [`is_spatial()`](http://animovement.dev/aniframe/reference/is_spatial.md)
  : Test whether the spatial columns match the metadata
- [`ensure_is_spatial()`](http://animovement.dev/aniframe/reference/ensure_is_spatial.md)
  : Ensure the spatial columns match the metadata
- [`is_cartesian()`](http://animovement.dev/aniframe/reference/is_cartesian.md)
  : Test whether a data frame uses a Cartesian coordinate system
- [`is_cartesian_1d()`](http://animovement.dev/aniframe/reference/is_cartesian_1d.md)
  : Test for a 1‑D Cartesian coordinate system
- [`is_cartesian_2d()`](http://animovement.dev/aniframe/reference/is_cartesian_2d.md)
  : Test for a 2‑D Cartesian coordinate system
- [`is_cartesian_3d()`](http://animovement.dev/aniframe/reference/is_cartesian_3d.md)
  : Test for a 3‑D Cartesian coordinate system
- [`is_polar()`](http://animovement.dev/aniframe/reference/is_polar.md)
  : Test whether a data frame uses a polar coordinate system
- [`is_cylindrical()`](http://animovement.dev/aniframe/reference/is_cylindrical.md)
  : Test whether a data frame uses a cylindrical coordinate system
- [`is_spherical()`](http://animovement.dev/aniframe/reference/is_spherical.md)
  : Test whether a data frame uses a spherical coordinate system
- [`ensure_is_cartesian()`](http://animovement.dev/aniframe/reference/ensure_is_cartesian.md)
  : Internal guard for Cartesian checks
- [`ensure_is_cartesian_1d()`](http://animovement.dev/aniframe/reference/ensure_is_cartesian_1d.md)
  : Internal guard for 1‑D Cartesian checks
- [`ensure_is_cartesian_2d()`](http://animovement.dev/aniframe/reference/ensure_is_cartesian_2d.md)
  : Internal guard for 2‑D Cartesian checks
- [`ensure_is_cartesian_3d()`](http://animovement.dev/aniframe/reference/ensure_is_cartesian_3d.md)
  : Internal guard for 3‑D Cartesian checks
- [`ensure_is_polar()`](http://animovement.dev/aniframe/reference/ensure_is_polar.md)
  : Internal guard for polar checks
- [`ensure_is_cylindrical()`](http://animovement.dev/aniframe/reference/ensure_is_cylindrical.md)
  : Internal guard for cylindrical checks
- [`ensure_is_spherical()`](http://animovement.dev/aniframe/reference/ensure_is_spherical.md)
  : Internal guard for spherical checks

## Helpers

- [`rad_to_deg()`](http://animovement.dev/aniframe/reference/rad_to_deg.md)
  : Convert radians to degrees
- [`deg_to_rad()`](http://animovement.dev/aniframe/reference/deg_to_rad.md)
  : Convert degrees to radians
- [`convert_nan_to_na()`](http://animovement.dev/aniframe/reference/convert_nan_to_na.md)
  : Convert NaN to NA in numeric columns
