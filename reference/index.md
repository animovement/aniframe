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

## Metadata handling

Functions for reading, setting, and retrieving the metadata attached to
an aniframe.

- [`get_metadata()`](http://animovement.dev/aniframe/reference/get_metadata.md)
  : Get metadata
- [`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
  : Set metadata for an aniframe
- [`set_unit_time()`](http://animovement.dev/aniframe/reference/set_unit_time.md)
  : Set the temporal unit of an aniframe object
- [`set_unit_space()`](http://animovement.dev/aniframe/reference/set_unit_space.md)
  : Set the spatial unit of an aniframe object
- [`set_unit_angle()`](http://animovement.dev/aniframe/reference/set_unit_angle.md)
  : Set the angular unit of an aniframe object
- [`set_sampling_rate()`](http://animovement.dev/aniframe/reference/set_sampling_rate.md)
  : Set the sampling rate of an aniframe object
- [`get_trackball_calibration_factor()`](http://animovement.dev/aniframe/reference/get_trackball_calibration_factor.md)
  : Calculate trackball calibration factor
- [`default_metadata()`](http://animovement.dev/aniframe/reference/default_metadata.md)
  : Default metadata structure

## Transformations

These functions allow you to make tranformations to your coordinate
system, such as translations, rotations or conversion to polar
coordinates.

- [`transform_to_egocentric()`](http://animovement.dev/aniframe/reference/transform_to_egocentric.md)
  : Transform coordinates to egocentric reference frame
- [`translate_coords()`](http://animovement.dev/aniframe/reference/translate_coords.md)
  : Translate coordinates (Cartesian)
- [`rotate_coords()`](http://animovement.dev/aniframe/reference/rotate_coords.md)
  : Rotate coordinates in Cartesian space (2D or 3D)
- [`map_to_cartesian()`](http://animovement.dev/aniframe/reference/map_to_cartesian.md)
  : Map from polar to Cartesian coordinates
- [`map_to_polar()`](http://animovement.dev/aniframe/reference/map_to_polar.md)
  : Map from Cartesian to polar coordinates
- [`map_to_cylindrical()`](http://animovement.dev/aniframe/reference/map_to_cylindrical.md)
  : Map from Cartesian to cylindrical coordinates
- [`map_to_spherical()`](http://animovement.dev/aniframe/reference/map_to_spherical.md)
  : Map from Cartesian to spherical coordinates
- [`cartesian_to_rho()`](http://animovement.dev/aniframe/reference/cartesian_to_rho.md)
  : Cartesian radius (ρ) from coordinates
- [`cartesian_to_phi()`](http://animovement.dev/aniframe/reference/cartesian_to_phi.md)
  : Cartesian azimuth (φ) from coordinates
- [`cartesian_to_theta()`](http://animovement.dev/aniframe/reference/cartesian_to_theta.md)
  : Polar angle (θ) from Cartesian coordinates
- [`polar_to_x()`](http://animovement.dev/aniframe/reference/polar_to_x.md)
  : Convert polar radius to Cartesian x‑coordinate
- [`polar_to_y()`](http://animovement.dev/aniframe/reference/polar_to_y.md)
  : Convert polar radius to Cartesian y‑coordinate
- [`spherical_to_z()`](http://animovement.dev/aniframe/reference/spherical_to_z.md)
  : Convert cylindrical radius and polar angle to Cartesian z‑coordinate
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

## Helpers

- [`rad_to_deg()`](http://animovement.dev/aniframe/reference/rad_to_deg.md)
  : Convert radians to degrees
- [`deg_to_rad()`](http://animovement.dev/aniframe/reference/deg_to_rad.md)
  : Convert degrees to radians
- [`calculate_angular_difference()`](http://animovement.dev/aniframe/reference/calculate_angular_difference.md)
  : Calculate angular difference
- [`wrap_angle()`](http://animovement.dev/aniframe/reference/wrap_angle.md)
  : Constrain angles to \[0, 2π)
- [`unwrap_angle()`](http://animovement.dev/aniframe/reference/unwrap_angle.md)
  : Remove constrain for angles to keep within \[0, 2π)
- [`diff_angle()`](http://animovement.dev/aniframe/reference/diff_angle.md)
  : Difference of angular values

## Data manipulation (dplyr‑style verbs)

S3 methods that mirror dplyr verbs for aniframe objects.

- [`arrange(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/arrange.aniframe.md)
  : Arrange rows of an aniframe
- [`filter(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/filter.aniframe.md)
  : Filter rows of an aniframe
- [`group_by(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/group_by.aniframe.md)
  : Group an aniframe
- [`mutate(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/mutate.aniframe.md)
  : Mutate columns in an aniframe
- [`relocate(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/relocate.aniframe.md)
  : Relocate columns in an aniframe
- [`rename(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/rename.aniframe.md)
  : Rename columns in an aniframe
- [`select(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/select.aniframe.md)
  : Select columns from an aniframe
- [`slice(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/slice.aniframe.md)
  : Slice rows from an aniframe
- [`tbl_sum(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/tbl_sum.aniframe.md)
  : Custom tibble summary for aniframe
- [`ungroup(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/ungroup.aniframe.md)
  : Ungroup an aniframe

## Conversion helpers

Methods that let you treat aniframe objects like regular data frames.

- [`as.data.frame(`*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/as.data.frame.aniframe.md)
  : Convert aniframe to regular data frame
- [`` `[`( ``*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/sub-.aniframe.md)
  : Subset aniframe with \[
- [`` `[[`( ``*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/sub-sub-.aniframe.md)
  : Extract single column from aniframe with \[\[
- [`` `[[<-`( ``*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/sub-subset-.aniframe.md)
  : Column assignment for aniframe with \[\[\<-
- [`` `[<-`( ``*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/subset-.aniframe.md)
  : Subset assignment for aniframe with \[\<-
- [`` `$<-`( ``*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/cash-set-.aniframe.md)
  : Column assignment for aniframe with \$\<-
- [`` `$`( ``*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/cash-.aniframe.md)
  : Extract column from aniframe with \$
- [`` `names<-`( ``*`<aniframe>`*`)`](http://animovement.dev/aniframe/reference/names-set-.aniframe.md)
  : Rename columns with names\<-
