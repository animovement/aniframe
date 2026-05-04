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
- [`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)
  : Set the coordinate origin
- [`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md)
  : Set the y-axis frame height
- [`default_metadata()`](http://animovement.dev/aniframe/reference/default_metadata.md)
  : Default metadata structure

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
