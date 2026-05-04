# Default metadata structure

Returns a list containing the default metadata fields and their initial
values for an aniframe object. Most fields are initialized as `NA` and
should be set appropriately for your data.

## Usage

``` r
default_metadata()
```

## Value

A named list with the following fields:

- `source`: Data source identifier (character, NA)

- `source_version`: Version of the data source (character, NA)

- `filename`: Original filename(s) (character vector, NA). Accepts a
  vector of length 1 or more — readers that load from multiple files
  (e.g. `aniread::read_trackball()`) populate this with all source
  paths.

- `sampling_rate`: Sampling rate in Hz (numeric, NA)

- `start_datetime`: Start date and time of recording (POSIXct, NA)

- `reference_frame`: Reference frame (factor, "allocentric")

- `coordinate_system`: Coordinate system (factor, "cartesian")

- `origin`: Location of the (0,0) coordinate relative to the recording
  frame (factor, "bottom_left"). Permitted values: "bottom_left",
  "top_left".

- `y_height`: Height of the recording frame in y-axis units (numeric,
  NA). Used by
  [`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)
  to reflect y coordinates when switching origin conventions.

- `connections`: Named list of connection tables, one per identity or
  temporal variable (typically `keypoint` for skeletons; could also be
  `individual` for social networks). Each entry is a 2-column tibble of
  `from`/`to` pairs. Default is an empty list. Manage via
  [`set_connections()`](http://animovement.dev/aniframe/reference/set_connections.md),
  [`get_connections()`](http://animovement.dev/aniframe/reference/get_connections.md),
  [`add_connections()`](http://animovement.dev/aniframe/reference/add_connections.md)
  and
  [`remove_connections()`](http://animovement.dev/aniframe/reference/remove_connections.md).

## See also

[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md),
[`get_metadata()`](http://animovement.dev/aniframe/reference/get_metadata.md)
