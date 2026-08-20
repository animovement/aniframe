# Set metadata

Sets or updates metadata for an aniframe or anievent object. Metadata
can be provided either as named arguments or as a list. If the object
already has metadata, the new values will be merged with existing
values, with new values taking precedence.

Character values for factor fields will be automatically converted to
factors if they match allowed levels.

Default metadata fields include:

- `source`: Data source identifier

- `source_version`: Version of the data source

- `filename`: Original filename(s) — accepts a character vector (length
  1 or more) for readers that load from multiple files

- `sampling_rate`: Sampling rate in Hz

- `start_datetime`: Start date and time of recording

- `reference_frame`: Reference frame (default: "allocentric")

- `coordinate_system`: Coordinate system (default: "cartesian")

- `origin`: Location of the (0,0) coordinate (default: "bottom_left")

- `y_height`: Height of the recording frame in y-axis units (default:
  NA)

For backwards compatibility, the deprecated field `point_of_reference`
is accepted as an alias for `origin` and emits a deprecation warning.

## Usage

``` r
set_metadata(data, ..., metadata = NULL)
```

## Arguments

- data:

  An aniframe or anievent object.

- ...:

  Named metadata values (e.g., `sampling_rate = 30, source = "sleap"`)

- metadata:

  Alternatively, a named list of metadata. Cannot be used simultaneously
  with `...`

## Value

The object with updated metadata.

## See also

[`get_metadata()`](https://animovement.dev/aniframe/reference/get_metadata.md),
[`default_metadata()`](https://animovement.dev/aniframe/reference/default_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set metadata using named arguments
data <- set_metadata(data, sampling_rate = 30, source = "sleap")

# Set metadata using a list
md <- list(sampling_rate = 30, source = "sleap")
data <- set_metadata(data, metadata = md)
} # }
```
