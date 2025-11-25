# Set metadata for an aniframe

Sets or updates metadata for an aniframe object. Metadata can be
provided either as named arguments or as a list. If the aniframe already
has metadata, the new values will be merged with existing values, with
new values taking precedence.

Default metadata fields include:

- `source`: Data source identifier

- `source_version`: Version of the data source

- `filename`: Original filename

- `sampling_rate`: Sampling rate in Hz

- `start_datetime`: Start date and time of recording

- `reference_frame`: Reference frame (default: "allocentric")

- `coordinate_system`: Coordinate system (default: "cartesian")

- `point_of_reference`: Point of reference (default: "bottom_left")

## Usage

``` r
set_metadata(data, ..., metadata = NULL)
```

## Arguments

- data:

  An aniframe object

- ...:

  Named metadata values (e.g., `sampling_rate = 30, source = "sleap"`)

- metadata:

  Alternatively, a named list of metadata. Cannot be used simultaneously
  with `...`

## Value

The aniframe object with updated metadata

## See also

[`get_metadata()`](http://animovement.dev/aniframe/reference/get_metadata.md),
[`default_metadata()`](http://animovement.dev/aniframe/reference/default_metadata.md)

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
