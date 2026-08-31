# Get metadata

Get metadata

## Usage

``` r
get_metadata(data, fields = NULL)
```

## Arguments

- data:

  An aniframe or anievent object.

- fields:

  If only specific metadata fields should be returned. A field the
  object does not carry gives `NULL`; a name that is not a metadata
  field at all is an error.

## Value

The metadata associated with the object.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
names(get_metadata(af))
#>  [1] "source"            "source_version"    "source_format"    
#>  [4] "filename"          "sampling_rate"     "sampling_interval"
#>  [7] "start_datetime"    "variables_index"   "variables_what"   
#> [10] "variables_when"    "variables_where"   "variables_event"  
#> [13] "axes"              "unit_space"        "unit_angle"       
#> [16] "unit_time"         "reference_frame"   "coordinate_system"
#> [19] "axis_directions"   "axis_extents"      "handedness"       
#> [22] "connections"       "spec_version"     

# A single field can be pulled out by name
get_metadata(af, 'sampling_rate')
#> [1] NA
```
