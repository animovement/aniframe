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
#>  [1] "source"            "source_version"    "filename"         
#>  [4] "sampling_rate"     "sampling_interval" "start_datetime"   
#>  [7] "variables_index"   "variables_what"    "variables_when"   
#> [10] "variables_where"   "variables_event"   "axes"             
#> [13] "unit_space"        "unit_angle"        "unit_time"        
#> [16] "reference_frame"   "coordinate_system" "axis_directions"  
#> [19] "axis_extents"      "handedness"        "connections"      
#> [22] "spec_version"     

# A single field can be pulled out by name
get_metadata(af, 'sampling_rate')
#> [1] NA
```
