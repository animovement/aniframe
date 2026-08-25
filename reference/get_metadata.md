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

  If only specific metadata fields should be returned.

## Value

The metadata associated with the object.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
names(get_metadata(af))
#>  [1] "source"            "source_version"    "filename"         
#>  [4] "sampling_rate"     "start_datetime"    "variables_what"   
#>  [7] "variables_when"    "variables_where"   "variables_event"  
#> [10] "unit_space"        "unit_angle"        "unit_time"        
#> [13] "reference_frame"   "coordinate_system" "origin"           
#> [16] "y_height"          "connections"       "spec_version"     

# A single field can be pulled out by name
get_metadata(af, 'sampling_rate')
#> [1] NA
```
