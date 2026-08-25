# Internal guard for polar checks

Internal guard for polar checks

## Usage

``` r
ensure_is_polar(data)
```

## Arguments

- data:

  An aniframe.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
# Passes silently when the coordinate system matches
try(ensure_is_polar(af))
#> Error in ensure_is_polar(af) : 
#>   This data frame is not in a polar coordinate system.
```
