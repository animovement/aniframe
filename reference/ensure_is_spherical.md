# Internal guard for spherical checks

Internal guard for spherical checks

## Usage

``` r
ensure_is_spherical(data)
```

## Arguments

- data:

  An aniframe.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
# Passes silently when the coordinate system matches
try(ensure_is_spherical(af))
#> Error in ensure_is_spherical(af) : 
#>   This data frame is not in a spherical coordinate system.
```
