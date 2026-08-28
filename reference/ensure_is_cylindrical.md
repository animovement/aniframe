# Internal guard for cylindrical checks

Internal guard for cylindrical checks

## Usage

``` r
ensure_is_cylindrical(data)
```

## Arguments

- data:

  An aniframe.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
# Passes silently when the coordinate system matches
try(ensure_is_cylindrical(af))
#> Error in ensure_coordinate_system(data, "cylindrical", "cylindrical") : 
#>   This aniframe is not in a cylindrical coordinate system.
#> ℹ coordinate_system is "cartesian_2d".
#> ℹ Convert the coordinates first; anispace has the transformations.
```
