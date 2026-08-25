# Internal guard for 3‑D Cartesian checks

Internal guard for 3‑D Cartesian checks

## Usage

``` r
ensure_is_cartesian_3d(data)
```

## Arguments

- data:

  An aniframe.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
try(ensure_is_cartesian_3d(af))
#> Error in ensure_is_cartesian_3d(af) : 
#>   This data frame is not in a 3D Cartesian coordinate system. Requires
#> 'x', 'y' and 'z' columns with non-NA values.
```
