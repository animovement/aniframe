# Internal guard for 1‑D Cartesian checks

Internal guard for 1‑D Cartesian checks

## Usage

``` r
ensure_is_cartesian_1d(data)
```

## Arguments

- data:

  An aniframe.

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
try(ensure_is_cartesian_1d(af))
#> Error in ensure_is_cartesian_1d(af) : 
#>   This data frame is not in a 1D Cartesian coordinate system. Requires
#> only 'x', 'y' or 'z'.
```
