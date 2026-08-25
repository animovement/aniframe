# Ensure object is an aniframe

Ensure object is an aniframe

## Usage

``` r
ensure_is_aniframe(x)
```

## Arguments

- x:

  An object to test

## Value

Error if not an aniframe

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
# Passes silently, and errors otherwise
ensure_is_aniframe(af)

try(ensure_is_aniframe(data.frame(x = 1)))
#> Error in ensure_is_aniframe(data.frame(x = 1)) : 
#>   Data is not an aniframe.
```
