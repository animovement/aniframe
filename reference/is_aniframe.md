# Check if object is an aniframe

Check if object is an aniframe

## Usage

``` r
is_aniframe(x)
```

## Arguments

- x:

  An object to test

## Value

Logical: TRUE if x inherits from aniframe

## Examples

``` r
af <- example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 1)
is_aniframe(af)
#> [1] TRUE

# A plain data frame is not one
is_aniframe(data.frame(x = 1))
#> [1] FALSE
```
