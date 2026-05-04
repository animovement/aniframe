# Set the coordinate origin

Sets or updates the `origin` metadata field, which records where the
(0,0) coordinate sits relative to the recording frame. When the new
origin differs from the current one, the y coordinates are reflected
around `y_height` so the data is expressed in the new convention.

## Usage

``` r
set_origin(data, origin)
```

## Arguments

- data:

  An aniframe object.

- origin:

  Character. One of `"bottom_left"` or `"top_left"`.

## Value

The aniframe with reflected y coordinates (when the origin changed) and
updated `origin` metadata.

## Details

The flip uses the formula `y_new = y_height - y_old`. The `y_height`
metadata field must therefore be set when the origin actually changes;
if it is `NA`, this function errors and asks the user to set it via
[`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md).
When the supplied `origin` matches the current value, the data is
returned unchanged.

## See also

[`set_y_height()`](http://animovement.dev/aniframe/reference/set_y_height.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- example_aniframe()
data <- set_y_height(data, y_height = 1080)
data <- set_origin(data, origin = "top_left")
} # }
```
