# Set the y-axis frame height

Sets or updates the `y_height` metadata field, which records the height
of the recording frame in y-axis units. Used by
[`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)
when reflecting y coordinates between origin conventions (e.g.
`bottom_left` \<-\> `top_left`).

Reader functions in `aniread` populate `y_height` automatically from the
source (e.g. video frame height). For aniframes constructed manually,
[`as_aniframe()`](http://animovement.dev/aniframe/reference/as_aniframe.md)
falls back to `max(y)`. Use this function to set the true frame height
when the auto-fallback is not appropriate.

## Usage

``` r
set_y_height(data, y_height)
```

## Arguments

- data:

  An aniframe object.

- y_height:

  A single positive finite numeric value.

## Value

The aniframe with updated `y_height` metadata.

## See also

[`set_origin()`](http://animovement.dev/aniframe/reference/set_origin.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- example_aniframe()
data <- set_y_height(data, y_height = 1080)
} # }
```
