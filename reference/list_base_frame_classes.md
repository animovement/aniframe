# Classes owned by dplyr, tibble and base R

The tail of the class vector that belongs to dplyr rather than to
animovement. [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)
returns these already set correctly, so they are never restored from the
input — doing so would, for instance, re-group the result of an
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Usage

``` r
list_base_frame_classes()
```

## Value

Character vector of class names.
