# Validate an aniframe

Re-checks, on demand, that an `aniframe`'s metadata still describes the
frame it is attached to. The two drift apart silently under ordinary
dplyr work:
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
drops a column without touching the metadata that names it, and
assignment can change a column's type. The invariants are therefore
checked rather than assumed:

## Usage

``` r
validate_aniframe(data)
```

## Arguments

- data:

  An aniframe object.

## Value

The input `data`, invisibly.

## Details

- every column named in `variables_what`, `variables_when`,
  `variables_where` and `variables_event` is present in the data — hard
  error;

- `time` is present and numeric — hard error;

- every column named in `variables_where` is numeric — hard error;

- `coordinate_system` agrees with `variables_where` — **warning** only.
  The frame is still usable, and the field is derived rather than
  declared, so it can be refreshed.

## See also

[`ensure_is_spatial()`](http://animovement.dev/aniframe/reference/ensure_is_spatial.md)
for the spatial subset of these checks, which is the part downstream
filters need on every call;
[`validate_anievent()`](http://animovement.dev/aniframe/reference/validate_anievent.md)
for the `anievent` equivalent.

## Examples

``` r
af <- aniframe(time = 1:5, x = 1:5, y = 1:5)
validate_aniframe(af)
```
