# Ensure a declared index names exactly one column

Split out from
[`ensure_valid_index()`](https://animovement.dev/anicore/reference/ensure_valid_index.md)
because
[`as_aniframe()`](https://animovement.dev/anicore/reference/as_aniframe.md)
needs it before the column is looked up, and under its own argument
name. Unchecked, a two-column `index` falls through to
[`resolve_index()`](https://animovement.dev/anicore/reference/resolve_index.md),
which reads anything but a single name as "unset" and answers `"time"`.

## Usage

``` r
ensure_index_name(index, arg = "index")
```

## Arguments

- index:

  The proposed index.

- arg:

  Name of the caller's argument, for the message.

## Value

`TRUE`, invisibly.
