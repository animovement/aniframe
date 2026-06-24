# Print method for animovement metadata

Renders the metadata as a single block, captured via
[`cli::cli_format_method()`](https://cli.r-lib.org/reference/cli_format_method.html)
and emitted with [`cat()`](https://rdrr.io/r/base/cat.html). Field names
and types are padded to fixed widths so the values line up in aligned
columns, similar to [`str()`](https://rdrr.io/r/utils/str.html).

## Usage

``` r
# S3 method for class 'aniframe_metadata'
print(x, ...)
```

## Arguments

- x:

  An `aniframe_metadata` list.

- ...:

  Unused.

## Value

`x`, invisibly.

## Details

The S3 class is named `aniframe_metadata` for historical reasons, but
the metadata substrate is shared by both
[`aniframe()`](http://animovement.dev/aniframe/reference/aniframe.md)
and
[`anievent()`](http://animovement.dev/aniframe/reference/anievent.md)
objects.
