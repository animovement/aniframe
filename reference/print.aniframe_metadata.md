# Print method for aniframe metadata

Renders the metadata as a single block — captured via
[`cli::cli_format_method()`](https://cli.r-lib.org/reference/cli_format_method.html)
and emitted with [`cat()`](https://rdrr.io/r/base/cat.html) — so there's
no leading newline and no blank lines between entries. This makes the
output render cleanly in HTML contexts such as Quarto / R Markdown.

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
