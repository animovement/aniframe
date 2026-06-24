# Re-clothe a dispatched result with its animovement class and metadata

After a generic strips a result down to a plain tibble (via
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)), restore the
animovement class (`aniframe` or `anievent`) and re-attach the metadata
captured from the original input.

## Usage

``` r
preserve_animovement_class(x, md, constructor)
```

## Arguments

- x:

  The bare result returned by
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html).

- md:

  Metadata captured before dispatch via
  [`get_metadata()`](http://animovement.dev/aniframe/reference/get_metadata.md).

- constructor:

  Internal class constructor — `new_aniframe` or `new_anievent`.

## Value

`x` with the animovement class and metadata restored.
