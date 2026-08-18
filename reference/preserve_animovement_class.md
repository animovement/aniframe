# Re-clothe a dispatched result with its animovement classes and metadata

After a generic strips a result down to a plain tibble (via
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)), restore the
animovement classes the input carried and re-attach its metadata.

## Usage

``` r
preserve_animovement_class(x, cls, md)
```

## Arguments

- x:

  The bare result returned by
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html).

- cls:

  Class vector of the original input, captured before dispatch.

- md:

  Metadata captured before dispatch via
  [`get_metadata()`](http://animovement.dev/aniframe/reference/get_metadata.md).

## Value

`x` with the animovement classes and metadata restored.

## Details

dplyr rebuilds only the classes it knows how to reconstruct, so by the
time [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) returns,
the whole animovement family is gone — `aniframe` / `anievent` and any
subclass a downstream package has built on top of them. Restoring the
*incoming* stack rather than asserting a fixed one is what lets such a
subclass (e.g. `animetric`'s `aniframe_kin`) survive a pipeline without
registering methods of its own.

Order is preserved, so a subclass stays ahead of its parent and keeps
dispatch priority over it.

The metadata goes back through
[`write_metadata()`](http://animovement.dev/aniframe/reference/write_metadata.md)
rather than
[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md):
this is a round-trip of metadata that came off a valid object,
structural fields included, and
[`set_metadata()`](http://animovement.dev/aniframe/reference/set_metadata.md)
refuses those by design.
