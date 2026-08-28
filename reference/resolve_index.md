# Resolve the index from a metadata list

Objects serialised before the field existed have no `index`. They were
built when a literal `time` column was mandatory, so that is what they
are indexed by, and defaulting here keeps them working untouched.

## Usage

``` r
resolve_index(md)
```

## Arguments

- md:

  A metadata list.

## Value

Length-one character vector.

## Details

`NA` — how an
[`anievent()`](https://animovement.dev/anicore/reference/anievent.md)
spells "not applicable" — falls back the same way. The only path that
reaches here with anievent metadata is a cast to
[`aniframe()`](https://animovement.dev/anicore/reference/aniframe.md),
which needs *some* index;
[`get_index()`](https://animovement.dev/anicore/reference/get_index.md)
refuses the anievent before it gets this far.
